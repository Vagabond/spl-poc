-module(lwt).

-behaviour(gen_server).

%% HNT to LWT, does not change
-define(ExchangeRate, 1000).

-record(state, {
    nonce = 0,
    holders = #{},
    %% pending amount of HNT that needs to be destroyed
    burns = 0,
    %% some stack of pending operations we need to do to the l2
    pending_operations = []
}).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/0]).

-export([transfer/3, convert_to_hnt/2, burn_to_dc/3]).

-export([oracle/0, update_from_chain/4]).

%% TODO staking/unstaking validators, ugh

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

transfer(Payer, Payee, Amount) ->
    gen_server:call(?MODULE, {transfer, Payer, Payee, Amount}, infinity).

convert_to_hnt(Payer, Amount) ->
    %% essentially we destroy some LWT and then send some of the HNT this contract
    %% controls to the Payer's address via the hnt contract api
    gen_server:call(?MODULE, {convert, Payer, Amount}, infinity).

burn_to_dc(Burner, Burnee, Amount) ->
    %% destroy LWT and mark the equivalent amount of HNT to be burned next time we
    %% send an update to the hnt contract
    gen_server:call(?MODULE, {burn, Burner, Burnee, Amount}, infinity).

oracle() ->
    %% get the nonce and the l2 pending operations stack
    %% multiple attempts at oracling may give longer lists of pending operations
    %% but we can simply select the longest common prefix
    gen_server:call(?MODULE, oracle, infinity).

update_from_chain(Nonce, OpCount, RewardShares, Power) ->
    gen_server:call(?MODULE, {update, Nonce, OpCount, RewardShares, Power}, infinity).

init([]) ->
    {ok, #state{}}.

handle_info(_Any, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_call({transfer, Payer, Payee, Amt}, _From, State) when Amt > 0 ->
    PayerHolding = maps:get(Payer, State#state.holders, 0),

    case PayerHolding =< Amt of
        %% cannot zero out payer on transfer
        true ->
            throw({reply, {error, insufficient_transfer_balance}, State});
        false ->
            NewHolders = credit(Payee, Amt, debit(Payer, Amt, State#state.holders)),
            {reply, ok, State#state{holders = NewHolders}}
    end;
handle_call({convert, Payer, Amount}, _From, State) ->
    case maps:get(Payer, State#state.holders, 0) >= Amount of
        false ->
            throw({reply, {error, insufficient_balance}, State});
        true ->
            ok
    end,
    NewHolders = debit(Payer, Amount, State#state.holders),
    %% module is a lazy identifier for this contract, would be a pubkey normally
    ok = hnt:transfer_hnt(?MODULE, Payer, Amount div ?ExchangeRate),
    {reply, ok, State#state{holders = NewHolders}};
handle_call({burn, Burner, Burnee, Amount}, _From, State) ->
    case maps:get(Burner, State#state.holders, 0) >= Amount of
        false ->
            throw({reply, {error, insufficient_balance}, State});
        true ->
            ok
    end,
    %% calculate the amount of DC this LWT is worth by converting LWT to HNT and
    %% then consulting the HNT price oracle.
    HNT = Amount div ?ExchangeRate,
    {ok, Price} = price_oracle:get_price(),
    %% TODO I forget the math to calculate DC here, fix it later
    DC = HNT * Price,
    {reply, ok, State#state{
        pending_operations = State#state.pending_operations ++ [{dc, Burnee, DC}],
        burns = State#state.burns + HNT
    }};
handle_call(oracle, _From, State) ->
    {reply, {ok, {State#state.nonce, State#state.pending_operations}}, State};
handle_call(
    {update, Nonce, OpCount, RewardShares, Power},
    _From,
    State = #state{nonce = Nonce, pending_operations = Ops}
) ->
    lager:info("LWT got an update msg, Current Holders: ~p", [State#state.holders]),
    {ok, HNT} = hnt:update_from_l2(?MODULE, Nonce, Power, State#state.burns),
    %% ok, we got some HNT, now we need to convert that to LWT and disburse it according to the reward shares
    LWT = HNT * ?ExchangeRate,
    TotalShares = maps:fold(
        fun(_K, V, Acc) ->
            Acc + V
        end,
        0,
        RewardShares
    ),
    RewardShare = LWT / TotalShares,
    NewHolders = maps:fold(
        fun(K, V, Acc) ->
            credit(K, trunc(V * RewardShare), Acc)
        end,
        State#state.holders,
        RewardShares
    ),
    lager:info("New Holders: ~p", [NewHolders]),
    %% Now we need to remove the first `OpCount' operations from our pending operations stack, zero out our burns
    %% and increment our nonce
    {reply, ok, State#state{
        nonce = Nonce + 1,
        pending_operations = lists:sublist(Ops, OpCount + 1, length(Ops)),
        burns = 0,
        holders = NewHolders
    }}.

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).

credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).
