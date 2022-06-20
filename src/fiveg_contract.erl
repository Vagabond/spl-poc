%%%-------------------------------------------------------------------
%% @doc
%% This module models the 5G SubDAO contract
%% In its state it maintains:
%%
%% - Nonce of synced state with 5G chain
%%
%% - Holders of 5G tokens
%%
%% - Pending operations for the 5G chain
%%
%% @end
%%%-------------------------------------------------------------------

-module(fiveg_contract).

-behaviour(gen_server).

-include("util.hrl").

-record(state, {
    nonce = 0,
    holders = #{},
    %% some stack of pending operations we need to do to the l2
    pending_operations = [],
    chain_ht = 0
}).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/1]).

-export([transfer/3, convert_to_hnt/2]).

-export([oracle/0, update_from_chain/5]).

-export([state/0]).

%% @private
start_link(LWTHolders) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LWTHolders], []).

%% @doc Get LWT contract state
state() ->
    gen_server:call(?MODULE, state, infinity).

%% @doc Transfer `Amount' LWTs from `Payer' to `Payee'.
transfer(Payer, Payee, Amount) ->
    gen_server:call(?MODULE, {transfer, Payer, Payee, Amount}, infinity).

%% @doc Convert `Amount' of `Payer''s LWT into HNT at the HNT-LWT exchange rate.
convert_to_hnt(Payer, Amount) ->
    %% essentially we destroy some LWT and then send some of the HNT this contract
    %% controls to the Payer's address via the hnt contract api
    gen_server:call(?MODULE, {convert, Payer, Amount}, infinity).

%% @doc Function to get the state of the LWT contract.
oracle() ->
    %% get the nonce and the l2 pending operations stack
    %% multiple attempts at oracling may give longer lists of pending operations
    %% but we can simply select the longest common prefix
    gen_server:call(?MODULE, oracle, infinity).

update_from_chain(Nonce, OpCount, RewardShares, Power, ChainHt) ->
    gen_server:call(
        ?MODULE, {update, Nonce, OpCount, RewardShares, Power, ChainHt}, infinity
    ).

%% @private
init([LWTHolders]) ->
    {ok, #state{holders = LWTHolders}}.

%% @private
handle_info(_Any, State) ->
    {noreply, State}.

%% @private
handle_cast(_Any, State) ->
    {noreply, State}.

%% @private
handle_call(
    state,
    _From,
    State = #state{holders = Holders, chain_ht = ChainHt, nonce = Nonce}
) ->
    Reply = #{holders => Holders, chain_ht => ChainHt, nonce => Nonce},
    {reply, {ok, Reply}, State};
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
    ok = hnt_contract:transfer_hnt(?MODULE, Payer, Amount div ?HNT_TO_5GT_RATE),
    {reply, ok, State#state{holders = NewHolders}};
handle_call(oracle, _From, State) ->
    {reply, {ok, {State#state.nonce, State#state.pending_operations}}, State};
handle_call(
    {update, Nonce, OpCount, RewardShares, Power, ChainHt},
    _From,
    State = #state{nonce = Nonce, pending_operations = Ops}
) ->
    lager:debug("5GT got an update msg, Current Holders: ~p", [State#state.holders]),
    {ok, HNT, BurnHNT} = hnt_contract:update_from_l2(?MODULE, Nonce, Power),

    %% ok, we may have some HNT for burning to 5GT-DC, add this to pending_operations
    DCBurns = maps:fold(
        fun(Key, Amount, Acc) ->
            %% XXX: This conversion may be incorrect
            FiveGTDC = util:hnt_to_dc(Amount) * ?HNT_TO_5GT_RATE,
            [{fivegt_dc, Key, FiveGTDC} | Acc]
        end,
        [],
        BurnHNT
    ),
    lager:debug("New dc_burns: ~p", [DCBurns]),

    %% ok, we got some HNT, now we need to convert that to LWT and disburse it according to the reward shares
    FiveGT = HNT * ?HNT_TO_5GT_RATE,
    TotalShares = maps:fold(
        fun(_K, V, Acc) ->
            Acc + V
        end,
        0,
        RewardShares
    ),
    RewardShare = FiveGT / TotalShares,
    NewHolders0 = maps:fold(
        fun(K, V, Acc) ->
            credit(K, trunc(V * RewardShare), Acc)
        end,
        State#state.holders,
        RewardShares
    ),
    lager:debug("New Holders: ~p", [NewHolders0]),

    lager:debug("Ops ~p, OpCount ~p", [Ops, OpCount]),
    NewPendingOps = lists:sublist(Ops, OpCount + 1, length(Ops)),
    lager:debug("new pending ops ~p", [NewPendingOps]),
    %% Now we need to remove the first `OpCount' operations from our pending operations stack and increment our nonce
    {reply, ok, State#state{
        nonce = Nonce + 1,
        pending_operations = NewPendingOps ++ DCBurns,
        chain_ht = ChainHt
    }}.

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).

credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).
