-module(hnt).

%% HNT minting contract, provides HNT to L2 contracts and
%% security token holders and manages balances for HNT and security tokens

-behaviour(gen_server).

-record(l2, {
    exchange_rate,
    nonce = 0,
    %% how much share should it get
    power = 1,
    %% how much we have to pay out since this l2 last updated
    pending_payouts = 0
}).

-record(state, {
    %% security token payout table
    security_holders = #{},
    %% who is holding HNT, includes contract addresses
    hnt_holders = #{},
    %% map of l2s and their conversion rate/last update time
    l2s = #{},
    %% whenever any l2 last updated us, used for tracking emissions
    last_update = erlang:monotonic_time(millisecond)
}).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/3]).

-export([
    transfer_security/3,
    transfer_hnt/3
    %get_hnt_balance/1,
    %get_security_balance/1
]).

-export([
    update_from_l2/3
]).

%% how many HNT per millisecond
-define(EmissionRate, 0.001).
%% security tokens reward %
-define(SecurityPercent, 0.35).
%% how many total security tokens can ever exist
-define(SecurityCount, 100).

start_link(SecurityHolders, HNTHolders, L2s) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SecurityHolders, HNTHolders, L2s], []).

transfer_security(Payer, Payee, Amount) ->
    gen_server:call(?MODULE, {transfer_security, Payer, Payee, Amount}, infinity).

transfer_hnt(Payer, Payee, Amount) ->
    gen_server:call(?MODULE, {transfer_hnt, Payer, Payee, Amount}, infinity).

update_from_l2(From, NewPower, Burns) ->
    gen_server:call(?MODULE, {update, From, NewPower, Burns}).

init([SecurityHolders, HNTHolders, L2s]) ->
    L2Recs = maps:map(
        fun(_K, V) ->
            #l2{exchange_rate = V}
        end,
        L2s
    ),

    {ok, #state{security_holders = SecurityHolders, hnt_holders = HNTHolders, l2s = L2Recs}}.

handle_info(_Any, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_call(
    {transfer_security, Payer, Payee, Amount},
    _From,
    State = #state{security_holders = SecurityHolders}
) ->
    case maps:get(Payer, SecurityHolders, 0) of
        Balance when Balance >= Amount ->
            NewSecurityHolders = credit(Payee, Amount, debit(Payer, Amount, SecurityHolders)),
            {reply, ok, State#state{security_holders = NewSecurityHolders}};
        _ ->
            {reply, {error, insufficient_balance}, State}
    end;
handle_call({transfer_hnt, Payer, Payee, Amount}, _From, State = #state{hnt_holders = HNTHolders}) ->
    case maps:get(Payer, HNTHolders, 0) of
        Balance when Balance >= Amount ->
            NewHNTHolders = credit(Payee, Amount, debit(Payer, Amount, HNTHolders)),
            {reply, ok, State#state{hnt_holders = NewHNTHolders}};
        _ ->
            {reply, {error, insufficient_balance}, State}
    end;
handle_call({update, From, Nonce, NewPower, Burns}, _From, State) ->
    %% so we have to do a bunch of stuff here
    %% * we have to compute how long it's been since the last
    %%   update from this l2 to understand how many HNT we are dealing with
    %% * We have to compute the rewards split
    %% * We have to credit that # of hnt to the l2 contract addresses
    %% * We have to do any HNT burns
    %% * We have to pay out security token dividends
    %% * Update the protocol power and the update times, etc
    %% * Return the HNT disbursed as the result

    case maps:find(From, State#state.l2s) of
        error ->
            throw({reply, {error, unknown_l2}, State});
        {ok, #l2{nonce = N}} when Nonce /= N ->
            throw({reply, {error, bad_l2_nonce}, State});
        {ok, _} ->
            %% make sure the amount to burn is less than we hold
            case maps:get(From, State#state.hnt_holders, 0) < Burns of
                true ->
                    throw({reply, {error, overburned}, State});
                false ->
                    ok
            end,
            Now = erlang:monotonic_time(millisecond),
            Elapsed = Now - State#state.last_update,
            NewState = emit(Elapsed, State),
            %% ok, now get the updated L2 and pull out all the
            %% pending payouts
            L2 = maps:get(From, NewState#state.l2s),
            %% apply the payments and the burns to the l2 contract's address
            %% Note that the burn could actually go back into some pre-mined amount
            %% or could be tracked for "contuining emissions" once HNT is "fully mined"
            NewHNTHolders = credit(
                From, L2#l2.pending_payouts, debit(From, Burns, NewState#state.hnt_holders)
            ),
            {reply, {ok, L2#l2.pending_payouts}, NewState#state{
                hnt_holders = NewHNTHolders,
                last_update = Now,
                l2s = maps:put(
                    From,
                    L2#l2{pending_payouts = 0, power = NewPower, nonce = L2#l2.nonce + 1},
                    NewState#state.l2s
                )
            }}
    end.

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).

credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).

emit(Elapsed, State) ->
    TotalToEmit = trunc(Elapsed * ?EmissionRate),
    SecurityAmount = trunc(TotalToEmit * ?SecurityPercent),
    L2Amount = TotalToEmit - SecurityAmount,
    SecurityShare = SecurityAmount div ?SecurityCount,
    %% pay out security dividends
    NewHNTHolders = maps:fold(
        fun(K, V, Acc) ->
            credit(K, V * SecurityShare, Acc)
        end,
        State#state.hnt_holders,
        State#state.security_holders
    ),
    %% pay out l2 contracts
    PowerSum = maps:fold(
        fun(_K, V, Acc) ->
            Acc + V#l2.power
        end,
        0,
        State#state.l2s
    ),
    L2Share = L2Amount div PowerSum,
    NewL2s = maps:map(
        fun(_K, V = #l2{pending_payouts = Pending, power = Power}) ->
            V#l2{pending_payouts = Pending + (Power * L2Share)}
        end,
        State#state.l2s
    ),
    State#state{l2s = NewL2s, hnt_holders = NewHNTHolders}.
