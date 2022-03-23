%%%-------------------------------------------------------------------
%% @doc
%% This module models the LoRaWAN Helium chain as a L2.
%% @end
%%%-------------------------------------------------------------------

-module(lwt_chain).

%% this module pretends to be a L2 chain, in this case Helium's chain

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/1]).

-export([add_hotspot/2, fund_owner/2, get_validator_init_ht/1]).

-record(state, {
    oracles = [],
    %% account => balance
    dc_balances = #{},
    %% address => owner
    hotspots = #{},
    %% val_address => {owner, init_height}
    validators = #{},
    %% account => reward share
    pending_rewards = #{}
}).

-define(HotspotAddFee, 100).
%% Number of validators to reward
-define(CGCount, 2).

fund_owner(Payer, DCAmt) ->
    gen_server:call(?MODULE, {fund_owner, Payer, DCAmt}, infinity).

add_hotspot(Payer, HotspotAddress) ->
    gen_server:call(?MODULE, {add_hotspot, Payer, HotspotAddress}, infinity).

get_validator_init_ht(ValidatorAddress) ->
    gen_server:call(?MODULE, {get_validator_init_ht, ValidatorAddress}, infinity).

start_link(InitialHotspots) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [InitialHotspots], []).

init([InitialHotspots]) ->
    erlang:send_after(rand:uniform(5000), self(), oracle),
    erlang:send_after(rand:uniform(5000), self(), reward),
    {ok, #state{hotspots = InitialHotspots}}.

handle_info(reward, State = #state{hotspots = Hotspots}) ->
    erlang:send_after(rand:uniform(5000), self(), reward),
    case maps:size(Hotspots) of
        0 ->
            %% noone to reward
            {noreply, State};
        N ->
            %% pick a random hotspot and give them an award
            Winner = lists:nth(rand:uniform(N), maps:values(Hotspots)),
            lager:debug("Rewarding hotspot owner ~p += 1", [Winner]),

            %% pick some random k validators and reward their owners some lwt
            ValWinners = [
                O
             || {O, _} <- lists:sublist(shuffle(maps:values(State#state.validators)), ?CGCount)
            ],
            lager:debug("Rewarding validator owners ~p += 1", [ValWinners]),

            NewPendingRewards = lists:foldl(
                fun(W, Acc) ->
                    credit(W, 1, Acc)
                end,
                State#state.pending_rewards,
                ValWinners ++ [Winner]
            ),
            {noreply, State#state{pending_rewards = NewPendingRewards}}
    end;
handle_info(oracle, State = #state{oracles = Oracles0, pending_rewards = Rewards}) ->
    lager:debug("Got oracle msg, pending_rewards: ~p", [Rewards]),
    erlang:send_after(rand:uniform(5000), self(), oracle),
    {ok, {Nonce, Ops}} = lwt:oracle(),
    %% see if we have 3 oracles with this nonce and we have rewards to do
    Oracles = [{N, O} || {N, O} <- Oracles0 ++ [{Nonce, Ops}], N == Nonce],
    case length(Oracles) >= 3 andalso maps:size(Rewards) > 0 of
        false ->
            {noreply, State#state{oracles = Oracles}};
        true ->
            %% find the shortest common operations prefix
            SortedOracles = lists:sort(
                fun({_, A}, {_, B}) ->
                    length(A) =< length(B)
                end,
                Oracles
            ),
            [{_, ShortestOps} | _] = SortedOracles,
            case
                lists:all(
                    fun({_, O}) ->
                        lists:prefix(ShortestOps, O)
                    end,
                    SortedOracles
                )
            of
                true ->
                    %% ok, all the oracles are coherent
                    Power = maps:size(State#state.hotspots),
                    ok = lwt:update_from_chain(Nonce, length(ShortestOps), Rewards, Power),
                    lager:debug("Protocol Power (# of hotspots) ~p", [Power]),
                    %% apply the pending operations list
                    {NewHolders, NewValidators} = lists:foldl(
                        fun
                            ({dc, Account, Value}, {HAcc, VAcc}) ->
                                lager:debug("Crediting ~p with ~p", [Account, Value]),
                                {credit(Account, Value, HAcc), VAcc};
                            (
                                {stake_validator, Owner, ValidatorAddress, InitHeight},
                                {HAcc, VAcc}
                            ) ->
                                lager:debug("Adding validator: ~p, owner: ~p", [
                                    ValidatorAddress, Owner
                                ]),
                                {HAcc, add_validator(ValidatorAddress, {Owner, InitHeight}, VAcc)};
                            (
                                {unstake_validator, Owner, ValidatorAddress},
                                {HAcc, VAcc}
                            ) ->
                                lager:debug("Removing validator: ~p, owner: ~p", [
                                    ValidatorAddress, Owner
                                ]),
                                {HAcc, remove_validator(ValidatorAddress, VAcc)}
                        end,
                        {State#state.dc_balances, State#state.validators},
                        ShortestOps
                    ),
                    {noreply, State#state{
                        oracles = [],
                        pending_rewards = #{},
                        dc_balances = NewHolders,
                        validators = NewValidators
                    }};
                false ->
                    {noreply, State#state{oracles = []}}
            end
    end;
handle_info(_Any, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_call({fund_owner, Payer, DCAmt}, _From, State = #state{dc_balances = DCs}) ->
    {reply, ok, State#state{dc_balances = credit(Payer, DCAmt, DCs)}};
handle_call({add_hotspot, Owner, HotspotAddress}, _From, State = #state{dc_balances = DCs}) ->
    case maps:get(Owner, DCs, 0) >= ?HotspotAddFee of
        true ->
            {reply, ok, State#state{
                hotspots = maps:put(HotspotAddress, Owner, State#state.hotspots),
                dc_balances = debit(Owner, ?HotspotAddFee, DCs)
            }};
        false ->
            {reply, {error, insufficient_balance}, State}
    end;
handle_call(
    {get_validator_init_ht, ValidatorAddress}, _From, State = #state{validators = Validators}
) ->
    case lists:member(ValidatorAddress, maps:keys(Validators)) of
        false ->
            {reply, {error, unknown_validator2}, State};
        true ->
            {_Owner, InitHeight} = maps:get(ValidatorAddress, Validators),
            {reply, {ok, InitHeight}, State}
    end.

credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).

add_validator(Key, Value, Map) ->
    maps:put(Key, Value, Map).

remove_validator(Key, Map) ->
    maps:remove(Key, Map).

shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].
