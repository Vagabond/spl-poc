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

-export([add_hotspot/2]).

-export([height/0, state/0]).

-record(validator, {
    %% validator owner pubkey
    owner,
    %% height at which the validator got staked
    init_height,
    %% validator stake happens immediately, but unstake re-credit happens over block time
    %% hence we have unstaking -> unstaked statuses
    status = staked :: staked | unstaking | unstaked,
    %% height at which the validator got unstaked
    unstake_height
}).

-record(state, {
    oracles = [],
    %% account => balance
    dc_balances = #{},
    %% address => owner
    hotspots = #{},
    %% val_address => #validator
    validators = #{},
    %% account => reward share
    pending_rewards = #{},
    height = 0
}).

-include("util.hrl").

height() ->
    gen_server:call(?MODULE, height, infinity).

state() ->
    gen_server:call(?MODULE, state, infinity).

add_hotspot(Payer, HotspotAddress) ->
    gen_server:call(?MODULE, {add_hotspot, Payer, HotspotAddress}, infinity).

start_link(InitialHotspots) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [InitialHotspots], []).

init([InitialHotspots]) ->
    erlang:send_after(rand:uniform(5000), self(), oracle),
    erlang:send_after(rand:uniform(5000), self(), reward),
    erlang:send_after(1000, self(), increment_height),
    {ok, #state{hotspots = InitialHotspots}}.

handle_info(increment_height, State = #state{height = Ht, validators = Validators}) ->
    erlang:send_after(1000, self(), increment_height),
    NewValidators = maps:fold(
        fun
            (_Key, #validator{status = unstaking, unstake_height = H}, Acc) when H =< Ht ->
                %% remove it
                %% XXX this is not how the chain works today, validators are not removed
                Acc;
            (K, V, Acc) ->
                maps:put(K, V, Acc)
        end,
        #{},
        Validators
    ),
    {noreply, State#state{height = Ht + 1, validators = NewValidators}};
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

            %% pick some random `CGCount` validators and reward their owners some lwt
            ValWinners = [
                O
             || {O, _} <- lists:sublist(shuffle(maps:values(State#state.validators)), ?CGCount)
            ],
            lager:debug("Rewarding validator owners ~p += 1", [ValWinners]),

            %% update pending_rewards for validator owners
            NewPendingRewards = lists:foldl(
                fun(W, Acc) ->
                    credit(W, 1, Acc)
                end,
                State#state.pending_rewards,
                ValWinners ++ [Winner]
            ),
            {noreply, State#state{pending_rewards = NewPendingRewards}}
    end;
handle_info(oracle, State = #state{oracles = Oracles0, pending_rewards = Rewards, height = Height}) ->
    lager:debug("Got oracle msg, pending_rewards: ~p", [Rewards]),
    erlang:send_after(rand:uniform(5000), self(), oracle),
    {ok, {Nonce, Ops}} = lwt_contract:oracle(),
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
                    ok = lwt_contract:update_from_chain(
                        Nonce,
                        length(ShortestOps),
                        Rewards,
                        Power,
                        Height,
                        %% just return the owner, not the whole record
                        maps:map(fun(_K, #validator{owner = O}) -> O end, State#state.validators)
                    ),
                    lager:debug("Protocol Power (# of hotspots) ~p", [Power]),
                    %% apply the pending operations list
                    {NewHolders, NewValidators} = lists:foldl(
                        fun
                            ({lwt_dc, Account, Value}, {HAcc, VAcc}) ->
                                lager:debug("LWTDC Crediting ~p with ~p", [Account, Value]),
                                {credit(Account, Value, HAcc), VAcc};
                            ({dc, Account, Value}, {HAcc, VAcc}) ->
                                lager:debug("Crediting ~p with ~p", [Account, Value]),
                                {credit(Account, Value, HAcc), VAcc};
                            ({stake_validator, Owner, ValidatorAddress}, {HAcc, VAcc}) ->
                                case maps:find(ValidatorAddress, VAcc) of
                                    {ok, CurrentOwner} ->
                                        %% don't steal from ruins
                                        lager:warning(
                                            "Validator ~p already exists with owner ~p (proposed owner ~p)",
                                            [ValidatorAddress, CurrentOwner, Owner]
                                        ),
                                        {HAcc, VAcc};
                                    error ->
                                        lager:debug("Adding validator: ~p, owner: ~p", [
                                            ValidatorAddress, Owner
                                        ]),
                                        {HAcc, add_validator(ValidatorAddress, Owner, Height, VAcc)}
                                end;
                            ({unstake_validator, Owner, ValidatorAddress}, {HAcc, VAcc}) ->
                                lager:debug("Unstake validator: ~p, owner: ~p", [
                                    ValidatorAddress,
                                    Owner
                                ]),
                                {HAcc, unstake_validator(ValidatorAddress, Height, VAcc)}
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

handle_call(
    state,
    _From,
    State = #state{
        oracles = Oracles,
        dc_balances = DCBalances,
        hotspots = Hotspots,
        validators = Validators,
        pending_rewards = PR,
        height = Ht
    }
) ->
    Reply = #{
        oracles => Oracles,
        dc_balances => DCBalances,
        hotspots => Hotspots,
        validators => Validators,
        pending_rewards => PR,
        height => Ht
    },
    {reply, {ok, Reply}, State};
handle_call(height, _From, State = #state{height = Ht}) ->
    {reply, {ok, Ht}, State};
handle_call({add_hotspot, Owner, HotspotAddress}, _From, State = #state{dc_balances = DCs}) ->
    case maps:get(Owner, DCs, 0) >= ?HotspotAddFee of
        true ->
            {reply, ok, State#state{
                hotspots = maps:put(HotspotAddress, Owner, State#state.hotspots),
                dc_balances = debit(Owner, ?HotspotAddFee, DCs)
            }};
        false ->
            {reply, {error, insufficient_balance}, State}
    end.

credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).

add_validator(Key, Owner, Height, Map) ->
    maps:put(Key, #validator{owner = Owner, init_height = Height}, Map).

unstake_validator(Key, Height, Map) ->
    maps:update_with(
        Key,
        fun(Validator) ->
            Validator#validator{
                status = unstaking, unstake_height = Height + ?ValidatorUnstakeBlocks
            }
        end,
        Map
    ).

shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].
