%%%-------------------------------------------------------------------
%% @doc
%% This module models the 5G Helium chain as a L2.
%% @end
%%%-------------------------------------------------------------------

-module(fiveg_chain).

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/1]).

-export([add_device/2]).

-export([height/0, state/0]).

-include("util.hrl").

-record(state, {
    oracles = [],
    %% account => balance
    dc_balances = #{},
    %% account => reward share
    pending_rewards = #{},
    %% address => owner
    devices = #{},
    height = 0
}).

%% API

start_link(InitialDevices) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [InitialDevices], []).

height() ->
    gen_server:call(?MODULE, height, infinity).

state() ->
    gen_server:call(?MODULE, state, infinity).

add_device(Payer, DeviceID) ->
    gen_server:call(?MODULE, {add_device, Payer, DeviceID}, infinity).

%% Genserver Callbacks

init([InitialDevices]) ->
    erlang:send_after(rand:uniform(5000), self(), oracle),
    erlang:send_after(rand:uniform(5000), self(), reward),
    erlang:send_after(1000, self(), increment_height),
    {ok, #state{devices = InitialDevices}}.

handle_info(increment_height, State = #state{height = Ht}) ->
    erlang:send_after(2000, self(), increment_height),
    {noreply, State#state{height = Ht + 1}};
handle_info(reward, State = #state{devices = Devices}) ->
    erlang:send_after(rand:uniform(5000), self(), reward),
    case maps:size(Devices) of
        0 ->
            %% noone to reward
            {noreply, State};
        N ->
            %% pick a random hotspot and give them an award
            Winner = lists:nth(rand:uniform(N), maps:values(Devices)),
            lager:debug("~p, Rewarding owner ~p += 1", [?MODULE, Winner]),
            {noreply, State#state{pending_rewards = credit(Winner, 1, State#state.pending_rewards)}}
    end;
handle_info(oracle, State = #state{oracles = Oracles0, pending_rewards = Rewards, height = Height}) ->
    lager:debug("~p, Got oracle msg, pending_rewards: ~p", [?MODULE, Rewards]),
    erlang:send_after(rand:uniform(5000), self(), oracle),
    {ok, {Nonce, Ops}} = fiveg_contract:oracle(),
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
                    Power = maps:size(State#state.devices),
                    ok = fiveg_contract:update_from_chain(
                        Nonce,
                        length(ShortestOps),
                        Rewards,
                        Power,
                        Height
                    ),
                    lager:debug("Protocol: ~p, Power (# of devices): ~p", [?MODULE, Power]),
                    %% apply the pending operations list
                    NewHolders = lists:foldl(
                        fun({fivegt_dc, Account, Value}, HAcc) ->
                            lager:debug("5GTDC Crediting ~p with ~p", [Account, Value]),
                            credit(Account, Value, HAcc)
                        end,
                        State#state.dc_balances,
                        ShortestOps
                    ),
                    {noreply, State#state{
                        oracles = [],
                        pending_rewards = #{},
                        dc_balances = NewHolders
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
        pending_rewards = PR,
        dc_balances = DCBalances,
        devices = Devices,
        height = Ht
    }
) ->
    Reply = #{
        oracles => Oracles,
        pending_rewards => PR,
        height => Ht,
        dc_balances => DCBalances,
        devices => Devices
    },
    {reply, {ok, Reply}, State};
handle_call({add_device, Owner, DeviceID}, _From, State = #state{dc_balances = DCs}) ->
    case maps:get(Owner, DCs, 0) >= ?FiveGDeviceAddFee of
        true ->
            {reply, ok, State#state{
                devices = maps:put(DeviceID, Owner, State#state.devices),
                dc_balances = debit(Owner, ?FiveGDeviceAddFee, DCs)
            }};
        false ->
            {reply, {error, insufficient_balance}, State}
    end;
handle_call(height, _From, State = #state{height = Ht}) ->
    {reply, {ok, Ht}, State}.

credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).
