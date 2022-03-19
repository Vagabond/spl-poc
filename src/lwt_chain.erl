-module(lwt_chain).

%% this module pretends to be a L2 chain, in this case Helium's chain

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/1]).

-export([add_hotspot/2]).

-record(state, {
          oracles = [],
          dc_balances = #{}, %% account => balance
          hotspots = #{}, %% address => owner
          pending_rewards = #{} %% account => reward share
         }).

-define(HotspotAddFee, 100).

add_hotspot(Payer, HotspotAddress) ->
    gen_server:call(?MODULE, {add_hotspot, Payer, HotspotAddress}, infinity).

start_link(InitialHotspots) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [InitialHotspots], []).

init([InitialHotspots]) ->
    erlang:send_after(rand:uniform(5000), self(), oracle),
    erlang:send_after(rand:uniform(500), self(), reward),
    {ok, #state{hotspots=InitialHotspots}}.

handle_info(reward, State = #state{hotspots=Hotspots}) ->
    erlang:send_after(rand:uniform(500), self(), reward),
    case maps:size(Hotspots) of
        0 ->
            %% noone to reward
            {noreply, State};
        N ->
            %% pick a random hotspot and give them an award
            Winner = lists:nth(rand:uniform(N), maps:values(Hotspots)),
            {noreply, State#state{pending_rewards = credit(Winner, 1, State#state.pending_rewards)}}
    end;
handle_info(oracle, State = #state{oracles=Oracles0, pending_rewards=Rewards}) ->
    erlang:send_after(rand:uniform(5000), self(), oracle),
    {ok, {Nonce, Ops}} = lwt:oracle(),
    %% see if we have 3 oracles with this nonce and we have rewards to do
    Oracles = [ {N, O} || {N, O} <- Oracles0 ++ [{Nonce, Ops}], N == Nonce ],
    case length(Oracles) >= 3 andalso maps:size(Rewards) > 0 of
        false ->
            {noreply, State#state{oracles=Oracles}};
        true ->
            %% find the shortest common operations prefix
            SortedOracles = lists:sort(fun({_, A}, {_, B}) ->
                                length(A) =< length(B)
                        end, Oracles),
            [{_, ShortestOps}|_] = SortedOracles,
            case lists:all(fun({_, O}) ->
                                   lists:prefix(ShortestOps, O)
                           end, SortedOracles) of
                true ->
                    %% ok, all the oracles are coherent
                    Power = maps:size(State#state.hotspots),
                    ok = lwt:update_from_chain(Nonce, length(ShortestOps), Rewards, Power),
                    %% apply the pending operations list
                    NewHolders = lists:foldl(fun({dc, Account, Value}, Acc) ->
                                                     credit(Account, Value, Acc)
                                             end, State#state.dc_balances, ShortestOps),
                    {noreply, State#state{oracles=[], pending_rewards=#{}, dc_balances=NewHolders}};
                false ->
                    {noreply, State#state{oracles=[]}}
            end
    end;
handle_info(_Any, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_call({add_hotspot, Owner, HotspotAddress}, _From, State = #state{dc_balances = DCs}) ->
    case maps:get(Owner, DCs, 0) >= ?HotspotAddFee of
        true ->
            {reply, ok, State#state{hotspots=maps:put(HotspotAddress, Owner, State#state.hotspots),
                                    dc_balances = debit(Owner, ?HotspotAddFee, DCs)}};
        false ->
            {reply, {error, insufficient_balance}, State}
    end.


credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).

