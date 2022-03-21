-module(periodic_hotspot).

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/0]).

-define(owners, [vihu, andrew, amir, hashcode, marc, mark]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {}).

init([]) ->
    ok = lists:foreach(
        fun(Owner) ->
            lwt_chain:fund_owner(Owner, 100000)
        end,
        ?owners
    ),
    schedule_add_hotspot(),
    {ok, #state{}}.

handle_info(schedule_add_hotspot, State) ->
    schedule_add_hotspot(),
    {noreply, State};
handle_info(_Any, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_call(_Any, _From, State) ->
    {reply, ok, State}.

schedule_add_hotspot() ->
    erlang:send_after(rand:uniform(10000), self(), schedule_add_hotspot),

    #{public := PubKey, secret := _PrivKey} = libp2p_crypto:generate_keys(ed25519),
    {ok, HotspotName} = erl_angry_purple_tiger:animal_name(libp2p_crypto:pubkey_to_bin(PubKey)),
    Owner = lists:nth(rand:uniform(length(?owners)), ?owners),
    lager:debug("Adding hotspot ~p for ~p", [HotspotName, Owner]),
    lwt_chain:add_hotspot(Owner, list_to_atom(HotspotName)),

    ok.
