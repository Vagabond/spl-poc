%%%-------------------------------------------------------------------
%% @doc
%% == periodic_fiveg_device ==
%%
%% This just models 5G devices (base stations) getting adding to the helium blockchain
%% over time.
%%
%% Notably:
%%
%% - It has a fixed list of owners for demonstration purpose
%% - Generates some random hotspot name
%% - Calls fiveg_chain:add_device to add the device
%% - Repeat adding devices every 1-10 seconds
%%
%% @end
%%%-------------------------------------------------------------------

-module(periodic_fiveg_device).

-behaviour(gen_server).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/0]).

-define(owners, [vihu, andrew, amir, hashcode, marc, mark]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {}).

init([]) ->
    %% In order to be able to add devices to 5G chain,
    %% holders of 5GT must convert to HNT and then burn
    %% that HNT to 5GT DC.
    %% - Convert 200000000 5GT to HNT
    %% - Burn 50000 HNT to get 5GT DC
    FiveGTToConvert = 20000000000 div 100,
    HNTToBurn = 500000 div 20,
    ok = lists:foreach(
        fun(Owner) ->
            ok = fiveg_contract:convert_to_hnt(Owner, FiveGTToConvert),
            ok = hnt_contract:burn_into_l2(fiveg_contract, Owner, Owner, HNTToBurn)
        end,
        ?owners
    ),

    schedule_add_device(),
    {ok, #state{}}.

handle_info(schedule_add_device, State) ->
    schedule_add_device(),
    {noreply, State};
handle_info(_Any, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_call(_Any, _From, State) ->
    {reply, ok, State}.

schedule_add_device() ->
    erlang:send_after(rand:uniform(10000), self(), schedule_add_device),

    #{public := PubKey, secret := _PrivKey} = libp2p_crypto:generate_keys(ed25519),
    {ok, DeviceID} = erl_angry_purple_tiger:animal_name(libp2p_crypto:pubkey_to_bin(PubKey)),
    Owner = lists:nth(rand:uniform(length(?owners)), ?owners),
    lager:debug("Adding DeviceID ~p for ~p", [DeviceID, Owner]),
    fiveg_chain:add_device(Owner, list_to_atom(DeviceID)),

    ok.
