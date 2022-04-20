-module(hnt_to_lwt_dc_burn_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/util.hrl").

-export([
    all/0, init_per_testcase/2, end_per_testcase/2
]).

-export([
    basic/1
]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [basic].

%%--------------------------------------------------------------------
%% TEST CASE SETUP
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    application:ensure_all_started(lager),
    application:ensure_all_started(spl_poc),
    Config.

%%--------------------------------------------------------------------
%% TEST CASE TEARDOWN
%%--------------------------------------------------------------------
end_per_testcase(_, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

basic(_Config) ->
    %% Starting with 0 hnt
    0 = hnt_contract:get_hnt_balance(vihu),
    0 = hnt_contract:get_hnt_balance(andrew),

    %% Convert some LWT to HNT
    ToConvert = 1000000000000,

    ok = lwt_contract:convert_to_hnt(vihu, ToConvert div 5),
    ok = lwt_contract:convert_to_hnt(andrew, ToConvert div 2),

    %% Wait 1 sec
    timer:sleep(1*1000),

    ?assertEqual(ToConvert div 5 div ?HNT_TO_LWT_RATE, hnt_contract:get_hnt_balance(vihu)),
    ?assertEqual(ToConvert div 2 div ?HNT_TO_LWT_RATE, hnt_contract:get_hnt_balance(andrew)),

    %% TODO: Add more checks around total LWT in circulation...

    lager:info("lwt_chain state: ~p", [lwt_chain:state()]),


    HNTTOBurn = 500000000,

    %% Burn some HNT for LWT-DC
    ok = hnt_contract:burn_into_l2(lwt_contract, maps:get(nonce, element(2, lwt_contract:state())), vihu, HNTTOBurn div 20),
    ok = hnt_contract:burn_into_l2(lwt_contract, maps:get(nonce, element(2, lwt_contract:state())), andrew, HNTTOBurn div 10),

    %% Wait 30 secs
    timer:sleep(30*1000),

    lager:info("lwt_chain state: ~p", [lwt_chain:state()]),

    ok.
