%%%-------------------------------------------------------------------
%% @doc spl_poc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(spl_poc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("util.hrl").

-define(SERVER, ?MODULE).

-define(WORKER(I, Mod, Args), #{
    id => I,
    start => {Mod, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [I]
}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },

    %% Each person holds 200000 LWT = 200 HNT
    %% except vihu and andrew cuz they will stake some validator(s)
    LWTHolders = #{
        evan => 200000 * ?STONES_PER_LWT,
        jay => 200000 * ?STONES_PER_LWT,
        % 20K HNT equivalent
        vihu => 200000 * 100 * ?STONES_PER_LWT,
        % 20K HNT equivalent
        andrew => 200000 * 100 * ?STONES_PER_LWT,
        amir => 200000 * ?STONES_PER_LWT,
        hashcode => 200000 * ?STONES_PER_LWT,
        marc => 200000 * ?STONES_PER_LWT,
        mark => 200000 * ?STONES_PER_LWT
    },

    lager:debug("LWT Holders (in stones): ~p", [LWTHolders]),

    %% Each person holds 2000000 5GT = 200 HNT
    FiveGTHolders = #{
        evan => 200000 * ?TONES_PER_5GT,
        jay => 200000 * ?TONES_PER_5GT,
        % 20K HNT equivalent
        vihu => 200000 * 100 * ?TONES_PER_5GT,
        % 20K HNT equivalent
        andrew => 200000 * 100 * ?TONES_PER_5GT,
        amir => 200000 * ?TONES_PER_5GT,
        hashcode => 200000 * ?TONES_PER_5GT,
        marc => 200000 * ?TONES_PER_5GT,
        mark => 200000 * ?TONES_PER_5GT
    },
    lager:debug("5GT Holders (in tones): ~p", [FiveGTHolders]),

    LWTBackedHNT = maps:fold(
        fun(_K, V, Acc) -> (V div ?HNT_TO_LWT_RATE) * ?BONES_PER_HNT + Acc end, 0, LWTHolders
    ),
    FiveGTBackedHNT = maps:fold(
        fun(_K, V, Acc) -> (V div ?HNT_TO_5GT_RATE) * ?BONES_PER_HNT + Acc end, 0, FiveGTHolders
    ),
    BackedHNT = LWTBackedHNT + FiveGTBackedHNT,
    lager:debug("Total BackedHNT (in bones): ~p", [BackedHNT]),

    ChildSpecs = [
        ?WORKER(po, price_oracle, []),
        ?WORKER(hnt_contract, hnt_contract, [
            #{},
            #{lwt_contract => LWTBackedHNT, fiveg_contract => FiveGTBackedHNT},
            #{lwt_contract => ?HNT_TO_LWT_RATE, fiveg_contract => ?HNT_TO_5GT_RATE}
        ]),
        ?WORKER(lwt_contract, lwt_contract, [LWTHolders]),
        ?WORKER(fiveg_contract, fiveg_contract, [FiveGTHolders]),
        ?WORKER(lwt_chain, lwt_chain, [#{'tall-blonde-condor' => andrew}]),
        ?WORKER(fiveg_chain, fiveg_chain, [#{'zealous-fiery-wren' => vihu}]),
        ?WORKER(periodic_hotspot, periodic_hotspot, []),
        ?WORKER(periodic_fiveg_device, periodic_fiveg_device, [])
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
