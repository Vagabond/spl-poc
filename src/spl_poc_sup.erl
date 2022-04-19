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
        vihu => 200000 * 100 * ?STONES_PER_LWT,         % 20K HNT equivalent
        andrew => 200000 * 100 * ?STONES_PER_LWT,       % 20K HNT equivalent
        amir => 200000 * ?STONES_PER_LWT,
        hashcode => 200000 * ?STONES_PER_LWT,
        marc => 200000 * ?STONES_PER_LWT,
        mark => 200000 * ?STONES_PER_LWT
    },

    lager:info("LWT Holders (in stones): ~p", [LWTHolders]),

    %% Total backed HNT = 20000 * 8 = 160000 HNT (stored in bones)
    BackedHNT = maps:fold(fun(_K, V, Acc) -> (V div ?HNT_TO_LWT_RATE) * ?BONES_PER_HNT + Acc end, 0, LWTHolders),
    lager:info("Total BackedHNT (in bones): ~p", [BackedHNT]),

    ChildSpecs = [
        ?WORKER(po, price_oracle, []),
        ?WORKER(hnt_contract, hnt_contract, [
            #{},
            #{lwt_contract => BackedHNT},
            #{lwt_contract => ?HNT_TO_LWT_RATE}
        ]),
        ?WORKER(lwt_contract, lwt_contract, [LWTHolders]),
        ?WORKER(lwt_chain, lwt_chain, [#{tall_blonde_condor => andrew}]),
        ?WORKER(periodic_hotspot, periodic_hotspot, [])
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
