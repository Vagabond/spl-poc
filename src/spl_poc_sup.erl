%%%-------------------------------------------------------------------
%% @doc spl_poc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(spl_poc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

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
    ChildSpecs = [
        ?WORKER(po, price_oracle, []),
        ?WORKER(hnt, hnt, [
            #{kenny => 20, kyle => 80},
            #{eric => 10, stan => 20},
            #{lwt => 1000}
        ]),
        ?WORKER(lwt, lwt, [])
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
