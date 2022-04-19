%%%-------------------------------------------------------------------
%% @doc
%% == price_oracle ==
%%
%% We assume a real price oracle contract exists, this is just a stub
%% as this should be a solved problem
%%
%% @end
%%%-------------------------------------------------------------------

-module(price_oracle).

-behaviour(gen_server).

-record(state, {price :: pos_integer()}).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/0, get_price/0, set_price/1]).

-include("util.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_price() ->
    gen_server:call(?MODULE, get_price, infinity).

set_price(Price) ->
    gen_server:call(?MODULE, {set_price, Price}, infinity).

init([]) ->
    %% 25 USD
    {ok, #state{price = 25 * ?ORACLE_PRICE_SCALING_FACTOR}}.

handle_info(_Any, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_call(get_price, _From, State) ->
    {reply, {ok, State#state.price}, State};
handle_call({set_price, Price}, _From, State) ->
    {reply, ok, State#state{price = Price}}.
