%%%-------------------------------------------------------------------
%% @doc simple_app top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bakery_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Oven = {oven, {oven, start_link, []}, permanent, 6000, worker, [oven]},
    Baker = {baker, {baker, start_link, []}, permanent, 6000, worker, [baker]},
    {ok, {{one_for_all, 0, 1}, [Oven, Baker]}}.

%%====================================================================
%% Internal functions
%%====================================================================
