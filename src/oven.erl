%%%-------------------------------------------------------------------
%%% @author mbenza
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2018 9:22 PM
%%%-------------------------------------------------------------------
-module(oven).
-author("mbenza").

-behaviour(gen_server).

%% API
-export([start_link/0, bake_bread/2, set_oven/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(BAKE_TIME_PER_BATCH, 3000).
-define(BATCH_SIZE, 5).

-record(state, {
  mode :: on | off
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%
% 1. Execute bake_bread, but in the OVEN process.  gen_server:call will invoke handle_call on the OVEN process
% 
bake_bread(Quantity, Callback) when is_integer(Quantity) andalso Quantity > 0 andalso is_function(Callback, 1) ->
  gen_server:call(?SERVER, {bake_bread, Quantity, Callback}, infinity).

set_oven(on) ->
  gen_server:call(?SERVER, {turn_on}, infinity);
set_oven(off) ->
  gen_server:call(?SERVER, {turn_off}, infinity).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  % Initialize to off
  {ok, #state{mode = off}}.

handle_call({bake_bread, _Quantity, _Callback}, _From, #state{mode = off} = State) ->
  % Don't bake bread if the oven is off
  {reply, {error, oven_off}, State};

handle_call({bake_bread, Quantity, Callback}, _From, #state{mode = on} = State) ->
  %
  % 2. Bake the bread.  Just defer to split_order.
  %
  util:log("Oven: Baking ~p loaves ~n", [Quantity]),
  split_order(Quantity, Callback),
  {reply, ok, State};

handle_call({turn_on}, _From, State) ->
  {reply, ok, State#state{mode = on}};

handle_call({turn_off}, _From, State) ->
  {reply, ok, State#state{mode = off}};

handle_call(_Request, _From, State) ->
  {reply, {error, unexpected_call}, State}.


handle_cast({do_bake_bread, _Quantity, _Callback}, #state{mode = off} = State) ->
  % TODO: Something witty
  {noreply, State};
handle_cast({do_bake_bread, Quantity, Callback}, State) ->
  timer:sleep(?BAKE_TIME_PER_BATCH),
  util:log("Oven: DING!  ~p loaves ready~n", [Quantity]),
  % The bread is done
  case Callback(Quantity) of
    ok ->
      ok;
    _Else ->
      % If our callback doesn't handle the bread being done, the bread and oven catch on fire
      throw(catch_fire)
  end,
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% 3. Bake the minimum of (Quantity, ?BATCH_SIZE).  If there's some left over, split it
%   into another bake
%
split_order(Quantity, Callback) when Quantity > 0 ->
  Count = min(Quantity, ?BATCH_SIZE),
  gen_server:cast(?SERVER, {do_bake_bread, min(Quantity, Count), Callback}),
  split_order(Quantity - Count, Callback);
split_order(_Quantity, _Callback) ->
  ok.

