%%%-------------------------------------------------------------------
%%% @author mbenza
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Aug 2018 2:15 PM
%%%-------------------------------------------------------------------
-module(baker).
-author("mbenza").

-behaviour(gen_server).

%% API
-export([start_link/0, sell_bread/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_QUANTITY, 5).

-type queue_entry() :: {
    From :: term(),
    QuantityNeeded :: non_neg_integer(),
    QuantityRequested :: non_neg_integer()}.

-record(state, {
  in_stock :: non_neg_integer(),
  queue :: [queue_entry()]
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

sell_bread(Quantity) ->
  try
    gen_server:call(?SERVER, {sell_bread, Quantity}, infinity)
  catch
    error:timeout ->
      {error, out_of_bread}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  %
  % 1. Here is where the baker is initialized.  The baker turns on the oven and puts in some bread to bake.
  %
  
  util:log("%s: Baker: Opening bakery~n", []),
  ok = oven:set_oven(on),
  ok = oven:bake_bread(?DEFAULT_QUANTITY, fun bread_ready/1),
  
  %
  % 2. Set up the default state to an empty bakery.
  %
  {ok, #state{
    in_stock = 0,
    queue = []
  }}.

%
% 3. Handle a request to sell bread.  Note that Quantity is bound to the first and pattern matched
%    to the second.  So this function will be executed when the amount in stock == the amount requested
%
handle_call({sell_bread, Quantity}, _From, #state{in_stock = Quantity} = State) ->
  util:log("Baker: Kaching!  ~p loaves ordered.  Out of stock.  Baking more~n", [Quantity]),
  %
  % 4. Bake more bread.
  %
  case oven:bake_bread(Quantity, fun bread_ready/1) of
    ok ->
      {reply, {ok, lists:duplicate(Quantity, bread)}, State#state{in_stock = 0}};
    {error, _} = E ->
    % TODO
      {reply, E, State}
  end;
%
% 5. If less bread was ordered than in stock, sell that stock.
%
handle_call({sell_bread, Quantity}, _From, #state{in_stock = InStock} = State) when Quantity < InStock ->
  util:log("Baker: Kaching!  ~p loaves ordered.  ~p remaining~n", [Quantity, InStock - Quantity]),
  {reply, {ok, lists:duplicate(Quantity, bread)}, State#state{in_stock = InStock - Quantity}};
%
% 5. If more bread was ordered, take some from the stock, and allocate it to an order.  
%
handle_call({sell_bread, Quantity}, From, #state{in_stock = InStock, queue = Queue} = State) when Quantity > InStock ->
  OrderNumber = order_number(os:timestamp()),
  util:log("Baker: Kaching!  ~p loaves ordered, but only ~p in stock.  Allocating ~p to order ~p and baking ~p loaves~n",
            [Quantity, InStock, InStock, OrderNumber, Quantity - InStock + ?DEFAULT_QUANTITY]),
  case oven:bake_bread(Quantity - InStock + ?DEFAULT_QUANTITY, fun bread_ready/1) of
    ok ->
      %
      % 6. Set stock to 0, queue up the order represented as {IdValue, WaitingOnN, TotalRequested}
      %
      QueueEntry = {From, OrderNumber, Quantity - InStock, Quantity},
      {noreply, State#state{in_stock = 0, queue = Queue ++ [QueueEntry]}};
    {error, _} = E ->
      {reply, E, State}
  end;
  
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%
% SEE bread_ready/1 FIRST.
%
handle_cast({bread_ready, Quantity}, #state{in_stock = InStock, queue = Queue} = State) ->
  %
  % 8. Handle bread being ready.
  %
  {NewInStock, NewQueue} = handle_bread_ready(Quantity, InStock, Queue),
  {noreply, State#state{in_stock = NewInStock, queue = NewQueue }};
handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%
% 7. Cast a message to the *baker* process regardless of what process this function executes on.  (Casts have no reply)
%
bread_ready(Quantity) ->
  gen_server:cast(?SERVER, {bread_ready, Quantity}).

%
% 9. (Executing as baker process)  Fill up the stock, manage the selling queue
%    
handle_bread_ready(Quantity, InStock, [] = _Queue) ->
  % If no one is in the queue, fill up the stock
  util:log("Baker.  Queue empty.  Adding ~p to stock.  Total in stock: ~p~n", [Quantity, Quantity + InStock]),
  {Quantity + InStock, []};
handle_bread_ready(Quantity, InStock, [{From, OrderNumber, 0, QuantityRequested} | T]) ->
  % Handle an order being completed
  util:log("Baker.  Order ~p complete.  Sending ~p loaves~n", [OrderNumber, QuantityRequested]),
  gen_server:reply(From, {ok, lists:duplicate(QuantityRequested, bread)}),
  handle_bread_ready(Quantity, InStock, T);
handle_bread_ready(0, InStock, Queue) ->
  util:log("Baker.  No more loaves are ready~n", []),
  % If there's nothing left that's ready, no work left to do.
  {InStock, Queue};
handle_bread_ready(Quantity, InStock, [{From, OrderNumber, QuantityNeeded, QuantityRequested} | T]) ->
  % Partially fill an order.  Notice the tail call to keep processing the queue / bread remaining.
  ToOrder = lists:min([Quantity, QuantityNeeded]),
  util:log("Baker.  ~p loaves ready.  Allocating ~p loaves to order ~p.~n", [Quantity, ToOrder, OrderNumber]),
  UpdatedOrder = {From, OrderNumber, QuantityNeeded - ToOrder, QuantityRequested},
  handle_bread_ready(Quantity - ToOrder, InStock, [UpdatedOrder | T]).

order_number({_Megas, Secs, Millis}) ->
    ((Secs rem 1000) * 1000) + (Millis div 1000).
