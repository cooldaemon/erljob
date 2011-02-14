-module({{appid}}_store).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([set/3, unset/1, lookup/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%% I/F
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

set(Key, Value, Expire) ->
  gen_server:cast(?MODULE, {set, Key, Value, Expire}).

unset(Key) ->
  gen_server:cast(?MODULE, {unset, Key}).

lookup(Key) ->
  gen_server:call(?MODULE, {lookup, Key}).

%% Callbacks
init(_Args) ->
  {ok, {}}.

handle_call({lookup, Key}, _From, State) ->
  case get(Key) of
    undefined -> {reply, {error, undefined}, State};
    Value     -> {reply, {ok, Value}, State}
  end;

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({set, Key, Value, Expire}, State) ->
  put(Key, Value),
  set_expire(Key, Expire),
  {noreply, State};

handle_cast({unset, Key}, State) ->
  erase(Key),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal
set_expire(_Key, 0) ->
  ok;
set_expire(Key, Expire) ->
  erljob:add_job(Key, {?MODULE, unset}, Key, Expire, 1).

