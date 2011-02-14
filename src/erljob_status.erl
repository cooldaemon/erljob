%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module has status for jobs.

-module(erljob_status).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([create/1, delete/1, set/3, lookup/2, ensure_lookup/2]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

-define(ENSURE_LOOKUP_SLEEP_TIME, 100).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

create(Name) ->
  gen_server:call(?MODULE, {create, Name}).

delete(Name) ->
  gen_server:cast(?MODULE, {delete, Name}).

set(Name, Key, Value) ->
  gen_server:cast(?MODULE, {set, Name, Key, Value}).

lookup(Name, Key) ->
  gen_server:call(?MODULE, {lookup, Name, Key}).

ensure_lookup(Name, Key) ->
  case gen_server:call(?MODULE, {lookup, Name, Key}) of
    undefined ->
      timer:sleep(?ENSURE_LOOKUP_SLEEP_TIME),
      ensure_lookup(Name, Key);
    Value ->
      Value
  end.

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, {ets:new(erljob_status, [bag, private])}}.

handle_call({lookup, Name, Key}, _From, {Ets}) ->
  lookup_reply(ets:lookup(Ets, Name), Key, {Ets});

handle_call({create, Name}, _From, {Ets}) ->
  create_reply(ets:lookup(Ets, Name), Name, {Ets});

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({delete, Name}, {Ets}) ->
  ets:delete(Ets, Name),
  {noreply, {Ets}};

handle_cast({set, Name, Key, Value}, {Ets}) ->
  Lookup = ets:lookup(Ets, Name),
  ets:delete(Ets, Name),
  set(Lookup, Key, Value, {Ets}),
  {noreply, {Ets}};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

lookup_reply([], _Key, State) ->
  {reply, undefined, State};
lookup_reply([{_Name, {Sup, _Status}}], sup, State) ->
  {reply, Sup, State};
lookup_reply([{_Name, {_Sup, Status}}], status, State) ->
  {reply, Status, State};
lookup_reply(_Lookup, _Key, State) ->
  {reply, unknown_key, State}.

create_reply([], Name, {Ets}) ->
  ets:insert(Ets, {Name, {undefined, undefined}}),
  {reply, ok, {Ets}};
create_reply(_Lookup, _Name, State) ->
  {reply, exist, State}.

set([], _Key, _Value, _State) ->
  ok;
set([{Name, {_Sup, Status}}], sup, Value, {Ets}) ->
  ets:insert(Ets, {Name, {Value, Status}});
set([{Name, {Sup, _Status}}], status, Value, {Ets}) ->
  ets:insert(Ets, {Name, {Sup, Value}});
set(_Lookup, _Key, _Value, _State) ->
  ok.

