%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module has several functions for a supervisor.

-module(sup_utils).

-export([start_link/1, start_link/2, stop/1]).
-export([
  spec/0, spec/1, spec/2,
  worker_spec/1, worker_spec/2, worker_spec/3, worker_spec/4, worker_spec/5,
  sup_spec/1, sup_spec/2, sup_spec/4
]).

-define(MAX_RESTART, 9999999).
-define(TIME, 1).
-define(SHUTDOWN_WAITING_TIME, 2000).

start_link(Name) ->
  start_link(Name, []).

start_link(Name, Args) ->
  supervisor:start_link({local, Name}, Name, Args).

stop(Name) ->
  case whereis(Name) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    _Other ->
      not_started
  end.

spec() ->
  spec([]).

spec(ChildSpecs) ->
  spec(one_for_one, ChildSpecs).

spec(RestartStrategy, ChildSpecs) ->
  {ok, {
    {RestartStrategy, ?MAX_RESTART, ?TIME},
    lists:map(fun convert_spec/1, ChildSpecs)
  }}.

convert_spec({worker, Module}) ->
  worker_spec(Module);
convert_spec({worker, Module, Args}) ->
  worker_spec(Module, Args);
convert_spec({worker, Module, Function, Args}) ->
  worker_spec(Module, Function, Args);
convert_spec({worker, Id, RestartStrategy, Module, Args}) ->
  worker_spec(Id, RestartStrategy, Module, Args);
convert_spec({worker, Id, RestartStrategy, Module, Function, Args}) ->
  worker_spec(Id, RestartStrategy, Module, Function, Args);
convert_spec({sup, Module}) ->
  sup_spec(Module);
convert_spec({sup, Module, Args}) ->
  sup_spec(Module, Args);
convert_spec({sup, Id, Module, Args, Modules}) ->
  sup_spec(Id, Module, Args, Modules);
convert_spec(Spec) ->
  Spec.

worker_spec(Module) ->
  worker_spec(Module, []).

worker_spec(Module, Args) ->
  worker_spec(Module, start_link, Args).

worker_spec(Module, Function, Args) ->
  worker_spec(Module, permanent, Module, Function, Args).

worker_spec(Id, RestartStrategy, Module, Args) ->
  worker_spec(Id, RestartStrategy, Module, start_link, Args).

worker_spec(Id, RestartStrategy, Module, Function, Args) ->
  {
    Id,
    {Module, Function, Args},
    RestartStrategy,
    ?SHUTDOWN_WAITING_TIME,
    worker,
    []
  }.

sup_spec(Module) ->
  sup_spec(Module, []).

sup_spec(Module, Args) ->
  sup_spec(Module, Module, Args, []).

sup_spec(Id, Module, Args, Modules) ->
  {
    Id,
    {Module, start_link, Args},
    permanent,
    infinity,
    supervisor,
    Modules
  }.

