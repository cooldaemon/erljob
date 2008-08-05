%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module has several functions for a supervisor.

%% Copyright 2008 Masahito Ikuta
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(sup_utils).

-export([start_link/2, stop/1]).
-export([
  spec/2,
  worker_spec/2, worker_spec/3, worker_spec/4, worker_spec/5,
  sup_spec/2, sup_spec/4
]).

-define(MAX_RESTART, 9999999).
-define(TIME, 1).
-define(SHUTDOWN_WAITING_TIME, 2000).

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

spec(RestartStrategy, ChildSpecs) ->
  {ok, {{RestartStrategy, ?MAX_RESTART, ?TIME}, ChildSpecs}}.

worker_spec(Module, Args) ->
  worker_spec(Module, start_link, Args).

worker_spec(Module, Function, Args) ->
  {
    Module,
    {Module, Function, Args},
    permanent,
    ?SHUTDOWN_WAITING_TIME,
    worker,
    []
  }.

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

