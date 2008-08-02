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
-export([spec/2, worker_spec/2, worker_spec/3, supervisor_spec/2]).

-define(MAX_RESTART, 0).
-define(TIME, 1).
-define(SHUTDOWN_WAITING_TIME, 2000).

%% @equiv supervisor:start_link(
%%  {local, Name:atom()}, Name:atom(), Args:[term()])
start_link(Name, Args) ->
  supervisor:start_link({local, Name}, Name, Args).

%% @doc Stop the supervisor for Name.
%% @spec stop(Name:atom()) -> ok | not_started
stop(Name) ->
  case whereis(Name) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    _Other ->
      not_started
  end.

%% @doc Return the spec for a supervisor.
%% @spec spec(RestartStrategy:atom(), ChildSpecs:[term()]) ->
%%  {ok, {Option:term(), ChildSpecs:[term()]}}
spec(RestartStrategy, ChildSpecs) ->
  {ok, {{RestartStrategy, ?MAX_RESTART, ?TIME}, ChildSpecs}}.

%% @equiv worker_spec(Module:atom(), start_link, Args:[term()])
worker_spec(Module, Args) ->
  worker_spec(Module, start_link, Args).

%% @doc Return the worker spec for Module.
%% @spec worker_spec(Module:atom(), Function:atom(), Args:[term()]) ->
%%  WorkerSpec:term()
worker_spec(Module, Function, Args) ->
  {
    Module,
    {Module, Function, Args},
    permanent,
    ?SHUTDOWN_WAITING_TIME,
    worker,
    [Module]
  }.

%% @doc Return the supervisor spec for Module.
%% @spec worker_spec(Module:atom(), Args:[term()]) ->
%%  SupervisorSpec:term()
supervisor_spec(Module, Args) ->
  StartFunc = {Module, start_link, Args},
  {Module, StartFunc, permanent, infinity, supervisor, []}.

