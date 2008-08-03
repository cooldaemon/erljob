%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc A supervisor for the erljob_controller.

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

-module(erljob_controller_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/4]).
-export([init/1]).

-define(SHUTDOWN_WAITING_TIME, 2000).

%% @equiv sup_utils:start_link(?MODULE, [])
start_link() -> sup_utils:start_link(?MODULE, []).

%% @equiv supervisor:start_child(
%%  ?MODULE, [Job:term(), Arg:term(), Interval:integer(), Count:integer()])
start_child(Job, Arg, Interval, Count) ->
  supervisor:start_child(?MODULE, [{Job, Arg, Interval, Count}]).

%% @doc Callback for supervisor.
%% @spec init(_Args:[]) -> Spec:term()
init(_Args) ->
  sup_utils:spec(simple_one_for_one, [{
    undefined,
    {erljob_controller, start_link, []},
    temporary,
    ?SHUTDOWN_WAITING_TIME,
    worker,
    []
  }]).

