%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc A supervisor for the erljob.

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

-module(erljob_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([init/1]).

%% @equiv sup_utils:start_link(?MODULE, [])
start_link() -> sup_utils:start_link(?MODULE, []).

%% @equiv sup_utils:stop(?MODULE)
stop() -> sup_utils:stop(?MODULE).

%% @doc Callback for supervisor.
%% @spec init(_Args:[]) -> Spec:term()
init(_Args) ->
  sup_utils:spec(one_for_one, [
    sup_utils:worker_spec(erljob_db, []),
    sup_utils:supervisor_spec(erljob_job_sup, [])
  ]).

