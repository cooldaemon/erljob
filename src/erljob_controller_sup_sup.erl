%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc A supervisor for the erljob_controller_sup.

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

-module(erljob_controller_sup_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1, stop_child/1]).
-export([init/1]).

start_link() -> sup_utils:start_link(?MODULE, []).

start_child({Name, _State}=Arg) ->
  supervisor:start_child(
    ?MODULE,
    sup_utils:sup_spec(Name, erljob_controller_sup, [Arg], dynamic) 
  ).

stop_child(Name) ->
  supervisor:terminate_child(?MODULE, Name),
  supervisor:delete_child(?MODULE, Name).

init(_Args) ->
  sup_utils:spec(one_for_one, []).

