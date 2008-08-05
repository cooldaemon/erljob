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

-export([start_link/1, stop/1]).
-export([init/1]).

start_link(Arg) -> supervisor:start_link(?MODULE, [Arg]).

stop(Pid) -> exit(Pid, normal).

init([{Name, _State}=Arg]) ->
  IdSuffix = binary_to_list(term_to_binary(Name)),
  sup_utils:spec(one_for_all,
    lists:map(fun (Module) ->
      sup_utils:worker_spec(
        atom_to_list(Module) ++ IdSuffix,
        transient, Module, [Arg]
      )
    end, [erljob_controller_status, erljob_controller])
  ).

