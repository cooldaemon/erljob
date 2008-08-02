%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module executes a job.

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

-module(erljob_job).

-export([start_link/1]).
-export([init/2]).
-export([start/1, stop/1, delete/1]).

start_link(Arg) ->
  proc_lib:start_link(?MODULE, init, [self(), Arg]).

init(Parent, {Function, FunctionArg, Interval, Count}) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Function, FunctionArg, Interval, Count, start).

loop(_Function, _FunctionArg, _Interval, 0, _State) -> ok;
loop(Function, FunctionArg, Interval, Count, State) ->
  receive
    start ->
      loop(Function, FunctionArg, Interval, Count, start);
    stop ->
      loop(Function, FunctionArg, Interval, Count, stop);
    delete ->
      ok
  after Interval ->
    case State of
      start ->
        loop(Function, Function(FunctionArg), Interval, Count - 1, State);
      _State -> 
        loop(Function, FunctionArg, Interval, Count, State)
    end
  end.

start(Pid)  -> Pid ! start.
stop(Pid)   -> Pid ! stop.
delete(Pid) -> Pid ! delete.

