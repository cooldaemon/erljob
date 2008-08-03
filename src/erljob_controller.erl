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

-module(erljob_controller).

-export([start_link/1]).
-export([init/2]).
-export([restart/1, suspend/1, finish/1]).

start_link(Arg) ->
  proc_lib:start_link(?MODULE, init, [self(), Arg]).

init(Parent, Arg) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Arg, run).

loop({_Job, _JobArg, _Interval, 0}, _State) -> ok;
loop({_Job, _JobArg, Interval, _State}=Arg, State) ->
  receive
    restart -> loop(Arg, start);
    suspend -> loop(Arg, suspend);
    finish  -> ok
  after Interval ->
    run_job(Arg, State)
  end.

run_job({{Module, Function}=Job, JobArg, Interval, Count}, run) ->
  loop({Job, Module:Function(JobArg), Interval, Count - 1}, run);
run_job({Job, JobArg, Interval, Count}, run) ->
  loop({Job, Job(JobArg), Interval, Count - 1}, run);
run_job(Arg, State) ->
  loop(Arg, State).

restart(Pid) -> Pid ! restart.
suspend(Pid) -> Pid ! stop.
finish(Pid)  -> Pid ! finish.

