%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc erljob is a job scheduler.
%%
%%  Here's a quick example illustrating how to use erljob: 
%%  ```
%%    erljob:start(),
%%    erljob:add_job(hi, fun (_X) -> io:fwrite("Hi!"), ok end, ok, 1000, 2),
%%    erljob:stop()
%%  '''

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

-module(erljob).

-export([start/0, stop/0]). 
-export([add_job/5, delete_job/1, restart_job/1, suspend_job/1]). 

%% @equiv application:start(erljob, permanent)
start() ->
  application:start(erljob, permanent).

%% @equiv application:stop(erljob)
stop() -> application:stop(erljob).

add_job(Name, Job, State, Interval, Count) ->
  add_job(erljob_status:create(Name), Name, Job, State, Interval, Count).

add_job(exist, _Name, _Job, _State, _Interval, _Count) -> exist;
add_job(ok, Name, Job, State, Interval, Count) ->
  {ok, Pid} = erljob_controller_sup_sup:start_child(
    {Name, {Job, State, Interval, Count, run}}
  ),
  erljob_status:set(Name, sup, Pid),
  ok.

delete_job(Name)  -> modify_run_state(Name, finish).
restart_job(Name) -> modify_run_state(Name, run).
suspend_job(Name) -> modify_run_state(Name, suspend).

modify_run_state(Name, RunState) ->
  erljob_controller_status:set(
    erljob_status:lookup(Name, status), run_status, RunState
  ).

