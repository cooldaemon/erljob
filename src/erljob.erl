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
-export([
  add_job/5,
  delete_job/1, resume_job/1, suspend_job/1,
  set_job/2, set_state/2, set_interval/2, set_count/2,
  lookup_run_state/1,
  lookup_job/1, lookup_state/1, lookup_interval/1, lookup_count/1
]). 

%% @equiv application:start(erljob, permanent)
start() ->
  application:start(erljob, permanent).

%% @equiv application:stop(erljob)
stop() -> application:stop(erljob).

%% @type job() = function() | {M::atom(), F::atom()}.

%% @doc add a job.
%% @spec 
%%   add_job(
%%     Name::term(),
%%     Job::job(),
%%     State::term(),
%%     Interval::integer(),
%%     Count::integer() | infinity
%%   ) -> ok | exist
add_job(Name, Job, State, Interval, Count) ->
  add_job(erljob_status:create(Name), Name, Job, State, Interval, Count).

add_job(exist, _Name, _Job, _State, _Interval, _Count) -> exist;
add_job(ok, Name, Job, State, Interval, Count) ->
  {ok, Pid} = erljob_controller_sup_sup:start_child(
    {Name, {Job, State, Interval, Count, run}}
  ),
  erljob_status:set(Name, sup, Pid),
  ok.

%% @doc delete a job.
%% @spec delete_job(Name::term()) -> ok
delete_job(Name) -> modify_status(Name, run_state, finish).

%% @doc resume a job.
%% @spec resume_job(Name::term()) -> ok
resume_job(Name) -> modify_status(Name, run_state, run).

%% @doc suspend a job.
%% @spec suspend_job(Name::term()) -> ok
suspend_job(Name) -> modify_status(Name, run_status, suspend).

%% @doc set a job.
%% @spec set_job(Name::term(), Job::job()) -> ok
set_job(Name, Job) -> modify_status(Name, job, Job).

%% @doc set a job state.
%% @spec set_state(Name::term(), State::term()) -> ok
set_state(Name, State) -> modify_status(Name, job_state, State).

%% @doc set a interval time.
%% @spec set_interval(Name::term(), Interval::integer()) -> ok
set_interval(Name, Interval) -> modify_status(Name, interval, Interval).

%% @doc set a run count.
%% @spec set_count(Name::term(), Count::integer()) -> ok
set_count(Name, Count) -> modify_status(Name, count, Count).

modify_status(Name, Key, Value) ->
  erljob_controller_status:set(
    erljob_status:lookup(Name, status), Key, Value
  ),
  ok.

%% @doc lookup a run state.
%% @spec lookup_run_state(Name::term()) -> run | suspend | finish
lookup_run_state(Name) -> lookup_status(Name, run_state).

%% @doc lookup a job.
%% @spec lookup_job(Name::term()) -> job()
lookup_job(Name) -> lookup_status(Name, job).

%% @doc lookup a job state.
%% @spec lookup_state(Name::term()) -> term()
lookup_state(Name) -> lookup_status(Name, job_state).

%% @doc lookup a interval time.
%% @spec lookup_interval(Name::term()) -> integer()
lookup_interval(Name) -> lookup_status(Name, interval).

%% @doc lookup a run count.
%% @spec lookup_count(Name::term()) -> integer()
lookup_count(Name) -> lookup_status(Name, count).

lookup_status(Name, Key) ->
  erljob_controller_status:lookup(
    erljob_status:lookup(Name, status), Key
  ).

