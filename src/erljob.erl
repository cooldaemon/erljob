%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc erljob is a job scheduler.
%%
%%  Here's a quick example illustrating how to use erljob: 
%%  ```
%%    erljob:start(),
%%    erljob:add_job(hi, fun () -> io:fwrite("Hi!") end, 10000, 10),
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
  add_jobs/2, add_job/4, delete_job/1,
  start_job/1, stop_job/1,
  lookup/0, lookup/1
]). 

%% @equiv application:start(erljob, permanent)
start() ->
  application:start(erljob, permanent).

%% @equiv application:stop(erljob)
stop() -> application:stop(erljob).

add_jobs(_Name, []) -> ok;
add_jobs(Name, [{Function, Interval, Count} | Jobs]) ->
  add_job(Name, Function, Interval, Count),
  add_jobs(Name, Jobs).

add_job(Name, Function, Interval, Count) ->
  {ok, Pid} = erljob_job_sup:start_child(Function, Interval, Count),
  erljob_db:add(Pid, Name, Function, Interval, Count),
  ok.

delete_job(Name) ->
  modify_job(Name, [fun erljob_job:delete/1, fun erljob_db:delete/1]).

start_job(Name) ->
  modify_job(Name, [fun erljob_job:start/1]).

stop_job(Name) ->
  modify_job(Name, [fun erljob_job:stop/1]).

modify_job(Name, Actions) ->
  modify_jobs(erljob_db:lookup(Name), Actions).

modify_jobs([], _Action) -> ok;
modify_jobs(
  [{Pid, _Name, _Function, _Interval, _Count, _State} | Jobs],
  Actions
) ->
  lists:foreach(fun (Action) -> Action(Pid) end, Actions),
  modify_jobs(Jobs, Actions).

%% @equiv erljob_db:lookup()
lookup() -> erljob_db:lookup().

%% @equiv erljob_db:lookup(Name)
lookup(Name) -> erljob_db:lookup(Name).

