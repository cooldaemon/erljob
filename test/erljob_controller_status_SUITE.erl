%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008

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

-module(erljob_controller_status_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("erljob_test.hrl").

sequences() -> [{sequences1, [testcase1, testcase2]}].
all() -> [{sequence, sequences1}].

init_per_testcase(_TestCase, Config) ->
  erljob_status:start_link(),
  erljob_status:create(foo),
  Config.

end_per_testcase(_TestCase, _Config) ->
  erljob_status:stop(),
  ok.

testcase1() -> [].
testcase1(_Config) ->
  case erljob_controller_status:start_link(
    {foo, {fun (_State) -> bar end, bar, 100, 10, run}}
  ) of
    {ok, Pid} ->
      ?assertEqual(erljob_controller_status:stop(Pid), stopped, case2);
    Error ->
      ct:fail({case1, Error})
  end.

testcase2() -> [].
testcase2(_Config) ->
  Job = fun (_State) -> bar end,
  {ok, Pid} = erljob_controller_status:start_link(
    {foo, {Job, bar, 100, 10, run}}
  ),
  timer:sleep(500),
  ?assertEqual(erljob_status:lookup(foo, status), Pid, case2),

  GetAssert = fun (Name, Expr, Msg) ->
    ?assertEqual(erljob_controller_status:lookup(Pid, Name), Expr, Msg)
  end,
  GetAssert(job, Job, case3),
  GetAssert(job_state, bar, case4),
  GetAssert(interval, 100, case5),
  GetAssert(count, 10, case6),
  GetAssert(run_state, run, case7),
  GetAssert(controller_pid, undefined, case8),

  SetAssert = fun (Name, Expr, Msg) ->
    erljob_controller_status:set(Pid, Name, Expr),
    timer:sleep(500),
    GetAssert(Name, Expr, Msg)
  end,
  SetAssert(controller_pid, self(), case9),
  SetAssert(job_state, baz, case10),
  SetAssert(count, 100, case11),

  Job2 = fun (_State) -> bar end,
  SetAssert(job, Job2, case12),
  controller_mock(case13),

  SetAssert(interval, 1000, case14),
  controller_mock(case15),

  SetAssert(run_state, finish, case15),
  controller_mock(case16),

  erljob_controller_status:stop(Pid),
  ok.

controller_mock(Msg) ->
  receive
    change_state -> ok
  after 500 ->
    ct:fail({Msg, timeout})
  end.
 
