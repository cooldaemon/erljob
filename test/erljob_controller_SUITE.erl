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

-module(erljob_controller_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("erljob_test.hrl").

all() -> [testcase1].

init_per_testcase(_TestCase, Config) ->
  erljob_status:start_link(),
  erljob_status:create(foo),

  Job = fun
    ({Pid, Sum}) -> Pid ! Sum, {Pid, Sum + 1};
    (State)      -> State
  end,
  erljob_controller_status:start_link(
    {foo, {Job, {self(), 0}, 100, 2, run}}
  ),

  Config.

end_per_testcase(_TestCase, _Config) ->
  erljob_controller_status:stop(
    erljob_status:lookup(foo, status)
  ),
  erljob_status:stop(),
  ok.

testcase1() -> [].
testcase1(_Config) ->
  case erljob_controller:start_link({foo, {}}) of
    {ok, _Pid} -> ok;
    Error      -> ct:fail({case1, Error})
  end,
  receiver(),
  ok.

receiver() ->
  receive
    0 -> receiver();
    1 -> ok
  after 200 ->
    ct:fail({case2, timeout})
  end.
 
