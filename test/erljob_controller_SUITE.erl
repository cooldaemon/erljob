%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008

-module(erljob_controller_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/erljob_test.hrl").

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
 
