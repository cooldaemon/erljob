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

-module(erljob_status_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("erljob_test.hrl").

all() -> [testcase1].

init_per_testcase(_TestCase, Config) ->
  erljob_status:start_link(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  erljob_status:stop(),
  ok.

testcase1() -> [].
testcase1(_Conf) ->
  ?assertEqual(erljob_status:lookup(foo, sup), undefined, case1),
  ?assertEqual(erljob_status:lookup(foo, status), undefined, case2),

  ?assertEqual(erljob_status:create(foo), ok, case3),
  ?assertEqual(erljob_status:create(foo), exist, case4),

  ?assertEqual(erljob_status:lookup(foo, sup), undefined, case5),
  ?assertEqual(erljob_status:lookup(foo, status), undefined, case6),

  erljob_status:set(foo, sup, bar),
  erljob_status:set(foo, status, baz),
  timer:sleep(200),
  ?assertEqual(erljob_status:lookup(foo, sup), bar, case7),
  ?assertEqual(erljob_status:lookup(foo, status), baz, case8),

  erljob_status:delete(foo),
  ?assertEqual(erljob_status:lookup(foo, sup), undefined, case9),
  ?assertEqual(erljob_status:lookup(foo, status), undefined, case10),

  ?assertEqual(erljob_status:create(foo), ok, case11),
  spawn_link(fun () ->
    timer:sleep(1500),
    erljob_status:set(foo, sup, bar)
  end),
  ?assertEqual(erljob_status:ensure_lookup(foo, sup), bar, case12),

  ok.

