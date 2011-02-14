-module({{appid}}_SUITE).

-compile(export_all).

-include("ct.hrl").

sequences() -> [{seq, [
  set_get,
  set_unset_get,
  expire
]}].

all() -> [{sequence, seq}].

init_per_testcase(_TestCase, Config) ->
  ok = {{appid}}:start(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  {{appid}}:stop().

set_get(_Config) ->
  ok = {{appid}}:set(foo, bar, 0),
  timer:sleep(100),
  case {{appid}}:lookup(foo) of
    {ok, bar} -> ok;
    Other     -> ct:fail(Other)
  end.

set_unset_get(_Config) ->
  ok = {{appid}}:set(foo, bar, 0),
  timer:sleep(100),
  ok = {{appid}}:unset(foo),
  timer:sleep(100),
  case {{appid}}:lookup(foo) of
    {error, undefined} -> ok;
    Other              -> ct:fail(Other)
  end.

expire(_Config) ->
  ok = {{appid}}:set(foo, bar, 200),
  timer:sleep(100),
  case {{appid}}:lookup(foo) of
    {ok, bar} -> ok;
    Error     -> ct:fail(Error)
  end,
  timer:sleep(150),
  case {{appid}}:lookup(foo) of
    {error, undefined} -> ok;
    Ok                 -> ct:fail(Ok)
  end.
