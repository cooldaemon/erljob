-module({{appid}}).

-export([start/0, stop/0]).
-export([set/3, unset/1, lookup/1]).

start() -> application:start({{appid}}).
stop()  -> application:stop({{appid}}).

set(Key, Value, Expire) -> {{appid}}_store:set(Key, Value, Expire).
unset(Key)              -> {{appid}}_store:unset(Key).
lookup(Key)             -> {{appid}}_store:lookup(Key).
