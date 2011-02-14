-module({{appid}}_sup).

-export([start_link/0, stop/0]).
-export([init/1]).

start_link() ->
  sup_utils:start_link(?MODULE).

stop() ->
  sup_utils:stop(?MODULE).

init(_Args) ->
  sup_utils:spec([
    {sup, erljob_sup},
    {worker, {{appid}}_store}
  ]).

