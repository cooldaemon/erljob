%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc A supervisor for the erljob.

-module(erljob_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([init/1]).

start_link() -> sup_utils:start_link(?MODULE).

stop() -> sup_utils:stop(?MODULE).

init(_Args) ->
  sup_utils:spec([
    {worker, erljob_status},
    {worker, erljob_cleaner},
    {sup, erljob_controller_sup_sup}
  ]).

