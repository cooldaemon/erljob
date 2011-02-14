%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc A supervisor for the erljob_controller_sup.

-module(erljob_controller_sup_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1, stop_child/1]).
-export([init/1]).

start_link() -> sup_utils:start_link(?MODULE).

start_child({Name, _State}=Arg) ->
  supervisor:start_child(
    ?MODULE,
    sup_utils:sup_spec(Name, erljob_controller_sup, [Arg], dynamic) 
  ).

stop_child(Name) ->
  supervisor:terminate_child(?MODULE, Name),
  supervisor:delete_child(?MODULE, Name).

init(_Args) ->
  sup_utils:spec().

