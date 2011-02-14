%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc A supervisor for the erljob_controller.

-module(erljob_controller_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Arg) -> supervisor:start_link(?MODULE, [Arg]).

init([{Name, _State}=Arg]) ->
  IdSuffix = binary_to_list(term_to_binary(Name)),
  sup_utils:spec(
    lists:map(fun (Module) ->
      {
        worker,
        atom_to_list(Module) ++ IdSuffix,
        transient, Module, [Arg]
      }
    end, [erljob_controller_status, erljob_controller])
  ).

