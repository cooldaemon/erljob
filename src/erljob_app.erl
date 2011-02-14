%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc An application module for erljob.

-module(erljob_app).
-behaviour(application).

-export([start/2, stop/1]). 

start(_Type, _Args) -> erljob_sup:start_link().
stop(_State) -> ok.

