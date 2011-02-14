%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module stops erljob_controller_sup.

-module(erljob_cleaner).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([exit_job/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

exit_job(Name) ->
  gen_server:cast(?MODULE, {exit_job, Name}).

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, {}}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({exit_job, Name}, State) ->
  erljob_status:delete(Name),
  erljob_controller_sup_sup:stop_child(Name),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

