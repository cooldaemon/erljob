%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module has status for jobs.

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

-module(erljob_status).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([add/1, delete/1, dump/0, lookup/1]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%% @equiv gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @equiv gen_server:call(?MODULE, stop)
stop() ->
  gen_server:call(?MODULE, stop).

add(Arg) ->
  gen_server:cast(?MODULE, {add, Arg}).

delete(Name) ->
  gen_server:cast(?MODULE, {delete, Name}).

dump() ->
  gen_server:call(?MODULE, dump).

lookup(Name) ->
  gen_server:call(?MODULE, {lookup, Name}).

%% @spec init(_Args:[]) -> {ok, []}
init(_Args) ->
  process_flag(trap_exit, true),
  {ok, {ets:new(erljob_status, [bag, private])}}.

%% @type form() = {pid(), Tag}.

handle_call(dump, _From, {Ets}) ->
  {
    reply,
    ets:match(Ets, {'$1', {'$2', '$3', '$4', '$5', '$6'}}),
    {Ets} 
  };

handle_call({lookup, Name}, _From, {Ets}) ->
  {
    reply,
    ets:match(Ets, {'$1', {Name, '$3', '$4', '$5', '$6'}}),
    {Ets} 
  };

%% @doc stop server.
%% @spec handle_call(stop, _From:from(), State:term()) ->
%%  {stop, normal, stopped, State:term()}
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

%% @spec handle_call(_Message:term(), _From:from(), State:term()) ->
%%  {reply, ok, State:term()}.
handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({add, {Pid, Name, Job, Arg, Interval, Count}}, {Ets}) ->
  ets:insert(Ets, {Pid, {Name, Job, Arg, Interval, Count}}),
  {noreply, {Ets}};

handle_cast({delete, Name}, {Ets}) ->
  ets:match_detele(Ets, {'$1', {Name, '_', '_', '_', '_'}}),
  {noreply, {Ets}};

%% @spec handle_cast(_Message:term(), _State:term()) ->
%%  {noreply, State:term()}
handle_cast(_Message, State) ->
  {noreply, State}.

%% @spec handle_cast(_Info:term(), _State:term()) ->
%%  {noreply, State:term()}
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(_Reason:term(), _State:term()) -> ok
terminate(_Reason, _State) ->
  ok.

%% @spec code_change(_Reason:term(), _State:term()) -> ok
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

