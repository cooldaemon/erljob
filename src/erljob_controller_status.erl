%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module has status for a job controller.

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

-module(erljob_controller_status).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([lookup/2, set/3]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%% @equiv gen_server:start_link(?MODULE, [Arg:term()], [])
start_link({Name, Arg}) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Arg], []),
  erljob_status:set(Name, status, Pid),
  {ok, Pid}.

%% @equiv gen_server:call(Pid, stop)
stop(Pid) ->
  gen_server:call(Pid, stop).

lookup(Pid, Name) ->
  gen_server:call(Pid, {lookup, Name}).

set(Pid, Name, Value) ->
  gen_server:cast(Pid, {set, {Name, Value}}).

%% @spec init([Arg:term()]) -> {ok, Arg:term()}
init([{Job, JobState, Interval, Count, RunState}]) ->
  process_flag(trap_exit, true),
  {ok, {Job, JobState, Interval, Count, RunState, undefined}}.

handle_call(
  {lookup, Name},
  _From,
  {Job, JobState, Interval, Count, RunState, Pid}=State
) ->
  Result = case Name of
    job            -> Job;
    job_state      -> JobState;
    interval       -> Interval;
    count          -> Count;
    run_state      -> RunState;
    controller_pid -> Pid;
    _Other         -> unknown_name
  end,
  {reply, Result, State};

%% @doc stop server.
%% @spec handle_call(stop, _From:from(), State:term()) ->
%%  {stop, normal, stopped, State:term()}
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

%% @spec handle_call(_Message:term(), _From:from(), State:term()) ->
%%  {reply, ok, State:term()}.
handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast(
  {set, {Name, Value}},
  {Job, JobState, Interval, Count, RunState, Pid}=State
) ->
  NewState = case Name of
    job ->
      send_change_state(Pid),
      {Value, JobState, Interval, Count, RunState, Pid};
    job_state ->
      {Job, Value, Interval, Count, RunState, Pid};
    interval ->
      send_change_state(Pid),
      {Job, JobState, Value, Count, RunState, Pid};
    count ->
      {Job, JobState, Interval, Value, RunState, Pid};
    run_state ->
      send_change_state(Pid),
      {Job, JobState, Interval, Count, Value, Pid};
    controller_pid ->
      {Job, JobState, Interval, Count, RunState, Value};
    _Other ->
      State
  end,
  {noreply, NewState};

%% @spec handle_cast(_Message:term(), _State:term()) ->
%%  {noreply, State:term()}
handle_cast(_Message, State) ->
  {noreply, State}.

send_change_state(undefined) -> ok;
send_change_state(Pid)  -> Pid ! change_state.

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

