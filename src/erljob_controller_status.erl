%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module has status for a job controller.

-module(erljob_controller_status).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([lookup/2, set/3]).
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

start_link({Name, Arg}) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Arg], []),
  erljob_status:set(Name, status, Pid),
  {ok, Pid}.

stop(Pid) ->
  gen_server:call(Pid, stop).

lookup(Pid, Name) ->
  gen_server:call(Pid, {lookup, Name}).

set(Pid, Name, Value) ->
  gen_server:cast(Pid, {set, {Name, Value}}).

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

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

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

handle_cast(_Message, State) ->
  {noreply, State}.

send_change_state(undefined) -> ok;
send_change_state(Pid)  -> Pid ! change_state.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

