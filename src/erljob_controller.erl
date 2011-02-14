%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2008
%% @doc This module executes a job.

-module(erljob_controller).

-export([start_link/1]).
-export([init/2]).

-define(SUSPEND_SLEEP_TIME, 1000).

start_link({Name, _State}) ->
  proc_lib:start_link(?MODULE, init, [self(), Name]).

init(Parent, Name) ->
  StatusPid = erljob_status:ensure_lookup(Name, status),
  proc_lib:init_ack(Parent, {ok, self()}),
  erljob_controller_status:set(StatusPid, controller_pid, self()),
  loop({Name, StatusPid}).

loop({_SupId, StatusPid}=State) ->
  loop(
    State,
    lists:map(fun (Name) ->
      erljob_controller_status:lookup(StatusPid, Name)
    end, [job, job_state, interval, count, run_state])
  ).

loop({SupId, _StatusPid}, [_Job, _JobState, _Interval, 0, _RunState]) ->
  finish(SupId);
loop({SupId, _StatusPid}, [_Job, _JobState, _Interval, _Count, finish]) ->
  finish(SupId);
loop(State, [_Job, _JobState, _Interval, _Count, suspend]) ->
  timer:sleep(?SUSPEND_SLEEP_TIME),
  loop(State);
loop(State, [_Job, _JobState, Interval, _Count, _RunState]=Status) ->
  receive
    change_state -> loop(State)
  after Interval ->
    run_job(State, Status)
  end.

run_job(
  {_SupId, StatusPid}=State, [Job, JobState, _Interval, Count, run]
) ->
  erljob_controller_status:set(
    StatusPid, job_state, exec_job(Job, JobState)
  ),
  decrement_count(StatusPid, Count),
  loop(State);
run_job(State, _Status) ->
  loop(State).

exec_job({M, F}, Arg) -> which_arg(catch M:F(Arg), Arg);
exec_job(Job, Arg)    -> which_arg(catch Job(Arg), Arg).
which_arg({'EXIT', _Reason}, Arg) -> Arg;
which_arg(Arg, _OldArg)           -> Arg.

decrement_count(_StatusPid, infinity) -> ok;
decrement_count(StatusPid, Count) ->
  erljob_controller_status:set(StatusPid, count, Count - 1).

finish(SupId) ->
  erljob_cleaner:exit_job(SupId),
  ok.

