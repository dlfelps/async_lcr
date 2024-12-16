-module(leader).
-export([run/1, timed_run/2, timed_run/1]).
-import(lists,[seq/2, map/2, zip/2, append/2, sort/1, foreach/2, filter/2, max/1, sum/1]).
-import(timer,[tc/3]).
-import(math,[pow/2, sqrt/1]).


build_ring(worst_case, Num_nodes) ->
  Pids = [spawn(async_lcr, disconnected,[]) || _ <- seq(1,Num_nodes)],
  Pids_sorted = sort(Pids),
  [H|T] = Pids_sorted,
  Rotated = append(T, [H]),
  Combined = zip(Pids_sorted, Rotated),
  foreach(fun({Neighbor_pid, Node_pid}) -> Node_pid ! {connect, Neighbor_pid} end, Combined), %reverse order for best case
  Pids_sorted;
build_ring(random_case, Num_nodes) ->
  Pids = [spawn(async_lcr, disconnected,[]) || _ <- seq(1,Num_nodes)],
  Pids_sorted = [X||{_,X} <- sort([ {rand:uniform(), N} || N <- Pids])], % sorted randomly
  [H|T] = Pids_sorted,
  Rotated = append(T, [H]),
  Combined = zip(Pids_sorted, Rotated),
  foreach(fun({Neighbor_pid, Node_pid}) -> Node_pid ! {connect, Neighbor_pid} end, Combined), 
  Pids_sorted;
build_ring(best_case, Num_nodes) ->
  Pids = [spawn(async_lcr, disconnected,[]) || _ <- seq(1,Num_nodes)],
  Pids_sorted = sort(Pids),
  [H|T] = Pids_sorted,
  Rotated = append(T, [H]),
  Combined = zip(Pids_sorted, Rotated),
  foreach(fun({Node_pid, Neighbor_pid}) -> Node_pid ! {connect, Neighbor_pid} end, Combined), %reverse order for best case
  Pids_sorted.


run(Num_nodes) ->
  Pids = build_ring(worst_case, Num_nodes),
  foreach(fun(Node_pid) -> Node_pid ! {self(), start} end, Pids),
  receive
    {leader_found} -> true
  end.

sample_variance(Times, Mean) ->
  Diff = map(fun(T) -> T - Mean end, Times ),
  Squared = map(fun(D) -> pow(D,2) end, Diff),
  Num_runs = length(Times),
  sum(Squared) / (Num_runs - 1).

timed_run(Num_nodes) ->
  tc(leader, run, [Num_nodes]).

timed_run(Num_nodes, Num_runs) ->
  Results = [timed_run(Num_nodes) || _ <- seq(1,Num_runs)],
  Times = [ X || {X,_} <- Results],
  Mean = sum(Times) / Num_runs,
  Std = sqrt(sample_variance(Times, Mean)),
  {Mean, Std}.


