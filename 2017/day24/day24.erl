-module(day24).
-export([start/0]).

read_input(Filename)
  -> { ok, Input } = file:read_file(Filename),
     lists:map(fun(X) ->
                   [A,B|_] = re:split(X, "/"),
                   { AInt, _ } = string:to_integer(A),
                   { BInt, _ } = string:to_integer(B),
                   { AInt, BInt }
               end, re:split(string:chomp(Input), "\n")).

strongest_bridge(Input)
  -> strongest_bridge(Input, 0, 0, []).

strongest_bridge([], _LastEdge, Sum, Train)
  -> { Sum, Train };
strongest_bridge(Input, LastEdge, Sum, Train)
  -> { Matching, _ } = lists:partition(fun(X) ->
                                              { A, B } = X,
                                              (A == LastEdge) or (B == LastEdge)
                                          end, Input),
     if length(Matching) == 0 -> { Sum, Train };
        true ->
          MatchingSums = lists:map(fun(X) ->
                                      { _, Remaining } = lists:partition(fun(Y) -> Y == X end, Input),
                                      { A, B } = X,
                                      NewTrain = lists:append(Train, [X]),
                                      NewSum = Sum + A + B,
                                      if A == LastEdge -> strongest_bridge(Remaining, B, NewSum, NewTrain);
                                         true -> strongest_bridge(Remaining, A, NewSum, NewTrain)
                                      end
                                   end, Matching),
          [FirstSum|_] = lists:sort(fun(X, Y) ->
                                        { A, _ } = X,
                                        { B, _ } = Y,
                                        B =< A
                                    end, MatchingSums),
          FirstSum
     end.

longest_bridge(Input)
  -> longest_bridge(Input, 0, 0, []).

longest_bridge([], _LastEdge, Sum, Train)
  -> { Sum, Train };
longest_bridge(Input, LastEdge, Sum, Train)
  -> { Matching, _ } = lists:partition(fun(X) ->
                                              { A, B } = X,
                                              (A == LastEdge) or (B == LastEdge)
                                          end, Input),
     if length(Matching) == 0 -> { Sum, Train };
        true ->
          MatchingSums = lists:map(fun(X) ->
                                      { _, Remaining } = lists:partition(fun(Y) -> Y == X end, Input),
                                      { A, B } = X,
                                      NewTrain = lists:append(Train, [X]),
                                      NewSum = Sum + A + B,
                                      if A == LastEdge -> longest_bridge(Remaining, B, NewSum, NewTrain);
                                         true -> longest_bridge(Remaining, A, NewSum, NewTrain)
                                      end
                                   end, Matching),
          [FirstSum|_] = lists:sort(fun(X, Y) ->
                                        { A, I } = X,
                                        { B, J } = Y,
                                        if length(I) == length(J) -> B =< A;
                                           true -> length(J) =< length(I)
                                        end
                                    end, MatchingSums),
          FirstSum
     end.

start()
  -> SampleInput = read_input("sample"),
     io:fwrite("~p~n", [strongest_bridge(SampleInput)]),
     io:fwrite("~p~n", [longest_bridge(SampleInput)]),
     ProblemInput = read_input("problem"),
     io:fwrite("~p~n", [strongest_bridge(ProblemInput)]),
     io:fwrite("~p~n", [longest_bridge(ProblemInput)]).
