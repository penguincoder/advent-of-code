-module(day13).
-export([start/0]).

calc_severity(Input)
  -> CalcMap = lists:foldl(fun(X, Sum) ->
                               [Index|Depth] = re:split(X, ": "),
                               { IndexInt, _ } = string:to_integer(Index),
                               { DepthInt, _ } = string:to_integer(Depth),
                               maps:put(IndexInt, DepthInt, Sum)
                           end, maps:new(), re:split(Input, "\n")),
     LastStep = lists:max(maps:keys(CalcMap)),
     InitialSeverity = calc_severity(CalcMap, update_positions(CalcMap, 0), 0, 0, LastStep, 0),
     SafeDelay = calc_delay(CalcMap, LastStep, 0),
     { InitialSeverity, SafeDelay }.

calc_delay(CalcMap, LastStep, Delay)
  -> PositionMap = update_positions(CalcMap, Delay),
     CurrentSeverity = calc_delay(CalcMap, PositionMap, Delay, 0, LastStep),
     if CurrentSeverity == 0 -> Delay;
        true -> calc_delay(CalcMap, LastStep, Delay + 1)
     end.
calc_delay(_CalcMap, _PositionMap, _Picosecond, PacketIndex, LastStep)
  when PacketIndex > LastStep -> 0;
calc_delay(CalcMap, PositionMap, Picosecond, PacketIndex, LastStep)
  -> CurrentPosition = maps:get(PacketIndex, PositionMap, -1),
     if CurrentPosition == 0 -> 1;
        true -> calc_delay(CalcMap, update_positions(CalcMap, Picosecond + 1), Picosecond + 1, PacketIndex + 1, LastStep)
     end.

calc_severity(_CalcMap, _PositionMap, _Picosecond, PacketIndex, LastStep, Score)
  when PacketIndex > LastStep -> Score;
calc_severity(CalcMap, PositionMap, Picosecond, PacketIndex, LastStep, Score)
  -> CurrentPosition = maps:get(PacketIndex, PositionMap, -1),
     UpdatedPositions = update_positions(CalcMap, Picosecond + 1),
     if CurrentPosition == 0 -> calc_severity(CalcMap, UpdatedPositions, Picosecond + 1, PacketIndex + 1, LastStep, Score + (PacketIndex * maps:get(PacketIndex, CalcMap)));
        true -> calc_severity(CalcMap, UpdatedPositions, Picosecond + 1, PacketIndex + 1, LastStep, Score)
     end.

update_positions(CalcMap, Picosecond)
  -> lists:foldl(fun(X, Sum) ->
                     CurrentDepth = maps:get(X, CalcMap),
                     if (Picosecond div (CurrentDepth - 1)) rem 2 == 0 -> maps:put(X, (Picosecond rem (CurrentDepth - 1)), Sum);
                        true -> maps:put(X, (CurrentDepth - (Picosecond rem CurrentDepth)), Sum)
                     end
                 end, maps:new(), maps:keys(CalcMap)).

start()
  -> io:fwrite("~p~n", [calc_severity("0: 3
1: 2
4: 4
6: 4")]),
     io:fwrite("~p~n", [calc_severity("0: 3
1: 2
2: 4
4: 6
6: 4
8: 6
10: 5
12: 6
14: 9
16: 6
18: 8
20: 8
22: 8
24: 8
26: 8
28: 8
30: 12
32: 14
34: 10
36: 12
38: 12
40: 10
42: 12
44: 12
46: 12
48: 12
50: 12
52: 14
54: 14
56: 12
62: 12
64: 14
66: 14
68: 14
70: 17
72: 14
74: 14
76: 14
82: 14
86: 18
88: 14
96: 14
98: 44")]).
