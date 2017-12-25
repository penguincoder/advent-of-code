-module(day25).
-export([start/0]).

part_1()
  -> state_a(maps:new(), 0, 0, 6).

state_a(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_a(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index + 1,
                          state_b(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_a(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 0, Tape),
                          NewIndex = Index - 1,
                          state_b(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

state_b(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_b(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index - 1,
                          state_a(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_b(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index + 1,
                          state_a(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

diagnostic_checksum(Tape)
  -> maps:fold(fun(_K, V, Sum) -> Sum + V end, 0, Tape).

part_2()
  -> state_a2(maps:new(), 0, 0, 12861455).

state_a2(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_a2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index + 1,
                          state_b2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_a2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 0, Tape),
                          NewIndex = Index - 1,
                          state_b2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

state_b2(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_b2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index - 1,
                          state_c2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_b2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 0, Tape),
                          NewIndex = Index + 1,
                          state_e2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

state_c2(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_c2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index + 1,
                          state_e2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_c2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 0, Tape),
                          NewIndex = Index - 1,
                          state_d2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

state_d2(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_d2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index - 1,
                          state_a2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_d2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index - 1,
                          state_a2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

state_e2(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_e2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 0, Tape),
                          NewIndex = Index + 1,
                          state_a2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_e2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 0, Tape),
                          NewIndex = Index + 1,
                          state_f2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

state_f2(Tape, _Index, _CurrentBit, Steps)
  when Steps == 0 -> Tape;
state_f2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 0 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index + 1,
                          state_e2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1);
state_f2(Tape, Index, CurrentBit, Steps)
  when CurrentBit == 1 -> NewTape = maps:put(Index, 1, Tape),
                          NewIndex = Index + 1,
                          state_a2(NewTape, NewIndex, maps:get(NewIndex, NewTape, 0), Steps - 1).

start()
  -> io:fwrite("sample diagnostic checksum ~p~n", [diagnostic_checksum(part_1())]),
     io:fwrite("problem diagnostic checksum ~p~n", [diagnostic_checksum(part_2())]).
