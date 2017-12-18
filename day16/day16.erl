-module(day16).
-export([start/0]).

dance([], Registers)
  -> Registers;
dance([Head|Tail], Registers)
  -> Instr = string:slice(Head, 0, 1),
     Args = string:slice(Head, 1),
     NewRegisters = process_instruction(Instr, Args, Registers),
     dance(Tail, NewRegisters).

process_instruction(Instr, Args, Registers)
  when Instr == <<"s">> -> Len = string:length(Registers),
                           { Count, _ } = string:to_integer(Args),
                           Index = Len - Count,
                           First = string:slice(Registers, Index),
                           Second = string:slice(Registers, 0, Index),
                           string:concat(First, Second);
process_instruction(Instr, Args, Registers)
  when Instr == <<"x">> -> [FirstRegister|Rest] = re:split(Args, "/"),
                           [SecondRegister|_] = Rest,
                           { FirstIndex, _ } = string:to_integer(FirstRegister),
                           { SecondIndex, _ } = string:to_integer(SecondRegister),
                           swap_registers(FirstIndex, SecondIndex, Registers);
process_instruction(Instr, Args, Registers)
  when Instr == <<"p">> -> [FirstRegister|Rest] = re:split(Args, "/"),
                           [SecondRegister|_] = Rest,
                           IndexA = char_index(FirstRegister, Registers),
                           IndexB = char_index(SecondRegister, Registers),
                           swap_registers(IndexA, IndexB, Registers).

char_index(Char, String)
  -> StrLen = string:length(String),
     StrLen - string:length(string:find(String, Char)).

find_char_index(Index)
  when Index == nomatch -> 0;
find_char_index(Index)
  -> Index.

swap_registers(A, B, Registers)
  when B < A -> swap_registers(B, A, Registers);
swap_registers(A, B, Registers)
  -> RegisterList = list_to_tuple(Registers),
     ValueA = element(A + 1, RegisterList),
     ValueB = element(B + 1, RegisterList),
     tuple_to_list(setelement(B + 1, setelement(A + 1, RegisterList, ValueB), ValueA)).

dance_countdown(Steps, Instructions, Registers)
  -> dance_countdown(Steps, Instructions, Registers, Steps, []).

dance_countdown(Steps, _Instructions, Registers, _OriginalSteps, _PreviousSteps)
  when Steps == 0 -> Registers;
dance_countdown(Steps, Instructions, Registers, OriginalSteps, PreviousSteps)
  -> CurrentRegisters = dance(Instructions, Registers),
     Found = lists:any(fun(X) -> string:equal(X, CurrentRegisters) end, PreviousSteps),
     if Found -> Index = OriginalSteps rem length(PreviousSteps),
                 lists:nth(Index, PreviousSteps);
        true -> dance_countdown(Steps - 1, Instructions, CurrentRegisters, OriginalSteps, lists:append(PreviousSteps, [CurrentRegisters]))
     end.

start()
  -> SampleInstructions = re:split("s1,x3/4,pe/b", ","),
     io:fwrite("~p~n", [dance(SampleInstructions, "abcde")]),
     { ok, File } = file:read_file("input"),
     InputInstructions = re:split(unicode:characters_to_list(string:chomp(File)), ","),
     io:fwrite("~p~n", [dance(InputInstructions, "abcdefghijklmnop")]),
     io:fwrite("~p~n", [dance_countdown(1000000000, InputInstructions, "abcdefghijklmnop")]).
