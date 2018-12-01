-module(day23).
-export([start/0]).

value_or_register(X, Registers)
  when X >= <<"a">>, X =< <<"z">> -> maps:get(X, Registers, 0);
value_or_register(X, _Registers)
  -> { NewValue, _ } = string:to_integer(X),
     NewValue.

format_input(Input)
  -> lists:map(fun(X) -> [A|Args] = re:split(X, " "), { A, Args } end, re:split(Input, "\n")).

assemble(Input)
  -> Instructions = format_input(Input),
     assemble(Instructions, length(Instructions), 1, 0, maps:new()).

debug_assemble(Input)
  -> Instructions = format_input(Input),
     assemble(Instructions, length(Instructions), 1, 0, maps:put(<<"a">>, 1, maps:new())).

assemble(_Instructions, MaxInstructions, Ip, LastFrequency, Registers)
  when Ip < 1; Ip > MaxInstructions -> { LastFrequency, Registers };
assemble(Instructions, MaxInstructions, Ip, LastFrequency, Registers)
  -> { Op, Args } = lists:nth(Ip, Instructions),
     %io:fwrite("ip ~p op ~p args ~p~n~p~n", [Ip, Op, Args, Registers]),
     case Op of
       <<"add">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, maps:put(Src, ExistingValue + NewValue, Registers));
       <<"sub">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, maps:put(Src, ExistingValue - NewValue, Registers));
       <<"jnz">> ->
         [Src,Dest|_] = Args,
         SrcValue = value_or_register(Src, Registers),
         NewValue = value_or_register(Dest, Registers),
         if SrcValue /= 0 -> assemble(Instructions, MaxInstructions, Ip + NewValue, LastFrequency, Registers);
            true -> assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, Registers)
         end;
       <<"jgz">> ->
         [Src,Dest|_] = Args,
         SrcValue = value_or_register(Src, Registers),
         NewValue = value_or_register(Dest, Registers),
         if SrcValue > 0 -> assemble(Instructions, MaxInstructions, Ip + NewValue, LastFrequency, Registers);
            true -> assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, Registers)
         end;
       <<"mod">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, maps:put(Src, ExistingValue rem NewValue, Registers));
       <<"mul">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         Registers2 = maps:put("mul_count", maps:get("mul_count", Registers, 0) + 1, Registers),
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, maps:put(Src, ExistingValue * NewValue, Registers2));
       <<"rcv">> ->
         [Dest|_] = Args,
         NewValue = value_or_register(Dest, Registers),
         if NewValue /= 0 -> LastFrequency;
            true -> assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, Registers)
         end;
       <<"set">> ->
         [Src,Dest|_] = Args,
         NewValue = value_or_register(Dest, Registers),
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, maps:put(Src, NewValue, Registers));
       <<"snd">> ->
         [Dest|_] = Args,
         NewValue = value_or_register(Dest, Registers),
         assemble(Instructions, MaxInstructions, Ip + 1, NewValue, Registers);
        <<>> ->
         io:fwrite("invalid instruction: ~p~n", [{ Op, Args }]),
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, Registers)
    end.

is_prime(Check)
  -> is_prime(Check, 2, math:sqrt(Check)).
is_prime(Check, Min, _Max)
  when Check rem Min == 0 -> false;
is_prime(Check, Min, Max)
  when Min > Max -> Check /= 1;
is_prime(Check, Min, Max)
  -> is_prime(Check, Min + 1, Max).

get_num_nonprimes(Start, End)
  -> get_num_nonprimes(Start, End, 1, is_prime(Start)).
get_num_nonprimes(Start, End, Count, _IsPrime)
  when Start == End -> Count;
get_num_nonprimes(Start, End, Count, IsPrime)
  when IsPrime -> NewStart = Start + 17,
                  get_num_nonprimes(NewStart, End, Count, is_prime(NewStart));
get_num_nonprimes(Start, End, Count, IsPrime)
  when not IsPrime -> NewStart = Start + 17,
                      get_num_nonprimes(NewStart, End, Count + 1, is_prime(NewStart)).

start()
  -> { ok, File } = file:read_file("problem"),
     io:fwrite("~p~n", [ assemble(File) ]),
     %io:fwrite("~p~n", [ debug_assemble(File) ]).
     io:fwrite("~p~n", [ get_num_nonprimes(109900, 126900) ]).
