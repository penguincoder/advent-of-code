-module(day18).
-export([start/0]).

value_or_register(X, Registers)
  when X >= <<"a">>, X =< <<"z">> -> maps:get(X, Registers, 0);
value_or_register(X, _Registers)
  -> { NewValue, _ } = string:to_integer(X),
     NewValue.

assemble(Input)
  -> Instructions = lists:map(fun(X) -> [A|Args] = re:split(X, " "), { A, Args } end, re:split(Input, "\n")),
     assemble(Instructions, length(Instructions), 1, 0, maps:new()).

assemble(_Instructions, MaxInstructions, Ip, LastFrequency, _Registers)
  when Ip < 1; Ip > MaxInstructions -> LastFrequency;
assemble(Instructions, MaxInstructions, Ip, LastFrequency, Registers)
  -> { Op, Args } = lists:nth(Ip, Instructions),
     case Op of
       <<"add">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, maps:put(Src, ExistingValue + NewValue, Registers));
       <<"jgz">> ->
         [Src,Dest|_] = Args,
         NewValue = value_or_register(Dest, Registers),
         RegisterValue = maps:get(Src, Registers, 0),
         if RegisterValue > 0 -> assemble(Instructions, MaxInstructions, Ip + NewValue, LastFrequency, Registers);
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
         assemble(Instructions, MaxInstructions, Ip + 1, LastFrequency, maps:put(Src, ExistingValue * NewValue, Registers));
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
         assemble(Instructions, MaxInstructions, Ip + 1, NewValue, Registers)
    end.

assemble2(Input)
  -> Instructions = lists:map(fun(X) -> [A|Args] = re:split(X, " "), { A, Args } end, re:split(Input, "\n")),
     assemble2(Instructions, length(Instructions), 1, maps:put(<<"p">>, 0, maps:new()), [], 0, 1, maps:put(<<"p">>, 1, maps:new()), []).

assemble2(Instructions, MaxInstructions, Ip0, Registers0, Queue0, SndCount, Ip1, Registers1, Queue1)
  -> { NewIp0, NewRegisters0, NewQueue0, NewQueue1, _ } = assemble3(Instructions, MaxInstructions, Ip0, Registers0, Queue0, Queue1),
     { NewIp1, NewRegisters1, AnotherNewQueue1, AnotherNewQueue0, Count } = assemble3(Instructions, MaxInstructions, Ip1, Registers1, NewQueue1, NewQueue0),
     NewSndCount = SndCount + Count,
     %io:fwrite("newsndcount ~p~n", [NewSndCount]),
     %io:fwrite("ip0 ~p ip1 ~p~nq0 ~p~nq1 ~p~n", [NewIp0, NewIp1, AnotherNewQueue0, AnotherNewQueue1]),
     if Ip0 == NewIp0, Ip1 == NewIp1 -> NewSndCount;
        true -> assemble2(Instructions, MaxInstructions, NewIp0, NewRegisters0, AnotherNewQueue0, NewSndCount, NewIp1, NewRegisters1, AnotherNewQueue1)
     end.

assemble3(_Instructions, MaxInstructions, Ip, Registers, Queue, OtherQueue)
  when Ip < 1; Ip > MaxInstructions -> { Ip, Registers, Queue, OtherQueue, 0 };
assemble3(Instructions, _MaxInstructions, Ip, Registers, Queue, OtherQueue)
  -> { Op, Args } = lists:nth(Ip, Instructions),
     case Op of
       <<"add">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         { Ip + 1, maps:put(Src, ExistingValue + NewValue, Registers), Queue, OtherQueue, 0 };
       <<"jgz">> ->
         [Src,Dest|_] = Args,
         SrcValue = value_or_register(Src, Registers),
         NewValue = value_or_register(Dest, Registers),
         if SrcValue > 0 -> { Ip + NewValue, Registers, Queue, OtherQueue, 0 };
            true -> { Ip + 1, Registers, Queue, OtherQueue, 0 }
         end;
       <<"mod">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         { Ip + 1, maps:put(Src, (ExistingValue rem NewValue), Registers), Queue, OtherQueue, 0 };
       <<"mul">> ->
         [Src,Dest|_] = Args,
         ExistingValue = maps:get(Src, Registers, 0),
         NewValue = value_or_register(Dest, Registers),
         { Ip + 1, maps:put(Src, (ExistingValue * NewValue), Registers), Queue, OtherQueue, 0 };
       <<"rcv">> ->
         [Dest|_] = Args,
         QueueLen = length(Queue),
         if QueueLen == 0 -> { Ip, Registers, Queue, OtherQueue, 0 };
            true -> [Head|Tail] = Queue, { Ip + 1, maps:put(Dest, Head, Registers), Tail, OtherQueue, 0 }
         end;
       <<"set">> ->
         [Src,Dest|_] = Args,
         NewValue = value_or_register(Dest, Registers),
         { Ip + 1, maps:put(Src, NewValue, Registers), Queue, OtherQueue, 0 };
       <<"snd">> ->
         [Dest|_] = Args,
         NewValue = value_or_register(Dest, Registers),
         { Ip + 1, Registers, Queue, lists:append(OtherQueue, [NewValue]), 1 }
    end.

start()
  -> io:fwrite("~p~n", [ assemble("set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2") ]),
     Instructions = "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 952
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19",
     io:fwrite("~p~n", [ assemble(Instructions) ]),
     io:fwrite("~p~n", [ assemble2("snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d") ]),
     io:fwrite("~p~n", [ assemble2(Instructions) ]).
