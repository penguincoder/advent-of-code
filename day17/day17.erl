-module(day17).
-export([start/0]).

spin_the_lock(NextValue, SpinQueue, Index, _Steps)
  when NextValue == 2018 -> lists:nth(Index + 1 rem 2018, SpinQueue);
spin_the_lock(NextValue, SpinQueue, Index, Steps)
  -> NextPosition = (Index + Steps) rem NextValue,
     { First, Last } = lists:split(NextPosition, SpinQueue),
     spin_the_lock(NextValue + 1, lists:append([First, [NextValue], Last]), NextPosition + 1, Steps).

spin_the_lock2(NextValue, ValAtOne, _Index, _Steps)
  when NextValue == 50000001 -> ValAtOne;
spin_the_lock2(NextValue, ValAtOne, Index, Steps)
  -> NextPosition = (Index + Steps) rem NextValue,
     if NextPosition == 0 -> spin_the_lock2(NextValue + 1, NextValue, NextPosition + 1, Steps);
        true -> spin_the_lock2(NextValue + 1, ValAtOne, NextPosition + 1, Steps)
     end.

start()
  -> io:fwrite("~p~n", [spin_the_lock(1, [0], 0, 3)]),
     io:fwrite("~p~n", [spin_the_lock(1, [0], 0, 303)]),
     io:fwrite("~p~n", [spin_the_lock2(1, 0, 0, 303)]).
