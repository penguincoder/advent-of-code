-module(day3).
-export([start/0]).

next_largest_square(Target)
  -> recurse_next_largest_square(1, Target).

recurse_next_largest_square(X, Target)
  when (X rem 2) =/= 0, (X*X) >= Target -> X;
recurse_next_largest_square(X, Target)
  -> recurse_next_largest_square(X+1, Target).

position_in_outer_shell(Square, Target)
  -> PreviousSquare = (Square - 2) * (Square - 2),
     Target - PreviousSquare.

position_on_side(Position, Square)
  when Position == 0 -> Square - 1;
position_on_side(_Position, Square)
  when Square == 1 -> 1;
position_on_side(Position, Square)
  -> Offset = Position rem (Square - 1),
     if Offset == 0 -> Square - 1;
               true -> Offset end.

distance_to_center(Position, Square)
  -> Center = Square div 2,
     VertOffset = abs(Center - Position),
     Center + VertOffset.

find_distance(Target)
  -> Square = next_largest_square(Target),
     Position = position_in_outer_shell(Square, Target),
     Index = position_on_side(Position, Square),
     distance_to_center(Index, Square).

start()
  -> io:fwrite("~B ~B~n", [1, find_distance(1)]),
     io:fwrite("~B ~B~n", [12, find_distance(12)]),
     io:fwrite("~B ~B~n", [23, find_distance(23)]),
     io:fwrite("~B ~B~n", [1024, find_distance(1024)]),
     io:fwrite("~B ~B~n", [368078, find_distance(368078)]),
     io:fwrite("https://oeis.org/A141481/b141481.txt~n").
