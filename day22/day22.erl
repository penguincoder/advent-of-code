-module(day22).
-export([start/0]).

process_input(Filename)
  -> { ok, Input } = file:read_file(Filename),
     Lines = re:split(unicode:characters_to_list(string:chomp(Input)), "\n"),
     { process_line(Lines, 0, maps:new()), (length(Lines) div 2) }.

process_line([], _Index, Map)
  -> Map;
process_line([Head|Tail], Index, Map)
  -> process_line(Tail, Index + 1, process_chars(re:split(Head, ""), Index, 0, Map)).

process_chars([], _Row, _Column, Map)
  -> Map;
process_chars([Head|Tail], Row, Column, Map)
  when Head == <<"#">> -> process_chars(Tail, Row, Column + 1, maps:put({ Column, Row }, infected, Map));
process_chars([_Head|Tail], Row, Column, Map)
  -> process_chars(Tail, Row, Column + 1, Map).

run_map(Map, Center)
  -> run_map(Map, 10000, 0, up, Center, Center, maps:get({ Center, Center }, Map, clean)).

run_map(_Map, Iterations, Infections, _Direction, _X, _Y, _NodeState)
  when Iterations == 0 -> Infections;
run_map(Map, Iterations, Infections, Direction, X, Y, NodeState)
  when NodeState == clean -> NewDirection = turn_left(Direction),
                             { NewX, NewY } = move(NewDirection, X, Y),
                             run_map(maps:put({ X, Y }, infected, Map), Iterations - 1, Infections + 1, NewDirection, NewX, NewY, maps:get({ NewX, NewY }, Map, clean));
run_map(Map, Iterations, Infections, Direction, X, Y, NodeState)
  when NodeState == infected -> NewDirection = turn_right(Direction),
                                { NewX, NewY } = move(NewDirection, X, Y),
                                run_map(maps:put({ X, Y }, clean, Map), Iterations - 1, Infections, NewDirection, NewX, NewY, maps:get({ NewX, NewY }, Map, clean)).

turn_left(Direction)
  when Direction == up -> left;
turn_left(Direction)
  when Direction == left -> down;
turn_left(Direction)
  when Direction == down -> right;
turn_left(Direction)
  when Direction == right -> up.

turn_right(Direction)
  when Direction == up -> right;
turn_right(Direction)
  when Direction == right -> down;
turn_right(Direction)
  when Direction == down -> left;
turn_right(Direction)
  when Direction == left -> up.

move(Direction, X, Y)
  when Direction == up -> { X, Y - 1 };
move(Direction, X, Y)
  when Direction == left -> { X - 1, Y };
move(Direction, X, Y)
  when Direction == down -> { X, Y + 1 };
move(Direction, X, Y)
  when Direction == right -> { X + 1, Y }.

run_map2(Map, Center)
  -> run_map2(Map, 10000000, 0, up, Center, Center, maps:get({ Center, Center }, Map, clean)).

run_map2(_Map, Iterations, Infections, _Direction, _X, _Y, _NodeState)
  when Iterations == 0 -> Infections;
run_map2(Map, Iterations, Infections, Direction, X, Y, NodeState)
  when NodeState == clean -> NewDirection = turn_left(Direction),
                             { NewX, NewY } = move(NewDirection, X, Y),
                             run_map2(maps:put({ X, Y }, weakened, Map), Iterations - 1, Infections, NewDirection, NewX, NewY, maps:get({ NewX, NewY }, Map, clean));
run_map2(Map, Iterations, Infections, Direction, X, Y, NodeState)
  when NodeState == weakened -> { NewX, NewY } = move(Direction, X, Y),
                             run_map2(maps:put({ X, Y }, infected, Map), Iterations - 1, Infections + 1, Direction, NewX, NewY, maps:get({ NewX, NewY }, Map, clean));
run_map2(Map, Iterations, Infections, Direction, X, Y, NodeState)
  when NodeState == infected -> NewDirection = turn_right(Direction),
                                { NewX, NewY } = move(NewDirection, X, Y),
                                run_map2(maps:put({ X, Y }, flagged, Map), Iterations - 1, Infections, NewDirection, NewX, NewY, maps:get({ NewX, NewY }, Map, clean));
run_map2(Map, Iterations, Infections, Direction, X, Y, NodeState)
  when NodeState == flagged -> NewDirection = reverse_direction(Direction),
                               { NewX, NewY } = move(NewDirection, X, Y),
                               run_map2(maps:put({ X, Y }, clean, Map), Iterations - 1, Infections, NewDirection, NewX, NewY, maps:get({ NewX, NewY }, Map, clean)).

reverse_direction(Direction)
  when Direction == up -> down;
reverse_direction(Direction)
  when Direction == down -> up;
reverse_direction(Direction)
  when Direction == left -> right;
reverse_direction(Direction)
  when Direction == right -> left.

start()
  -> { SampleInput, SampleCenter } = process_input("sample"),
     io:fwrite("sample center ~p number of infections ~p~n", [SampleCenter, run_map(SampleInput, SampleCenter)]),
     { ProblemInput, ProblemCenter } = process_input("problem"),
     io:fwrite("problem center ~p number of infections ~p~n", [ProblemCenter, run_map(ProblemInput, ProblemCenter)]),
     io:fwrite("sample part 2 center ~p number of infections ~p~n", [SampleCenter, run_map2(SampleInput, SampleCenter)]),
     io:fwrite("problem part 2 center ~p number of infections ~p~n", [ProblemCenter, run_map2(ProblemInput, ProblemCenter)]).
