-module(day19).
-export([start/0]).

find_start_col([Head|_Tail], Index)
  when Head == "|" -> Index;
find_start_col([_Head|Tail], Index)
  -> find_start_col(Tail, Index + 1).

map_input(InputList)
  -> map_input(InputList, dict:new(), 0).

map_input([], Maze, _Row)
  -> Maze;
map_input([Head|Tail], Maze, Row)
  -> NewMaze = map_row(re:split(string:chomp(Head), ""), Maze, 0, Row),
     map_input(Tail, NewMaze, Row + 1).

map_row([], Maze, _Col, _Row)
  -> Maze;
map_row([Head|Tail], Maze, Col, Row)
  -> map_row(Tail, dict:store({ Col, Row }, Head, Maze), Col + 1, Row).

trace_maze(Maze, X, Y, Marked, VisitedChars, Steps, DeltaX, DeltaY)
  -> Key = { X, Y },
     Marked2 = dict:store(Key, 1, Marked),
     Value = dict:fetch(Key, Maze),
     if Value >= <<"A">>, Value =< <<"Z">> ->
          Terminal = is_terminal(Maze, X, Y, Marked2, DeltaX, DeltaY),
          if Terminal -> { VisitedChars, Steps };
             true -> trace_maze(Maze, X + DeltaX, Y + DeltaY, Marked2, lists:append(VisitedChars, [Value]), Steps + 1, DeltaX, DeltaY)
          end;
        Value == <<"+">> ->
          { NewDeltaX, NewDeltaY } = find_new_direction(Maze, X, Y, Marked2),
          trace_maze(Maze, X + NewDeltaX, Y + NewDeltaY, Marked, VisitedChars, Steps + 1, NewDeltaX, NewDeltaY);
        true ->
          trace_maze(Maze, X + DeltaX, Y + DeltaY, Marked2, VisitedChars, Steps + 1, DeltaX, DeltaY)
     end.

is_terminal(Maze, X, Y, Marked, DeltaX, DeltaY)
  -> NextVal = dict:fetch({ X + DeltaX, Y + DeltaY }, Maze),
     NextVal == <<" ">>.

find_new_direction(Maze, X, Y, Marked)
    -> NorthVal = get_new_direction_val(Maze, X, Y - 1, Marked, <<"|">>),
       SouthVal = get_new_direction_val(Maze, X, Y + 1, Marked, <<"|">>),
       EastVal = get_new_direction_val(Maze, X + 1, Y, Marked, <<"-">>),
       WestVal = get_new_direction_val(Maze, X - 1, Y, Marked, <<"-">>),
       if NorthVal -> { 0, -1 };
          SouthVal -> { 0, 1 };
          EastVal -> { 1, 0 };
          WestVal -> { -1, 0 }
       end.

get_new_direction_val(Maze, X, Y, Marked, DesiredChar)
  -> Key = { X, Y },
     HasKey = dict:is_key(Key, Marked),
     if HasKey ->
          IsMarked = dict:fetch(Key, Marked),
          if IsMarked == 1 -> false;
             true ->
               Piece = dict:fetch(Key, Maze),
               Piece == DesiredChar
          end;
        true -> false
     end.

start()
  -> { ok, File } = file:read_file("input"),
     InputList = re:split(unicode:characters_to_list(string:chomp(File)), "\n"),
     Maze = map_input(InputList),
     Marked = dict:fold(fun(Key, _Value, Acc) -> dict:store(Key, 0, Acc) end, dict:new(), Maze),
     io:fwrite("~p~n", [trace_maze(Maze, 157, 0, Marked, [], 1, 0, 1)]).
