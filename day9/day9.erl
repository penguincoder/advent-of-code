-module(day9).
-export([start/0]).

calc_group_score(String)
  -> calc_group_score(re:split(String, ""), 0, false, 0, false, 0).
calc_group_score([<<>>], Score, _LastCharWasEx, _Depth, _FoundGarbage, CharCount)
  -> {Score, CharCount};
calc_group_score([_Head|Tail], Score, LastCharWasEx, Depth, FoundGarbage, CharCount)
  when LastCharWasEx == true, FoundGarbage == true -> calc_group_score(Tail, Score, false, Depth, true, CharCount);
calc_group_score([Head|Tail], Score, _LastCharWasEx, Depth, FoundGarbage, CharCount)
  when Head == <<"!">> -> calc_group_score(Tail, Score, true, Depth, FoundGarbage, CharCount);
calc_group_score([Head|Tail], Score, _LastCharWasEx, Depth, FoundGarbage, CharCount)
  when Head == <<"{">>, FoundGarbage == false -> calc_group_score(Tail, Score, false, Depth + 1, false, CharCount);
calc_group_score([Head|Tail], Score, _LastCharWasEx, Depth, FoundGarbage, CharCount)
  when Head == <<"}">>, FoundGarbage == false -> calc_group_score(Tail, Score + Depth, false, Depth - 1, false, CharCount);
calc_group_score([Head|Tail], Score, _LastCharWasEx, Depth, FoundGarbage, CharCount)
  when Head == <<"<">>, FoundGarbage == false -> calc_group_score(Tail, Score, false, Depth, true, CharCount);
calc_group_score([Head|Tail], Score, _LastCharWasEx, Depth, _FoundGarbage, CharCount)
  when Head == <<">">> -> calc_group_score(Tail, Score, false, Depth, false, CharCount);
calc_group_score([_Head|Tail], Score, _LastCharWasEx, Depth, FoundGarbage, CharCount)
  when FoundGarbage == true -> calc_group_score(Tail, Score, false, Depth, FoundGarbage, CharCount + 1);
calc_group_score([_Head|Tail], Score, _LastCharWasEx, Depth, FoundGarbage, CharCount)
  -> calc_group_score(Tail, Score, false, Depth, FoundGarbage, CharCount).

start()
  -> io:fwrite("~p~n", [calc_group_score("{}")]),
     io:fwrite("~p~n", [calc_group_score("{{{}}}")]),
     io:fwrite("~p~n", [calc_group_score("{{},{}}")]),
     io:fwrite("~p~n", [calc_group_score("{{{},{},{{}}}}")]),
     io:fwrite("~p~n", [calc_group_score("{<a>,<a>,<a>,<a>}")]),
     io:fwrite("~p~n", [calc_group_score("{{<ab>},{<ab>},{<ab>},{<ab>}}")]),
     io:fwrite("~p~n", [calc_group_score("{{<!!>},{<!!>},{<!!>},{<!!>}}")]),
     io:fwrite("~p~n", [calc_group_score("{{<a!>},{<a!>},{<a!>},{<ab>}}")]),
     {ok, File} = file:read_file("input"),
     io:fwrite("~p~n", [calc_group_score(unicode:characters_to_list(File))]).
