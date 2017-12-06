-module(day6).
-export([start/0]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

redistribute_banks(BankConfiguration)
  -> Max = lists:max(BankConfiguration),
     Index = index_of(Max, BankConfiguration) - 1,
     %io:fwrite("max ~B index ~B~n", [Max, Index]),
     RawArray = array:from_list(BankConfiguration),
     redistribute_banks(array:set(Index, 0, RawArray), Index, Max, length(BankConfiguration)).

redistribute_banks(Array, _Index, RemainingBanks, _MaxSize)
  when RemainingBanks == 0 -> array:to_list(Array);
redistribute_banks(Array, Index, RemainingBanks, MaxSize)
  -> Target = (Index + 1) rem MaxSize,
     OldBankValue = array:get(Target, Array),
     %io:fwrite("target ~B oldbankvalue ~B~n", [Target, OldBankValue]),
     redistribute_banks(array:set(Target, OldBankValue + 1, Array), Target, RemainingBanks - 1, MaxSize).

steps_until_repeated(Input)
  -> steps_until_repeated(Input, [], []).
steps_until_repeated(Input, Visited, UniqList)
  when length(Visited) > 0, length(UniqList) /= length(Visited) -> [_|PriorVisits] = Visited,
                                                                   InputIndex = index_of(Input, PriorVisits),
                                                                   VisitedLength = length(Visited),
                                                                   {VisitedLength,Input,InputIndex};
steps_until_repeated(Input, Visited, _UniqList)
  -> NewConfiguration = redistribute_banks(Input),
     NewVisited = [NewConfiguration|Visited],
     steps_until_repeated(NewConfiguration, NewVisited, lists:usort(NewVisited)).

start()
  -> io:fwrite("~p~n", [steps_until_repeated([0,2,7,0])]),
     io:fwrite("~p~n", [steps_until_repeated([4,1,15,12,0,9,9,5,5,8,7,3,14,5,12,3])]).
