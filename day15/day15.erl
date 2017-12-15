-module(day15).
-export([start/0]).

gen_a_value(Val)
  -> (Val * 16807) rem 2147483647.

gen_b_value(Val)
  -> (Val * 48271) rem 2147483647.

gen_a_value2(Val)
  -> Result = (Val * 16807) rem 2147483647,
     if Result rem 4 == 0 -> Result;
        true -> gen_a_value2(Result)
     end.

gen_b_value2(Val)
  -> Result = (Val * 48271) rem 2147483647,
     if Result rem 8 == 0 -> Result;
        true -> gen_b_value2(Result)
     end.

lower_16_matches(ValA, ValB)
  -> PaddedValA = <<ValA:32>>,
     PaddedValB = <<ValB:32>>,
     <<_A:16,B:16>> = PaddedValA,
     <<_C:16,D:16>> = PaddedValB,
     B == D.

judge_matches(Start, _GenA, _GenB, MatchCount, _GenAFun, _GenBFun)
  when Start == 0 -> MatchCount;
judge_matches(Start, GenA, GenB, MatchCount, GenAFun, GenBFun)
  -> GenAResult = GenAFun(GenA),
     GenBResult = GenBFun(GenB),
     Match = lower_16_matches(GenAResult, GenBResult),
     if Match -> judge_matches(Start - 1, GenAResult, GenBResult, MatchCount + 1, GenAFun, GenBFun);
        true -> judge_matches(Start - 1, GenAResult, GenBResult, MatchCount, GenAFun, GenBFun)
     end.

start()
  -> io:fwrite("~p~n", [judge_matches(40000000, 65, 8921, 0, fun gen_a_value/1, fun gen_b_value/1)]),
     io:fwrite("~p~n", [judge_matches(40000000, 703, 516, 0, fun gen_a_value/1, fun gen_b_value/1)]),
     io:fwrite("~p~n", [judge_matches(5000000, 65, 8921, 0, fun gen_a_value2/1, fun gen_b_value2/1)]),
     io:fwrite("~p~n", [judge_matches(5000000, 703, 516, 0, fun gen_a_value2/1, fun gen_b_value2/1)]).
