:- use_module(library(dcg/basics)).

mul(N) -->
  "mul(", number(N1), ",", number(N2), ")", {N is N1 * N2}.

muls(Result) -->
  ignores, mul(N1), ignores, muls(N2), {Result is N1 + N2}.
muls(0) --> [].

ignores --> [].
ignores --> [_], ignores.

solve_part1(N) :-
  once(phrase_from_file(muls(N), "./day3_input.txt")).

parser([Op|Rest]) --> ignores, token(Op), ignores, parser(Rest).
parser([]) --> [].

token(mul(N)) -->
  mul(N).
token(do) --> "do()".
token(dont) --> "don't()".

solve_part2(N) :-
  once(phrase_from_file(parser(Tokens), "./day3_input.txt")),
  process(Tokens, N).

process([], 0).
process([mul(X)|Rest], Out) :-
  process(Rest, Out0),
  Out is Out0 + X.

process([dont|Rest], Out) :-
  drop_until_do([dont|Rest], NewRest),
  process(NewRest, Out).

process([do|Rest], Out) :-
  process(Rest, Out).

drop_until_do([dont], []).
drop_until_do([dont, do|Rest], Rest).
drop_until_do([dont, A|Rest], Out) :-
  A \= do,
  drop_until_do([dont|Rest], Out).
