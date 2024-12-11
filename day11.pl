:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

row(Row) -->
  sequence(number, white, Row), blanks.

input(Row) :-
  phrase_from_file(row(Row), "day11_input.txt").

:- table next_number/2.
next_number(0, [1]) :- !.
next_number(Number, Next) :-
  Number =\= 0,
  split(Number, Next), !.
next_number(Number, [Next]) :-
  Number =\= 0,
  \+ split(Number, _),
  Next is Number * 2024, !.

split(Number, [Number1, Number2]) :-
  number_codes(Number, Codes),
  length(Codes, Length),
  Length mod 2 =:= 0,
  Mid is Length // 2,
  length(Left, Mid),
  length(Right, Mid),
  append(Left, Right, Codes),
  number_codes(Number1, Left),
  number_codes(Number2, Right), !.

solve_part1(Result) :-
  phrase_from_file(row(Row), "day11_input.txt"),
  solve(25, Row, Result).

solve_part2(Result) :-
  phrase_from_file(row(Row), "day11_input.txt"),
  solve(75, Row, Result).

solve(N, Row, Result) :-
  foldl(next_number_n_runner(N), Row, 0, Result).

:- table next_number_n_runner/4.
next_number_n_runner(N, In, Acc, Out) :-
  next_number_n(N, In, O),
  Out is Acc + O.

:- table next_number_n/3.
next_number_n(0, _, 1) :- !.
next_number_n(N, Number, Result) :-
  N > 0,
  N1 is N - 1,
  next_number(Number, NextNumbers),
  foldl(next_number_n_runner(N1), NextNumbers, 0, Result), !.

blink_(Numbers, NextNumbers) :-
  foldl(next_number_runner, Numbers, [], Result),
  append(Result, NextNumbers).

next_number_runner(Number, Acc, NextNumbers) :-
  next_number(Number, Next),
  append(Acc, [Next], NextNumbers).

:- begin_tests(day11).

test(blink_once) :-
  blink_([125, 17], Next),
  assertion(Next == [253000, 1, 7]).

test(blink_twice) :-
  blink_([125, 17], Next1),
  blink_(Next1, Next2),
  assertion(Next2 == [253, 0, 2024, 14168]).

test(solve_part1) :-
  solve(25, [125, 17], Result),
  assertion(Result == 55312).

:- end_tests(day11).
