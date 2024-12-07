:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

equations(Equations) -->
  sequence(equation, "\n", Equations), blanks.

equation(eq(TestValue, Numbers)) -->
  number(TestValue), ":", white, sequence(number, " ", Numbers), ("\n" | "").

solve_part1(TotalCalibrationValue) :-
  phrase_from_file(equations(Equations), "./day7_input.txt"),
  solve_part1(Equations, TotalCalibrationValue).

solve_part1(Equations, TotalCalibrationValue) :-
  include(true_equation, Equations, TrueEquations),
  maplist(equation_value, TrueEquations, Values),
  sum_list(Values, TotalCalibrationValue).

solve_part2(TotalCalibrationValue) :-
  phrase_from_file(equations(Equations), "./day7_input.txt"),
  solve_part2(Equations, TotalCalibrationValue).

solve_part2(Equations, TotalCalibrationValue) :-
  include(true_equation_with_concat, Equations, TrueEquations),
  maplist(equation_value, TrueEquations, Values),
  sum_list(Values, TotalCalibrationValue).

true_equation(eq(TestValue, List)) :-
  compute(List, 0, TestValue).

compute([], Acc, Acc).
compute([H|T], Acc, Value) :-
  (NextAcc is Acc * H; NextAcc is Acc + H),
  compute(T, NextAcc, Value).

equation_value(eq(Value, _), Value).

true_equation_with_concat(eq(TestValue, List)) :-
  compute_with_concat(List, 0, TestValue).

compute_with_concat([], Acc, Acc).
compute_with_concat([H|T], Acc, Value) :-
  (NextAcc is Acc * H; NextAcc is Acc + H),
  compute_with_concat(T, NextAcc, Value).
compute_with_concat([H|T], Acc, Value) :-
  number_string(Acc, AccString),
  number_string(H, HString),
  string_concat(AccString, HString, NextAccString),
  number_string(NextAcc, NextAccString),
  compute_with_concat(T, NextAcc, Value).

test_input(I) :-
  I = `190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
`.

:- begin_tests(day7).

test(parse_equation) :-
  phrase(equation(eq(TestValue, Numbers)), `190: 10 19`),
  assertion(TestValue =:= 190),
  assertion(Numbers == [10, 19]).

test(parse_multiple_equations) :-
  phrase(equations(Equations), `190: 10 19
3267: 81 40 27
292: 11 6 16 20`),
  assertion(Equations == [eq(190, [10, 19]), eq(3267, [81, 40, 27]), eq(292, [11, 6, 16, 20])]).

test(true_equation_positive) :-
  true_equation(eq(190, [10, 19])), !.

test(true_equation_positive2) :-
  true_equation(eq(3267, [81, 40, 27])), !.

test(true_equation_negative, [fail]) :-
  true_equation(eq(192, [17, 8, 14])), !.

test(solve_part1_with_test_input) :-
  test_input(I),
  phrase(equations(Equations), I),
  solve_part1(Equations, TotalCalibrationValue),
  assertion(TotalCalibrationValue =:= 3749), !.

test(solve_part2_with_test_input) :-
  test_input(I),
  phrase(equations(Equations), I),
  solve_part2(Equations, TotalCalibrationValue),
  assertion(TotalCalibrationValue =:= 11387), !.

test(compute_with_concat) :-
  compute_with_concat([15, 6], 0, 156), !.

test(compute_with_concat2) :-
  compute_with_concat([6, 8, 6, 15], 0, 7290), !.

:- end_tests(day7).
