:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

lines([NumList|Rest]) -->
  line(NumList), eol, lines(Rest).
lines([]) --> [].

line([N1|Rest]) -->
  number(N1), white, line(Rest).
line([N]) --> number(N).

solve_part1(SafeReportCount) :-
  phrase_from_file(lines(ListOfNumList), "./day2_input.txt"),
  include(is_safe, ListOfNumList, SafeReports),
  length(SafeReports, SafeReportCount).

is_safe([A, B | Rest]) :-
  Diff #= abs(A - B),
  Diff in 1..3,
  (
    A #< B -> increasing([B|Rest])
  ; A #> B -> decreasing([B|Rest])).

increasing([_]).
increasing([A, B | Rest]) :-
  Diff #= B - A,
  Diff in 1..3,
  increasing([B|Rest]).

decreasing([_]).
decreasing([A, B | Rest]) :-
  Diff #= A - B,
  Diff in 1..3,
  decreasing([B|Rest]).

solve_part2(SafeReportCount) :-
  phrase_from_file(lines(ListOfNumList), "./day2_input.txt"),
  include(is_safe_tolerance1, ListOfNumList, SafeReports),
  length(SafeReports, SafeReportCount).

is_safe_tolerance1(List) :-
  increasing(List, 1);
  decreasing(List, 1).

increasing(List, 1) :-
  increasing(List);
  select(_, List, NextList),
  increasing(NextList).

decreasing(List, 1) :-
  decreasing(List);
  select(_, List, NextList),
  decreasing(NextList).
