:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

map(Matrix) -->
  sequence(row, Matrix), blanks.

row(Row) -->
  sequence(cell, Row), "\n".

cell(Cell) -->
  [X], { code_type(X, digit(Cell)) }.

solve_part1(Score) :-
  phrase_from_file(map(Matrix), "day10_input.txt"),
  solve_part1(Matrix, Score).

solve_part1(Matrix, Score) :-
  findall(S, (zero(Matrix, ZeroPosition), score(Matrix, ZeroPosition, S)), Scores),
  sum_list(Scores, Score).

zero(Matrix, I-J) :-
  nth1(I, Matrix, Row),
  nth1(J, Row, 0).

score(Matrix, I-J, Score) :-
  Queue = [0-I-J],
  bfs(Queue, [I-J], Matrix, 0, Score).

bfs([], _, _, Score, Score).
bfs([9-_-_|Queue], Visited, Matrix, CurrentScore, Result) :-
  NextScore is CurrentScore + 1,
  bfs(Queue, Visited, Matrix, NextScore, Result).
bfs([V-I-J|Queue], Visited, Matrix, CurrentScore, Result) :-
  V =\= 9,
  findall(NextPosition,
          (next_move(Matrix, V-I-J, NextPosition),
           \+ memberchk(NextPosition, Visited)),
          NextPositions),
  append(Visited, NextPositions, NextVisited),
  append(Queue, NextPositions, NextQueue),
  bfs(NextQueue, NextVisited, Matrix, CurrentScore, Result).

next_move(Matrix, V-I-J, NextValue-NextI-NextJ) :-
  delta(RD-CD),
  NextI #= I + RD,
  NextJ #= J + CD,
  nth1(NextI, Matrix, NextRow),
  nth1(NextJ, NextRow, NextValue),
  NextValue #= V + 1.

delta(RD-CD) :-
  member(RD-CD, [1-0, -1-0, 0-1, 0-(-1)]).

solve_part2(Score) :-
  phrase_from_file(map(Matrix), "day10_input.txt"),
  solve_part2(Matrix, Score).

solve_part2(Matrix, Score) :-
  findall(S, (zero(Matrix, ZeroPosition), score2(Matrix, ZeroPosition, S)), Scores),
  sum_list(Scores, Score).

score2(Matrix, I-J, Score) :-
  findall(Path, dfs(0-I-J, [], Matrix, Path), AllPaths),
  length(AllPaths, Score).

dfs(9-I-J, Visited, _, [I-J|Visited]).
dfs(V-I-J, Visited, Matrix, Path) :-
  V =\= 9,
  NextVisited = [I-J|Visited],
  next_move(Matrix, V-I-J, NextPosition),
  dfs(NextPosition, NextVisited, Matrix, Path).

test_input(Input) :-
  Input = `89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
`.

test_matrix(Matrix) :-
  test_input(Input),
  phrase(map(Matrix), Input).


:- begin_tests(day10).

test(map) :-
  test_input(Input),
  phrase(map(Matrix), Input),
  Expected =[[8, 9, 0, 1, 0, 1, 2, 3],
             [7, 8, 1, 2, 1, 8, 7, 4],
             [8, 7, 4, 3, 0, 9, 6, 5],
             [9, 6, 5, 4, 9, 8, 7, 4],
             [4, 5, 6, 7, 8, 9, 0, 3],
             [3, 2, 0, 1, 9, 0, 1, 2],
             [0, 1, 3, 2, 9, 8, 0, 1],
             [1, 0, 4, 5, 6, 7, 3, 2]],
  forall(between(1, 8, I), (
    nth1(I, Matrix, Row),
    nth1(I, Expected, ExpectedRow),
    assertion(Row == ExpectedRow)
  )).

test(solve_part1) :-
  test_matrix(Matrix),
  solve_part1(Matrix, Score),
  assertion(Score =:= 36).

test(solve_part2) :-
  test_matrix(Matrix),
  solve_part2(Matrix, Score),
  assertion(Score =:= 81).

:- end_tests(day10).
