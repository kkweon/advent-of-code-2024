:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% DCG rules for parsing the input
rows(Rows) --> sequence(row, Rows), blanks.
row(Row) --> sequence(cell, Row), "\n".
cell(dot) --> ".".
cell(hash) --> "#".
cell(guard(up)) --> "^".
cell(guard(down)) --> "v".
cell(guard(left)) --> "<".
cell(guard(right)) --> ">".

% Solve Part 1
solve_part1(NumberOfDistinctPositions) :-
  phrase_from_file(rows(Rows), 'day6_input.txt'),
  solve_part1(Rows, NumberOfDistinctPositions).

solve_part1(Matrix, NumberOfDistinctPositions) :-
  guard_position(Matrix, guard(Direction, Position)),
  remove_guard(Matrix, Position, TmpMatrix),
  step_forward(TmpMatrix, guard(Direction, Position), [], Positions),
  sort(Positions, SortedPositions),
  length(SortedPositions, NumberOfDistinctPositions).

% Remove the guard from the matrix
remove_guard(Matrix, R-C, NextMatrix) :-
  nth1(R, Matrix, Row),
  replace_nth(Row, C, dot, NextRow),
  replace_nth(Matrix, R, NextRow, NextMatrix).

% Find the initial guard position
guard_position(Matrix, guard(Direction, Position)) :-
  nth1(RowIndex, Matrix, Row),
  nth1(ColumnIndex, Row, guard(Direction)),
  Position = RowIndex-ColumnIndex.

% Step forward until the guard is gone
step_forward(Matrix, Guard, Visited, Positions) :-
  (Guard = nil ->
    Positions = Visited
  ; Guard = guard(_, Position),
    next_guard_position(Matrix, Guard, NextGuard),
    step_forward(Matrix, NextGuard, [Position|Visited], Positions)).

% Determine the next guard position
next_guard_position(Matrix, guard(Direction, Position), NextGuard) :-
  next_position(Direction, Position, NextPosition),
  ( can_move(Matrix, NextPosition) -> NextGuard = guard(Direction, NextPosition)
  ; out_of_bounds(Matrix, NextPosition) -> NextGuard = nil
  ; rotate(Direction, NextDirection),
    next_guard_position(Matrix, guard(NextDirection, Position), NextGuard)).

% Calculate the next position based on direction
next_position(Direction, R-C, NewR-NewC) :-
  Deltas = [up-(-1, 0), down-(1, 0), left-(0, -1), right-(0, 1)],
  member((Direction-(RDelta, CDelta)), Deltas),
  NewR is R + RDelta,
  NewC is C + CDelta.

% Check if the guard can move to the next position
can_move(Matrix, R-C) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, dot).

% Check if the position is out of bounds
out_of_bounds(Matrix, R-C) :-
  length(Matrix, NRow),
  [Row|_] = Matrix,
  length(Row, NCol),
  (R < 1; NRow < R; C < 1; NCol < C).

% Rotate the guard's direction
rotate(D, Next) :-
  Direction = [up, right, down, left],
  nth0(Index, Direction, D),
  NextIndex is (Index + 1) mod 4,
  nth0(NextIndex, Direction, Next).

% Solve Part 2
solve_part2(NumberOfSteps) :-
  phrase_from_file(rows(Rows), 'day6_input.txt'),
  solve_part2(Rows, NumberOfSteps).

solve_part2(Matrix, NumberOfSteps) :-
  guard_position(Matrix, Guard),
  Guard = guard(_, Position),
  remove_guard(Matrix, Position, TmpMatrix),
  step_forward(TmpMatrix, Guard, [], Positions),
  % Drop the first position because it's the starting point.
  reverse(Positions, [_|ReversedPositions]),
  sort(ReversedPositions, SortedPositions),
  concurrent_maplist(paradox_runner(TmpMatrix, Guard), SortedPositions, Result),
  sum_list(Result, NumberOfSteps).

% Check for paradoxical loops
paradox_runner(Matrix, Guard, Position, Result) :-
  place_object(Matrix, Position, NewMatrix),
  ( guard_in_loop(NewMatrix, Guard) -> Result is 1
  ; Result is 0).

% Place an object in the matrix
place_object(Matrix, R-C, NewMatrix) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, dot),
  replace_nth(Row, C, hash, NewRow),
  replace_nth(Matrix, R, NewRow, NewMatrix).

% Check if the guard is in a loop
guard_in_loop(Matrix, Guard) :-
  guard_in_loop(Matrix, Guard, []).

guard_in_loop(_, Guard, Visited) :-
  Guard \= nil,
  memberchk(Guard, Visited).

guard_in_loop(Matrix, Guard, Visited) :-
  Guard \= nil,
  \+ memberchk(Guard, Visited),
  next_guard_position(Matrix, Guard, NextGuard),
  guard_in_loop(Matrix, NextGuard, [Guard|Visited]).

% Replace an element in a list
replace_nth(List, N, NewElement, NewList) :-
  nth1(N, List, _, Rest),
  nth1(N, NewList, NewElement, Rest).

% Test input
test_input(I) :-
  I = `....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
`.

solve_part1_with_test_input(NumberOfDistinctPositions) :-
  test_input(I),
  phrase(rows(Rows), I),
  solve_part1(Rows, NumberOfDistinctPositions).

solve_part2_with_test_input(NumberOfSteps) :-
  test_input(I),
  once(phrase(rows(Rows), I)),
  solve_part2(Rows, NumberOfSteps).
