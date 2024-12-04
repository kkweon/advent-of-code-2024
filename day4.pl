:- use_module(library(dcg/basics)).

% Data parsing
data([Row|Rows]) --> row(Row), data(Rows).
data([Row]) --> row(Row).

row(Row) --> string_without("\n", Row), "\n".

% Solve Part 1
solve_part1(TotalCount) :-
  phrase_from_file(data(Matrix), 'day4_input.sql'),
  matrix_dimensions(Matrix, NRow, NCol),
  findall(Count, (between(1, NRow, R), between(1, NCol, C), count_xmas(Matrix, R-C, Count)), Counts),
  sum_list(Counts, TotalCount).

% Test input
test_input(Matrix) :-
  Matrix = [`MMMSXXMASM`, `MSAMXMSMSA`, `AMXSXMAAMM`, `MSAMASMSMX`, `XMASAMXAMM`,
            `XXAMMXXAMA`, `SMSMSASXSS`, `SAXAMASAAA`, `MAMMMXMMMM`, `MXMXAXMASX`].

% Count XMAS occurrences
count_xmas(Matrix, R-C, Count) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, 0'X),
  directions(Directions),
  include(is_xmas(Matrix, R-C), Directions, XMasList),
  length(XMasList, Count).

is_xmas(Matrix, R-C, Direction) :-
  next_cells(R-C, Direction, 4, [], NextCells),
  pick_letters(Matrix, NextCells, Letters),
  Letters == `XMAS`.

% Pick letters from matrix
pick_letters(_, [], []).
pick_letters(Matrix, [R-C|NextCells], [Letter|Letters]) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, Letter),
  pick_letters(Matrix, NextCells, Letters).

% Generate next cells
next_cells(_, _, L, Acc, NextCells) :- length(Acc, L), NextCells = Acc.
next_cells(R-C, Direction, L, Acc, NextCells) :-
  length(Acc, N), N < L,
  append(Acc, [R-C], NextAcc),
  next_cell(R-C, Direction, Next),
  next_cells(Next, Direction, L, NextAcc, NextCells).

% Define next cell based on direction
next_cell(R-C, up, NextR-C) :- NextR is R - 1.
next_cell(R-C, down, NextR-C) :- NextR is R + 1.
next_cell(R-C, left, R-NextC) :- NextC is C - 1.
next_cell(R-C, right, R-NextC) :- NextC is C + 1.
next_cell(R-C, upleft, NextR-NextC) :- NextR is R - 1, NextC is C - 1.
next_cell(R-C, upright, NextR-NextC) :- NextR is R - 1, NextC is C + 1.
next_cell(R-C, downleft, NextR-NextC) :- NextR is R + 1, NextC is C - 1.
next_cell(R-C, downright, NextR-NextC) :- NextR is R + 1, NextC is C + 1.

% Directions
directions([up, down, left, right, upleft, upright, downleft, downright]).

% Solve Part 2
solve_part2(TotalCount) :-
  phrase_from_file(data(Matrix), 'day4_input.sql'),
  solve_part2(Matrix, Count),
  TotalCount is Count / 2.

solve_part2(Matrix, TotalCount) :-
  matrix_dimensions(Matrix, NRow, NCol),
  findall(Count, (between(1, NRow, R), between(1, NCol, C), count_x_mas(Matrix, R-C, Count)), Counts),
  sum_list(Counts, TotalCount).

% Count XMAS diagonals
count_x_mas(Matrix, R-C, Count) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, 0'M),
  xmas_directions(Directions),
  include(is_x_mas(Matrix, R-C), Directions, XMasList),
  length(XMasList, Count).

is_x_mas(Matrix, R-C, Direction) :-
  next_diag_cells(R-C, Direction, [NextCells1, NextCells2]),
  pick_letters(Matrix, NextCells1, Letters1),
  pick_letters(Matrix, NextCells2, Letters2),
  (Letters1 == `MAS`; Letters1 == `SAM`),
  (Letters2 == `MAS`; Letters2 == `SAM`).

% Generate next diagonal cells
next_diag_cells(R-C, upleft, [NextCells1, NextCells2]) :-
  next_cells(R-C, upleft, 3, [], NextCells1),
  NextC is C - 2,
  next_cells(R-NextC, upright, 3, [], NextCells2).

next_diag_cells(R-C, upright, [NextCells1, NextCells2]) :-
  next_cells(R-C, upright, 3, [], NextCells1),
  NextC is C + 2,
  next_cells(R-NextC, upleft, 3, [], NextCells2).

next_diag_cells(R-C, downleft, [NextCells1, NextCells2]) :-
  next_cells(R-C, downleft, 3, [], NextCells1),
  NextC is C - 2,
  next_cells(R-NextC, downright, 3, [], NextCells2).

next_diag_cells(R-C, downright, [NextCells1, NextCells2]) :-
  next_cells(R-C, downright, 3, [], NextCells1),
  NextC is C + 2,
  next_cells(R-NextC, downleft, 3, [], NextCells2).

% XMAS directions
xmas_directions([upleft, upright, downleft, downright]).

% Helper to get matrix dimensions
matrix_dimensions(Matrix, NRow, NCol) :-
  length(Matrix, NRow),
  [Row|_] = Matrix,
  length(Row, NCol).
