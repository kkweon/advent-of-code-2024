:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

map(Matrix) --> sequence(row, Matrix), blanks.
row(Row) --> sequence(cell, Row), "\n".

cell(antenna(Char)) --> [Value], { code_type(Value, alnum), char_code(Char, Value) }.
cell(dot) --> ".".

solve_part1(NumberOfAntinodes) :-
  phrase_from_file(map(Matrix), "./day8_input.txt"),
  solve_part1(Matrix, NumberOfAntinodes).

solve_part1(Matrix, NumberOfAntinodes) :-
  length(Matrix, NRow),
  [Row|_] = Matrix,
  length(Row, NCol),
  findall(R-C, (between(1, NRow, R), between(1, NCol, C)), Candidates),
  concurrent_maplist(antinode_runner(Matrix), Candidates, Numbers),
  sum_list(Numbers, NumberOfAntinodes).

antinode_runner(Matrix, R-C, Output) :-
  (antinode(Matrix, R-C) -> Output = 1; Output = 0).

solve_part2(NumberOfAntinodes) :-
  phrase_from_file(map(Matrix), "./day8_input.txt"),
  solve_part2(Matrix, NumberOfAntinodes).

solve_part2(Matrix, NumberOfAntinodes) :-
  length(Matrix, NRow),
  [Row|_] = Matrix,
  length(Row, NCol),
  findall(R-C, (between(1, NRow, R), between(1, NCol, C)), Candidates),
  concurrent_maplist(antinode_with_any_distance_runner(Matrix), Candidates, Numbers),
  sum_list(Numbers, NumberOfAntinodes).

antinode_with_any_distance_runner(Matrix, R-C, Output) :-
  (antinode_with_any_distance(Matrix, R-C) -> Output = 1; Output = 0).

antinode(Matrix, R-C) :-
  antenna(Matrix, R-C, Antenna, AntennaPosition, Distance),
  antenna(Matrix, R-C, Antenna, Antenna2Position, DoubleDistance),
  AntennaPosition \= Antenna2Position,
  DoubleDistance #= Distance * 2,
  in_line_from_origin(R-C, AntennaPosition, Antenna2Position).

antinode_with_any_distance(Matrix, R-C) :-
  antenna(Matrix, R-C, Antenna, AntennaPosition, _),
  antenna(Matrix, R-C, Antenna, Antenna2Position, _),
  AntennaPosition \= Antenna2Position,
  in_line_from_origin(R-C, AntennaPosition, Antenna2Position).

antinode_with_any_distance(Matrix, R-C) :-
  antenna(Matrix, R-C, Antenna, R-C, _),
  antenna(Matrix, R-C, Antenna, Antenna2Position, _),
  R-C \= Antenna2Position.

antenna(Matrix, ROrigin-COrigin, Antenna, R-C, Distance) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, Antenna),
  Antenna = antenna(_),
  Distance is abs(R - ROrigin) + abs(C - COrigin).

% True if R2-C2 is in the line from R1-C1 from ROrigin-COrigin.
in_line_from_origin(ROrigin-COrigin, R1-C1, R2-C2) :-
  % Same row.
  R1 #= ROrigin,
  R2 #= ROrigin,
  !,
  (chain([COrigin, C1, C2], #=<); chain([COrigin, C1, C2], #>=)).

in_line_from_origin(ROrigin-COrigin, R1-C1, R2-C2) :-
  % Same column.
  C1 #= COrigin,
  C2 #= COrigin,
  !,
  (chain([ROrigin, R1, R2], #=<); chain([ROrigin, R1, R2], #>=)).

in_line_from_origin(ROrigin-COrigin, R1-C1, R2-C2) :-
  % Same diagonal.
  slope(ROrigin-COrigin, R1-C1, Slope),
  slope(ROrigin-COrigin, R2-C2, Slope).

slope(ROrigin-COrigin, R1-C1, Slope) :-
  RDelta #= R1 - ROrigin,
  CDelta #= C1 - COrigin,
  simplify(RDelta-CDelta, Slope).

simplify(A-B, X-Y) :-
  ((A #= 0; B #= 0) -> X = A, Y = B
  ; GCD is gcd(abs(A), abs(B)),
    X #= A // GCD,
    Y #= B // GCD).

test_input(Input) :-
  Input = `............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
`.

test_matrix(Matrix) :-
  test_input(Input),
  phrase(map(Matrix), Input).

test_input2(Input) :-
% (2, 4) is antinode because (4, 5), (6, 6) are antennas
% (2, 1), (4, 2)
%
  Input = `..........
..........
..........
....a.....
..........
.....a....
..........
..........
..........
..........
`.

:- begin_tests(day8).

test(can_parse_map) :-
  Input = `...
.0.
.A.
`,
  phrase(map(Matrix), Input),
  assertion(Matrix == [
    [dot, dot, dot],
    [dot, antenna('0'), dot],
    [dot, antenna('A'), dot]
  ]).

test(slope) :-
  slope(1-1, 4-4, Slope),
  assertion(Slope = 1-1).

test(slope_2) :-
  slope(1-1, 4-2, Slope),
  assertion(Slope =:= 3-1).

test(solve_part1_with_test_input) :-
  test_input(Input),
  phrase(map(Matrix), Input),
  solve_part1(Matrix, NumberOfAntinodes),
  assertion(NumberOfAntinodes =:= 14).

test(solve_part2_with_test_input) :-
  test_input(Input),
  phrase(map(Matrix), Input),
  solve_part2(Matrix, NumberOfAntinodes),
  assertion(NumberOfAntinodes =:= 34).

:- end_tests(day8).
