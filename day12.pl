:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- dynamic cell/4.

garden(G) -->
  sequence(plant_row, G), blanks.

plant_row(R) -->
  sequence(plant, R), "\n".

plant(DowncasedAtom) -->
  [C], { code_type(C, upper), atom_codes(Atoms, [C]),
  downcase_atom(Atoms, DowncasedAtom) }.

groups(Matrix, SortedGroups) :-
  length(Matrix, NRow),
  [Row|_] = Matrix,
  length(Row, NCol),
  findall(G,
    (
      between(1, NRow, R),
      between(1, NCol, C),
      find_group_at(Matrix, R-C, G)
    ), UnsortedGroups),
  sort(UnsortedGroups, SortedGroups).

group_perimeter(Matrix, Group, Perimeter) :-
  findall(P, (member(R-C, Group), perimeter_at(Matrix, R-C, P)), PerimeterList),
  sum_list(PerimeterList, Perimeter).

group_price(Matrix, Group, Price) :-
  group_perimeter(Matrix, Group, Perimeter),
  length(Group, Area),
  Price is Perimeter * Area.

matrix_price(Matrix, Price) :-
  groups(Matrix, Groups),
  findall(Price,
    (
      member(Group, Groups),
      group_price(Matrix, Group, Price)
    ),
    Prices),
  sum_list(Prices, Price).

:- table plant_at/3.
plant_at(Matrix, R-C, Plant) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, Plant).

perimeter_at(Matrix, R-C, P) :-
  plant_at(Matrix, R-C, Plant),
  findall(1,
    (
      delta(RDelta-CDelta),
      R1 is R + RDelta,
      C1 is C + CDelta,
      plant_at(Matrix, R1-C1, Plant)
    ),
    Neighbors),
  length(Neighbors, NeighborCount),
  P is 4 - NeighborCount.

find_group_at(Matrix, R-C, G) :-
  plant_at(Matrix, R-C, Plant),
  findall(P,
    connected_plants(Matrix, R-C, Plant, P),
    Plants),
  sort(Plants, G).

:- table connected_plants/4.
connected_plants(_, R-C, _, R-C).
connected_plants(Matrix, R-C, Plant, R1-C1) :-
  delta(RDelta-CDelta),
  R1 is R + RDelta,
  C1 is C + CDelta,
  plant_at(Matrix, R1-C1, Plant).

connected_plants(Matrix, R-C, Plant, R3-C3) :-
  connected_plants(Matrix, R-C, Plant, R2-C2),
  connected_plants(Matrix, R2-C2, Plant, R3-C3).

delta(Delta) :-
  member(Delta, [1-0, -1-0, 0-1, 0-(-1)]).

test_input1(Input) :-
  Input = `AAAA
BBCD
BBCC
EEEC
`.

test_matrix1(G) :-
  test_input1(Input),
  phrase(garden(G), Input).

solve(Part1) :-
  phrase_from_file(garden(G), 'day12_input.txt'),
  matrix_price(G, Price),
  Price = Part1.

:- begin_tests(garden).

test(garden) :-
  phrase(garden(G), `AAAA
BBCD
BBCC
EEEC
`),
  assertion(G == [
    [a, a, a, a],
    [b, b, c, d],
    [b, b, c, c],
    [e, e, e, c]
  ]).

test(connected_plants) :-
    test_matrix1(G),
    groups(G, Groups),
    assertion(Groups == [
        [1-1, 1-2, 1-3, 1-4],
        [2-1, 2-2, 3-1, 3-2],
        [2-3, 3-3, 3-4, 4-4],
        [2-4],
        [4-1, 4-2, 4-3]
    ]).

test(group_perimeter) :-
  test_matrix1(G),
  groups(G, _),
  group_perimeter(G, [2-3, 3-3, 3-4, 4-4], Perimeter),
  assertion(Perimeter =:= 10).

test(perimeter_at) :-
  test_matrix1(G),
  perimeter_at(G, 2-3, Perimeter),
  assertion(Perimeter =:= 3).

test(matrix_price) :-
  test_matrix1(G),
  matrix_price(G, Price),
  assertion(Price =:= 140).

:- end_tests(garden).
