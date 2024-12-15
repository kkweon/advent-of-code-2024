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

build_cells(Matrix) :-
  retractall(cell(_, _, _, _)),
  length(Matrix, NRow),
  [Row|_] = Matrix,
  length(Row, NCol),
  forall(
    (
      between(1, NRow, R),
      between(1, NCol, C)
    ),
    build_cell(R, C, Matrix)
  ),
  groups(NRow-NCol, Groups),
  format('Groups: ~w~n', [Groups]),
  length(Groups, NGroup),
  forall(between(1, NGroup, G),
    (
      nth1(G, Groups, Group),
      assign_group(Group, G)
    )
  ).

build_cell(R, C, Matrix) :-
  nth1(R, Matrix, Row),
  nth1(C, Row, Cell),
  assert(cell(R, C, Cell, _)).

assign_group([], _).
assign_group([R-C|Rest], G) :-
  retract(cell(R, C, V, _)),
  assert(cell(R, C, V, G)),
  assign_group(Rest, G).

groups(NRow-NCol, Groups) :-
  findall(Group,
    (
      between(1, NRow, R),
      between(1, NCol, C),
      build_group(R-C, Group)
    ),
    Groups
  ),
  maplist(sort, Groups, SortedGroups),
  sort(SortedGroups, Groups).

build_group(R-C, Group) :-
  findall(Cell,
    (
      connected(R-C, Cell)
    ), Group),
  format('Group: ~w~n', [Group]).

:- table connected/2.

connected(R1-C1, R2-C2) :-
  cell(R1, C1, V, _),
  delta(RD-CD),
  R2 is R1 + RD,
  C2 is C1 + CD,
  cell(R2, C2, V, _).
connected(R1-C1, R2-C2) :-
  connected(R1-C1, R3-C3),
  connected(R3-C3, R2-C2).

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

:- end_tests(garden).
