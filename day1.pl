:- use_module(library(dcg/basics)).

list_reader([X-Y|Rest]) --> number(X), whites, number(Y), blanks, list_reader(Rest).
list_reader([]) --> [].

solve(D) :-
  phrase_from_file(list_reader(List), "./day1_input.txt"),
  pairs_keys(List, FirstList),
  pairs_values(List, SecondList),
  msort(FirstList, FirstListSorted),
  msort(SecondList, SecondListSorted),
  pairs_keys_values(Pairs, FirstListSorted, SecondListSorted),
  foldl(dist, Pairs, 0, D).

dist(A-B, Acc, NextAcc) :-
  Diff is abs(A - B),
  NextAcc is Acc + Diff.
