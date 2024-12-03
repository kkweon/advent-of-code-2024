:- use_module(library(dcg/basics)).

list_reader([X-Y|Rest]) -->
  number(X), whites, number(Y), blanks, list_reader(Rest).
list_reader([]) --> [].

solve(Similarity) :-
  phrase_from_file(list_reader(Input), "./day1_input.txt"),
  pairs_keys(Input, FirstList),
  pairs_values(Input, SecondList),
  msort(SecondList, SecondListSorted),
  clumped(SecondListSorted, NumAppearances),
  foldl(get_similarity(NumAppearances), FirstList, 0, Similarity).

get_similarity(NumAppearances, X, Acc, NextAcc) :-
  member(X-C, NumAppearances),
  NextAcc is Acc + X * C;
  \+ memberchk(X-_, NumAppearances),
  NextAcc is Acc.
