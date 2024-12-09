:- set_prolog_flag(stack_limit, 1000000000000).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).

disk_map(Files) -->
  sequence(file_map, Files), blanks.

file_map(file(FileBlock, FreeBlock)) -->
  single_digit(FileBlock),
  optional(single_digit(FreeBlock), { FreeBlock = 0 }).

single_digit(D) -->
  digit(C), { code_type(C, digit(D)) }.

solve_part1(Checksum) :-
  phrase_from_file(disk_map(Files), "./day9_input.txt"),
  solve_part1(Files, Checksum).

solve_part1(Files, Checksum) :-
  length(Files, NFiles),
  End #= NFiles - 1,
  numlist(0, End, Indices),
  pairs_keys_values(FilePairs, Indices, Files),
  build_compact_list(FilePairs, [], CompactList),
  calculate_checksum(CompactList, Checksum).

calculate_checksum(CompactList, Checksum) :-
  length(CompactList, CompactListLength),
  CompactListEnd #= CompactListLength - 1,
  numlist(0, CompactListEnd, CompactListIndices),
  scalar_product(CompactList, CompactListIndices, #=, Checksum).

build_compact_list([], Acc, Acc).
build_compact_list([FileID-file(FileBlock, FreeBlock) | FilePairs], Acc, CompactList) :-
  length(Prefix, FileBlock),
  repeat(FileID, FileBlock, Prefix),
  % Make the suffix list
  make_suffix(FreeBlock, FilePairs, [], Suffix-NextFilePairs),
  append(Prefix, Suffix, SingleChecksumList),
  append(Acc, SingleChecksumList, NewAcc),
  build_compact_list(NextFilePairs, NewAcc, CompactList).

make_suffix(0, Files, Acc, Acc-Files).
make_suffix(FreeBlock, [], Acc, Acc-[]) :-
  FreeBlock #> 0.
make_suffix(FreeBlock, FilePairs, Acc, Result) :-
  FreeBlock #> 0,
  length(FilePairs, NFiles),
  NFiles #> 0,
  nth1(NFiles, FilePairs, FileID-file(FileBlock, FB), FilePairsRest),
  RemainingFileBlock #= FileBlock - FreeBlock,
  ConsumedFileBlock is min(FileBlock, FreeBlock),
  NextFreeBlock #= FreeBlock - ConsumedFileBlock,
  (
    RemainingFileBlock #> 0 ->
    append(FilePairsRest, [FileID-file(RemainingFileBlock, FB)], NextFilePairs)
  ;
    NextFilePairs = FilePairsRest
  ),
  repeat(FileID, ConsumedFileBlock, CurrSuffix),
  append(Acc, CurrSuffix, NewAcc),
  make_suffix(NextFreeBlock, NextFilePairs, NewAcc, Result).

repeat(V, N, Result) :-
  length(Result, N),
  foreach(between(1, N, I), nth1(I, Result, V)).

solve_part2(Checksum) :-
  phrase_from_file(disk_map(Files), "./day9_input.txt"),
  solve_part2(Files, Checksum).

solve_part2(Files, Checksum) :-
  length(Files, NFiles),
  End #= NFiles - 1,
  numlist(0, End, Indices),
  pairs_keys_values(FilePairs, Indices, Files),
  file_pairs_to_list(FilePairs, ListOfList),
  build_compact_list3(ListOfList, NestedCompactList),
  flatten(NestedCompactList, FlattenedCompactList),
  fill_with_zeros(FlattenedCompactList, FilledFlattenedCompactList),
  calculate_checksum(FilledFlattenedCompactList, Checksum).

fill_with_zeros(List, Out) :-
  maplist(fill_with_zeros_helper, List, Out).

fill_with_zeros_helper(nil, 0).
fill_with_zeros_helper(H, H) :-
  H \= nil.

flatten([], []).
flatten([H-T|Rest], Out) :-
  append(H, T, NewH),
  flatten(Rest, NewRest),
  append(NewH, NewRest, Out).

build_compact_list3(List, Out) :-
  reverse(List, ReversedList),
  compact_list3_runner(ReversedList, [], Out).

compact_list3_runner([], Acc, Acc).

compact_list3_runner([H-T|Rest], Acc, Out) :-
  % can H fit in Reversed Rest?
  move_(H, Rest, NewRest),
  length(H, HLength),
  [ID|_] = H,
  repeat(nil, HLength, NewH),
  compact_list3_runner(NewRest, [NewH-T|Acc], Out).
compact_list3_runner([H-T|Rest], Acc, Out) :-
  % cannot move
  \+move_(H, Rest, _),
  compact_list3_runner(Rest, [H-T|Acc], Out).

move_(FileBlocks, Rest, NewRest) :-
  length(FileBlocks, FileBlocksLength),
  reverse(Rest, ReversedRest),
  nth1(I, ReversedRest, FileB-FreeB),
  count_nil(FreeB, FreeBLength),
  FreeBLength #>= FileBlocksLength,
  merge(FreeB, FileBlocks, NewFreeB),
  replace_nth1(ReversedRest, I, FileB-NewFreeB, NewRestReversed),
  reverse(NewRestReversed, NewRest).

count_nil([], 0).
count_nil([H|T], N) :-
  H \= nil,
  count_nil(T, N).
count_nil([nil|T], N) :-
  N #= 1 + N0,
  count_nil(T, N0).

merge(L1, [], L1) :- !.
merge([], L2, L2) :- !.
merge([H1|T1], [H2|T2], [H1|NewFreeB]) :-
  H1 \= nil,
  merge(T1, [H2|T2], NewFreeB).
merge([nil|T1], [H2|T2], [H2|NewFreeB]) :-
  merge(T1, T2, NewFreeB).



replace_nth1(List, N, NewElement, NewList) :-
  N_1 #= N - 1,
  length(Prefix, N_1),
  append(Prefix, [_|Suffix], List),
  append(Prefix, [NewElement|Suffix], NewList).


file_pairs_to_list([], []).
file_pairs_to_list([FileID-file(FileBlock, FreeBlock) | FilePairs], [Head | Rest]) :-
  repeat(FileID, FileBlock, Prefix),
  repeat(nil, FreeBlock, Suffix),
  Head = Prefix-Suffix,
  file_pairs_to_list(FilePairs, Rest).

build_compact_list2([], Acc, Acc).
build_compact_list2([FileID-file(FileBlock, FreeBlock) | FilePairs], Acc, CompactList) :-
  length(Prefix, FileBlock),
  repeat(FileID, FileBlock, Prefix),
  move_files_until_full(FreeBlock, FilePairs, Suffix-NextFilePairs),
  append(Prefix, Suffix, SingleCompactList),
  length(SingleCompactList, SingleCompactListLength),
  (SingleCompactListLength #< FileBlock + FreeBlock ->
    Diff #= FileBlock + FreeBlock - SingleCompactListLength,
    repeat(0, Diff, Padding),
    append(SingleCompactList, Padding, NewSingleCompactList),
    append(Acc, NewSingleCompactList, NewAcc)
  ;
    append(Acc, SingleCompactList, NewAcc)
  ),
  build_compact_list2(NextFilePairs, NewAcc, CompactList).

move_files_until_full(0, FilePairs, []-FilePairs).
move_files_until_full(FreeBlocks, FilePairs, Suffix-NextFilePairs) :-
  FreeBlocks #> 0,
  pick_file_from_last(FreeBlocks, FilePairs, LastFilePair, FilePairsRest),
  ( LastFilePair == nil ->
    repeat(0, FreeBlocks, Suffix),
    NextFilePairs = FilePairsRest
  ;
    LastFilePair = FileID-file(FileBlock, FreeBlock),
    RemainingFreeBlocks #= FreeBlocks - FileBlock,
    repeat(FileID, FileBlock, Prefix),
    append(Prefix, Suffix0, Suffix),
    move_files_until_full(RemainingFreeBlocks, FilePairsRest, Suffix0-NextFilePairs)
  ).

pick_file_from_last(FreeBlocks, FilePairs, LastFilePair, FilePairsRest) :-
  reverse(FilePairs, ReversedFilePairs),
  include(fits_in_free_blocks(FreeBlocks), ReversedFilePairs, FittedFiles),
  length(FittedFiles, NFiles),
  (
    NFiles #> 0 ->
    [LastFilePair | _] = FittedFiles,
    LastFilePair = FileID-_,
    select(FileID-_, FilePairs, FilePairsRest)
  ;
    LastFilePair = nil,
    FilePairsRest = FilePairs
  ).

fits_in_free_blocks(FreeBlocks, _-file(FileBlock, _)) :-
  FreeBlocks #>= FileBlock.


test_input(Input) :-
  Input = `2333133121414131402`.

test_files(Files) :-
  test_input(Input),
  phrase(disk_map(Files), Input).

:- begin_tests(day9).

test(disk_map) :-
  phrase(disk_map(Files), `12345`),
  Expected = [file(1, 2), file(3, 4), file(5, 0)],
  assertion(Files == Expected).

test(solve_part1) :-
  Files = [file(1, 2), file(3, 4), file(5, 0)],
  solve_part1(Files, Checksum),
  ExpectedList = [0, 2, 2, 1, 1, 1, 2, 2, 2],
  length(ExpectedList, ExpectedLength),
  End #= ExpectedLength - 1,
  numlist(0, End, ExpectedIndices),
  scalar_product(ExpectedList, ExpectedIndices, #=, ExpectedChecksum),
  assertion(Checksum =:= ExpectedChecksum).

test(make_suffix) :-
  FilePairs = [1-file(3, 4), 2-file(5, 0)],
  make_suffix(2, FilePairs, [], Suffix-Rest),
  assertion(Suffix == [2, 2]),
  assertion(Rest == [1-file(3, 4), 2-file(3, 0)]).

test(solve_part1_test) :-
  test_files(Files),
  solve_part1(Files, Checksum),
  assertion(Checksum =:= 1928).

test(checksum_compact_list) :-
  CompactList = [
    0, 0, 9, 9, 2, 1, 1, 1, 7, 7, 7, 0, 4, 4, 0, 3, 3, 3, 0, 0, 0, 0, 5, 5, 5, 5, 0, 6, 6, 6, 6, 0, 0, 0, 0, 0, 8, 8, 8, 8, 0, 0
  ],
  calculate_checksum(CompactList, Checksum),
  assertion(Checksum =:= 2858).

test(solve_part2_test) :-
  test_files(Files),
  solve_part2(Files, Checksum),
  assertion(Checksum =:= 2858).

test(file_pairs_to_list) :-
  FilePairs = [0-file(1, 2), 1-file(3, 4), 2-file(5, 0)],
  file_pairs_to_list(FilePairs, ListOfList),
  assertion(ListOfList == [[0]-[nil, nil], [1, 1, 1]-[nil, nil, nil, nil], [2, 2, 2, 2, 2]-[]]).

test(build_compact_list3) :-
  ListOfList = [[0]-[nil, nil, nil], [1, 1, 1]-[nil, nil, nil, nil], [2, 2, 2, 2, 2]-[]],
  build_compact_list3(ListOfList, CompactList),
  assertion(CompactList ==
    [
      [0]-[1,1,1],
      [nil, nil, nil]-[nil,nil,nil,nil],
      [2,2,2,2,2]-[]
    ]).

test(build_compact_list32) :-
  % 00...111...2...333.44.5555.6666.777.888899
  Input = [
    [0, 0]-[nil, nil, nil], [1, 1, 1]-[nil, nil, nil], [2]-[nil, nil, nil],
    [3, 3, 3]-[nil],
    [4, 4]-[nil],
    [5, 5, 5, 5]-[nil],
    [6, 6, 6, 6]-[nil],
    [7, 7, 7]-[nil],
    [8, 8, 8, 8]-[],
    [9, 9]-[]
  ],
  build_compact_list3(Input, CompactList),
  assertion(CompactList ==
    [
      [0, 0]-[9, 9, 2],
      [1, 1, 1]-[7, 7, 7],
      [nil]-[4, 4, nil],
      [3, 3, 3]-[nil],
      [nil, nil]-[nil],
      [5, 5, 5, 5]-[nil],
      [6, 6, 6, 6]-[nil],
      [nil, nil, nil]-[nil],
      [8, 8, 8, 8]-[],
      [nil, nil]-[]
    ]).


test(move_fill_backs_first) :-
  FileBlocks = [1, 1, 1],
  Rest = [[1]-[nil, nil, nil], [2]-[nil, nil, nil]],
  move_(FileBlocks, Rest, NewRest),
  assertion(NewRest == [
    [1]-[nil, nil, nil],
    [2]-[1, 1, 1]
  ]).

:- end_tests(day9).
