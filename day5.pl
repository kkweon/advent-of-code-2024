:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- dynamic before_after/2.

parser(Rules, Updates) -->
    sequence(rule_before_after, Rules),
    blanks,
    sequence(rule_update, Updates),
    eos.

rule_before_after(A-B) -->
    number(A), "|", number(B), "\n".

rule_update(Numbers) -->
    sequence(number, ",", Numbers), "\n".

example_input(Out) :-
    Out = `47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
`.

solve_part1(Sum) :-
    retractall(before_after(_, _)),
    phrase_from_file(parser(Rules, Updates), "./day5_input.txt"),
    foreach(member(A-B, Rules), assertz(before_after(A, B))),
    include(valid_update, Updates, ValidUpdates),
    maplist(pick_middle, ValidUpdates, MiddleList),
    sum_list(MiddleList, Sum),
    retractall(before_after(_, _)).

solve_part2(Sum) :-
    retractall(before_after(_, _)),
    phrase_from_file(parser(Rules, Updates), "./day5_input.txt"),
    foreach(member(A-B, Rules), assertz(before_after(A, B))),
    include([In]>>(\+ valid_update(In)), Updates, InvalidUpdates),
    maplist(invalid_valid, InvalidUpdates, ValidUpdates),
    maplist(pick_middle, ValidUpdates, MiddleList),
    sum_list(MiddleList, Sum),
    retractall(before_after(_, _)).

valid_update([_]).
valid_update([A, B|Rest]) :-
    before_after(A, B),
    valid_update([B|Rest]).

pick_middle(Numbers, Middle) :-
    length(Numbers, Len),
    MiddleIdx is Len // 2,
    nth0(MiddleIdx, Numbers, Middle).

invalid_valid(List, Valid) :-
    predsort(compare_before_after, List, Valid).

compare_before_after(Order, A, B) :-
    (before_after(A, B) -> Order = (<)
    ; A =:= B -> Order = (=)
    ; Order = (>)).
