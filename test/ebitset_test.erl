-module(ebitset_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

init_test_() ->
    [test_zero(), test_empty_two(), test_two()].

test_zero() ->
    ?_assertEqual(0, ebitset:count(ebitset:new())).

test_empty_two() ->
    S1 = ebitset:new(),
    ebitset:set(S1, 1),
    ebitset:set(S1, 3),
    ebitset:set(S1, 5),
    S2 = ebitset:new(),
    ebitset:set(S2, 2),
    ebitset:set(S2, 4),
    ?_assertEqual(0, ebitset:count(ebitset:intersection(S1,S2))).

test_two() ->
    S1 = ebitset:new(),
    ebitset:set(S1, 1),
    ebitset:set(S1, 3),
    ebitset:set(S1, 5),
    S2 = ebitset:new(),
    ebitset:set(S2, 2),
    ebitset:set(S2, 4),
    ?_assertEqual(5, ebitset:count(ebitset:union(S1,S2))).

multiset([], S) ->
    S;
multiset([H|L], S) ->
    multiset(L, ebitset:set(S, H)).

multiset(L) ->
    multiset(L, ebitset:new()).

allset() ->
    multiset(lists:seq(0, 255)).

min([]) -> 0;
min(L) -> lists:min(L).
max([]) -> 0;
max(L) -> lists:max(L).
union(L1, L2) -> lists:umerge(lists:usort(L1), lists:usort(L2)).
intersection(L1, L2) ->
    L = lists:usort(L1),
    lists:subtract(L, lists:subtract(L, L2)).
difference(L1, L2) -> 
    L = lists:usort(L1),
    lists:subtract(L, L2).

prop_set_by_list() ->
    ?FORALL(L, list(non_neg_integer()), length(lists:usort(L)) == ebitset:count(multiset(L))).

prop_zero_intersection() ->
    ?FORALL({L1, L2}, {list(even_non_neg_integer()), list(odd_non_neg_integer())}, 0 == ebitset:count(ebitset:intersection(multiset(L1), multiset(L2)))).

prop_union() ->
    ?FORALL({L1, L2}, {list(non_neg_integer()), list(non_neg_integer())}, length(union(L1,L2)) == ebitset:count(ebitset:union(multiset(L1), multiset(L2)))).

prop_union_count() ->
    ?FORALL({L1, L2}, {list(non_neg_integer()), list(non_neg_integer())}, ebitset:union_count(multiset(L1),multiset(L2)) == ebitset:count(ebitset:union(multiset(L1), multiset(L2)))).

prop_intersection() ->
    ?FORALL({L1, L2}, {list(non_neg_integer()), list(non_neg_integer())}, length(intersection(L1,L2)) == ebitset:count(ebitset:intersection(multiset(L1), multiset(L2)))).

prop_intersects() ->
    ?FORALL({L1, L2}, {list(non_neg_integer()), list(non_neg_integer())}, (length(intersection(L1,L2)) > 0) == ebitset:intersects(multiset(L1), multiset(L2))).

prop_difference() ->
    ?FORALL({L1, L2}, {list(non_neg_integer()), list(non_neg_integer())}, length(difference(L1,L2)) == ebitset:count(ebitset:difference(multiset(L1), multiset(L2)))).

prop_minimum() ->
    ?FORALL(L, list(range(0, 255)), min(L) == ebitset:minimum(ebitset:intersection(allset(), multiset(L)))).
prop_maximum() ->
    ?FORALL(L, list(range(0, 255)), max(L) == ebitset:maximum(ebitset:intersection(allset(), multiset(L)))).

prop_all_union() ->
    ?FORALL(L, list(range(0, 255)), 256 == ebitset:count(ebitset:union(allset(), multiset(L)))).

even_non_neg_integer() ->
    ?SUCHTHAT(X, non_neg_integer(), X rem 2 =:= 0).
odd_non_neg_integer() ->
    ?SUCHTHAT(X, non_neg_integer(), X rem 2 =:= 1).
