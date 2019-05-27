-module(ebitset_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BITSZ, ebitset:tilesize() * ebitset:tilesize()).

init_test_() ->
    [test_zero(), test_empty_two(), test_two()].

test_zero() ->
    S = ebitset:new(),
    [?_assertEqual(0, ebitset:count(S)),
     ?_assertEqual(0, ebitset:minimum(S)),
     ?_assertEqual(0, ebitset:maximum(S))].

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

op_test_() ->
    [test_difference(), test_difference1(), test_difference2()].

test_difference() ->
    S1 = ebitset:new(),
    S2 = ebitset:new(),
    ?_assertEqual(0, ebitset:difference_count(S1, S2)).

test_difference1() ->
    S1 = ebitset:new(),
    S2 = ebitset:new(),
    ebitset:set(S2, 3),
    ?_assertEqual(0, ebitset:difference_count(S1, S2)).

test_difference2() ->
    S1 = ebitset:new(),
    S2 = ebitset:new(),
    ebitset:set(S1, 3),
    ?_assertEqual(1, ebitset:difference_count(S1, S2)).

multiset([], S) ->
    S;
multiset([H|L], S) ->
    multiset(L, ebitset:set(S, H)).

multiset(L) ->
    multiset(L, ebitset:new()).

multiunset([], S) ->
    S;
multiunset([H|L], S) ->
    multiunset(L, ebitset:unset(S, H)).

allset(SZ) ->
    multiset(lists:seq(0, SZ-1)).

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
symmetric_difference(L1, L2) ->
    LL1 = lists:usort(L1),
    LL2 = lists:usort(L2),
    LL = union(LL1, LL2),
    lists:subtract(LL, intersection(LL1, LL2)).

prop_set_by_list() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:new(L))).

prop_zero_intersection() ->
    ?FORALL({L1, L2}, {list(even_non_neg_integer(?BITSZ)), list(odd_non_neg_integer(?BITSZ))}, 0 == ebitset:count(ebitset:intersection(multiset(L1), ebitset:new(L2)))).

prop_zero_unset() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(multiunset(L, ebitset:new(L)))).

prop_union() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(union(L1,L2)) == ebitset:count(ebitset:union(ebitset:new(L1), multiset(L2)))).

prop_union_count() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, ebitset:union_count(multiset(L1),multiset(L2)) == ebitset:count(ebitset:union(multiset(L1), ebitset:new(L2)))).

prop_intersection() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(intersection(L1,L2)) == ebitset:count(ebitset:intersection(ebitset:new(L1), multiset(L2)))).

prop_intersection_zero() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(ebitset:intersection(ebitset:new(L), ebitset:new()))).

prop_intersects() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, (length(intersection(L1,L2)) > 0) == ebitset:intersects(multiset(L1), ebitset:new(L2))).

prop_difference() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(difference(L1,L2)) == ebitset:count(ebitset:difference(multiset(L1), ebitset:new(L2)))).

prop_difference_count() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(difference(L1,L2)) == ebitset:difference_count(multiset(L1), ebitset:new(L2))).

prop_difference_zero_count1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:difference_count(ebitset:new(), multiset(L))).

prop_difference_zero_count2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:difference_count(ebitset:new(L), ebitset:new())).

prop_symmetric_difference() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(symmetric_difference(L1,L2)) == ebitset:count(ebitset:symmetric_difference(multiset(L1), ebitset:new(L2)))).

prop_symmetric_difference_count() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(symmetric_difference(L1,L2)) == ebitset:symmetric_difference_count(multiset(L1), ebitset:new(L2))).

prop_symmetric_difference_zero_count() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:symmetric_difference_count(multiset(L), ebitset:new(L))).

prop_symmetric_difference_same_count() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:symmetric_difference_count(multiset(L), ebitset:new())).

prop_minimum() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), min(L) == ebitset:minimum(ebitset:intersection(allset(?BITSZ), ebitset:new(L)))).
prop_maximum() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), max(L) == ebitset:maximum(ebitset:intersection(allset(?BITSZ), ebitset:new(L)))).

prop_all_union() ->
    prop_all_union(?BITSZ).

prop_all_union(SZ) ->
    ?FORALL(L, list(bitile_idx_range(SZ)), SZ == ebitset:count(ebitset:union(allset(SZ), ebitset:new(L)))).

even_non_neg_integer(SZ) ->
    ?SUCHTHAT(X, bitile_idx_range(SZ), X rem 2 =:= 0).
odd_non_neg_integer(SZ) ->
    ?SUCHTHAT(X, bitile_idx_range(SZ), X rem 2 =:= 1).

bitile_idx_range(SZ) ->
    range(0, SZ-1).
