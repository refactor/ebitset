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

%% set TESTs
prop_set_by_list() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:new(L))).

prop_zero_unset() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(multiunset(L, ebitset:new(L)))).

%% union TESTs
prop_union() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(union(L1,L2)) == ebitset:count(ebitset:union(ebitset:new(L1), multiset(L2)))).

prop_union_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), ?BITSZ == ebitset:count(ebitset:union(allset(?BITSZ), ebitset:new(L)))).

prop_union_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), ?BITSZ == ebitset:count(ebitset:union(ebitset:new(L), allset(?BITSZ)))).

prop_union_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:union(ebitset:new(), ebitset:new(L)))).

prop_union_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:union(ebitset:new(L), ebitset:new()))).

%% union-count TESTs
prop_union_count() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, ebitset:union_count(multiset(L1),multiset(L2)) == ebitset:count(ebitset:union(multiset(L1), ebitset:new(L2)))).

prop_union_count_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:union_count(multiset(L),ebitset:new())).

prop_union_count_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:union_count(ebitset:new(), ebitset:new(L))).

prop_union_count_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), ?BITSZ == ebitset:union_count(ebitset:new(L), allset(?BITSZ))).

prop_union_count_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), ?BITSZ == ebitset:union_count(allset(?BITSZ), ebitset:new(L))).

%% intersection TESTs
prop_intersection() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(intersection(L1,L2)) == ebitset:count(ebitset:intersection(ebitset:new(L1), multiset(L2)))).

prop_intersection_zero() ->
    ?FORALL({L1, L2}, {list(even_non_neg_integer(?BITSZ)), list(odd_non_neg_integer(?BITSZ))}, 0 == ebitset:count(ebitset:intersection(multiset(L1), ebitset:new(L2)))).

prop_intersection_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(ebitset:intersection(ebitset:new(L), ebitset:new()))).

prop_intersection_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(ebitset:intersection(ebitset:new(), ebitset:new(L)))).

prop_intersection_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:intersection(allset(?BITSZ), ebitset:new(L)))).

prop_intersection_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:intersection(ebitset:new(L), allset(?BITSZ)))).

prop_intersection_same() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:intersection(ebitset:new(L), ebitset:new(L)))).

%% intersects TESTs
prop_intersects() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, (length(intersection(L1,L2)) > 0) == ebitset:intersects(multiset(L1), ebitset:new(L2))).

prop_intersects_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), false == ebitset:intersects(multiset(L), ebitset:new())).

prop_intersects_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), false == ebitset:intersects(ebitset:new(), ebitset:new(L))).

prop_intersects_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)),
            ?IMPLIES(no_empty(L),
                     true == ebitset:intersects(allset(?BITSZ), ebitset:new(L)))).

prop_intersects_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)),
            ?IMPLIES(no_empty(L),
                     true == ebitset:intersects(ebitset:new(L), allset(?BITSZ)))).

prop_intersects_same() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)),
            ?IMPLIES(no_empty(L), 
                     true == ebitset:intersects(multiset(L), ebitset:new(L)))).

no_empty([]) -> false;
no_empty(L) -> true.

%% difference TESTs
prop_difference() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(difference(L1,L2)) == ebitset:count(ebitset:difference(multiset(L1), ebitset:new(L2)))).

prop_difference_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:difference(ebitset:new(L), ebitset:new()))).

prop_difference_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(ebitset:difference(ebitset:new(), ebitset:new(L)))).

prop_difference_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(ebitset:difference(ebitset:new(L), allset(?BITSZ)))).

prop_difference_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), (?BITSZ - length(lists:usort(L))) == ebitset:count(ebitset:difference(allset(?BITSZ), ebitset:new(L)))).

prop_difference_same() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(ebitset:difference(ebitset:new(L), ebitset:new(L)))).

%% difference-count TESTs
prop_difference_count() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(difference(L1,L2)) == ebitset:difference_count(multiset(L1), ebitset:new(L2))).

prop_difference_count_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:difference_count(ebitset:new(), multiset(L))).

prop_difference_count_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:difference_count(ebitset:new(L), ebitset:new())).

prop_difference_count_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:difference_count(ebitset:new(L), allset(?BITSZ))).

prop_difference_count_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), (?BITSZ - length(lists:usort(L))) == ebitset:difference_count(allset(?BITSZ), ebitset:new(L))).

prop_difference_count_same() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:difference_count(ebitset:new(L), ebitset:new(L))).

%% symmetric-difference TESTs
prop_symmetric_difference() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(symmetric_difference(L1,L2)) == ebitset:count(ebitset:symmetric_difference(multiset(L1), ebitset:new(L2)))).

prop_symmetric_difference_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:symmetric_difference(ebitset:new(), ebitset:new(L)))).

prop_symmetric_difference_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:count(ebitset:symmetric_difference(ebitset:new(L), ebitset:new()))).

prop_symmetric_difference_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), (?BITSZ - length(lists:usort(L))) == ebitset:count(ebitset:symmetric_difference(allset(?BITSZ), ebitset:new(L)))).

prop_symmetric_difference_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), (?BITSZ - length(lists:usort(L))) == ebitset:count(ebitset:symmetric_difference(ebitset:new(L), allset(?BITSZ)))).

prop_symmetric_difference_same() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:count(ebitset:symmetric_difference(ebitset:new(L), ebitset:new(L)))).

%% symmetric-difference-count TESTs
prop_symmetric_difference_count() ->
    ?FORALL({L1, L2}, {list(bitile_idx_range(?BITSZ)), list(bitile_idx_range(?BITSZ))}, length(symmetric_difference(L1,L2)) == ebitset:symmetric_difference_count(multiset(L1), ebitset:new(L2))).

prop_symmetric_difference_count_zero1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:symmetric_difference_count(multiset(L), ebitset:new())).

prop_symmetric_difference_count_zero2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), length(lists:usort(L)) == ebitset:symmetric_difference_count(ebitset:new(), ebitset:new(L))).

prop_symmetric_difference_count_all1() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), (?BITSZ - length(lists:usort(L))) == ebitset:symmetric_difference_count(allset(?BITSZ), ebitset:new(L))).

prop_symmetric_difference_count_all2() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), (?BITSZ - length(lists:usort(L))) == ebitset:symmetric_difference_count(ebitset:new(L), allset(?BITSZ))).

prop_symmetric_difference_count_same() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), 0 == ebitset:symmetric_difference_count(ebitset:new(L), ebitset:new(L))).

%% minimum & maximum TESTs
prop_minimum() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), min(L) == ebitset:minimum(ebitset:intersection(allset(?BITSZ), ebitset:new(L)))).
prop_maximum() ->
    ?FORALL(L, list(bitile_idx_range(?BITSZ)), max(L) == ebitset:maximum(ebitset:intersection(allset(?BITSZ), ebitset:new(L)))).

%%
%% private helper
even_non_neg_integer(SZ) ->
    ?SUCHTHAT(X, bitile_idx_range(SZ), X rem 2 =:= 0).
odd_non_neg_integer(SZ) ->
    ?SUCHTHAT(X, bitile_idx_range(SZ), X rem 2 =:= 1).

bitile_idx_range(SZ) ->
    range(0, SZ-1).
