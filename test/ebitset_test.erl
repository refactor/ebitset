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

prop_set_by_list() ->
    ?FORALL(L, list(non_neg_integer()), length(lists:usort(L)) == ebitset:count(multiset(L))).
