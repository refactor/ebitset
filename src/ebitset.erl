-module(ebitset).
-export([new/0]).
-export([new/1]).
-export([copy/1]).
-export([union/2]).
-export([union_count/2]).
-export([intersection/2]).
-export([intersects/2]).
-export([difference/2]).
-export([symmetric_difference/2]).
-export([count/1]).
-export([minimum/1]).
-export([maximum/1]).
-export([set_by_list/2]).
-export([set_by_rawbinary/2]).
-export([set/2]).
-export([unset/2]).
-export([get/2]).

-export([tilesize/0]).

-on_load(init/0).

init() ->
    SoName = filename:join(case code:priv_dir(?MODULE) of
                               {error, bad_name} ->
                                   Dir = code:which(?MODULE),
                                   filename:join([filename:dirname(Dir),"..", "priv"]);
                               Dir -> Dir
                           end, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).

-spec new() -> reference().
new() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec new([non_neg_integer()] | binary()) -> reference().
new(L) when is_list(L)->
    RawBin = << <<X:32/native-integer>> || X <- L >>,
    from_binary(RawBin);
new(L) when is_binary(L)->
    from_binary(L).

from_binary(_Bin) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec copy(reference()) -> reference().
copy(_Bitset) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% OR
-spec union(reference(), reference()) -> reference().
union(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec union_count(reference(), reference()) -> non_neg_integer().
union_count(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec intersection(reference(), reference()) -> reference().
intersection(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% AND
-spec intersects(reference(), reference()) -> boolean().
intersects(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec difference(reference(), reference()) -> reference().
difference(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% XOR 
-spec symmetric_difference(reference(), reference()) -> reference().
symmetric_difference(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec set_by_list(reference(), [non_neg_integer()]) -> reference().
set_by_list(Bitset, L) ->
    RawBin = << <<X:32/native-integer>> || X <- L >>,
    set_by_rawbinary(Bitset, RawBin).

-spec set_by_rawbinary(reference(), non_neg_integer()) -> reference().
set_by_rawbinary(_Bitset, _L) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec set(reference(), non_neg_integer()) -> reference().
set(_Bitset, _Idx) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec unset(reference(), non_neg_integer()) -> reference().
unset(_Bitset, _Idx) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec get(reference(), non_neg_integer()) -> boolean().
get(_Bitset, _Idx) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec count(reference()) -> non_neg_integer().
count(_Bitset) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec minimum(reference()) -> non_neg_integer().
minimum(_Bitset) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec maximum(reference()) -> non_neg_integer().
maximum(_Bitset) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec tilesize() -> 256 | 512.
tilesize() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
