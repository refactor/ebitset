-module(ebitset).
-export([new/0]).
-export([copy/1]).
-export([union/2]).
-export([intersection/2]).
-export([count/1]).
-export([set/2]).
-export([get/2]).

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

-spec copy(reference()) -> reference().
copy(_Bitset) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec union(reference(), reference()) -> reference().
union(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec intersection(reference(), reference()) -> reference().
intersection(_Bitset1, _Bitset2) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec set(reference(), non_neg_integer()) -> reference().
set(_Bitset, _Idx) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec get(reference(), non_neg_integer()) -> boolean().
get(_Bitset, _Idx) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

count(_Bitset) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
