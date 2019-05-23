-module(ebitset).
-export([new/0]).
-export([count/1]).

-on_load(init/0).

init() ->
    SoName = filename:join(case code:priv_dir(?MODULE) of
                               {error, bad_name} ->
                                   Dir = code:which(?MODULE),
                                   filename:join([filename:dirname(Dir),"..", "priv"]);
                               Dir -> Dir
                           end, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).

new() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

count(_Bitset) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
