-module(checkout).

-export([total/3]).

-type item() :: string().
-type price() :: integer().

-spec total([item()], [{item(), price()}], any()) -> price().
total(ItemList, PriceList, _Specials) ->
    lists:sum([proplists:get_value(Item, PriceList) || Item <- ItemList]).
