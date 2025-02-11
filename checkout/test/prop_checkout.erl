-module(prop_checkout).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

% Property
prop_no_special1() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList}, item_price_list(),
            ExpectedPrice =:= checkout:total(ItemList, PriceList, [])).

prop_no_special2() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList}, item_price_list(),
            collect(
                bucket(length(ItemList), 10),
                ExpectedPrice =:= checkout:total(ItemList, PriceList, [])
            )).

prop_special() ->
    ?FORALL({ItemList, ExpectedPrice, PriceList, SpecialList}, item_price_special(),
        ExpectedPrice =:= checkout:total(ItemList, PriceList, SpecialList)).

prop_expected_result() ->
    ?FORALL({ItemList, PriceList, SpecialList}, lax_lists(),
        collect(
          item_list_type(ItemList, PriceList),
          try checkout:total(ItemList, PriceList, SpecialList) of
              N when is_integer(N) -> true
          catch
              error:{unknown_item, _} -> true;
              error:invalid_price_list -> true;
              error:invalid_special_list -> true;
              _:_ -> false
          end)).

prop_dupe_list_invalid() ->
    ?FORALL(PriceList, dupe_list(),
        false =:= checkout:valid_price_list(PriceList)).

prop_dupe_specials_invalid() ->
    ?FORALL(SpecialList, dupe_special_list(),
        false =:= checkout:valid_special_list(SpecialList)).

% Generator
item_price_list() ->
    ?LET(PriceList, price_list(),
        ?LET({ItemList, ExpectedPrice}, item_list(PriceList),
            {ItemList, ExpectedPrice, PriceList})).

price_list() ->
    ?LET(PriceList, non_empty(list({non_empty(string()), integer()})),
        lists:ukeysort(1, PriceList)). % remove duplicates.

item_list(PriceList) ->
    ?SIZED(Size, item_list(Size, PriceList, {[], 0})).

item_list(0, _, Acc) -> Acc;
item_list(N, PriceList, {ItemAcc, PriceAcc}) ->
    ?LET({Item, Price}, elements(PriceList),
        item_list(N-1, PriceList, {[Item|ItemAcc], Price+PriceAcc})).

bucket(N, Unit) ->
    (N div Unit) * Unit.

item_price_special() ->
    ?LET(PriceList, price_list(),
        ?LET(SpecialList, special_list(PriceList),
            ?LET({{RegularItems, RegularExpected},
                  {SpecialItems, SpecialExpected}},
                  {regular_gen(PriceList, SpecialList),
                   special_gen(PriceList, SpecialList)},
                   % merge initial list
                  {shuffle(RegularItems ++ SpecialItems),
                    RegularExpected + SpecialExpected,
                    PriceList, SpecialList}))).

special_list(PriceList) ->
    Items = [Name || {Name, _} <- PriceList],
    ?LET(Specials, list({elements(Items), choose(2, 5), integer()}),
            lists:ukeysort(1, Specials)).

regular_gen(PriceList, SpecialList) ->
    regular_gen(PriceList, SpecialList, [], 0).
regular_gen([], _, Items, Price) ->
    {Items, Price};
regular_gen([{Item, Cost}|PriceList], SpecialList, Items, Price) ->
    CountGen = case lists:keyfind(Item, 1, SpecialList) of
        {_, Limit, _} -> choose(0, Limit-1);
        _ -> non_neg_integer()
    end,
    ?LET(Count, CountGen,
         regular_gen(PriceList, SpecialList,
                    ?LET(V, vector(Count, Item), V ++ Items),
                    Cost*Count + Price)).

special_gen(_, SpecialList) ->
    special_gen(SpecialList, [], 0).
special_gen([], Items, Price) ->
    {Items, Price};
special_gen([{Item, Count, Cost} | SpecialList], Items, Price) ->
    ?LET(Multiplier, non_neg_integer(),
        special_gen(SpecialList,
                    ?LET(V, vector(Count * Multiplier, Item), V ++ Items),
                    Cost * Multiplier + Price)).

shuffle(L) ->
    Shuffled = lists:sort([{rand:uniform(), X} || X <- L]),
    [X || {_, X} <- Shuffled].

lax_lists() ->
    KnownItems = ["A", "B", "C"],
    MaybeKnownItemGen = elements(KnownItems ++ [string()]),
    {list(MaybeKnownItemGen), % ItemList
     list({MaybeKnownItemGen, integer()}), % PriceList
     list({MaybeKnownItemGen, integer(), integer()})}. % SpecialList

item_list_type(Items, Prices) ->
    case lists:all(fun(X) -> has_price(X, Prices) end, Items) of
        true -> valid;
        false -> prices_missing
    end.

has_price(Item, ItemList) ->
    proplists:get_value(Item, ItemList) =/= undefined.

dupe_list() ->
    ?LET(Items, non_empty(list(string())),
        vector(length(Items)+1, {elements(Items), integer()})).

dupe_special_list() ->
    ?LET(Items, non_empty(list(string())),
        vector(length(Items)+1, {elements(Items), integer(), integer()})).
