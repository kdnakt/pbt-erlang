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

shuffle(L) ->
    Shuffled = lists:sort([{rand:uniform(), X} || X <- L]),
    [X || {_, X} <- Shuffled].
