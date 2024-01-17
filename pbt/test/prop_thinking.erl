-module(prop_thinking).
-include_lib("proper/include/proper.hrl").

% Property
prop_biggest() ->
    ?FORALL(List, non_empty(list(integer())),
        begin
            thinking:biggest(List) =:= model_biggest(List)
        end).

model_biggest(List) ->
    lists:last(lists:sort(List)).

prop_last() ->
    ?FORALL({List, KnownLast}, {list(number()), number()},
        begin
            KnownList = List ++ [KnownLast],
            KnownLast =:= lists:last(KnownList)
        end).
