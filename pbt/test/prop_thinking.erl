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

