-module(prop_thinking).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%
%%% Property
%%%%%%%%%%%%%%%%%
prop_biggest() ->
    ?FORALL(List, non_empty(list(integer())),
        begin
            thinking:biggest(List) =:= model_biggest(List)
        end).

prop_last() ->
    ?FORALL({List, KnownLast}, {list(number()), number()},
        begin
            KnownList = List ++ [KnownLast],
            KnownLast =:= lists:last(KnownList)
        end).

prop_sort() ->
    ?FORALL(List, list(term()),
        is_ordered(lists:sort(List))
    ).

prop_same_size() ->
    ?FORALL(L, list(number()),
        length(L) =:= length(lists:sort(L))).

prop_no_added() ->
    ?FORALL(L, list(number()),
        begin
            Sorted = lists:sort(L),
            lists:all(fun(Element) -> lists:member(Element, L) end, Sorted)
        end).

prop_no_removed() ->
    ?FORALL(L, list(number()),
        begin
            Sorted = lists:sort(L),
            lists:all(fun(Element) -> lists:member(Element, Sorted) end, L)
        end).

prop_symmetric() ->
    ?FORALL(Data, list({atom(), any()}),
        begin
            Encoded = encode(Data), is_binary(Encoded) andalso
            Data =:= decode(Encoded)
        end).

%%%%%%%%%%%%%%%%%%%%%
%%% Generators
%%%%%%%%%%%%%%%%%%%%%
model_biggest(List) ->
    lists:last(lists:sort(List)).

is_ordered([A,B|T]) ->
    A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % 2要素未満のリスト
    true.

encode(T) -> term_to_binary(T).
decode(T) -> binary_to_term(T).

