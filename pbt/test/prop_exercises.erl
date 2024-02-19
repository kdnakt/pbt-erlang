-module(prop_exercises).
-include_lib("proper/include/proper.hrl").

prop_set_union() ->
    ?FORALL({ListA, ListB}, {list(number()), list(number())},
        begin
            SetA = sets:from_list(ListA),
            SetB = sets:from_list(ListB),
            ModelUnion = lists:usort(ListA ++ ListB),
            lists:sort(sets:to_list(sets:union(SetA, SetB))) =:= ModelUnion
        end).


prop_dict_merge() ->
    ?FORALL({ListA, ListB}, {list({term(), term()}), list({term(), term()})},
        begin
            Merged = dict:merge(fun(_Key, V1, _V2) -> V1 end,
                                dict:from_list(ListA),
                                dict:from_list(ListB)),
            extract_keys(lists:sort(dict:to_list(Merged)))
            ==
            lists:usort(extract_keys(ListA ++ ListB))
        end).

% helper

extract_keys(List) -> [K || {K,_} <- List].

