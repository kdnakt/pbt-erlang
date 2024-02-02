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
