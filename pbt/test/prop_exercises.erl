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

prop_count_words() ->
    ?FORALL(String, non_empty(string()),
        word_count(String) =:= alt_word_count(String)
    ).

% helper

extract_keys(List) -> [K || {K,_} <- List].

alt_word_count(String) -> space(String).

space([]) -> 0;
space([$\s|Str]) -> space(Str);
space(Str) -> word(Str).

word([]) -> 1;
word([$\s|Str]) -> 1+space(Str);
word([_|Str]) -> word(Str).


% Implementation
word_count(String) ->
    Stripped = string:trim(dedupe_spaces(String), both, " "),
    Spaces = lists:sum([1 || Char <- Stripped, Char =:= $\s]),
    case Stripped of
        "" -> 0;
        _ -> Spaces + 1
    end.

dedupe_spaces([]) -> [];
dedupe_spaces([$\s,$\s|Rest]) -> dedupe_spaces([$\s|Rest]);
dedupe_spaces([H|T]) -> [H|dedupe_spaces(T)].

