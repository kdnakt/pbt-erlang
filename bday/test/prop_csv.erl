-module(prop_csv).
-include_lib("proper/include/proper.hrl").

prop_roundtrip() ->
    ?FORALL(Maps, csv_source(),
        Maps =:= bday_csv:decode(bday_csv:encode(Maps))).

csv_source() ->
    ?LET(Size, pos_integer(),
        ?LET(Keys, header(Size),
             list(entry(Size, Keys)))).

entry(Size, Keys) ->
    ?LET(Vals, record(Size),
        maps:from_list(lists:zip(Keys, Vals))).

header(Size) -> vector(Size, name()).

record(Size) -> vector(Size, field()).

name() -> field().

field() -> oneof([unquoted_text(), quotable_text()]).

unquoted_text() -> list(elements(textdata())).

quotable_text() -> list(elements([$\r, $\n, $", $,] ++ textdata())).

textdata() ->
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    ":;<=>?@ !#$%&'()*+-./[\\]fi_`{|}~".

