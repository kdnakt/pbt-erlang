-module(prop_csv).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

prop_roundtrip() ->
    ?FORALL(Maps, csv_source(),
        Maps =:= bday_csv:decode(bday_csv:encode(Maps))).

csv_source() ->
    ?LET(Size, pos_integer(),
        ?LET(Keys, header(Size+1),
             list(entry(Size+1, Keys)))).

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

%%% EUnit

%% @doc for the ambiguity of RFC 4180
one_column_bug_test() ->
    ?assertEqual("\r\n\r\n", bday_csv:encode([#{""=>""},#{""=>""}])),
    ?assertEqual([#{""=>""}], bdat_csv:decode("\r\n\r\n")).

rfc_record_per_line_test() ->
    ?assertEqual([#{"aaa" => "zzz", "bbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n")).

rfc_optional_trailing_crlf_test() ->
    ?assertEqual([#{"aaa" => "zzz", "bbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx")).

rfc_double_quote_test() ->
    ?assertEqual([#{"aaa" => "zzz", "bbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("\"aaa\",\"bbb\",\"ccc\"\r\nzzz,yyy,xxx")).

rfc_crlf_escape_test() ->
    ?assertEqual([#{"aaa" => "zzz", "b\r\nbb" => "yyy", "ccc" => "xxx"}],
                 bday_csv:decode("\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx")).

rfc_double_quote_escape_test() ->
    ?assertEqual([#{"aaa" => "", "b\"bb" => "", "ccc" => ""}],
                 bday_csv:decode("\"aaa\",\"b\"\"bb\",\"ccc\"\r\n,,")).

dupe_keys_unsupported_test() ->
    CSV = "field_name,field_name,field_name\r\n"
          "aaa,bbb,ccc\r\n"
          "zzz,yyy,xxx\r\n",
    [Map1,Map2] = bday_csv:decode(CSV),
    ?assertEqual(1, length(maps:keys(Map1))),
    ?assertEqual(1, length(maps:keys(Map2))),
    ?assertMatch(#{"field_name" := _}, Map1),
    ?assertMatch(#{"field_name" := _}, Map2).
