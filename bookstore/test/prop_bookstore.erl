-module(prop_bookstore).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

title() ->
    ?LET(S, string(), elements([S, unicode:characters_to_binary(S)])).

author() ->
    ?LET(S, string(), elements([S, unicode:characters_to_binary(S)])).

isbn() ->
    ?LET(ISBN,
        [oneof(["978", "979"]),
         ?LET(X, range(0, 9999), integer_to_list(X)),
         ?LET(X, range(0, 9999), integer_to_list(X)),
         ?LET(X, range(0, 999), integer_to_list(X)),
         frequency([{10, range($0, $9)}, {1, "X"}])],
         iolist_to_binary(lists:join("-", ISBN))).
