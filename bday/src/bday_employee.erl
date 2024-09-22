-module(bday_employee).

-export([from_csv/1]).

-ifdef(TEST).
-export([adapt_csv_result/1]).
-endif.

-opaque employee() :: #{string() := term()}.
-opaque handle() :: {raw, [employee()]}.
-export_type([handle/0, employee/0]).

-spec from_csv(string()) -> handle().

from_csv(String) ->
    {raw, [adapt_csv_result(Map) || Map <- bday_csv:decode(String)]}.

-spec adapt_csv_result(map()) -> employee().
adapt_csv_result(Map) ->
    NewMap = maps:fold(fun(K,V,NewMap) -> NewMap#{trim(K) => maybe_null(trim(V))} end,
              #{}, Map),
    DoB = maps:get("date_of_birth", NewMap),
    NewMap#{"date_of_birth" => parse_date(DoB)}.

trim(Str) -> string:trim(Str, leading, " ").

maybe_null("") -> undefined;
maybe_null(Str) -> Str.

parse_date(Str) ->
    [Y,M,D] = [list_to_integer(X) || X <- string:lexemes(Str, "/")],
    {Y,M,D}.

