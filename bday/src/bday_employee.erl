-module(bday_employee).

-export([from_csv/1]).

-ifdef(TEST).
-export([adapt_csv_result/1]).
-endif.

-opaque employee() :: #{string() := term()}.
-opaque handle() :: {raw, [employee()]}.
-export_type([handle/0, employee/0]).
-export([from_csv/1, last_name/1, first_name/1, date_of_birth/1, email/1]).
-export([fetch/1]).

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

-spec filter_birthday(handle(), calendar:date()) -> handle().
filter_birthday({raw, Employees}, Date) ->
    {raw, bday_filter:birthday(Employees, Date)}.

-spec fetch(handle()) -> [employee()].
fetch({raw, Maps}) -> Maps.

-spec last_name(employee()) -> string() | undefined.
last_name(#{"last_name" := Name}) -> Name.

-spec first_name(employee()) -> string() | undefined.
first_name(#{"first_name" := Name}) -> Name.

-spec date_of_birth(employee()) -> calebdar:date().
date_of_birth(#{"date_of_birth" := DoB}) -> DoB.

-spec email(employee()) -> string().
email(#{"email" := Email}) -> Email.

