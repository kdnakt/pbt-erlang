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

