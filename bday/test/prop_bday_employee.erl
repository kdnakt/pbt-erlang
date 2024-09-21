-module(prop_bday_employee).
-include_lib("proper/include/proper.hrl").

% Property

prop_fix_csv_leading_space() ->
    ?FORALL(Map, raw_employee_map(),
        begin
            Emp = bday_employee:adapt_csv_result(Map),
            Strs = [X || X <- maps:keys(Emp) ++ maps:values(Emp), is_list(X)],
            lists:all(fun(String) -> hd(String) =/= $\s end, Strs)
        end).

% Generator

raw_employee_map() ->
    ?LET(PropList,
        [{"last_name", prop_csv:field()},
         {" first_name", whitespaced_text()},
         {" date_of_birth", text_date()},
         {" email", whitespaced_text()}],
        maps:from_list(PropList)).

whitespaced_text() ->
    ?LET(Txt, prop_csv:field(), " " ++ Txt).

text_date() ->
    ?LET({Y,M,D}, {choose(1900,2020), choose(1,12), choose(1,31)},
        lists:flatten(io_lib:format(" ~w/~2..0w/~2..0w", [Y,M,D]))).


