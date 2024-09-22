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

prop_fix_csv_date_of_birth() ->
    ?FORALL(Map, raw_employee_map(),
        case bday_employee:adapt_csv_result(Map) of
            #{"date_of_birth" := {Y,M,D}} ->
                is_integer(Y) and is_integer(M) and is_integer(D);
            _ ->
                false
        end).

prop_handle_access() ->
    ?FORALL(Maps, non_empty(list(raw_employee_map())),
        begin
            CSV = bday_csv:encode(Maps),
            Handle = bday_employee:from_csv(CSV),
            Partial = bday_employee:filter_birthday(Handle, date()),
            ListFull = bday_employee:fetch(Handle),
            true = is_list(bday_employee:fetch(Partial)),
            % assert not crash
            _ = [{bday_employee:first_name(X),
                  bday_employee:last_name(X),
                  bday_employee:email(X),
                  bday_employee:date_of_birth(X)} || X <- ListFull],
            true
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


