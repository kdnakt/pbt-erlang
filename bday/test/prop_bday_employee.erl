-module(prop_bday_employee).
-include_lib("proper/include/proper.hrl").

% Property

prop_fix_csv_leading_space() ->
    ?FORALL(MAp, raw_employee_map(),
        begin
            Emp = bday_employee:adapt_csv_result(Map),
            Strs = [X || X <- maps:keys(Emp) ++ maps:values(Emp), is_list(X)],
            lists:all(fun(STring) -> hd(String) =/= $\s end, Strs)
        end).


