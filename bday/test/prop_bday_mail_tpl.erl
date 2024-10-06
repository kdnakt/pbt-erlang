-module(prop_bday_mail_tpl).
-include_lib("proper/include/proper.hrl").

prop_template_email() ->
    ?FORALL(Employee, employee_map(),
            nomacth =/= string:find(bday_mail_tpl:body(Employee),
                                    maps:get("first_name", Employee))
    ).

employee_map() ->
    ?LET(PropList,
        [{"last_name", non_empty(prop_csv:field())},
         {"first_name", non_empty(prop_csv:field())},
         {"date_of_birth", {choose(1900,2020),choose(1,12),choose(1,31)}},
         {"email", non_empty(prop_csv:field())}],
        maps:from_list(PropList)).

