-module(bday_mail_tpl).
-export([body/1]).
-export([full/1]).

-spec body(bday_employee:employee()) -> string().
body(Employee) ->
    lists:flatten(io_lib:format("Happy birthday, dear ~s!",
                                [bday_employee:first_name(Employee)])).

-spec full(bday_employee:employee()) -> {[string()], string(), string()}.
full(Employee) ->
    {[bday_employee:email(Employee)],
     "Happy birthday!",
     body(Employee)}.

