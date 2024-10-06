-module(bday).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Path]) ->
    {ok, Data} = file:read_file(Path),
    Handle = bday_employee:from_csv(binary_to_list(Data)),
    Query = bday_employee:filter_birthday(Handle, date()),
    BdaySet = bday_employee:fetch(Query),
    Mails = [bday_mail_tpl:full(Employee) || Employee <- BdaySet],
    [send_email(To, Topic, Body) || {To, Topic, Body} <- Mails].

%%====================================================================
%% Internal functions
%%====================================================================
send_email(To, Topic, Body) ->
    io:format("sent birthday email to ~p~n", [To]),
    io:format("topic: ~p~n", [Topic]),
    io:format("body: ~p~n", [Body]).
