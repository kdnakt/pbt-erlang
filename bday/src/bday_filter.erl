-module(bday_filter).
-export([birthday/2]).

birthday(People, {Year, 2, 28}) ->
    case calendar:is_leap_year(Year) of
        true -> filter_dob(People, 2, 28);
        false -> filter_dob(People, 2, 28) ++ filter_dob(People, 2, 29)
    end;
birthday(People, {_Year, Month, Day}) ->
    filter_dob(People, Month, Day).

filter_dob(People, Month, Day) ->
    lists:filter(
        fun(#{"date_of_birth" := {_,M,D}}) -> {Month,Day} == {M,D} end,
        People
    ).
