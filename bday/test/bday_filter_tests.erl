-module(bday_filter_tests).
-include_lib("eunit/include/eunit.hrl").


bday_filter_test() ->
    Years = generate_years_data(2018,2038),
    People = generate_people_for_year(3),
    lists:foreach(fun(YearData) -> 
        Birthdays = find_birthdays_for_year(People, YearData),
        every_birthday_once(People, Birthdays),
        on_right_date(People, Birthdays)
    end, Years).

find_birthdays_for_year(_, []) -> [];
find_birthdays_for_year(People, [Day|Year]) ->
    Found = bday_filter:birthday(People, Day),
    [{Day, Found} | find_birthdays_for_year(People, Year)].


%% Generators
generate_years_data(End, End) -> [];
generate_years_data(Start, End) ->
    [generate_year_data(Start) | generate_years_data(Start+1, End)].

generate_year_data(Year) ->
    DaysInFeb = case calendar:is_leap_year(Year) of
        true -> 29;
        false -> 28
    end,
    month(Year,1,31) ++ month(Year,2,DaysInFeb) ++ month(Year,3,31) ++
    month(Year,4,30) ++ month(Year,5,31) ++ month(Year,6,30) ++
    month(Year,7,31) ++ month(Year,8,31) ++ month(Year,9,30) ++
    month(Year,10,31) ++ month(Year,11,30) ++ month(Year,12,31).

month(Y,M,1) -> [{Y,M,1}];
month(Y,M,N) -> [{Y,M,N} | month(Y,M,N-1)].

generate_people_for_year(N) ->
    YearSeed = generate_year_data(2016),
    lists:append([people_for_year(YearSeed) || _ <- lists:seq(1,N)]).

people_for_year(Year) ->
    [person_for_date(Date) || Date <- Year].

person_for_date({_, M, D}) ->
    #{"name" => make_ref(),
      "date_of_birth" => {rand:uniform(100)+1900,M,D}}.


