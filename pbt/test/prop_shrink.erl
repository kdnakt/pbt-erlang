-module(prop_shrink).
-include_lib("proper/include/proper.hrl").
-compile([export_all, {no_auto_import,[date/0]}, {no_auto_import,[time/0]}]).

strdatetime() ->
    ?LET(DateTime, datetime(), to_str(DateTime)).

datetime() ->
    {date(), time(), timezone()}.

date() ->
    ?SUCHTHAT({Y,M,D}, {year(), month(), day()},
              calendar:valid_date(Y,M,D)).

year() ->
    ?SHRINK(range(0, 9999), [range(1970, 2000), range(1900, 2100)]).

month() ->
    range(1, 12).

day() ->
    range(1, 31).

time() ->
    {range(0, 24), range(0, 59), range(0, 60)}.

timezone() ->
    {elements(['+', '-']),
        ?SHRINK(range(0, 99), [range(0, 14), 0]),
        ?SHRINK(range(0, 99), [0, 15, 30, 45])}.

to_str({{Y,M,D}, {H,Mi,S}, {Sign,Ho,Mo}}) ->
    FormatStr = "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b~s~2..0b:~2..0b",
    lists:flatten(io_lib:format(FormatStr, [Y,M,D,H,Mi,S,Sign,Ho,Mo])).

tree(N) when N =< 1 ->
    {leaf, number()};

tree(N) ->
    PerBranch = N div 2,
    { branch, [tree(PerBranch), tree(PerBranch)]}.

tree_shrink(N) when N =< 1 ->
    {leaf, number()};
tree_shrink(N) ->
    PerBranch = N div 2,
    ?LETSHRINK([L, R], [tree_shrink(PerBranch), tree_shrink(PerBranch)],
               {branch, L, R}).
