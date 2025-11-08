-module(bookstore_init).
-export([main/1]).

main(_) ->
    ok = filelib:ensure_dir("postgres/data/.init-here"),
    io:format("initializing database...~n"),
    cmd("initdb -D postgres/data"),
    io:format("starting database...~n"),

    StartCmd = "pg_ctl -D postgres/data -l logfile start",
    case os:type() of
        {win32, _} -> spawn(fun() -> cmd(StartCmd) end);
        {unix, _} -> cmd(StartCmd)
    end,
    timer:sleep(5000),

    io:format("setting up bookstore_db database...~n"),
    cmd("psql -h localhost -d template1 -c"
        "\"CREATE DATABASE bookstore_db;\""),
    io:format("OK.~n"),
    init:stop().

cmd(Str) -> io:format("~s~n", [os:cmd(Str)]).
