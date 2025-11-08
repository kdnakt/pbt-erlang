-module(bookstore_init).
-export([main/1]).

main(_) ->
    timer:sleep(5000),
    io:format("OK.~n"),
    init:stop().
