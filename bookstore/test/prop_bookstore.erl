-module(prop_bookstore).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

prop_test() ->
    ?SETUP(fun() ->
        {ok, Apps} = application:ensure_all_started(bookstore),
        fun() -> [application:stop(App) || App <- Apps], ok end
    end,
    ?FORALL(Cmds, commands(?MODULE),
        begin
            bookstore_db:setup(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            bookstore_db:teardown(),
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                [History, State, Result]),
                                aggregate(command_names(Cmds), Result =:= ok))
        end)
    ).

initial_state() -> #{}.

command(_State) ->
    oneof([
        {call, bookstore_db, add_book, [isbn(), title(), author(), 1, 1]},
        {call, bookstore_db, add_book, [isbn()]},
        {call, bookstore_db, borrow_copy, [isbn()]},
        {call, bookstore_db, return_copy, [isbn()]},
        {call, bookstore_db, find_book_by_author, [author()]},
        {call, bookstore_db, find_book_by_title, [title()]},
        {call, bookstore_db, find_book_by_isbn, [isbn()]}
    ]).

precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.

postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    true.

next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    NewState = State,
    NewState.

title() ->
    ?LET(S, string(), elements([S, unicode:characters_to_binary(S)])).

author() ->
    ?LET(S, string(), elements([S, unicode:characters_to_binary(S)])).

isbn() ->
    ?LET(ISBN,
        [oneof(["978", "979"]),
         ?LET(X, range(0, 9999), integer_to_list(X)),
         ?LET(X, range(0, 9999), integer_to_list(X)),
         ?LET(X, range(0, 999), integer_to_list(X)),
         frequency([{10, range($0, $9)}, {1, "X"}])],
         iolist_to_binary(lists:join("-", ISBN))).
