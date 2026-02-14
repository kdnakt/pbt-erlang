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

command(State) ->
    AlwaysPossible = [
        {call, book_shim, add_book_new, [isbn(), title(), author(), 1, 1]},
        {call, book_shim, add_copy_new, [isbn()]},
        {call, book_shim, borrow_copy_unknown, [isbn()]},
        {call, book_shim, return_copy_unknown, [isbn()]},
        {call, book_shim, find_book_by_isbn_unknown, [isbn()]},
        {call, book_shim, find_book_by_author_unknown, [author()]},
        {call, book_shim, find_book_by_title_unknown, [title()]}
    ],
    ReliesOnState = case maps:size(State) of
        0 ->
            [];
        _ ->
            S = State,
            [{call, book_shim, add_book_existing,
                [isbn(S), title(), author(), 1, 1]},
             {call, book_shim, add_copy_existing, [isbn(S)]},
             {call, book_shim, borrow_copy_avail, [isbn(S)]},
             {call, book_shim, borrow_copy_unavail, [isbn(S)]},
             {call, book_shim, return_copy_existing, [isbn(S)]},
             {call, book_shim, return_copy_full, [isbn(S)]},
             {call, book_shim, find_book_by_isbn_exists, [isbn(S)]},
             {call, book_shim, find_book_by_author_matching, [author(S)]},
             {call, book_shim, find_book_by_title_matching, [title(S)]}   
            ]
    end,
    oneof(AlwaysPossible ++ ReliesOnState).

precondition(S, {call, _, add_book_new, [ISBN|_]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, add_copy_new, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, borrow_copy_unknown, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, return_copy_unknown, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, find_book_by_isbn_unknown, [ISBN]}) ->
    not has_isbn(S, ISBN);
precondition(S, {call, _, find_book_by_author_unknown, [Auth]}) ->
    not like_author(S, Auth);
precondition(S, {call, _, find_book_by_title_unknown, [Title]}) ->
    not like_title(S, Title);
precondition(S, {call, _, find_book_by_author_matching, [Auth]}) ->
    like_author(S, Auth);
precondition(S, {call, _, find_book_by_title_matching, [Title]}) ->
    like_title(S, Title);
precondition(S, {call, _Mod, _Fun, [ISBN|_]}) ->
    has_isbn(S, ISBN).

postcondition(_, {_, _, add_book_new, _}, ok) ->
    true;
postcondition(_, {_, _, add_book_existing, _}, {error, _}) ->
    true;
postcondition(_, {_, _, add_copy_existing, _}, ok) ->
    true;
postcondition(_, {_, _, add_copy_new, _}, {error, not_found}) ->
    true;
postcondition(_, {_, _, borrow_copy_avail, _}, ok) ->
    true;
postcondition(_, {_, _, borrow_copy_unavail, _}, {error, unavailable}) ->
    true;
postcondition(_, {_, _, borrow_copy_unknown, _}, {error, not_found}) ->
    true;
postcondition(_, {_, _, return_copy_full, _}, {error, _}) ->
    true;
postcondition(_, {_, _, return_copy_existing, _}, ok) ->
    true;
postcondition(_, {_, _, return_copy_unknown, _}, {error, not_found}) ->
    true;
postcondition(S, {_, _, find_book_by_isbn_exists, [ISBN]}, Res) ->
    Res =:= {ok, [maps:get(ISBN, S, undefined)]};
postcondition(_, {_, _, find_book_by_isbn_unknown, _}, {ok, []}) ->
    true;
postcondition(S, {_, _, find_book_by_author_matching, [Auth]}, {ok,Res}) ->
    Map = maps:filter(fun(_, {_,_,A,_,_}) -> nomatch =/= string:find(A, Auth) end, S),
    lists:sort(Res) =:= lists:sort(maps:values(Map));
postcondition(_, {_, _, find_book_by_author_unknown, _}, {ok, []}) ->
    true;
postcondition(S, {_, _, find_book_by_title_matching, [Title]}, {ok,Res}) ->
    Map = maps:filter(fun(_, {_,T,_,_,_}) -> nomatch =/= string:find(T, Title) end, S),
    lists:sort(Res) =:= lists:sort(maps:values(Map));
postcondition(_, {_, _, find_book_by_title_unknown, _}, {ok, []}) ->
    true;
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    true.

next_state(State, _, {call, _, add_book_new, [ISBN, Title, Author, Owned, Avail]}) ->
    State#{ISBN => {ISBN, Title, Author, Owned, Avail}};
next_state(State, _, {call, _, add_copy_existing, [ISBN]}) ->
    #{ISBN := {ISBN, Title, Author, Owned, Avail} } = State,
    State#{ISBN => {ISBN, Title, Author, Owned + 1, Avail + 1}};
next_state(State, _, {call, _, borrow_copy_avail, [ISBN]}) ->
    #{ISBN := {ISBN, Title, Author, Owned, Avail} } = State,
    State#{ISBN => {ISBN, Title, Author, Owned, Avail - 1}};
next_state(State, _, {call, _, return_copy_existing, [ISBN]}) ->
    #{ISBN := {ISBN, Title, Author, Owned, Avail} } = State,
    State#{ISBN => {ISBN, Title, Author, Owned, Avail + 1}};
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    NewState = State,
    NewState.

title() ->
    ?LET(S, string(), elements([S, unicode:characters_to_binary(S)])).
title(State) ->
    elements([partial(Title) || {_, Title, _, _, _} <- maps:values(State)]).

author() ->
    ?LET(S, string(), elements([S, unicode:characters_to_binary(S)])).
author(State) ->
    elements([partial(Author) || {_, _, Author, _, _} <- maps:values(State)]).

isbn() ->
    ?LET(ISBN,
        [oneof(["978", "979"]),
         ?LET(X, range(0, 9999), integer_to_list(X)),
         ?LET(X, range(0, 9999), integer_to_list(X)),
         ?LET(X, range(0, 999), integer_to_list(X)),
         frequency([{10, range($0, $9)}, {1, "X"}])],
         iolist_to_binary(lists:join("-", ISBN))).
isbn(State) ->
    elements(maps:keys(State)).


partial(String) ->
    L = string:length(String),
    ?LET({Start, Len}, {range(0, L), non_neg_integer()},
        string:substr(String, Start, Len)).

% Helpers
has_isbn(Map, ISBN) ->
    maps:is_key(ISBN, Map).

like_author(Map, Auth) ->
    lists:any(fun({_, _, A, _, _}) -> nomatch =/= string:find(A, Auth) end,
                maps:values(Map)).

like_title(Map, Title) ->
    lists:any(fun({_, T, _, _, _}) -> nomatch =/= string:find(T, Title) end,
                maps:values(Map)).
