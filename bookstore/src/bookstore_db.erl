-module(bookstore_db).
-export([load_queries/0, setup/0, teardown/0,
    add_book/3, add_book/5, add_copy/1, borrow_copy/1, return_copy/1,
    find_book_by_author/1, find_book_by_isbn/1, find_book_by_title/1
]).

load_queries() ->
    ets:new(bookstore_sql, [named_table, public, {read_concurrency, true}]),
    SQLFile = filename:join(code:priv_dir(bookstore), "queries.sql"),
    {ok, Queries} = eql:compile(SQLFile),
    ets:insert(bookstore_sql, Queries),
    ok.

setup() ->
    run_query(setup_table_books, []).

teardown() ->
    run_query(teardown_table_books, []).

add_book(ISBN, Title, Author) ->
    add_book(ISBN, Title, Author, 0, 0).

add_book(ISBN, Title, Author, Owned, Avail) ->
    BinTitle = iolist_to_binary(Title),
    BinAuthor = iolist_to_binary(Author),
    case run_query(add_book,
                        [ISBN, BinTitle, BinAuthor, Owned, Avail]) of
        {{insert, 0, 1}, []} -> ok;
        {error, Reason} -> {error, Reason};
        Other -> {error, Other  }
    end.

add_copy(ISBN) ->
    handle_single_update(run_query(add_copy, [ISBN])).

borrow_copy(ISBN) ->
    handle_single_update(run_query(borrow_copy, [ISBN])).

return_copy(ISBN) ->
    handle_single_update(run_query(return_copy, [ISBN])).

find_book_by_author(Author) ->
    handle_select(
        run_query(find_by_author, [iolist_to_binary(["%",Author,"%"])])
    ).

find_book_by_isbn(ISBN) ->
    handle_select(
        run_query(find_by_isbn, [ISBN])
    ).

find_book_by_title(Title) ->
    handle_select(
        run_query(find_by_title, [iolist_to_binary(["%",Title,"%"])])
    ).

run_query(Name, Args) ->
    with_connection(fun(Conn) -> run_query(Name, Args, Conn) end).

run_query(Name, Args, Conn) ->
    pgsql_connection:extended_query(query(Name), Args, Conn).

with_connection(Fun) ->
    {ok, Conn} = connect(),
    Res = Fun(Conn),
    close(Conn),
    Res.

connect() -> connect(application:get_env(bookstore, pg, [])).

connect(Args) ->
    try pgsql_connection:connect(Args) of
        {pgsql_connection, _} = Conn -> {ok, Conn}
    catch
        throw:Error -> {error, Error}
    end.

close(Conn) ->
    pgsql_connection:close(Conn).

query(Name) ->
    case ets:lookup(bookstore_sql, Name) of
        [] -> {query_not_found, Name};
        [{_, Query}] -> Query
    end.

handle_select({{select, _}, List}) -> {ok, List};
handle_select(Error) -> Error.

handle_single_update({{update, 1}, _}) -> ok;
handle_single_update({error, Reason}) -> {error, Reason};
handle_single_update(Other) -> {error, Other}.
