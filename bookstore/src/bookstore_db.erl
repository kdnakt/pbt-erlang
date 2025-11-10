-module(bookstore_db).
-export([load_queries/0, setup/0, teardown/0]).

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
