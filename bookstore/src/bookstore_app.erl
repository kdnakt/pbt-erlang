%%%-------------------------------------------------------------------
%% @doc bookstore public API
%% @end
%%%-------------------------------------------------------------------

-module(bookstore_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    bookstore_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
