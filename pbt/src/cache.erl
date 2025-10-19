-module(cache).
-export([start_link/1, stop/0, cache/2, find/1, flush/0]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(N) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, N, []).

stop() ->
    gen_server:stop(?MODULE).

init(N) ->
    ets:new(cache, [public, named_table]),
    ets:insert(cache, {count, 0, N}),
    {ok, nostate}.

% handle_call(_Call, _From, State) -> {noreply, State}.
handle_call({cache, Key, Value}, _From, State) ->
    case ets:match(cache, {'$1', {Key, '_'}}) of
        [[N]] ->
            ets:insert(cache, {N, {Key, Value}});
        [] ->
            erlang:yield(),
            case ets:lookup(cache, count) of
                [{count,Max,Max}] ->
                    ets:insert(cache, [{1,{Key, Value}}, {count, 1, Max}]);
                [{count,Current,Max}] ->
                    ets:insert(cache, [{Current + 1, {Key, Value}}, {count, Current + 1, Max}])
            end
    end,
    {reply, ok, State};
handle_call(flush, _From, State) ->
    [{count, _, Max}] = ets:lookup(cache, count),
    ets:delete_all_objects(cache),
    erlang:yield(),
    ets:insert(cache, {count, 0, Max}),
    {reply, ok, State}.

handle_cast(_Cast, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

find(Key) ->
    case ets:match(cache, {'_', {Key, '$1'}}) of
        [[Val]] -> {ok, Val};
        [] -> {error, not_found}
    end.

cache(Key, Value) ->
    gen_server:call(?MODULE, {cache, Key, Value}).
    % case ets:match(cache, {'$1', {Key, '_'}}) of
    %     [[N]] ->
    %         ets:insert(cache, {N, {Key, Value}});
    %     [] ->
    %         % erlang:yield(),
    %         case ets:lookup(cache, count) of
    %             [{count,Max,Max}] ->
    %                 ets:insert(cache, [{1,{Key, Value}}, {count, 1, Max}]);
    %             [{count,Current,Max}] ->
    %                 ets:insert(cache, [{Current + 1, {Key, Value}}, {count, Current + 1, Max}])
    %         end
    % end.

flush() ->
    gen_server:call(?MODULE, flush).
    % [{count, _, Max}] = ets:lookup(cache, count),
    % ets:delete_all_objects(cache),
    % % erlang:yield(),
    % ets:insert(cache, {count, 0, Max}).
