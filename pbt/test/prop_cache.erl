-module(prop_cache).
-include_lib("proper/include/proper.hrl").
-behaviour(proper_statem).
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-define(CACHE_SIZE, 10).

-record(state, {max=?CACHE_SIZE, count=0, entries=[]}).

prop_test() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                cache:start_link(?CACHE_SIZE),
                {History, State, Result} = run_commands(?MODULE, Cmds),
                cache:stop(),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [History,State,Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).

prop_parallel() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
      begin
          cache:start_link(?CACHE_SIZE),
          {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
          cache:stop(),
          ?WHENFAIL(io:format("=======~n"
                              "Failing command sequence:~n~p~n"
                              "At state: ~p~n"
                              "=======~n"
                              "History: ~p~n",
                              [Cmds,State,Result,History]),
                    aggregate(command_names(Cmds), Result =:= ok))
      end).

initial_state() ->
    #state{}.

command(_State) ->
    frequency([
        {1, {call, cache, find, [key()]}},
        {3, {call, cache, cache, [key(), val()]}},
        {1, {call, cache, flush, []}}
    ]).

precondition(#state{count=0}, {call, cache, flush, []}) ->
    false;
precondition(#state{}, {call, _Mod, _Fun, _Args}) ->
    true.

% Generators
key() ->
    oneof([range(1,?CACHE_SIZE), integer()]).

val() -> integer().

next_state(State, _, {call, cache, flush, _}) ->
    State#state{count=0, entries=[]};
next_state(S=#state{entries=L, count=N, max=M}, _Res,
          {call, cache, cache, [K, V]}) ->
    case lists:keyfind(K, 1, L) of
        false when N =:= M -> S#state{entries = tl(L) ++ [{K,V}]};
        false when N < M -> S#state{entries = L ++ [{K,V}], count=N+1};
        {K,_} -> S#state{entries = lists:keyreplace(K, 1, L, {K,V})}
    end;
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    State.

postcondition(#state{entries=L}, {call, cache, find, [Key]}, Res) ->
    case lists:keyfind(Key, 1, L) of
        false -> Res =:= {error, not_found};
        {Key, Val} -> Res =:= {ok, Val}
    end;
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res)  
    -> true.