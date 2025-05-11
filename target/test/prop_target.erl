-module(prop_target).
-include_lib("proper/include/proper.hrl").
-compile(export_all).


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_path(opts) -> [{search_steps, 100}]. % こうしないと1000回実行される
prop_path() ->
    ?FORALL_TARGETED(P, path(),
        begin
            {X,Y} = lists:foldl(fun move/2, {0,0}, P),
            io:format("~p",[{X,Y}]),
            ?MAXIMIZE(X-Y), % X-Yを最大化することで左下に向かうパスを試す
            true
        end).

prop_tree_regular(opts) -> [{numtests, 1000}].
prop_tree_regular() ->
    ?FORALL(T, tree(),
        begin
            Weight = sides(T),
            io:format(" ~p", [Weight]),
            true
        end).

prop_tree() ->
    ?FORALL_TARGETED(T, tree(),
        begin
            {Left, Right} = Weight = sides(T),
            io:format(" ~p", [Weight]),
            ?MAXIMIZE(Left-Right),
            true
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
path() -> list(oneof([left, right, up, down])).

tree() -> 
    ?LET(L, non_empty(list(integer())), to_tree(L)).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
move(left, {X,Y}) -> {X-1,Y};
move(right, {X,Y}) -> {X+1,Y};
move(up, {X,Y}) -> {X,Y+1};
move(down, {X,Y}) -> {X,Y-1}.

to_tree(L) ->
    lists:foldl(fun insert/2, undefined, L).

insert(N, {node, N, L, R}) -> {node, N, L, R};
insert(N, {node, M, L, R}) when N < M ->
    {node, M, insert(N, L), R};
insert(N, {node, M, L, R}) when N > M ->
    {node, M, L, insert(N, R)};
insert(N, {leaf, N}) -> {leaf, N};
insert(N, {leaf, M}) when N < M ->
    {node, N, undefined, {leaf, M}};
insert(N, {leaf, M}) when N > M ->
    {node, N, {leaf, M}, undefined};
insert(N, undefined) -> 
    {leaf, N}.

sides({node, _, Left, Right}) ->
    {LL, LR} = sides(Left),
    {RL, RR} = sides(Right),
    {count_inner(Left)+LL+LR, count_inner(Right)+RL+RR};
sides(_) -> {0, 0}.

count_inner({node, _, _, _}) -> 1;
count_inner(_) -> 0.
