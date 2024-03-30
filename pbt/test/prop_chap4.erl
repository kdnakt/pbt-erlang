-module(prop_chap4).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_dupes() ->
    ?FORALL(KV, list({key(), val()}),
        begin
            M = maps:from_list(KV),
            [maps:get(K, M) || {K, _V} <- KV], % Kがマップにない場合はクラッシュ
            collect(
                {dupes, to_range(5, length(KV) - length(lists:ukeysort(1,KV)))},
                true
            )
        end).

prop_collect1() ->
    ?FORALL(Bin, binary(), collect(byte_size(Bin), is_binary(Bin))).

prop_collect2() ->
    ?FORALL(Bin, binary(),
        collect(to_range(10, byte_size(Bin)), is_binary(Bin))).

prop_aggregate() ->
    Suits = [club, diamond, heart, spade],
    ?FORALL(Hand, vector(5, {oneof(Suits), choose(1, 13)}),
            aggregate(Hand, true)).

prop_escape() ->
    ?FORALL(Str, string(),
            aggregate(classes(Str), escape(Str))).

prop_resize() ->
    ?FORALL(Bin, resize(150, binary()),
        collect(to_range(10, byte_size(Bin)), is_binary(Bin))).

prop_profile1() ->
    ?FORALL(Profile, [{name, resize(10, string())},
                      {age, pos_integer()},
                      {bio, resize(350, string())}],
        begin
            NameLen = to_range(10,length(proplists:get_value(name, Profile))),
            BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
            aggregate([{name, NameLen}, {bio, BioLen}], true)
        end).

prop_profile2() ->
    ?FORALL(Profile, [{name, string()},
                      {age, pos_integer()},
                      {bio, ?SIZED(Size, resize(Size*35, string()))}],
        begin
            NameLen = to_range(10,length(proplists:get_value(name, Profile))),
            BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
            aggregate([{name, NameLen}, {bio, BioLen}], true)
        end).

prop_profile2_min() ->
    ?FORALL(Profile, [{name, string()},
                      {age, pos_integer()},
                      {bio, ?SIZED(Size, resize(min(100,Size)*35, string()))}],
        begin
            NameLen = to_range(10,length(proplists:get_value(name, Profile))),
            BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
            aggregate([{name, NameLen}, {bio, BioLen}], true)
        end).

prop_queue_naive() ->
    ?FORALL(List, list({term(), term()}),
        begin
            Queue = queue:from_list(List),
            queue:is_queue(Queue)
        end).

prop_queue_nicer() ->
    ?FORALL(Q, queue(),
        queue:is_queue(Q)).

prop_non_empty_list() ->
    ?FORALL(List, my_non_empty(list(term())),
        length(List) =/= 0).

prop_non_empty_map() ->
    ?FORALL(Map, my_non_empty_map(map(term(), term())),
        maps:size(Map) =/= 0).

prop_even1() ->
    ?FORALL({N, M}, {my_even1(), my_even1()},
        (N - M) rem 2 =:= 0).

prop_uneven1() ->
    ?FORALL({N, M}, {my_uneven1(), my_uneven1()},
        (N - M) rem 2 =:= 0).

prop_even2() ->
    ?FORALL({N, M}, {my_even2(), my_even2()},
        (N - M) rem 2 =:= 0).

prop_uneven2() ->
    ?FORALL({N, M}, {my_uneven2(), my_uneven2()},
        (N - M) rem 2 =:= 0).

prop_text_like() ->
    ?FORALL(Text, text_like(),
        aggregate(classes(Text), escape(Text))).

prop_mostly_sorted() ->
    ?FORALL(List, non_empty(mostly_sorted()),
        length(List) =/= 0).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
to_range(M, N) ->
    Base = N div M,
    {Base*M, (Base+1)*M}.

escape(_) -> true.

classes(Str) ->
    L = letters(Str),
    N = numbers(Str),
    P = punctuation(Str),
    O = length(Str) - (L+N+P),
    [{letters, to_range(5,L)}, {numbers, to_range(5,N)},
     {punctuation, to_range(5,P)}, {others, to_range(5,O)}].

letters(Str) ->
    length([1 || Char <- Str,
                 (Char >= $A andalso Char =< $Z) orelse
                 (Char >= $a andalso Char =< $z)]).

numbers(Str) ->
    length([1 || Char <- Str, Char >= $0, Char =< $9]).

punctuation(Str) ->
    length([1 || Char <- Str, lists:member(Char, ".,;:'\"-")]).

my_non_empty(ListOrBinGenerator) ->
    ?SUCHTHAT(L, ListOrBinGenerator, L =/= [] andalso L =/= <<>>).

my_non_empty_map(Gen) ->
    ?SUCHTHAT(G, Gen, G =/= #{}).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
key() -> oneof([range(1,10), integer()]).
val() -> term().

queue() ->
    ?LET(List, list({term(), term()}),
        queue:from_list(List)).

my_even1() -> ?SUCHTHAT(N, integer(), N rem 2 =:= 0).
my_uneven1() -> ?SUCHTHAT(N, integer(), N rem 2 =/= 0).

my_even2() -> ?LET(N, integer(), N * 2).
my_uneven2() -> ?LET(N, integer(), N * 2 + 1).

text_like() ->
    list(frequency([{80, range($a, $z)},
                    {10, $\s},
                    {1, $\n},
                    {1, oneof([$., $-, $!, $?, $,])},
                    {1, range($0, $9)}
                   ])).

mostly_sorted() ->
    ?LET(Lists,
         list(frequency([
            {5, sorted_list()},
            {1, list()}
         ])),
         lists:append(Lists)).

sorted_list() ->
    ?LET(L, list(), lists:sort(L)).
