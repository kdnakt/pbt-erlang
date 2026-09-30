-module(prop_break).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0, initial_state_data/0,
         unregistered/1, ok/1, tripped/1, blocked/1, % state generators
         precondition/4, postcondition/5, next_state_data/5]).

prop_test() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
        begin
            actual_system:start_link(),
            {History,State,Result} = proper_fsm:run_commands(?MODULE, Cmds), 
            actual_system:stop(),
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                [History,State,Result]),
                      aggregate(zip(proper_fsm:state_names(History),
                                    command_names(Cmds)), 
                                Result =:= ok))
        end).

-record(data, {
        limit = 3 :: pos_integer(),
        errors = 0 :: non_neg_integer(),
        timeouts = 0 :: non_neg_integer()
    }).

%% Initial state for the state machine
initial_state() -> unregistered.
%% Initial model data at the start. Should be deterministic.
initial_state_data() -> #data{}.

%% State commands generation
unregistered(_Data) -> [{ok, {call, break_shim, success, []}}].

% TODO
ok(_Data) -> [
    {history, {call, break_shim, success, []}},
    {history, {call, break_shim, err, [valid_error()]}},
    {tripped, {call, break_shim, err, [valid_error()]}},
    {history, {call, break_shim, ignored_error, [ignored_error()]}},
    {history, {call, break_shim, timeout, []}},
    {tripped, {call, break_shim, timeout, []}},
    {blocked, {call, break_shim, manual_block, []}},
    {ok, {call, break_shim, manual_deblock, []}},
    {ok, {call, break_shim, manual_reset, []}}
].

tripped(_Data) -> [
    {history, {call, break_shim, success, []}},
    {history, {call, break_shim, err, [valid_error()]}},
    {history, {call, break_shim, ignored_error, [ignored_error()]}},
    {history, {call, break_shim, timeout, []}},
    {blocked, {call, break_shim, manual_block, []}},
    {ok, {call, break_shim, manual_deblock, []}},
    {ok, {call, break_shim, manual_reset, []}}
].

blocked(_Data) -> [
    {history, {call, break_shim, success, []}},
    {history, {call, break_shim, err, [valid_error()]}},
    {history, {call, break_shim, ignored_error, [ignored_error()]}},
    {history, {call, break_shim, timeout, []}},
    {history, {call, break_shim, manual_block, []}},
    {history, {call, break_shim, manual_reset, []}},
    {ok, {call, break_shim, manual_deblock, []}}
].

valid_error() -> elements([badarg, badmatch, badarith, whatever]).

ignored_error() -> elements([ignore1, ignore2]).

%% Picks whether a command should be valid. 
precondition(unregistered, ok, _, {call, _, Call, _}) ->
    Call =:= success;
precondition(ok, To, #data{errors=N, limit=L}, {call, _, err, _}) ->
    (To =:= tripped andalso N + 1 =:= L) orelse (To =:= ok andalso N + 1 =/= L);
precondition(ok, To, #data{timeouts=N, limit=L}, {call, _, timeout, _}) ->
    (To =:= tripped andalso N + 1 =:= L) orelse (To =:= ok andalso N + 1 =/= L);
precondition(_From, _To, _Data, _Call) -> true.

%% Given the state states and data *prior* to the call
%% `{call, Mod, Fun, Args}', determine if the result `Res' (coming
%% from the actual system) makes sense.
postcondition(_From, _To, _Data, {call, _Mod, _Fun, _Args}, _Res) -> true.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed. 
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    NewData = Data,
    NewData.
