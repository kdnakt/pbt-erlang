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
ok(_Data) -> [].

tripped(_Data) -> [].

blocked(_Data) -> [].

%% Picks whether a command should be valid. 
precondition(_From, _To, #data{}, {call, _Mod, _Fun, _Args}) -> true.

%% Given the state states and data *prior* to the call
%% `{call, Mod, Fun, Args}', determine if the result `Res' (coming
%% from the actual system) makes sense.
postcondition(_From, _To, _Data, {call, _Mod, _Fun, _Args}, _Res) -> true.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed. 
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    NewData = Data,
    NewData.
