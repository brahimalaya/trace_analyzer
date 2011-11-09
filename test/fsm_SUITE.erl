%% ------------------------------------------------------------------------------
%% Copyright 2011 Rickard Olsson
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% ------------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% -----------------------------------------------------------------------------

-module(fsm_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/fsm.hrl").

%% Test server callbacks
-export([suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
         all/0]).

%% Test cases
-compile(export_all).

%%------------------------------------------------------------------------------
%% Common test callbacks
%%------------------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 1}}].

%%------------------------------------------------------------------------------
%% Setups
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

init_per_testcase(start_link_ok, Config) ->
    Config;
init_per_testcase(_Case, Config) ->
	fsm:start_link(),
	exit_if_dead(fsm),
    Config.

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(start_link_ok, _Config) ->
	fsm:stop(),
	ok;
end_per_testcase(_Case, _Config) ->
	fsm:stop(),
	ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [
		start_link_ok,
		start_link_state_dict_ok,

		get_state_with_effect_ok,
		adding_state_updates_current_state,

		added_transition_can_be_retrieved_by_effect,
		added_state_can_be_retrieved_by_effect
	].

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

start_link_ok(_Config) ->
	fsm:start_link(),
	exit_if_dead(fsm).

start_link_state_dict_ok(_Config) ->
	InitEffect = #effect{type = init},
	Dict = fsm:get_state_dict(),
	{ok, InitState} = dict:find(InitEffect, Dict),
	?assertMatch({state, InitEffect, init, _}, InitState).

get_state_with_effect_ok(_Config) ->
	InitEffect = #effect{type = init},
	CurrentState = fsm:get_current_state(),
	State = fsm:get_state_with_effect(CurrentState#state.effect),
	?assertMatch({state, InitEffect, init, _}, State).

added_transition_can_be_retrieved_by_effect(_Config) ->
	CurrentState = fsm:get_current_state(),
	Word = close,
	Effect = #effect{type={message, send}, content=closed},
	fsm:add_transition(Word, Effect),

	timer:sleep(200),

	State = fsm:get_state_with_effect(CurrentState#state.effect),

	DictFindResult = dict:find(Word, State#state.transitions),
	?assertMatch({ok, _}, DictFindResult),
	{ok, Transition} = DictFindResult,

	ExpectedTransition = #transition{word = close, effect = Effect},

	?assertEqual(ExpectedTransition, Transition).

added_state_can_be_retrieved_by_effect(_Config) ->
	Word = close,
	Effect = #effect{type={message, send}, content=closed},
	fsm:add_transition(Word, Effect),

	timer:sleep(200),

	State = fsm:get_state_with_effect(Effect),
	ExpectedState = #state{effect = Effect, name = Effect#effect.content},

	?assertEqual(State, ExpectedState).

adding_state_updates_current_state(_Config) ->
	Word = close,
	Effect = #effect{type={message, send}, content=closed},
	fsm:add_transition(Word, Effect),

	timer:sleep(200),

	CurrentState = fsm:get_current_state(),
	ExpectedState = #state{effect = Effect, name = Effect#effect.content},

	?assertEqual(ExpectedState, CurrentState).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

exit_if_dead(Pid) when is_pid(Pid) ->
	?assert(is_process_alive(Pid));
exit_if_dead(RegisteredName) ->
	Pid = whereis(RegisteredName),
	?assert(Pid /= undefined),
	?assert(is_pid(Pid)),
	exit_if_dead(Pid).

