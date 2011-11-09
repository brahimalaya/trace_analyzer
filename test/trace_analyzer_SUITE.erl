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

-module(trace_analyzer_SUITE).
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
	Message = 1234,
	Sent = lock,
	To = lock_port,
	Process = fsm_demo,
	TraceList = 
    [{trace, {timestamp, 1}, {process, Process}, {type, 'receive'},
			{msg, {'$gen_event', Message}}},
     {trace, {timestamp, 1}, {process, Process}, {type, send}, {msg, Sent}, 
		 {to, To}}],
 	
 	InputChannel = {Process, {message, 'receive'}},
	EffectChannel = {Process, {message, send}},

	LongTraceList = TraceList ++
		[{trace, {timestamp, 1}, {process, Process}, {type, 'receive'},
				{msg, {'$gen_event', 123}}},
		{trace, {timestamp, 1}, {process, Process}, {type, 'receive'},
				{msg, {'$gen_event', Message}}},
		 {trace, {timestamp, 1}, {process, Process}, {type, send}, {msg, unlock}, 
			 {to, To}}],

	Props = 
	[{message, Message},
	 {sent, Sent},
	 {to, To},
	 {process, Process},
	 {trace_list, TraceList},
	 {long_trace_list, LongTraceList},
	 {input_channel, InputChannel},
	 {effect_channel, EffectChannel}],

    Config ++ Props.

init_per_testcase(_Case, Config) ->
	fsm:start_link(),
    Config.

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(_Case, _Config) ->
	ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [
		start_create_fsm_ok,
		start_create_fsm_state_dict_ok,
		start_create_fsm_long_trace_list_ok,

		all_effects_before_next_input_ok,

		next_input_ok
	].

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

start_create_fsm_ok(Config) ->
	TraceList = ?config(trace_list, Config),
	InputChannel = ?config(input_channel, Config),
	EffectChannel = ?config(effect_channel, Config),

	done = trace_analyzer:start_create_fsm(TraceList, InputChannel, EffectChannel).

start_create_fsm_states_ok(Config) ->
	TraceList = ?config(trace_list, Config),
	InputChannel = ?config(input_channel, Config),
	EffectChannel = ?config(effect_channel, Config),
	Sent = ?config(sent, Config),
	EffectType = {message, send},
	InitEffect = #effect{type=init, content=undefined},
	LockEffect = #effect{type={message,send}, content=lock},

	done = trace_analyzer:start_create_fsm(TraceList, InputChannel, EffectChannel),

	Dict = fsm:get_state_dict(),

	{ok, InitState} = dict:find(InitEffect, Dict),
	{ok, LockState} = dict:find(LockEffect, Dict),

	% TODO: Tests states using state_master here
	ok.

start_create_fsm_state_dict_ok(Config) ->
	TraceList = ?config(trace_list, Config),
	InputChannel = ?config(input_channel, Config),
	EffectChannel = ?config(effect_channel, Config),
	Sent = ?config(sent, Config),
	EffectType = {message, send},
	InitEffect = #effect{type=init, content=undefined},
	LockEffect = #effect{type=EffectType, content=Sent},

	done = trace_analyzer:start_create_fsm(TraceList, InputChannel, EffectChannel),

	Dict = fsm:get_state_dict(),

	?assert(dict:is_key(InitEffect, Dict)),
	?assert(dict:is_key(LockEffect, Dict)),
	?assertEqual(dict:size(Dict), 2),

	{ok, InitState} = dict:find(InitEffect, Dict),
	{ok, LockState} = dict:find(LockEffect, Dict),

	?assertMatch({state, _, _, _}, InitState),
	?assertMatch({state, _, _, _}, LockState),
	?assert(InitState /= LockState),

	ok.

start_create_fsm_long_trace_list_ok(Config) ->
	LongTraceList = ?config(long_trace_list, Config),
	InputChannel = ?config(input_channel, Config),
	EffectChannel = ?config(effect_channel, Config),
	Sent = ?config(sent, Config),
	Message = {'$gen_event', ?config(message, Config)},
	EffectType = {message, send},
	InitEffect = #effect{type=init},
	LockEffect = #effect{type={message,send}, content=lock},
	UnlockEffect = #effect{type={message,send}, content=unlock},

	done = trace_analyzer:start_create_fsm(LongTraceList, InputChannel, EffectChannel),

	timer:sleep(100),

	Dict = fsm:get_state_dict(),
	?assertEqual(3, dict:size(Dict)),

	{ok, InitState} = dict:find(InitEffect, Dict),
	?assertMatch({state, InitEffect,_,_}, InitState),
	?assertEqual(1, dict:size(InitState#state.transitions)),
	{ok, LockTrans} = dict:find(Message, InitState#state.transitions),
	?assertMatch({transition, Message, LockEffect}, LockTrans),

	{ok, LockState} = dict:find(LockEffect, Dict),
	?assertMatch({state, LockEffect,_,_}, LockState),
	?assertEqual(1, dict:size(LockState#state.transitions)),
	{ok, UnlockTrans} = dict:find(Message, LockState#state.transitions),
	?assertMatch({transition, Message, UnlockEffect}, UnlockTrans),

	{ok, UnlockState} = dict:find(UnlockEffect, Dict),
	?assertEqual(0, dict:size(UnlockState#state.transitions)).

all_effects_before_next_input_ok(Config) ->
	TraceList = ?config(trace_list, Config),
	InputChannel = ?config(input_channel, Config),
	EffectChannel = ?config(effect_channel, Config),
	Sent = ?config(sent, Config),

	{Effects, TraceListTail} = trace_analyzer:all_effects_before_next_input(
		[lists:nth(2, TraceList)], InputChannel, EffectChannel, []),

	ExpectedEffects = [Sent],
	ExpectedTraceListTail = [],

	?assertEqual(ExpectedTraceListTail, TraceListTail),

	?assertEqual(ExpectedEffects, Effects),

	ok.

next_input_ok(Config) ->
	TraceList = ?config(trace_list, Config),
	InputChannel = ?config(input_channel, Config),
	EffectChannel = ?config(effect_channel, Config),
	Message = ?config(message, Config),
	Process = ?config(process, Config),
	Sent = ?config(sent, Config),
	To = ?config(to, Config),

	{Input, TraceAfterInput} = trace_analyzer:next_input(TraceList,
		InputChannel),

	ExpectedInput = {'$gen_event', Message},
	ExpectedTraceAfterInput = [{trace, {timestamp, 1}, {process, Process}, 
			{type, send}, {msg, Sent}, {to, To}}],

	?assertEqual(ExpectedTraceAfterInput, TraceAfterInput),

	?assertEqual(ExpectedInput, Input),

	ok.


%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

