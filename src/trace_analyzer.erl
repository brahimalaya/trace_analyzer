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

-module(trace_analyzer).
-include("../include/fsm.hrl").

%% -----------------------------------------------------------------------------
%% API Function Exports
%% -----------------------------------------------------------------------------

-export([start_create_fsm/3, start_create_fsm/4, create_fsm/3, create_fsm/4]).

%% -----------------------------------------------------------------------------
%% Export all functions when compiled for testing to allowing testing
%% internal functions. Eunit supports this by default, common test
%% needs to be compiled by adding {d, 'TEST', true} to erlc.
%% -----------------------------------------------------------------------------

-ifdef(TEST).
-compile(export_all).
-endif.

%% -----------------------------------------------------------------------------
%% Type Definitions
%% -----------------------------------------------------------------------------
-type process_name() :: atom().
-type input_channel() :: {process_name(), {message, 'receive'}}.
-type effect_channel() :: {process_name(), {message, 'send'}}.

%% -----------------------------------------------------------------------------
%% API Function Definitions
%% -----------------------------------------------------------------------------

% TODO: fix this register business, which funs should be exported and 
% should this be a gen_server? NOTE: Unregisters in create_fsm/3
start_create_fsm(TraceList, InputChannel, EffectChannel) ->
	start_create_fsm(TraceList, InputChannel, EffectChannel, []).

start_create_fsm(TraceList, InputChannel, EffectChannel, Handlers) ->
	Args = [TraceList, InputChannel, EffectChannel, Handlers],
	Pid = spawn_link(?MODULE, create_fsm, Args),
	register(trace_analyzer, Pid),
	trace_analyzer !  {self(), get_status},
	receive {Pid, {status, Status}} -> Status end.

% TODO: The channels need to be flexible and powerful, to allow defining
% a side effect channel to match on from pid to pid, and also i/o, calls etc
%% -----------------------------------------------------------------------------
%% @doc Creates an fsm out of a trace log using the specified channels.
%% The input channel defines the language given to the machine. The effect
%% channel describes state changes and distinguishable states
%% -----------------------------------------------------------------------------
-spec (create_fsm/3 :: ([], input_channel(), effect_channel()) -> ok).
create_fsm(TraceList, InputChannel, EffectChannel) ->
	create_fsm(TraceList, InputChannel, EffectChannel, []).

create_fsm(TraceList, InputChannel, EffectChannel, Handlers) ->
	fsm:start_link(Handlers),
	traverse_trace(TraceList, InputChannel, EffectChannel),
	receive {From, get_status} -> From ! {self(), {status, done}} end,
	unregister(trace_analyzer).

%% -----------------------------------------------------------------------------
%% Internal Function Definitions
%% -----------------------------------------------------------------------------

create_init_state() ->
	state:start_link(init_state, #effect{type=init, content=undefined}).

% TODO: Test so last input without side effect is added as transition
traverse_trace([], _InputChannel, _EffectChannel) ->
	ok;
traverse_trace(TraceList, InputChannel, EffectChannel) ->
	case next_input(TraceList, InputChannel) of
		no_input ->
			ok;
		{Input, TraceAfterInput} ->
			{Effects, TraceListTail} = 
			all_effects_before_next_input(TraceAfterInput, InputChannel, EffectChannel, []),

			case Effects of
				[] ->
					ok;
				_Other ->
					Effect = compress_effects(Effects),
					fsm:add_transition(Input, Effect)
			end,

			traverse_trace(TraceListTail, InputChannel, EffectChannel)
	end.


next_input([], _) ->
	no_input;
next_input([Trace|Tail], InputChannel) ->
	case extract_input(Trace, InputChannel) of
		no_input ->
			next_input(Tail, InputChannel);
		Input ->
			{Input, Tail}
	end.

extract_input({trace, _, {process, Process}, {type, 'receive'}, {msg, Message}},
		{Process, {message, 'receive'}}) ->
	Message;
extract_input(_Trace, {_Process, {message, 'receive'}}) ->
	no_input.

extract_effect({trace, _, {process, Process}, {type, send}, {msg, Message}, _},
		{Process,{message, send}}) ->
	Message;
extract_effect(_Trace, {_Process,{message, send}}) ->
	no_effect.

all_effects_before_next_input([], _InputChannel, _EffectChannel, Effects) ->
	{Effects, []};
all_effects_before_next_input([Trace | Tail], InputChannel, EffectChannel, Effects) ->
	EffectResult = extract_effect(Trace, EffectChannel),
	InputResult = extract_input(Trace, InputChannel),

	case {EffectResult, InputResult} of
		{no_effect, no_input} ->
			all_effects_before_next_input(Tail, InputChannel, EffectChannel, Effects);
		{Effect, no_input} ->
			AccEffect = [Effect | Effects],
			all_effects_before_next_input(Tail, InputChannel, EffectChannel, AccEffect);
		{_, _Input} ->
			{Effects, [Trace | Tail]}
	end.

%FIXME: Implement me!
% Compress side effect words into a single representation
compress_effects([Effect | _]) ->
	#effect{type={message,send}, content=Effect}.

