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

-module(fsm).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/fsm.hrl").

%% -----------------------------------------------------------------------------
%% Macros and Records
%% -----------------------------------------------------------------------------

-record(state_data, {state_dict = [], current_state, handlers = []}).

%% -----------------------------------------------------------------------------
%% API Function Exports
%% -----------------------------------------------------------------------------

-export([start_link/0, start_link/1, get_state_dict/0, get_state_with_effect/1,
		 add_transition/2, send_input/1, stop/0]).

%% -----------------------------------------------------------------------------
%% Export all functions when compiled for testing to allowing testing
%% internal functions. Eunit supports this by default, common test
%% needs to be compiled by adding {d, 'TEST', true} to erlc.
%% -----------------------------------------------------------------------------

-ifdef(TEST).
-compile(export_all).
-endif.

%% -----------------------------------------------------------------------------
%% gen_server Function Exports
%% -----------------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
		 code_change/3]).

%% -----------------------------------------------------------------------------
%% Type Definitions
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% API Function Definitions
%% -----------------------------------------------------------------------------

start_link() ->
  	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Handlers) ->
	Args = [{handlers, Handlers}],
  	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

get_state_dict() ->
	gen_server:call(?SERVER, get_state_dict).

get_state_with_effect(#effect{} = Effect) ->
	gen_server:call(?SERVER, {get_state_with_effect, Effect}).

get_current_state() ->
	gen_server:call(?SERVER, get_current_state).

add_transition(Word, #effect{} = Effect) ->
	gen_server:cast(?SERVER, {add_transition, Word, Effect}).

send_input(Word) ->
	gen_server:cast(?SERVER, {send_input, Word}).

stop() ->
	gen_server:cast(?SERVER, stop).

%% -----------------------------------------------------------------------------
%% gen_server Function Definitions
%% -----------------------------------------------------------------------------

init(Args) ->
	Handlers = proplists:get_value(handlers, Args, []),
	InitEffect = #effect{type = init},
	InitState = #state{effect = InitEffect, name = init},
	StateDict = dict:store(InitEffect, InitState, dict:new()),
	StateData = #state_data{state_dict = StateDict, current_state = InitState,
		handlers = Handlers},
	{ok, StateData}.

handle_call(get_state_dict, _From, #state_data{state_dict = StateDict} = State) ->
	{reply, StateDict, State};
handle_call({get_state_with_effect, #effect{} = Effect}, _From, 
		#state_data{state_dict = StateDict } = State) ->
	StateWithEffect = 
	case dict:find(Effect, StateDict) of
		error ->
			error;
		{ok, Value} ->
			Value
	end,
	{reply, StateWithEffect, State};
handle_call(get_current_state, _From, 
		#state_data{ current_state = CurrentState } = State) ->
	{reply, CurrentState, State}.

handle_cast({add_transition, Word, #effect{} = Effect}, #state_data{
		state_dict = StateDict, 
		current_state = CurrentState, 
		handlers = Handlers} = StateData) ->

	Transition = #transition{word = Word, effect = Effect},
	NewStateData = 
	case dict:find(Word, CurrentState#state.transitions) of
		error ->
			NewTransitions = dict:store(Word, Transition, CurrentState#state.transitions),
			UpdatedCurrentState = CurrentState#state{transitions = NewTransitions},
			UpdatedStateDict = dict:store(CurrentState#state.effect, 
				UpdatedCurrentState, StateDict),

			NewCurrentState = 
			case dict:find(Effect, StateDict) of
				{ok, Value} ->
					Value;
				error ->
					NewState = #state{effect = Effect, name = Effect#effect.content},
					notify_handlers(Handlers, add_state, NewState),
					NewState
			end,
			%notify_handlers(Handlers, add_transition, Transition),
			NewStateDict = dict:store(Effect, NewCurrentState, UpdatedStateDict),
			StateData#state_data{state_dict = NewStateDict, current_state = NewCurrentState};
		Transition ->
			{ok, NewCurrentState} = dict:find(Effect, StateDict),
			StateData#state_data{state_dict = StateDict, current_state = NewCurrentState};
		_OtherTransition ->
			exit("Trying to add ambigous transition!")
	end,
	{noreply, NewStateData};
handle_cast({send_input, Word}, #state_data{state_dict = _StateDict, 
		current_state = CurrentState} = State) ->
	NewCurrentState = state:handle_input(CurrentState, Word),
	NewStateData = State#state_data{current_state=NewCurrentState},
	{noreply, NewStateData};
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% -----------------------------------------------------------------------------
%% Internal Function Definitions
%% -----------------------------------------------------------------------------
notify_handlers(Handlers, Event, Properties) ->
	SendEventFun = fun(HandlerPid) -> HandlerPid ! {Event, Properties} end,
	lists:foreach(SendEventFun, Handlers).
