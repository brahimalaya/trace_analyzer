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

%% -----------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% ------------------------------------------------------------------

-module(fsm_demo).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

-export([locked/2, unlocked/2]).

-export([lock_loop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [1234], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	register(lock_port, spawn(?MODULE, lock_loop, [[]])),
  	{ok, unlocked, Args}.

locked(Code, [Code]) ->
	unlock(),
	{next_state, unlocked, [Code]};
locked(_Other, [Code]) ->
	{next_state, locked, [Code]}.

unlocked(Code, [Code]) ->
	lock(),
	{next_state, locked, [Code]};
unlocked(_Other, [Code]) ->
	{next_state, unlocked, [Code]}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) -> 
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _State) ->
  	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  	{ok, StateName, State}.

unlock() ->
	lock_port ! unlock.

lock() ->
	lock_port ! lock.

lock_loop(Args) ->
	receive _ ->
			ok
	end,
	lock_loop(Args).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

