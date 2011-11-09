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

-module(fsm_demo_SUITE).
-include_lib("common_test/include/ct.hrl").

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

init_per_testcase(_Case, Config) ->
	rec:start(),
	register(test_pid, self()),
    Config.

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.
end_per_testcase(_Case, _Config) ->
	rec:stop(),
	unregister(test_pid).

%% Returns the list of groups and test cases to be tested
all() ->
 	[
		rec_fsm_demo
	].

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

rec_fsm_demo(_Config) ->
	Filename = "fsm_trace.log",
	fsm_demo:start_link(),

	timer:sleep(100),
	rec:add_process(fsm_demo, Filename, [m]),

	timer:sleep(100),
	Message = 1234,
	gen_fsm:send_event(fsm_demo, Message),

	timer:sleep(100),
	Sent = lock,
	To = lock_port,
    {ok, Terms} = file:consult(Filename),
    [{trace, {timestamp, _}, {process, _RegProc}, {type, 'receive'},
			{msg, {'$gen_event', Message}}},
     {trace, {timestamp, _}, {process, fsm_demo}, {type, send}, {msg, Sent}, 
		 {to, To}}]
     	= Terms,
	 ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

