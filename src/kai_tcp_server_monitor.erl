% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(kai_tcp_server_monitor).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([
    register/2,
    increment/2, decrement/2,
    info/2
]).
-export([
    init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-include("kai.hrl").

% External APIs
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

stop(ServerRef) ->
    gen_server:call(ServerRef, stop).

register(ServerRef, Pid) ->
    gen_server:call(ServerRef, {register, Pid}).

increment(ServerRef, Pid) ->
    gen_server:cast(ServerRef, {increment, Pid}).

decrement(ServerRef, Pid) ->
    gen_server:cast(ServerRef, {decrement, Pid}).

info(ServerRef, Key) ->
    gen_server:call(ServerRef, {info, Key}).

% Callbacks
init(_Args) ->
    {ok, {_MonitorRefs = [], _Pids = []}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({register, Pid}, _From, {MonitorRefs, Pids}) ->
    {reply, ok, {
        [erlang:monitor(process, Pid) | MonitorRefs],
        Pids
    }};
      
handle_call({info, Key}, _From, State) ->
    {reply, state_to_info(State, Key), State};

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast({increment, Pid}, {MonitorRefs, Pids}) ->
    {noreply, {MonitorRefs, [Pid | Pids]}};

handle_cast({decrement, Pid}, {MonitorRefs, Pids}) ->
    {noreply, {MonitorRefs, lists:delete(Pid, Pids)}};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, _Type, Pid, _Info}, {MonitorRefs, Pids}) ->
    erlang:demonitor(MonitorRef),
    {noreply, {
        lists:delete(MonitorRef, MonitorRefs),
        lists:delete(Pid, Pids)
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal Functions
state_to_info({_MonitorRefs, Pids}, curr_connections) ->
    length(Pids); 

state_to_info(_State, _Key) ->
    undefined.

