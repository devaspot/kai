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

-module(kai_log).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([log/5]).
-export([
    init/1, terminate/2, handle_cast/2, handle_call/3, handle_info/2,
    code_change/3
]).

-include("kai.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).

init(_Args) ->
    case kai_config:get(logfile) of
        undefined ->
            {ok, []};
        File ->
            case file:open(File, [write, append]) of
                {ok, Fd} -> {ok, [{fd, Fd}]};
                Error    -> Error
            end
    end.

terminate(_Reason, State) ->
    case proplists:get_value(fd, State) of
        undefined -> ok;
        Fd        -> file:close(Fd)
    end.

log(Type, Pid, File, Line, Data, State) ->
    {{Year,Month,Day}, {Hour,Minute,Second}} = erlang:localtime(),
    {_MegaSec, _Sec, Usec} = now(),
    Data2 =
        if
            is_list(Data) -> lists:flatten(Data);
            true          -> Data
        end,
    Buf = io_lib:format(
        "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w [~s] (~p) ~s:~w: ~p\n",
        [Year, Month, Day, Hour, Minute, Second, Usec, Type, Pid, File, Line, Data2]
    ),
    case proplists:get_value(fd, State) of
        undefined -> io:format(    "~s", [Buf]);
        Fd        -> io:format(Fd, "~s", [Buf])
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.
handle_cast({log, Type, Pid, File, Line, Data}, State) ->
    log(Type, Pid, File, Line, Data, State),
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
log(Type, Pid, File, Line, Data) ->
    gen_server:cast(?SERVER, {log, Type, Pid, File, Line, Data}).
