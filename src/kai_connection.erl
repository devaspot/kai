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

-module(kai_connection).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([lease/2, lease/3, return/1, close/1, connections/0]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("kai.hrl").

-define(SERVER, ?MODULE).

-record(connection, {node, available, socket}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

do_lease({Address, Port} = Node, Pid, Opts, [], Acc) ->
    case gen_tcp:connect(
        Address, Port,
        [binary, {active, true}, {packet, 4}, {reuseaddr, true}],
        ?TIMEOUT
    ) of
        {ok, Socket} ->
            case gen_tcp:controlling_process(Socket, Pid) of
                ok ->
                    ok = inet:setopts(Socket, Opts),
                    Connection = #connection{
                        node      = Node,
                        available = false,
                        socket    = Socket
                    },
                    Connections = [Connection|lists:reverse(Acc)], % LRU
                    {ok, Socket, Connections};
                {error, Reason} ->
                    {error, Reason, Acc}
            end;
        {error, Reason} ->
            {error, Reason, Acc}
    end;
do_lease(Node, Pid, Opts, [#connection{node=Node, available=true, socket=Socket}|Rest], Acc) ->
    case gen_tcp:controlling_process(Socket, Pid) of
        ok ->
            ok = inet:setopts(Socket, Opts),
            Connection = #connection{
                node      = Node,
                available = false,
                socket    = Socket
            },
            Connections = [Connection|lists:reverse(Acc)] ++ Rest, % LRU
            flush(Socket),
            {ok, Socket, Connections};
        {error, Reason} ->
            Connections = Acc ++ Rest,
            {error, Reason, Connections}
    end;
do_lease(Node, Pid, Opts, [C|Rest], Acc) ->
    do_lease(Node, Pid, Opts, Rest, [C|Acc]).

lease(Node, Pid, Opts, Connections) ->
    case do_lease(Node, Pid, Opts, Connections, []) of
        {ok, Socket, Connections2} ->
            Connections3 = lru(Connections2),
            {reply, {ok, Socket}, Connections3};
        {error, Reason, Connections2} ->
            ?warning(io_lib:format("lease(~p, ~p) ~p",
                                   [Node, Pid, {error, Reason}])),
            {reply, {error, Reason}, Connections2}
    end.

flush(Socket) ->
    receive
        {tcp, Socket, _Bin} -> flush(Socket)
    after 0 -> ok
    end.    

lru(0, Rest, Acc) ->
    lists:reverse(Rest) ++ Acc;
lru(_N, [], Acc) ->
    Acc;
lru(N, [#connection{node=_, available=true, socket=Socket}|Rest], Acc) ->
    gen_tcp:close(Socket),
    lru(N-1, Rest, Acc);
lru(N, [#connection{node=_, available=false, socket=_} = C|Rest], Acc) ->
    lru(N, Rest, [C|Acc]).

lru(Connections) ->
    MaxConnections = kai_config:get(max_connections),
    Len = length(Connections),
    if
        Len > MaxConnections ->
            lru(Len - MaxConnections, lists:reverse(Connections), []);
        true ->
            Connections
    end.

do_return(_Socket, [], Acc) ->
    {error, enoent, Acc};
do_return(Socket, [#connection{node=Node, available=_, socket=Socket}|Rest], Acc) ->
    Connection = #connection{
        node      = Node,
        available = true,
        socket    = Socket
    },
    Connections = [Connection|lists:reverse(Acc)] ++ Rest, % LRU
    {ok, Connections};
do_return(Socket, [C|Rest], Acc) ->
    do_return(Socket, Rest, [C|Acc]).

return(Socket, Connections) ->
    case do_return(Socket, Connections, []) of
        {ok, Connections2} ->
            Connections3 = lru(Connections2),
            {reply, ok, Connections3};
        {error, Reason, Connections2} ->
            ?warning(io_lib:format("return(~p) ~p",
                                   [Socket, {error, Reason}])),
            {reply, {error, Reason}, Connections2}
    end.


do_close(_Socket, [], Acc) ->
    {error, enoent, Acc};
do_close(Socket, [#connection{node=_, available=_, socket=Socket}|Rest], Acc) ->
    gen_tcp:close(Socket),
    {ok, lists:reverse(Acc) ++ Rest};
do_close(Socket, [C|Rest], Acc) ->
    do_close(Socket, Rest, [C|Acc]).

close(Socket, Connections) ->
    case do_close(Socket, Connections, []) of
        {ok, Connections2} ->
            {reply, ok, Connections2};
        {error, Reason, Connections2} ->
            {reply, {error, Reason}, Connections2}
    end.

connections(Connections) ->
    {reply, {ok, Connections}, Connections}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({lease, Node, Pid, Opts}, _From, State) ->
    lease(Node, Pid, Opts, State);
handle_call({return, Socket}, _From, State) ->
    return(Socket, State);
handle_call({close, Socket}, _From, State) ->
    close(Socket, State);
handle_call(connections, _From, State) ->
    connections(State).
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
lease(Node, Pid) when is_pid(Pid)->
    gen_server:call(?SERVER, {lease, Node, Pid, []}).
lease(Node, Pid, Opts) when is_pid(Pid)->
    gen_server:call(?SERVER, {lease, Node, Pid, Opts}).
return(Socket) when is_port(Socket) ->
    case reset_controlling_process(Socket) of
        ok ->
            gen_server:call(?SERVER, {return, Socket});
        {error, Reason} ->
            ?warning(io_lib:format("return(~p) ~p", [Socket, {error, Reason}])),            
            gen_server:call(?SERVER, {close, Socket}),
            {error, Reason}
    end.
close(Socket) ->
    gen_server:call(?SERVER, {close, Socket}).
connections() ->
    gen_server:call(?SERVER, connections).

reset_controlling_process(Socket) ->
    case whereis(?SERVER) of
        Pid when is_pid(Pid) -> gen_tcp:controlling_process(Socket, Pid);
        _                    -> {error, esrch}
    end.
