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

-module(kai_connection_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test1, test2].

test1_api_start() ->
    {ok, ListeningSocket} =
        gen_tcp:listen(11012, [binary, {packet, 4}, {reuseaddr, true}]),
    test1_api_accpet(ListeningSocket).

test1_api_accpet(ListeningSocket) ->
    {ok, ApiSocket} = gen_tcp:accept(ListeningSocket),
    Pid = spawn(?MODULE, test1_api_proc, [ApiSocket]),
    gen_tcp:controlling_process(ApiSocket, Pid),
    test1_api_accpet(ListeningSocket).

test1_api_proc(ApiSocket) ->
    receive
        {tcp, ApiSocket, _Bin} ->
            gen_tcp:send(ApiSocket, term_to_binary(ok))
    end.

test1_api_send(Pid) ->
    {ok, Socket} = kai_connection:lease(?NODE2, self()),
    ok = gen_tcp:send(Socket, term_to_binary(ok)),
    Pid ! receive {tcp, Socket, Bin} -> binary_to_term(Bin) end.

test1() -> [].
test1(_Conf) ->
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {max_connections, 32},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2}
    ]),
    kai_connection:start_link(),

    spawn_link(?MODULE, test1_api_start, []),

    % lease and return

    {ok, Socket1} = kai_connection:lease(?NODE2, self()),
    {ok, Connections} = kai_connection:connections(),
    ?assertEqual(1, length(Connections)),

    {ok, Socket2} = kai_connection:lease(?NODE2, self()),
    {ok, Connections2} = kai_connection:connections(),
    ?assertEqual(2, length(Connections2)),

    ?assert(Socket1 =/= Socket2),
    
    ok = kai_connection:return(Socket1),
    {ok, Connections3} = kai_connection:connections(),
    ?assertEqual(2, length(Connections3)),

    {ok, Socket3} = kai_connection:lease(?NODE2, self(), [{active, true}, {packet, 4}]),

    ?assert(Socket1 =:= Socket3),

    ok = kai_connection:close(Socket3),
    {ok, Connections4} = kai_connection:connections(),
    ?assertEqual(1, length(Connections4)),

    % send and receive at different processes

    spawn_link(?MODULE, test1_api_send, [self()]),
    ?assert(receive ok -> true; _ -> false end),

    spawn_link(?MODULE, test1_api_send, [self()]),
    ?assert(receive ok -> true; _ -> false end),

    kai_config:stop(),
    kai_connection:stop().

test2() -> [].
test2(_Conf) ->
    MaxConnections = 32,

    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {max_connections, MaxConnections},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2}
    ]),
    kai_connection:start_link(),

    spawn_link(?MODULE, test1_api_start, []),

    % lease MaxConnections connections

    Sockets =
        lists:map(
          fun(_X) ->
                  {ok, Socket} = kai_connection:lease(?NODE2, self()),
                  Socket
          end,
          lists:seq(1, MaxConnections + 1)
         ),

    % # of connections can be greater than MaxConnections, because all
    % connections are in use

    {ok, Connections} = kai_connection:connections(),
    ?assertEqual(MaxConnections + 1, length(Connections)),

    % # of connections equals to MaxConnections, because a connection has
    % been returned

    [Socket|_] = Sockets,
    ok = kai_connection:return(Socket),
    {ok, Connections2} = kai_connection:connections(),
    ?assertEqual(MaxConnections, length(Connections2)),

    kai_config:stop(),
    kai_connection:stop().
