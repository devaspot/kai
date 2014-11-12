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

-module(kai_memcache_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test1].

test1() -> [].
test1(_Conf) ->
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {rpc_max_processes, 2},
        {memcache_port, 11211},
        {memcache_max_processes, 2},
        {max_connections, 32},
        {n, 1}, {r, 1}, {w, 1},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, ets}
    ]),
    kai_hash:start_link(),
    kai_store:start_link(),
    kai_stat:start_link(),
    kai_version:start_link(),
    kai_connection:start_link(),
    kai_rpc:start_link(),
    kai_memcache:start_link(),

    timer:sleep(100), % wait for starting kai_memcache

    {ok, MemcacheSocket} =
        gen_tcp:connect({127,0,0,1}, 11211, [binary, {packet, line}, {active, false}]),

    Value = <<"value-1">>,
    Buf = io_lib:format("set item-1 0 0 ~w\r\n~s\r\n", [byte_size(Value), Value]),
    gen_tcp:send(MemcacheSocket, Buf),

    ?assertEqual(
       {ok, <<"STORED\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),

    gen_tcp:send(MemcacheSocket, "get item-1\r\n"),

    ?assertEqual(
       {ok, <<"VALUE item-1 0 7\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),
    inet:setopts(MemcacheSocket, [{packet, raw}]),
    ?assertEqual(
       {ok, <<"value-1">>},
       gen_tcp:recv(MemcacheSocket, byte_size(<<"value-1">>))
      ),
    inet:setopts(MemcacheSocket, [{packet, line}]),
    ?assertEqual(
       {ok, <<"\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),
    ?assertEqual(
       {ok, <<"END\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),

    gen_tcp:send(MemcacheSocket, "delete item-1\r\n"),
    ?assertEqual(
       {ok, <<"DELETED\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),

    gen_tcp:send(MemcacheSocket, "delete item-2 0\r\n"),
    ?assertEqual(
       {ok, <<"NOT_FOUND\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),

    gen_tcp:send(MemcacheSocket, "get item-1\r\n"),
    ?assertEqual(
       {ok, <<"END\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),

    gen_tcp:send(MemcacheSocket, "no_such_command\r\n"),
    ?assertEqual(
       {ok, <<"ERROR\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),

    gen_tcp:send(MemcacheSocket, "stats\r\n"),
    {ok, Uptime              } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, Time                } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, Version             } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, Bytes               } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, CurrItems           } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, CurrConnections     } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, CmdGet              } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, CmdSet              } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, BytesRead           } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, BytesWrite          } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, LocalNode           } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, Quorum              } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, NumberOfBuckets     } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, NumberOfVirtualNodes} = gen_tcp:recv(MemcacheSocket, 0),
    {ok, Store               } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, Nodes               } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, UnreconciledGet     } = gen_tcp:recv(MemcacheSocket, 0),
%    {ok, Buckets             } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, ErlangProcs         } = gen_tcp:recv(MemcacheSocket, 0),
    {ok, ErlangVersion       } = gen_tcp:recv(MemcacheSocket, 0),
    {match, _S1, _L1} = regexp:match(binary_to_list(Uptime),               "uptime"),
    {match, _S2, _L2} = regexp:match(binary_to_list(Time),                 "time"),
    {match, _S3, _L3} = regexp:match(binary_to_list(Version),              "version"),
    {match, _S4, _L4} = regexp:match(binary_to_list(Bytes),                "bytes"),
    {match, _S5, _L5} = regexp:match(binary_to_list(CurrItems),            "curr_items"),
    {match, _S6, _L6} = regexp:match(binary_to_list(CurrConnections),            "curr_connections"),
    {match, _S7, _L7} = regexp:match(binary_to_list(CmdGet),               "cmd_get"),
    {match, _S8, _L8} = regexp:match(binary_to_list(CmdSet),               "cmd_set"),
    {match, _S9, _L9} = regexp:match(binary_to_list(BytesRead),            "bytes_read"),
    {match, _S0, _L0} = regexp:match(binary_to_list(BytesWrite),           "bytes_write"),
    {match, _Sa, _La} = regexp:match(binary_to_list(LocalNode),            "kai_node"),
    {match, _Sb, _Lb} = regexp:match(binary_to_list(Quorum),               "kai_quorum"),
    {match, _Sc, _Lc} = regexp:match(binary_to_list(NumberOfBuckets),      "kai_number_of_buckets"),
    {match, _Sd, _Ld} = regexp:match(binary_to_list(NumberOfVirtualNodes), "kai_number_of_virtual_nodes"),
    {match, _Se, _Le} = regexp:match(binary_to_list(Store),                "kai_store"),
    {match, _Sf, _Lf} = regexp:match(binary_to_list(Nodes),                "kai_curr_nodes"),
    {match, _Sk, _Lk} = regexp:match(binary_to_list(UnreconciledGet),      "kai_unreconciled_get"),
%    {match, _Sg, _Lg} = regexp:match(binary_to_list(Buckets),              "kai_curr_buckets"),
    {match, _Sh, _Lh} = regexp:match(binary_to_list(ErlangProcs),          "erlang_procs"),
    {match, _Si, _Li} = regexp:match(binary_to_list(ErlangVersion),        "erlang_version"),
    ?assertEqual(
       {ok, <<"END\r\n">>},
       gen_tcp:recv(MemcacheSocket, 0)
      ),

    gen_tcp:send(MemcacheSocket, "version\r\n"),
    {ok, Version2} = gen_tcp:recv(MemcacheSocket, 0),
    {match, _Sj, _Lj} = regexp:match(binary_to_list(Version2), "VERSION "),

    gen_tcp:send(MemcacheSocket, "quit\r\n"),

    gen_tcp:close(MemcacheSocket),

    kai_memcache:stop(),
    kai_rpc:stop(),
    kai_connection:stop(),
    kai_version:stop(),
    kai_stat:stop(),
    kai_store:stop(),
    kai_hash:stop(),
    kai_config:stop().
