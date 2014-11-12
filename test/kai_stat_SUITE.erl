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

-module(kai_stat_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test_all].

test_all() -> [].
test_all(_Conf) ->
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {memcache_port, 11211},
        {memcache_max_processes, 1},
        {n, 3}, {r, 2}, {w, 2},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, ets}
    ]),
    kai_hash:start_link(),
    kai_store:start_link(),
    kai_stat:start_link(),
    kai_memcache:start_link(),

    kai_stat:incr_cmd_get(),
    kai_stat:incr_cmd_set(),

    Data = #data{value = (<<"value-1">>)},
    kai_stat:add_bytes_read([Data, Data]),
    kai_stat:add_bytes_write(Data),
    lists:map(fun(Arg) ->
                      kai_stat:incr_unreconciled_get(Arg)
              end,
              [{2, true},{2, true},{2, false},{3,true},{5,false}]),

    [{uptime,                      Uptime              },
     {time,                        Time                },
     {version,                     Version             },
     {bytes,                       Bytes               },
     {curr_items,                  CurrItems           },
     {curr_connections,            CurrConnections     },
     {cmd_get,                     CmdGet              },
     {cmd_set,                     CmdSet              },
     {bytes_read,                  BytesRead           },
     {bytes_write,                 BytesWrite          },
     {kai_node,                    LocalNode           },
     {kai_quorum,                  Quorum              },
     {kai_number_of_buckets,       NumberOfBuckets     },
     {kai_number_of_virtual_nodes, NumberOfVirtualNodes},
     {kai_store,                   Store               },
     {kai_curr_nodes,              Nodes               },
     {kai_unreconciled_get,        UnreconciledGet     },
%     {kai_curr_buckets,            Buckets             },
     {erlang_procs,                ErlangProcs         },
     {erlang_version,              ErlangVersion       }] = kai_stat:all(),
    {match, _S1, _L1} = regexp:match(Uptime,        "[0-9]+"),
    {match, _S2, _L2} = regexp:match(Time,          "[0-9]+"),
    {match, _S3, _L3} = regexp:match(Version,       "[.0-9]+"),
    {match, _S4, _L4} = regexp:match(Bytes,         "[0-9]+"),
    ?assertEqual("0",               CurrItems           ),
    ?assertEqual("0",               CurrConnections     ),
    ?assertEqual("1",               CmdGet              ),
    ?assertEqual("1",               CmdSet              ),
    ?assertEqual("14",              BytesRead           ),
    ?assertEqual("7",               BytesWrite          ),
    ?assertEqual("127.0.0.1:11011", LocalNode           ),
    ?assertEqual("3,2,2",           Quorum              ),
    ?assertEqual("8",               NumberOfBuckets     ),
    ?assertEqual("2",               NumberOfVirtualNodes),
    ?assertEqual("ets",             Store               ),
    ?assertEqual("127.0.0.1:11011", Nodes               ),
    ?assertEqual("1(3) 0(1) 0(0) 1(1)", lists:flatten(UnreconciledGet)),
%    ?assertEqual("0 1 2 3 4 5 6 7", Buckets             ),
    {match, _S5, _L5} = regexp:match(ErlangProcs,   "[0-9]+"),
    {match, _S6, _L6} = regexp:match(ErlangVersion, "[.0-9]+"),

    kai_memcache:stop(),
    kai_stat:stop(),
    kai_store:stop(),
    kai_hash:stop(),
    kai_config:stop().
