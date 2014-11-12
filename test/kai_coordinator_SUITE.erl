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

-module(kai_coordinator_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").
-include("ct.hrl").

all() -> [test1, get_concurrent_data, put_concurrent_data,
         put_and_overwrite_stale_data, put_and_fail_overwrite_stale_data].

init_per_suite(Config) ->
    net_kernel:start([hoge, shortnames]),
    {N, R, W} = {2, 2, 2},
    Node1 = start_kai_node(11211, N, R, W),
    Config1 = [{node1, Node1} | Config],
    Node2 = start_kai_node(11212, N, R, W),
    Config2 = [{node2, Node2} | Config1],
    {ok, Localhost} = inet:getaddr(localhost, inet),
    rpc:call(Node1, kai_membership, check_node, [{Localhost, 11012}]),
    rpc:call(Node2, kai_membership, check_node, [{Localhost, 11011}]),
    timer:sleep(100),
    %%     p("nodes at Node1", rpc:call(Node1, kai_hash, node_list, [])),
    Config2.

end_per_suite(Config) ->
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),
    ok = slave:stop(Node1),
    ok = slave:stop(Node2),
    ok.

test1() -> [].
test1(_Conf) ->
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 21011},
        {rpc_max_processes, 2},
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

    ?assertEqual(
       ok,
       kai_coordinator:route({put, #data{
           key   = "item-1",
           flags = "0",
           value = (<<"value-1">>)}
       })
      ),

    ListOfData = kai_coordinator:route({get, #data{key="item-1"}}),
    ?assertEqual(1, length(ListOfData)),

    [Data|_] = ListOfData,
    ?assert(is_record(Data, data)),
    ?assertEqual("item-1", Data#data.key),
    ?assertEqual(3, Data#data.bucket),
    ?assertEqual(erlang:md5(<<"value-1">>), Data#data.checksum),
    ?assertEqual("0", Data#data.flags),
    ?assertEqual(<<"value-1">>, Data#data.value),

    ?assertEqual(
       ok,
       kai_coordinator:route({delete, #data{key="item-1"}})
      ),

    ?assertEqual(
       undefined,
       kai_coordinator:route({get, #data{key="item-1"}})
      ),

    ?assertEqual(
       undefined,
       kai_coordinator:route({delete, #data{key="item-1"}})
      ),

    kai_rpc:stop(),
    kai_connection:stop(),
    kai_version:stop(),
    kai_stat:stop(),
    kai_store:stop(),
    kai_hash:stop(),
    kai_config:stop().


get_concurrent_data() -> [].
get_concurrent_data(Config) ->
    Key = "key1",
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),

    ok = rpc:call(Node1, kai_coordinator, route, [{put, #data{key=Key, value="value1"}}]),
    [Data] = rpc:call(Node1, kai_coordinator, route, [{get, #data{key=Key}}]),
    IntentionalConcurrentVCAtNode1 = vclock:increment(rpc:call(Node1, kai_config, get, [node]),
                                               Data#data.vector_clocks),
    IntentionalConcurrentVCAtNode2 = vclock:increment(rpc:call(Node2, kai_config, get, [node]),
                                               Data#data.vector_clocks),
    ok = rpc:call(Node1, kai_store, put, [Data#data{vector_clocks=IntentionalConcurrentVCAtNode1}]),
    ok = rpc:call(Node2, kai_store, put, [Data#data{vector_clocks=IntentionalConcurrentVCAtNode2}]),

    GetResult = rpc:call(Node1, kai_coordinator, route, [{get, #data{key=Key}}]),
    p("get result:", GetResult),
    ?assertEqual(2, length(GetResult)),
    StatOfUnreconciledGet =
        proplists:get_value(kai_unreconciled_get,
                            rpc:call(Node1, kai_stat, all, [])),
    ?assertEqual("1(1)",
                 lists:sublist(lists:flatten(StatOfUnreconciledGet), 4)),

    ok.

put_concurrent_data() -> [].
put_concurrent_data(Config) ->
    Key = "key2",
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),
    ok = rpc:call(Node1, kai_coordinator, route, [{put, #data{key=Key, value="value1"}}]),
    [Data] = rpc:call(Node1, kai_coordinator, route, [{get, #data{key=Key}}]),
    IntentionalConcurrentVCAtNode1 = vclock:increment(rpc:call(Node1, kai_config, get, [node]),
                                               Data#data.vector_clocks),
    IntentionalConcurrentVCAtNode2 = vclock:increment(rpc:call(Node2, kai_config, get, [node]),
                                               Data#data.vector_clocks),
    ok = rpc:call(Node1, kai_store, put, [Data#data{vector_clocks=IntentionalConcurrentVCAtNode1}]),
    ok = rpc:call(Node2, kai_store, put, [Data#data{vector_clocks=IntentionalConcurrentVCAtNode2}]),
    {error, Reason} = rpc:call(Node1,
                               kai_coordinator, route,
                               [{put, #data{key=Key,
                                            flags = "0",
                                            value = (<<"value-1">>)}
                                }]),
    ?assertEqual(ebusy, Reason),
    p("put error reason:", Reason),
    ok.

put_and_overwrite_stale_data() -> [].
put_and_overwrite_stale_data(Config) ->
    Key = "key3",
    Node1 = ?config(node1, Config),
    _Node2 = ?config(node2, Config),
    ok = rpc:call(Node1, kai_coordinator, route, [{put, #data{key=Key, value="value1"}}]),
    [Data] = rpc:call(Node1, kai_coordinator, route, [{get, #data{key=Key}}]),
    IntentionalAhreadVCAtNode1 = vclock:increment(rpc:call(Node1, kai_config, get, [node]),
                                               Data#data.vector_clocks),
    ok = rpc:call(Node1, kai_store, put, [Data#data{vector_clocks=IntentionalAhreadVCAtNode1}]),
    ok = rpc:call(Node1,
                               kai_coordinator, route,
                               [{put, #data{key=Key,
                                            flags = "0",
                                            value = (<<"value-1">>)}
                                }]),
    ok.

put_and_fail_overwrite_stale_data() -> [].
put_and_fail_overwrite_stale_data(Config) ->
    Key = "key3",
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),
    ok = rpc:call(Node1, kai_coordinator, route, [{put, #data{key=Key, value="value1"}}]),
    [Data] = rpc:call(Node1, kai_coordinator, route, [{get, #data{key=Key}}]),
    IntentionalAhreadVCAtNode1 = vclock:increment(rpc:call(Node1, kai_config, get, [node]),
                                               Data#data.vector_clocks),
    ok = rpc:call(Node1, kai_store, put, [Data#data{vector_clocks=IntentionalAhreadVCAtNode1}]),
    %% FIXME: this rpc:call should succeceed. Correct pattern matching is as below:
    %% ok = rpc:call(Node2,
    {error, Reason} = rpc:call(Node2,
                               kai_coordinator, route,
                               [{put, #data{key=Key,
                                            flags = "0",
                                            value = (<<"value-1">>)}
                                }]),
    ?assertEqual(ebusy, Reason),
    p("put error reason:", Reason),
    ok.

start_kai_node(MemcachePort, N, R, W) ->
    {ok, Node} = slave:start(net_adm:localhost(),
                             list_to_atom("kai_test_" ++ integer_to_list(MemcachePort)),
                             "-pa ../../../ebin"),
    ok = rpc:call(Node, application, load, [kai]),
    p("kai loaded at", Node),

    lists:foreach(fun({Par, Value}) ->
                          rpc:call(Node, application, set_env, [kai, Par, Value])
                  end, [{logfile, "kai_test.log"},
                        {hostname, "127.0.0.1"},
                        {rpc_port, MemcachePort - 200},
                        {rpc_max_processes, 30},
                        {memcache_port, MemcachePort},
                        {memcache_max_processes, 30},
                        {max_connections, 32},
                        {n, N},
                        {r, R},
                        {w, W},
                        {number_of_buckets, 1024},
                        {number_of_virtual_nodes, 128}
                       ]),

    ok = rpc:call(Node, application, start, [kai]),
    p("kai started at", Node),
    Node.

p(Label, Message) ->
    ct:pal(default, "~p: ~p~n", [Label, Message]).
