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

-module(kai_store_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test_ets, test_dets,
          test_update_conflict_ets, test_update_conflict_dets,
          test_perf].

test(Conf) ->
    kai_config:start_link(Conf),
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11511},
        {rpc_max_processes, 2},
        {max_connections, 32},
        {n, 1}, {r, 1}, {w, 1},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2}
    ]),
    kai_version:start_link(),
    kai_store:start_link(),

    Data1 = #data{
        key           = "item-1",
        bucket        = 3,
        last_modified = now(),
        checksum      = erlang:md5(<<"value-1">>),
        flags         = "0",
        vector_clocks = vclock:fresh(),
        value         = (<<"value-1">>)
    },

    kai_store:put(Data1),

    ?assertEqual(
       Data1,
       kai_store:get(#data{key="item-1", bucket=3})
      ),
    ?assertEqual(
       undefined,
       kai_store:get(#data{key="item-2", bucket=1})
      ),

    Data2 = #data{
        key           = "item-2",
        bucket        = 1,
        last_modified = now(),
        checksum      = erlang:md5(<<"value-2">>),
        flags         = "0",
        vector_clocks = [],
        value         = (<<"value-2">>)
    },
    kai_store:put(Data2),
    ?assertEqual(
       Data2,
       kai_store:get(#data{key="item-2", bucket=1})
      ),

    Data3 = #data{
        key           = "item-3",
        bucket        = 3,
        last_modified = now(),
        checksum      = erlang:md5(<<"value-3">>),
        flags         = "0",
        vector_clocks = [],
        value         = (<<"value-3">>)
    },
    kai_store:put(Data3),
    ?assertEqual(
       Data3,
       kai_store:get(#data{key="item-3", bucket=3})
      ),

    {list_of_data, ListOfData1} = kai_store:list(1),
    ?assertEqual(1, length(ListOfData1)),
    ?assert(lists:keymember("item-2", 2, ListOfData1)),

    {list_of_data, ListOfData2} = kai_store:list(2),
    ?assertEqual(0, length(ListOfData2)),

    {list_of_data, ListOfData3} = kai_store:list(3),
    ?assertEqual(2, length(ListOfData3)),
    ?assert(lists:keymember("item-1", 2, ListOfData3)),
    ?assert(lists:keymember("item-3", 2, ListOfData3)),

    Data1b = #data{
        key           = "item-1",
        bucket        = 3,
        last_modified = now(),
        checksum      = erlang:md5(<<"value-1">>),
        flags         = "0",
        vector_clocks = [],
        value         = (<<"value-1b">>)
    },

    kai_store:put(Data1b),
    ?assertEqual(
       Data1b,
       kai_store:get(#data{key="item-1", bucket=3})
      ),

    kai_store:delete(#data{key="item-1", bucket=3}),

    ?assertEqual(
       undefined,
       kai_store:get(#data{key="item-1", bucket=3})
      ),

    {list_of_data, ListOfData4} = kai_store:list(3),
    ?assertEqual(1, length(ListOfData4)),
    ?assert(lists:keymember("item-3", 2, ListOfData4)),

    ?assert(is_integer(kai_store:info(bytes))),
    ?assertEqual(2, kai_store:info(size)),

    kai_store:stop(),
    kai_version:stop(),
    kai_config:stop(),

    ok.

test_ets() -> [].
test_ets(_Conf) ->
    test([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, ets}
    ]).

test_dets() -> [].
test_dets(_Conf) ->
    test([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, dets},
        {dets_dir, "."},
        {number_of_tables, 2}
    ]),
    file:delete("./1"), file:delete("./2").

test_update_conflict(Conf) ->
    kai_config:start_link(Conf),
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11511},
        {rpc_max_processes, 2},
        {max_connections, 32},
        {n, 1}, {r, 1}, {w, 1},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2}
    ]),
    kai_version:start_link(),
    kai_store:start_link(),

    Data1 = #data{
      key           = "item-1",
      bucket        = 3,
      last_modified = now(),
      checksum      = erlang:md5(<<"value-1">>),
      flags         = "0",
      vector_clocks = vclock:increment(node1, vclock:fresh()),
      value         = (<<"value-1">>)
     },
    kai_store:put(Data1),

    %% conflict vclock
    Data2 = Data1#data{
              vector_clocks = vclock:increment(node2, vclock:fresh())
             },

    {error, Reason} = kai_store:put(Data2),
    ct:pal(default, "~p: ~p~n", ["Reason", Reason]),

    kai_store:stop(),
    kai_version:stop(),
    kai_config:stop(),

    ok.

test_update_conflict_ets() -> [].
test_update_conflict_ets(_Conf) ->
    test_update_conflict([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, ets}
    ]).

test_update_conflict_dets() -> [].
test_update_conflict_dets(_Conf) ->
    test_update_conflict([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, dets},
        {dets_dir, "."},
        {number_of_tables, 2}
    ]),
    file:delete("./1"), file:delete("./2").

test_perf_put(T) ->
    lists:foreach(
      fun(I) ->
          Key = "item-" ++ integer_to_list(I),
          Value = list_to_binary("value-" ++ integer_to_list(I)),
          Data = #data{
              key           = Key,
              bucket        = 3,
              last_modified = now(),
              checksum      = erlang:md5(Value),
              flags         = "0",
              vector_clocks = [],
              value         = Value
          },
          kai_store:put(Data)
      end,
      lists:seq(1, T)
     ).

test_perf_get(T) ->
    lists:foreach(
      fun(I) ->
          Key = "item-" ++ integer_to_list(I),
          kai_store:get(#data{key=Key, bucket=0})
      end,
      lists:seq(1, T)
     ).

test_perf() -> [].
test_perf(_Conf) ->
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, ets}
    ]),
    kai_store:start_link(),

    T = 10000,

    {Usec, _} = timer:tc(?MODULE, test_perf_put, [T]),
    ?assert(Usec < 100*T),
    io:format("average time to put data: ~p [usec]", [Usec/T]),

    {Usec2, _} = timer:tc(?MODULE, test_perf_get, [T]),
    ?assert(Usec2 < 100*T),
    io:format("average time to get data: ~p [usec]", [Usec2/T]),
    kai_store:stop(),
    kai_config:stop().

p(Label, Message) ->
    ct:pal(default, "~p: ~p~n", [Label, Message]).
