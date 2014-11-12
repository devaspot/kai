%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License.  You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(kai_version_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

init_per_testcase(_TestCase, Conf) ->
    kai_config:start_link([
                           {hostname, "localhost"},
                           {rpc_port, 11011},
                           {memcache_port, 11211},
                           {max_connections, 2},
                           {n, 1}, {r, 1}, {w, 1},
                           {number_of_buckets, 8},
                           {number_of_virtual_nodes, 2}
                          ]),
    kai_version:start_link(),
    Conf.

end_per_testcase(_TestCase, _Conf) ->
    kai_config:stop(),
    kai_version:stop(),
    ok.

all() -> [test_update, test_order,
          test_cas_unique1,
          test_cas_unique2,
          test_cas_unique7,
          test_cas_unique16
         ].

test_update() -> [].
test_update(_Conf) ->
    VClock1 = vclock:increment(kai_config:get(node), vclock:fresh()),

    Data1 = #data{
      vector_clocks = VClock1
     },

    {ok, Data2} = kai_version:update(Data1),
    ct:log(test_update, "Data2.vector_clocks: ~p~n", [Data2#data.vector_clocks]),
    ?assert(is_list(Data2#data.vector_clocks)),
    ?assert(vclock:descends(Data2#data.vector_clocks, Data1#data.vector_clocks)),
    ?assertNot(vclock:descends(Data1#data.vector_clocks, Data2#data.vector_clocks)),

    ok.

test_order() -> [].
test_order(_Conf) ->
    VClock1 = vclock:increment(node1, vclock:fresh()),
    Data1 = #data{
      vector_clocks = VClock1
     },

    %% trivial case
    ?assertEqual(1, length(kai_version:order([Data1]))),

    %% two concurrent data
    Data2 = Data1#data{
              vector_clocks = vclock:increment(otherNode, vclock:fresh())
             },
    ListOfData12 = kai_version:order([Data1, Data2]),
    ?assertEqual(2, length(ListOfData12)),

    %% one data is dropped
    Data3 = Data1#data{
              vector_clocks = vclock:increment(otherNode2, Data1#data.vector_clocks)
             },
    ListOfData23 = kai_version:order([Data1, Data2, Data3]),
    ?assertEqual(2, length(ListOfData23)),
    ok.

test_cas_unique1() -> [].
test_cas_unique1(_Conf) ->
    Data1 = #data{
      checksum = list_to_binary(all_bit_on(16))
     },
    {ok, CasUnique} = kai_version:cas_unique([Data1]),
    Expected = list_to_binary([<<1:4, 2#1111:4>>, all_bit_on(7)]),
    ?assertEqual(Expected, CasUnique),
    ok.

test_cas_unique2() -> [].
test_cas_unique2(_Conf) ->
    Data1 = #data{
      checksum = <<16#FFFFFFFFFFFFFFFF:64, 0:64>>
     },
    Data2 = #data{
      checksum = <<0:64, 16#FFFFFFFFFFFFFFFF:64>>
     },
    {ok, CasUnique} = kai_version:cas_unique([Data1, Data2]),
    Expected = <<2:4, 2#1111:4, 16#FFFFFFF:26, 0:30>>,
    ?assertEqual(Expected, CasUnique),
    ok.

test_cas_unique7() -> [].
test_cas_unique7(_Conf) ->
    %% trunc(60/7) = 8
    ListOfData = lists:map(fun (I) ->
                                   #data{checksum = <<I:8, 0:120>>}
                           end,
                           lists:seq(1,7)),
    {ok, CasUnique} = kai_version:cas_unique(ListOfData),
    Expected = <<7:4, 1:8, 2:8, 3:8, 4:8, 5:8, 6:8, 7:8, 0:4>>,
    ?assertEqual(Expected, CasUnique),
    ok.

test_cas_unique16() -> [].
test_cas_unique16(_Conf) ->
    %% 16 exceeds 4bit range (2#1111 = 15)
    ListOfData = lists:map(fun (I) ->
                                   #data{checksum = <<I:4, 0:60>>}
                           end,
                           lists:seq(1,16)),
    {error, Reason} = kai_version:cas_unique(ListOfData),
    ?assert(string:str(Reason, "16") > 0),
    ok.

all_bit_on(Bytes) ->
    lists:duplicate(Bytes, 16#FF).

all_bit_off(Bytes) ->
    lists:duplicate(Bytes, 0).
