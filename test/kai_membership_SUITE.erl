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

-module(kai_membership_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test1].

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
        {tcp, ApiSocket, Bin} ->
            test1_api_send(ApiSocket, binary_to_term(Bin)),
            test1_api_proc(ApiSocket)
    end.

test1_api_send(ApiSocket, node_info) ->
    NodeInfo = {node_info, ?NODE2, ?INFO},
    gen_tcp:send(ApiSocket, term_to_binary(NodeInfo));
test1_api_send(ApiSocket, node_list) ->
    NodeList = [?NODE1, ?NODE2, ?NODE3, ?NODE4],
    gen_tcp:send(ApiSocket, term_to_binary({node_list, NodeList}));
test1_api_send(ApiSocket, {list, 3 = _Bucket}) ->
    ListOfData = [#data{
        key           = ("item-1"),
        bucket        = 3,
        last_modified = now(),
        checksum      = erlang:md5(<<"item-1">>)
    }],
    gen_tcp:send(ApiSocket, term_to_binary({list_of_data, ListOfData}));
test1_api_send(ApiSocket, {list, _Bucket}) ->
    gen_tcp:send(ApiSocket, term_to_binary({list_of_data, []}));
test1_api_send(ApiSocket, {get, #data{key="item-1", bucket=3}}) ->
    Data = #data{
        key           = "item-1",
        bucket        = 3,
        last_modified = now(),
        checksum      = erlang:md5(<<"value-1">>),
        flags         = "0",
        value         = (<<"value-1">>)
    },
    gen_tcp:send(ApiSocket, term_to_binary(Data)).

test1() -> [].
test1(_Conf) ->
    % This is NODE3, not NODE1
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11013},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2},
        {store, ets}
    ]),
    kai_hash:start_link(),
    kai_store:start_link(),
    kai_connection:start_link(),
    kai_sync:start_link(),
    kai_membership:start_link(),

    {replaced_buckets, _ReplacedBuckets}
        = kai_hash:update_nodes([{?NODE1, ?INFO}, {?NODE4, ?INFO}], []),

    Data1 = #data{
        key           = ("item-1"),
        bucket        = 3,
        last_modified = now(),
        checksum      = erlang:md5(<<"value-1">>), flags="0", value=(<<"value-1">>)
    },
    kai_store:put(Data1),

    spawn_link(?MODULE, test1_api_start, []),

    kai_membership:check_node(?NODE2),

    timer:sleep(100),

    {node_list, NodeList} = kai_hash:node_list(),
    ?assertEqual(4, length(NodeList)),
    ?assert(lists:member(?NODE2, NodeList)),

    {list_of_data, ListOfData} = kai_store:list(3),
    ?assertEqual(0, length(ListOfData)),

    kai_membership:check_node(?NODE1),

    timer:sleep(100),

    {node_list, NodeList2} = kai_hash:node_list(),
    ?assertEqual(3, length(NodeList2)),
    ?assertNot(lists:member(?NODE1, NodeList2)),

    {list_of_data, ListOfData2} = kai_store:list(3),
    ?assertEqual(1, length(ListOfData2)),
    ?assert(lists:keymember("item-1", 2, ListOfData2)),

    timer:sleep(2100), % timeout and check ?NODE4 by kai_hash:choose_node_randomly/0

    {node_list, NodeList3} = kai_hash:node_list(),
    ?assertEqual(2, length(NodeList3)),
    ?assertNot(lists:member(?NODE4, NodeList3)),

    kai_membership:stop(),
    kai_sync:stop(),
    kai_connection:stop(),
    kai_store:stop(),
    kai_hash:stop(),
    kai_config:stop().
