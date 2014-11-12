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

-module(kai_hash_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test_hash]. % test_hash_efficiency

% Hash ring of test1:
%
%             0  bucket-0   [3,2,1] -> [3,1,4]
%    98,830,170   item-4
%
%   536,870,912  bucket-1   [3,2,1] -> [3,1,4]
%   953,414,533   item-2
%
% 1,073,741,824  bucket-2   [3,2,1] -> [3,1,4]
% 1,258,937,939   NODE3(2)
%
% 1,610,612,736  bucket-3   [2,1,4] -> [1,4,3]
% 1,704,004,111   item-3
% 1,981,805,867   item-1
%
% 2,147,483,648  bucket-4   [2,1,4] -> [1,4,3]
% 2,203,089,259   NODE2(1)
% 2,311,136,591   NODE1(2)
% 2,365,722,681   NODE4(2)
%
% 2,684,354,560  bucket-5   [2,4,1] -> [4,1,3]
% 2,772,605,746   NODE2(2)
% 2,978,498,268   NODE4(1)
%
% 3,221,225,472  bucket-6   [1,3,2] -> [1,3,4]
% 3,495,790,055   NODE1(1)
%
% 3,758,096,384  bucket-7   [3,2,1] -> [3,1,4]
% 4,264,647,116   NODE3(1)

test_hash() -> [].
test_hash(_Conf) ->
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 3},
        {number_of_buckets, 8},
        {number_of_virtual_nodes, 2}
    ]),
    kai_hash:start_link(),

    {node_info, ?NODE1, Info1} = kai_hash:node_info(),
    ?assertEqual(?INFO, Info1),
    {node_info, ?NODE1, Info1} = kai_hash:node_info(?NODE1),
    ?assertEqual(?INFO, Info1),

    {node_list, NodeList1} = kai_hash:node_list(),
    ?assertEqual([?NODE1], NodeList1),

    {virtual_node_list, VirtualNodeList1} = kai_hash:virtual_node_list(),
    ?assertEqual(
       [{2311136591, ?NODE1},
        {3495790055, ?NODE1}],
       VirtualNodeList1
      ),

    {bucket_list, BucketList1} = kai_hash:bucket_list(),
    ?assertEqual(
       [{0, [?NODE1]},
        {1, [?NODE1]},
        {2, [?NODE1]},
        {3, [?NODE1]},
        {4, [?NODE1]},
        {5, [?NODE1]},
        {6, [?NODE1]},
        {7, [?NODE1]}],
       BucketList1
      ),

    {buckets, Buckets1} = kai_hash:buckets(),
    ?assertEqual([0,1,2,3,4,5,6,7], Buckets1),

    {replaced_buckets, ReplacedBuckets2} =
        kai_hash:update_nodes([{?NODE2, ?INFO}, {?NODE3, ?INFO}, {?NODE4, ?INFO}],
                              []),
    
    ?assertEqual(
       [{0,3,1}, {1,3,1}, {2,3,1}, {3,2,1}, {4,2,1}, {5,3,1}, {7,3,1}],
       ReplacedBuckets2
      ),

    {node_list, NodeList2} = kai_hash:node_list(),
    ?assertEqual(4, length(NodeList2)),
    ?assert(lists:member(?NODE2, NodeList2)),
    ?assert(lists:member(?NODE3, NodeList2)),
    ?assert(lists:member(?NODE4, NodeList2)),

    {virtual_node_list, VirtualNodeList2} = kai_hash:virtual_node_list(),
    ?assertEqual(
       [{1258937939, ?NODE3},
        {2203089259, ?NODE2},
        {2311136591, ?NODE1},
        {2365722681, ?NODE4},
        {2772605746, ?NODE2},
        {2978498268, ?NODE4},
        {3495790055, ?NODE1},
        {4264647116, ?NODE3}],
       VirtualNodeList2
      ),

    {bucket_list, BucketList2} = kai_hash:bucket_list(),
    ?assertEqual(
       [{0, [?NODE3, ?NODE2, ?NODE1]},
        {1, [?NODE3, ?NODE2, ?NODE1]},
        {2, [?NODE3, ?NODE2, ?NODE1]},
        {3, [?NODE2, ?NODE1, ?NODE4]},
        {4, [?NODE2, ?NODE1, ?NODE4]},
        {5, [?NODE2, ?NODE4, ?NODE1]},
        {6, [?NODE1, ?NODE3, ?NODE2]},
        {7, [?NODE3, ?NODE2, ?NODE1]}],
       BucketList2
      ),

    {buckets, Buckets2} = kai_hash:buckets(),
    ?assertEqual([0,1,2,3,4,5,6,7], Buckets2),

    {bucket, Bucket1} = kai_hash:find_bucket("item-1"),
    ?assertEqual(3, Bucket1),

    {replica, Replica1} = kai_hash:find_replica(Bucket1),
    ?assertEqual(2, Replica1),

    {nodes, Nodes1} = kai_hash:find_nodes(Bucket1),
    ?assertEqual([?NODE2, ?NODE1, ?NODE4], Nodes1),

    {nodes, Nodes2} = kai_hash:find_nodes("item-1"),
    ?assertEqual([?NODE2, ?NODE1, ?NODE4], Nodes2),

    {nodes, Nodes3} = kai_hash:find_nodes("item-2"),
    ?assertEqual([?NODE3, ?NODE2, ?NODE1], Nodes3),

    {node, Node1} = kai_hash:choose_node_randomly(),
    ?assertNot(Node1 == ?NODE1),

    {bucket, Bucket2} = kai_hash:choose_bucket_randomly(),
    ?assert((Bucket2 >= 0) or (Bucket2 < 8)), % TODO: choose it from my buckets

    {replaced_buckets, ReplacedBuckets3} = kai_hash:update_nodes([], [?NODE2]),

    ?assertEqual(
       [{0,2,3}, {1,2,3}, {2,2,3}, {3,1,2}, {4,1,2}, {5,2,3}, {7,2,3}],
       ReplacedBuckets3
      ),

    {node_list, NodeList3} = kai_hash:node_list(),
    ?assertEqual(3, length(NodeList3)),
    ?assertNot(lists:member(?NODE2, NodeList3)),

    {virtual_node_list, VirtualNodeList3} = kai_hash:virtual_node_list(),
    ?assertEqual(
       [{1258937939, ?NODE3},
        {2311136591, ?NODE1},
        {2365722681, ?NODE4},
        {2978498268, ?NODE4},
        {3495790055, ?NODE1},
        {4264647116, ?NODE3}],
       VirtualNodeList3
      ),

    {bucket_list, BucketList3} = kai_hash:bucket_list(),
    ?assertEqual(
       [{0, [?NODE3, ?NODE1, ?NODE4]},
        {1, [?NODE3, ?NODE1, ?NODE4]},
        {2, [?NODE3, ?NODE1, ?NODE4]},
        {3, [?NODE1, ?NODE4, ?NODE3]},
        {4, [?NODE1, ?NODE4, ?NODE3]},
        {5, [?NODE4, ?NODE1, ?NODE3]},
        {6, [?NODE1, ?NODE3, ?NODE4]},
        {7, [?NODE3, ?NODE1, ?NODE4]}],
       BucketList3
      ),

    {buckets, Buckets3} = kai_hash:buckets(),
    ?assertEqual([0,1,2,3,4,5,6,7], Buckets3),

    {nodes, Nodes4} = kai_hash:find_nodes("item-1"),
    ?assertEqual([?NODE1, ?NODE4, ?NODE3], Nodes4),

    {nodes, Nodes5} = kai_hash:find_nodes("item-2"),
    ?assertEqual([?NODE3, ?NODE1, ?NODE4], Nodes5),

    kai_hash:stop(),
    kai_config:stop().

test_hash_efficiency() -> [].
test_hash_efficiency(_Conf) ->
    io:format("simulate network of 64 nodes"),

    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 1},
        {n, 3},
        {number_of_buckets, 16384}, % 16,384 = 128*64*2
        {number_of_virtual_nodes, 128}
    ]),
    kai_hash:start_link(),

    Nodes =
    lists:map(
      fun(Port) -> {{{127,0,0,1}, Port}, ?INFO} end,
      lists:seq(2, 63)
     ),
    kai_hash:update_nodes(Nodes, []),

    Args = [[{{{127,0,0,1}, 64}, [{number_of_virtual_nodes, 128}]}], []],
    {Usec, _} = timer:tc(kai_hash, update, Args),
    io:format("time to add a node: ~p [usec]", [Usec]),
    ?assert(Usec < 300000),

    {Usec2, _} = timer:tc(kai_hash, find, ["item-1", 1]),
    io:format("time to find a node: ~p [usec]", [Usec2]),
    ?assert(Usec2 < 1000),

    {Usec3, _} = timer:tc(kai_hash, choose_node_randomly, []),
    io:format("time to choose a node randomly: ~p [usec]", [Usec3]),
    ?assert(Usec3 < 1000),

    {Usec4, _} = timer:tc(kai_hash, choose_bucket_randomly, []),
    io:format("time to choose a bucket randomly: ~p [usec]", [Usec4]),
    ?assert(Usec4 < 300000),

    {Usec5, _} = timer:tc(kai_hash, update, [[], [{{127,0,0,1}, 1}]]),
    io:format("time to remove a node: ~p [usec]", [Usec5]),
    ?assert(Usec5 < 300000),

    kai_hash:stop(),
    kai_config:stop().
