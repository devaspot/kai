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

-module(kai_config_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test1].

test1() -> [].
test1(_Conf) ->
    kai_config:start_link([
        {hostname, "localhost"},
        {rpc_port, 11011},
        {n, 2},
        {number_of_buckets, 16384},
        {number_of_virtual_nodes, 128}
    ]),

    ?assertEqual(
       ?NODE1,
       kai_config:get(node)
      ),
    ?assertEqual(
       16384,
       kai_config:get(number_of_buckets)
      ),
    ?assertEqual(
       128,
       kai_config:get(number_of_virtual_nodes)
      ),

    ?assertEqual(
       [?NODE1, 16384, 128],
       kai_config:get([node, number_of_buckets, number_of_virtual_nodes])
      ),

    ?assertEqual(
       {node_info, ?NODE1, [{number_of_virtual_nodes, 128}]},
       kai_config:node_info()
      ),

    kai_config:stop().
