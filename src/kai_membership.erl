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

-module(kai_membership).
-behaviour(gen_fsm).

-export([start_link/0, stop/0]).
-export([check_node/1]).
-export([
    init/1, ready/2, handle_event/3, handle_sync_event/4, handle_info/3,
    terminate/3, code_change/4
]).

-include("kai.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).
    
init(_Args) ->
    {ok, ready, [], ?TIMER}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

ping_nodes([], AvailableNodes, DownNodes) ->
    {AvailableNodes, DownNodes};
ping_nodes([Node|Nodes], AvailableNodes, DownNodes) ->
    case kai_rpc:node_info(Node) of
        {node_info, Node2, Info} ->
            ping_nodes(Nodes, [{Node2, Info}|AvailableNodes], DownNodes);
        {error, Reason} ->
            ?warning(io_lib:format("ping_nodes/3: ~p", [{error, Reason}])),
            ping_nodes(Nodes, AvailableNodes, [Node|DownNodes])
    end.

retrieve_node_list(Node) ->
    case kai_rpc:node_list(Node) of
        {node_list, RemoteNodeList} ->
            {node_list, LocalNodeList} = kai_hash:node_list(),
            NewNodes = RemoteNodeList -- LocalNodeList,
            OldNodes = LocalNodeList  -- RemoteNodeList,
            Nodes = NewNodes ++ OldNodes,
            LocalNode = kai_config:get(node),
            ping_nodes(Nodes -- [LocalNode], [], []);
        {error, Reason} ->
            ?warning(io_lib:format("retrieve_node_list/1: ~p", [{error, Reason}])),
            {[], [Node]}
    end.

sync_buckets([], _LocalNode) ->
    ok;
sync_buckets([{Bucket, NewReplica, OldReplica}|ReplacedBuckets], LocalNode) ->
    case {NewReplica, OldReplica} of
        {NewReplica, undefined } -> kai_sync:update_bucket(Bucket);
        {undefined,  OldReplica} -> kai_sync:delete_bucket(Bucket);
        _                        -> nop
    end,
    sync_buckets(ReplacedBuckets, LocalNode).

sync_buckets(ReplacedBuckets) ->
    LocalNode = kai_config:get(node),
    sync_buckets(ReplacedBuckets, LocalNode).

do_check_node({Address, Port}) ->
    {AvailableNodes, DownNodes} = retrieve_node_list({Address, Port}),
    {replaced_buckets, ReplacedBuckets} =
        kai_hash:update_nodes(AvailableNodes, DownNodes),
    sync_buckets(ReplacedBuckets).

ready({check_node, Node}, State) ->
    do_check_node(Node),
    {next_state, ready, State, ?TIMER};
ready(timeout, State) ->
    case kai_hash:choose_node_randomly() of
        {node, Node} -> do_check_node(Node);
        _            -> nop
    end,
    {next_state, ready, State, ?TIMER}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {next_state, ready, StateData, ?TIMEOUT}.
handle_info(_Info, _StateName, StateData) ->
    {next_state, ready, StateData, ?TIMEOUT}.
code_change(_OldVsn, _StateName, StateData, _Extra) ->
    {ok, ready, StateData}.

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).
check_node(Node) ->
    gen_fsm:send_event(?SERVER, {check_node, Node}).
