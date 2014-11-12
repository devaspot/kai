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

-module(kai_hash).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([
    update_nodes/2, find_bucket/1, find_replica/1, find_nodes/1,
    choose_node_randomly/0, choose_bucket_randomly/0,
    node_info/1, node_info/0, node_list/0, virtual_node_list/0, 
    bucket_list/0, buckets/0
]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("kai.hrl").

-define(SERVER, ?MODULE).
-define(HASH_LEN, 32).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], _Opts = []).

init(_Args) ->
    ets:new(node_list, [set, private, named_table]),
    ets:new(virtual_node_list, [ordered_set, private, named_table]),
    ets:new(buckets, [set, private, named_table]),

    {node_info, LocalNode, Info} = kai_config:node_info(),
    update_nodes([{LocalNode, Info}], [], _State = []),

    {ok, _State = []}.

terminate(_Reason, _State) ->
    ets:delete(node_list),
    ets:delete(virtual_node_list),
    ets:delete(buckets),
    ok.

hash(Key) ->
    <<HashedKey:?HASH_LEN/integer, _/binary>> = erlang:md5(Key),
    HashedKey.
hash({{N1,N2,N3,N4}, Port}, VirtualNode) ->
    <<HashedKey:?HASH_LEN/integer, _/binary>> =
        erlang:md5(<<N1,N2,N3,N4,Port:16,VirtualNode:16>>),
    HashedKey.

bucket_range(NumberOfBuckets) ->
    trunc( math:pow(2, ?HASH_LEN) / NumberOfBuckets ).

search_bucket_nodes(_HashedKey, _N, 0, Nodes) ->
    {nodes, lists:reverse(Nodes)};
search_bucket_nodes(HashedKey, N, I, Nodes) ->
    HashedNode =
        case ets:next(virtual_node_list, HashedKey) of
            '$end_of_table' -> ets:first(virtual_node_list);
            Other           -> Other
        end,
    [{_HashedNode, Node}|_] = ets:lookup(virtual_node_list, HashedNode),
    Nodes2 =
        case lists:member(Node, Nodes) of
            true -> Nodes;
            _    -> [Node|Nodes]
        end,
    case length(Nodes2) of
        N -> {nodes, lists:reverse(Nodes2)};
        _ -> search_bucket_nodes(HashedNode, N, I-1, Nodes2)
    end.

lists_index(_Elem, [], _I) ->
    undefined;
lists_index(Elem, [Head|Rest], I) ->
    case Elem =:= Head of
        true -> I;
        _    -> lists_index(Elem, Rest, I+1)
    end.
lists_index(Elem, List) ->
    lists_index(Elem, List, 1).

update_buckets(-1 = _Bucket, _LocalNode, _BucketRange, _N, _MaxSearch,
               ReplacedBuckets) ->
    {replaced_buckets, ReplacedBuckets};
update_buckets(Bucket, LocalNode, BucketRange, N, MaxSearch,
               ReplacedBuckets) ->
    {nodes, NewNodes} =
        search_bucket_nodes(Bucket * BucketRange, N, MaxSearch, []),
    case ets:lookup(buckets, Bucket) of
        [{Bucket, NewNodes}] ->
            update_buckets(Bucket-1, LocalNode, BucketRange, N, MaxSearch,
                           ReplacedBuckets);
        OldBucket ->
            ets:insert(buckets, {Bucket, NewNodes}),
            NewReplica = lists_index(LocalNode, NewNodes),
            OldReplica = 
                case OldBucket of
                    [{Bucket, OldNodes}] -> lists_index(LocalNode, OldNodes);
                    []                   -> undefined
                end,
            ReplacedBuckets2 =
                case {NewReplica, OldReplica} of
                    {Replica, Replica} ->
                        ReplacedBuckets;
                    _ ->
                        [{Bucket, NewReplica, OldReplica}|ReplacedBuckets]
                end,
            update_buckets(Bucket-1, LocalNode, BucketRange, N, MaxSearch,
                           ReplacedBuckets2)
    end.

update_buckets() ->
    [LocalNode, N, NumberOfBuckets] =
        kai_config:get([node, n, number_of_buckets]),
    BucketRange = bucket_range(NumberOfBuckets),
    NumberOfNodes = proplists:get_value(size, ets:info(node_list)),

    % Don't search other nodes to fill a bucket when NumberOfNodes is 1, since
    % they are never found.
    MaxSearch =
        case NumberOfNodes of
            1 -> 1;
            _ -> proplists:get_value(size, ets:info(virtual_node_list))
        end,

    update_buckets(NumberOfBuckets-1, LocalNode, BucketRange, N, MaxSearch, []).

add_nodes([]) ->
    ok;
add_nodes([{Node, Info}|Rest]) ->
    case ets:lookup(node_list, Node) of
        [{Node, _Info}|_] -> ok;
        [] ->
            ets:insert(node_list, {Node, Info}),
            NumberOfVirtualNodes =
                proplists:get_value(number_of_virtual_nodes, Info),
            lists:foreach(
              fun(VirtualNode) ->
                      HashedKey = hash(Node, VirtualNode),
                      ets:insert(virtual_node_list, {HashedKey, Node})
              end,
              lists:seq(1, NumberOfVirtualNodes)
             )
    end,
    add_nodes(Rest).

remove_nodes([]) ->
    ok;
remove_nodes([Node|Rest]) ->
    case ets:lookup(node_list, Node) of
        [{Node, Info}|_] ->
            ets:delete(node_list, Node),
            NumberOfVirtualNodes =
                proplists:get_value(number_of_virtual_nodes, Info),
            lists:foreach(
              fun(VirtualNode) ->
                      HashedKey = hash(Node, VirtualNode),
                      ets:delete(virtual_node_list, HashedKey)
              end,
              lists:seq(1, NumberOfVirtualNodes)
             );
        [] -> ok
    end,
    remove_nodes(Rest).

update_nodes(NodesToAdd, NodesToRemove, State) ->
    LocalNode = kai_config:get(node),
    Reply =
        case {NodesToAdd, NodesToRemove -- [LocalNode]} of
            {[], []} ->
                {replaced_buckets, []};
            _ ->
                ?info({update, NodesToAdd, NodesToRemove}),
                add_nodes(NodesToAdd),
                remove_nodes(NodesToRemove),
                update_buckets()
        end,
    {reply, Reply, State}.

do_find_bucket(Bucket, NumberOfBuckets) when is_integer(Bucket) ->
    Bucket rem NumberOfBuckets;
do_find_bucket(Key, NumberOfBuckets) ->
    hash(Key) div bucket_range(NumberOfBuckets).

find_bucket(KeyOrBucket, State) ->
    NumberOfBuckets = kai_config:get(number_of_buckets),
    {reply, {bucket, do_find_bucket(KeyOrBucket, NumberOfBuckets)}, State}.

find_replica(KeyOrBucket, State) ->
    LocalNode = kai_config:get(node),
    {reply, {nodes, Nodes}, State2} = find_nodes(KeyOrBucket, State),
    Replica = lists_index(LocalNode, Nodes),
    {reply, {replica, Replica}, State2}.

find_nodes(KeyOrBucket, State) ->
    NumberOfBuckets = kai_config:get(number_of_buckets),
    Bucket = do_find_bucket(KeyOrBucket, NumberOfBuckets),
    [{Bucket, Nodes}|_] = ets:lookup(buckets, Bucket),
    {reply, {nodes, Nodes}, State}.

choose_node_randomly(State) ->
    {{N1,N2,N3,N4}, Port} = kai_config:get(node),
    Head = {'$1', '_'},
    Cond = [{'=/=', '$1', {{{{N1,N2,N3,N4}}, Port}}}], % double tuple paranthesis
    Body = ['$1'],
    Nodes = ets:select(node_list, [{Head, Cond, Body}]),
    Len = length(Nodes),
    case Len of
        0 -> {reply, undefined, State};
        _ -> {reply, {node, lists:nth(random:uniform(Len), Nodes)}, State}
    end.

inversed_buckets(_Node, -1 = _Bucket, Buckets) ->
    Buckets;
inversed_buckets(Node, Bucket, Buckets) ->
    [{Bucket, Nodes}|_] = ets:lookup(buckets, Bucket),
    case lists:member(Node, Nodes) of
        true -> inversed_buckets(Node, Bucket-1, [Bucket|Buckets]);
        _    -> inversed_buckets(Node, Bucket-1, Buckets)
    end.

inversed_buckets(Node) ->
    NumberOfBuckets = kai_config:get(number_of_buckets),
    inversed_buckets(Node, NumberOfBuckets-1, []).

choose_bucket_randomly(State) ->
    LocalNode = kai_config:get(node),
    Buckets = inversed_buckets(LocalNode),
    Len = length(Buckets),
    case Len of
        0 -> {reply, undefined, State};
        _ -> {reply, {bucket, lists:nth(random:uniform(Len), Buckets)}, State}
    end.

do_node_info(Node, State) ->
    Head = {Node, '$2'},
    Cond = [],
    Body = ['$2'],
    [Info] = ets:select(node_list, [{Head, Cond, Body}]),
    {reply, {node_info, Node, Info}, State}.

do_node_info(State) ->
    LocalNode = kai_config:get(node),
    do_node_info(LocalNode, State).

node_list(State) ->
    NodeList = ets:tab2list(node_list),
    NodeList2 = lists:map(fun({Node, _Info}) -> Node end, NodeList),
    {reply, {node_list, NodeList2}, State}.

virtual_node_list(State) ->
    VirtualNodeList = ets:tab2list(virtual_node_list),
    {reply, {virtual_node_list, VirtualNodeList}, State}.

bucket_list(State) ->
    Buckets = ets:tab2list(buckets),
    {reply, {bucket_list, lists:sort(Buckets)}, State}.

buckets(State) ->
    LocalNode = kai_config:get(node),
    Buckets =
        lists:filter(
          fun(B) -> lists:member(LocalNode, element(2, B)) end,
          ets:tab2list(buckets)
         ),
    Buckets2 = [element(1, B) || B <- Buckets],
    {reply, {buckets, lists:sort(Buckets2)}, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({update_nodes, NodesToAdd, NodesToRemove}, _From, State) ->
    update_nodes(NodesToAdd, NodesToRemove, State);
handle_call({find_bucket, KeyOrBucket}, _From, State) ->
    find_bucket(KeyOrBucket, State);
handle_call({find_replica, KeyOrBucket}, _From, State) ->
    find_replica(KeyOrBucket, State);
handle_call({find_nodes, KeyOrBucket}, _From, State) ->
    find_nodes(KeyOrBucket, State);
handle_call(choose_node_randomly, _From, State) ->
    choose_node_randomly(State);
handle_call(choose_bucket_randomly, _From, State) ->
    choose_bucket_randomly(State);
handle_call({node_info, Node}, _From, State) ->
    do_node_info(Node, State);
handle_call(node_info, _From, State) ->
    do_node_info(State);
handle_call(node_list, _From, State) ->
    node_list(State);
handle_call(virtual_node_list, _From, State) ->
    virtual_node_list(State);
handle_call(bucket_list, _From, State) ->
    bucket_list(State);
handle_call(buckets, _From, State) ->
    buckets(State).
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
update_nodes(NodesToAdd, NodesToRemove) ->
    gen_server:call(?SERVER, {update_nodes, NodesToAdd, NodesToRemove}).
find_bucket(KeyOrBucket) ->
    gen_server:call(?SERVER, {find_bucket, KeyOrBucket}).
find_replica(KeyOrBucket) ->
    gen_server:call(?SERVER, {find_replica, KeyOrBucket}).
find_nodes(KeyOrBucket) ->
    gen_server:call(?SERVER, {find_nodes, KeyOrBucket}).
choose_node_randomly() ->
    gen_server:call(?SERVER, choose_node_randomly).
choose_bucket_randomly() ->
    gen_server:call(?SERVER, choose_bucket_randomly).
node_info() ->
    gen_server:call(?SERVER, node_info).
node_info(Node) ->
    gen_server:call(?SERVER, {node_info, Node}).
node_list() ->
    gen_server:call(?SERVER, node_list).
virtual_node_list() ->
    gen_server:call(?SERVER, virtual_node_list).
bucket_list() ->
    gen_server:call(?SERVER, bucket_list).
buckets() ->
    gen_server:call(?SERVER, buckets).
