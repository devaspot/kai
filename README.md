KAI
===

Kai is a distributed key-value datastore, which is mainly inspired
by Amazon's Dynamo. It brings high scalability and availability
to your Web sites. You can manage variety of contents with Kai,
as if Amazon's Dynamo stores shopping carts, catalogs, session states,
and so forth. Currently, Kai is deployed in a commercial service,
http://home.goo.ne.jp, which is a Japanese social networking service of more
than 10 million users.

Features
--------

* 16 nodes
* 2 GB/node, on memory
* 1,000 qps for whole system
* < 300ms for requests of 99%
* Fairly balancing loads
* Consitent hashing, nodes, buckets
* Secondary Indexes
* Storage Backends: ETS, DETS
* Quorum coordinator
* Sync data one by another
* Gossip-based protocol membership
* Memcache Interface

Goal
------

* 256 nodes
* 32 GB/node
* 100,000 qps for whole system
* Physical placement
* Merkle Tree sync
* Chord or Kademlia membeship

Standalone Mode
---------------

For practice, we begin with a stand-alone server, not clustered system.
The stand-alone Kai is not attractive, because it has no reliability
(no replication). The behavior is, however, quite similar with that
of well-known memcached, and so this can be a good starting point.
Before running Kai, stop a memcached if runnnig, because Kai uses
the same port number (11211) by default. Set parameters "n", "r",
and "w" to 1 in the configuration file "sys.config"; other parameters
remain in default (Details for the parameters are described in Configuration).

    [{kai, [
        {logfile, "kai.log"},
        {hostname, "localhost"},
        {rpc_port, 11011},
        {rpc_max_processes, 30},
        {memcache_port, 11211},
        {memcache_max_processes, 10},
        {max_connections, 32},
        {n, 1},
        {r, 1},
        {w, 1},
        {number_of_buckets, 1024},
        {number_of_virtual_nodes, 128},
        {store, ets},
        {dets_dir, "/path/to/dir"},
        {number_of_tables, 256}]}].

Cluster Mode
------------

This section describes how to run multiple Kai nodes as a clustered system.
While we will run all nodes on a single physical machine for convenience,
the cluster can be consisted of different machines just by changing IP
addresses and port numbers in the following example. In this example,
we run four distinct nodes on a single physical machine. Copy the
configuration file for them. They are named "kai1.config" to "kai4.config";
a node configured by kai1.config is called Node1, hereafter.
To replicate data, set the quorum parameter "n" to the degree of replication.
In this example, we set "n" to 3 in the configuration file "kai.config".
Other quorum parameters "r" and "w" is set in according to the quorum conditions.


| Name | Value |
|------|-------|
| n    |     3 |
| r    |     2 |
| w    |     2 |

Hosts config:

| Node1         | Value |
|---------------|-------|
| rpc_port      | 11011 |
| memcache_port | 11211 |

| Node2         | Value |
|---------------|-------|
| rpc_port      | 11012 |
| memcache_port | 11212 |

| Node3         | Value |
|---------------|-------|
| rpc_port      | 11013 |
| memcache_port | 11213 |

| Node4         | Value |
|---------------|-------|
| rpc_port      | 11014 |
| memcache_port | 11214 |

Staring Node1:

    % erl -pa ebin -config kai1
    > application:start(kai).

To make a cluster, start Node2 and add it to the cluster (that includes only
Node1 currently) by informing of their neighbors with function "kai_rpc:check_node/2".

    % erl -pa ebin -config kai2
    > application:start(kai).
    > kai_rpc:check_node({{127,0,0,1}, 11012}, {{127,0,0,1}, 11011}).
    > kai_rpc:check_node({{127,0,0,1}, 11011}, {{127,0,0,1}, 11012}).

We give a brief explanation this process. A Kai node maintains a node list
including members of the cluster it belongs to. In the initial state, each
node list includes only the node itself. By issuing function "kai_rpc:check_node/2",
a node of the first argument retrieves a node list from another node of
the second argument, and merge the list with that of itself. By calling
this function twice, a new node can be added to the cluster; inform a
new node of any cluster node by the first call, and vice versa (wait
for finishing data synchronization if some data have already been
stored in the cluster).

In this example, Node2 (127.0.0.1:11012) gets a node list of
Node1 (127.0.0.1:11011) by the first "kai_rpc:check_node/2"; Node2 
knows Node1. The second RPC informs Node1 of Node2.

Here, make sure Node1 and Node2 know each other.

    > kai_rpc:node_list({{127,0,0,1}, 11011}).
    {node_list,[{{127,0,0,1},11011},
            {{127,0,0,1},11012}]}

    > kai_rpc:node_list({{127,0,0,1}, 11012}).
    {node_list,[{{127,0,0,1},11011},
            {{127,0,0,1},11012}]}

In the same manner, add Node3 to the cluster.

    % erl -pa ebin -config kai3
    > application:start(kai).
    > kai_rpc:check_node({{127,0,0,1}, 11013}, {{127,0,0,1}, 11011}).
    > kai_rpc:check_node({{127,0,0,1}, 11011}, {{127,0,0,1}, 11013}).

By the third "kai_rpc:check_node/2", Node3 gets a node list of Node1,
which includes Node2 as well as Node1. The fourth "kai_rpc:check_node/2"
informs Node1 of Node3. Now, while Node1 and Node3 know all the three
nodes, Node2 does not know Node3 yet. Node2, however, gets to know
Node3 eventually, because each node periodically exchanges its node
list to a node randomly chosen among the node list. Every node,
finally, knows each other, and makes the cluster of the three nodes.
Finally, add Node4 to the cluster.

    % erl -pa ebin -config kai4
    > application:start(kai).
    > kai_rpc:check_node({{127,0,0,1}, 11014}, {{127,0,0,1}, 11011}).
    > kai_rpc:check_node({{127,0,0,1}, 11011}, {{127,0,0,1}, 11014}).

We make sure whether each node knows all by function "kai_rpc:node_list/1".
If the cluster is correctly made up, the output includes the four nodes like this.

    > kai_rpc:node_list({{127,0,0,1}, 11011}).
    {node_list,[{{127,0,0,1},11011},
            {{127,0,0,1},11012},
            {{127,0,0,1},11013},
            {{127,0,0,1},11014}]}

To remove a node, no procedure is needed. This implies that no
action is needed when a node is accidentally going down. However,
it's better to remove a node one by one; don't remove more than or
equal to N nodes before finishing data synchronization.

Statistics
----------

Statistics for the Kai node can be retrieved by issuing stats
command of memcache APIs. The parameters without "kai_" or "erlang_" prefix
are in common with memcache APIs. Some of Kai related parameters are
explained in Configuration in detail.

| Name             |  Description                                                            |
|------------------|-------------------------------------------------------------------------|
| uptime           | Number of seconds this server has been running. |
| time             | Current UNIX time according to the server. |
| version          | Version string of the Kai node. |
| bytes            | Current number of bytes used by the Kai node to store items. |
| curr_items       | Current number of items stored by the Kai node. |
| curr_connections | Number of open connections. |
| cmd_get          | Successful retrieval requests processed by the Kai node. |
| cmd_set          | Successful storage requests processed by the Kai node. |
| bytes_read       | Total number of bytes transferred from the Kai node. |
| bytes_write      | Total number of bytes transferred to the Kai node. |
| kai_node         | Socket address for internal RPC in the Kai node. |
| kai_quorum       | Parameters for the quorum protocol; i.e. N:R:W. |
| kai_buckets      | Number of buckets in the Kai cluster. |
| kai_vnodes       | Number of virtual nodes in the Kai node.  |
| kai_store        | Storage type used by the Kai node; ets or dets.  |
| kai_curr_nodes   | Membership in the current cluster. |
| kai_unreconciled | Cumulative number of retrieval requests with confliction. |
| erlang_procs     | Number of Erlang processes. |
| erlang_version   | Version string of Erlang. |

Some statistics (bytes, curr_items, cmd_get, cmd_set, bytes_read, and bytes_write) can be
drawn by Cacti, which is a complete network graphing solution designed to harness the power
of RRDTool's data storage and graphing functionality.

Credits
-------

* Takeshi Inoue
* Maxim Sokhatsky

OM A HUM
