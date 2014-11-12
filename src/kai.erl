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

-module(kai).
-behaviour(application).

-export([start/2, stop/1]).
-export([start/0]).

config([], Acc) ->
    Acc;
config([Key|Rest], Acc) ->
    case application:get_env(kai, Key) of
        undefined   -> config(Rest, Acc);
        {ok, Value} -> config(Rest, [{Key, Value}|Acc])
    end.

start(_Type, _Args) ->
    filelib:ensure_dir("kai/"),
    Args = config([
        logfile, hostname,
        rpc_port, rpc_max_processes,
        memcache_port, memcache_max_processes,
        max_connections,
        n, r, w,
        number_of_buckets, number_of_virtual_nodes,
        store, dets_dir, number_of_tables
    ], []),
    kai_sup:start_link(Args).

stop(_State) ->
    ok.

start() ->
    application:start(kai).
