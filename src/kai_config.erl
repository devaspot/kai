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

-module(kai_config).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([get/1, node_info/0]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("kai.hrl").

-define(SERVER, ?MODULE).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, _Opts = []).

init(Args) ->
    ets:new(config, [set, private, named_table]),

    lists:foreach(
      fun({Key, Value}) -> ets:insert(config, {Key, Value}) end,
      Args
     ),

    Hostname =
        case proplists:get_value(hostname, Args) of
            undefined -> {ok, H} = inet:gethostname(), H;
            H         -> H
        end,
    {ok, Address} = inet:getaddr(Hostname, inet),
    Port = proplists:get_value(rpc_port, Args),
    ets:insert(config, {node, {Address, Port}}),

    NumberOfBuckets = proplists:get_value(number_of_buckets, Args),
    Exponent = round( math:log(NumberOfBuckets) / math:log(2) ),
    ets:insert(config, {number_of_buckets, trunc( math:pow(2, Exponent) )}),

    {ok, []}.

terminate(_Reason, _State) ->
    ets:delete(config),
    ok.

do_get(Key) ->
    case ets:lookup(config, Key) of
        [{Key, Value}|_] -> Value;
        _                -> undefined
    end.

do_get([], ListOfValues) ->
    lists:reverse(ListOfValues);
do_get([Key|Rest], ListOfValues) ->
    do_get(Rest, [do_get(Key)|ListOfValues]).

get(ListOfKeys, State) when is_list(ListOfKeys)->
    {reply, do_get(ListOfKeys, []), State};
get(Key, State) ->
    {reply, do_get(Key), State}.

node_info(State) ->
    [LocalNode, NumberOfVirtualNode] =
        do_get([node, number_of_virtual_nodes], []),
    Info = [{number_of_virtual_nodes, NumberOfVirtualNode}],
    {reply, {node_info, LocalNode, Info}, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({get, Key}, _From, State) ->
    get(Key, State);
handle_call(node_info, _From, State) ->
    node_info(State).
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:call(?SERVER, stop).
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).
node_info() ->
    gen_server:call(?SERVER, node_info).
