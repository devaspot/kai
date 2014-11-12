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

-module(kai_store_ets).
-behaviour(gen_server).

-export([start_link/1]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("kai.hrl").

start_link(Server) ->
    gen_server:start_link({local, Server}, ?MODULE, [], _Opts = []).

init(_Args) ->
    ets:new(?MODULE, [set, private, named_table, {keypos, 2}]),
    {ok, []}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

do_list(Bucket, State) ->
    Head = #data{
        key           = '$1',
        bucket        = Bucket,
        last_modified = '$2',
        vector_clocks = '$3', 
        checksum      = '$4',
        flags         = '_',
        value         = '_'
    },
    Cond = [],
    Body = [{#data{
        key           = '$1',
        bucket        = Bucket,
        last_modified = '$2',
        vector_clocks = '$3',
        checksum      = '$4'
    }}],
    ListOfData = ets:select(?MODULE, [{Head, Cond, Body}]),
    {reply, {list_of_data, ListOfData}, State}.

do_get(#data{key=Key} = _Data, State) ->
    case ets:lookup(?MODULE, Key) of
        [Data] -> {reply, Data, State};
        _      -> {reply, undefined, State}
    end.

do_match(#data{key=Key} = _Data, State) ->
    case ets:match(?MODULE, Key) of
        [Data] -> {reply, Data, State};
        _      -> {reply, undefined, State}
    end.

do_put(Data, State) when is_record(Data, data) ->
    case ets:lookup(?MODULE, Data#data.key) of
        [StoredData] ->
            case vclock:descends(Data#data.vector_clocks, StoredData#data.vector_clocks) of
                true -> insert_and_reply(Data, State);
                _ -> {reply, {error, "stale or concurrent state found in kai_store"}, State}
            end;
        _ -> insert_and_reply(Data, State)
    end;
do_put(a, b) -> ok.

insert_and_reply(Data, State) ->
    ets:insert(?MODULE, Data),
    {reply, ok, State}.

do_delete(#data{key=Key} = _Data, State) ->
    case ets:lookup(?MODULE, Key) of
        [_Data2] ->
            ets:delete(?MODULE, Key),
            {reply, ok, State};
        _ ->
            {reply, undefined, State}
    end.

info(Name, State) ->
    Value =
        case Name of
            bytes ->
                % this code roughly estimates the size of stored objects,
                % since ets only store a reference to the binary
                Ets = erlang:system_info(wordsize) + ets:info(?MODULE, memory),
                Bin = erlang:memory(binary),
                Ets + Bin;
            size ->
                ets:info(?MODULE, size)
        end,
    {reply, Value, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({list, Bucket}, _From, State) ->
    do_list(Bucket, State);
handle_call({get, Data}, _From, State) ->
    do_get(Data, State);
handle_call({match, Data}, _From, State) ->
    do_match(Data, State);
handle_call({put, Data}, _From, State) ->
    do_put(Data, State);
handle_call({delete, Data}, _From, State) ->
    do_delete(Data, State);
handle_call({info, Name}, _From, State) ->
    info(Name, State).
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
