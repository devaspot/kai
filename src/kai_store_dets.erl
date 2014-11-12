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

-module(kai_store_dets).
-behaviour(gen_server).

-export([start_link/1]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-include("kai.hrl").
-record(state, {number_of_tables, tables}).

start_link(Server) ->
    gen_server:start_link({local, Server}, ?MODULE, [], _Opts = []).

init(_Args) ->
    Dir = kai_config:get(dets_dir),
    NumberOfTables = kai_config:get(number_of_tables),
    Tables =
        lists:map(
          fun(I) ->
                  Name = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(I)),
                  File = Dir ++ "/" ++ integer_to_list(I),
                  case dets:open_file(Name, [{type, set}, {keypos, 2}, {file, File}]) of
                      {ok, Table} -> {I, Table};
                      {error, Reason} -> ?info(Reason),
                                         exit(Reason)
                  end
          end,
          lists:seq(1, NumberOfTables)
         ),
    {ok, #state{number_of_tables = NumberOfTables, tables = Tables}}.

terminate(_Reason, State) ->
    lists:foreach(
      fun({_I, Table}) -> dets:close(Table) end,
      State#state.tables
     ),
    ok.

bucket_to_table(Bucket, State) ->
    I = Bucket rem State#state.number_of_tables + 1,
    proplists:get_value(I, State#state.tables).

do_list(Bucket, State) ->
    Table = bucket_to_table(Bucket, State),
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
    ListOfData = dets:select(Table, [{Head, Cond, Body}]),
    {reply, {list_of_data, ListOfData}, State}.

do_get(#data{key=Key, bucket=Bucket} = _Data, State) ->
    Table = bucket_to_table(Bucket, State),
    case dets:lookup(Table, Key) of
        [Data] -> {reply, Data, State};
        _      -> {reply, undefined, State}
    end.

do_match(#data{key=Key, bucket=Bucket} = _Data, State) ->
    Table = bucket_to_table(Bucket, State),
    Data = dets:match(Table, #data{key=Key, bucket=Bucket, value='$1'}),
    {reply, Data, State}.

do_put(Data, State) when is_record(Data, data) ->
    Table = bucket_to_table(Data#data.bucket, State),
    case dets:lookup(Table, Data#data.key) of
        [StoredData] ->
            case vclock:descends(Data#data.vector_clocks, StoredData#data.vector_clocks) of
                true -> insert_and_reply(Data, Table, State);
                _ -> {reply, {error, "stale or concurrent state found in kai_store"}, State}
            end;
        _ -> insert_and_reply(Data, Table, State)
    end.

insert_and_reply(Data, Table, State) ->
    dets:insert(Table, Data),
    dets:sync(Table),
    {reply, ok, State}.

do_delete(#data{key=Key, bucket=Bucket} = _Data, State) ->
    Table = bucket_to_table(Bucket, State),
    case dets:lookup(Table, Key) of
        [_Data2] ->
            dets:delete(Table, Key),
            dets:sync(Table),
            {reply, ok, State};
        _ ->
            {reply, undefined, State}
    end.

info(Name, State) ->
    Values =
        lists:map(
          fun(I) ->
                  T = proplists:get_value(I, State#state.tables),
                  case Name of
                      bytes -> dets:info(T, file_size);
                      size  -> dets:info(T, size)
                  end
          end,
          lists:seq(1, State#state.number_of_tables)
         ),
    {reply, lists:sum(Values), State}.

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
