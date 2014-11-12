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

-module(kai_memcache).
-behaviour(kai_tcp_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3]).

-include("kai.hrl").

-define(MEMCACHE_TIMEOUT, ?TIMEOUT).

start_link() ->
    kai_tcp_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        [],
        #tcp_server_option{
            port          = kai_config:get(memcache_port),
            max_processes = kai_config:get(memcache_max_processes)
        }
    ).

stop() -> kai_tcp_server:stop(?MODULE).

init(_Args) -> {ok, {}}.

handle_call(Socket, Data, State) ->
    dispatch(Socket, string:tokens(binary_to_list(Data), " \r\n"), State).

dispatch(_Socket, ["get", Key], State) ->
    do_get(Key, State, false);
dispatch(_Socket, ["gets", Key], State) ->
    do_get(Key, State, true);

dispatch(Socket, ["set", _Key, _Flags, "0", _Bytes] = Data, State) ->
    inet:setopts(Socket, [{packet, raw}]),
    Result = recv_set_data(Socket, Data, State),
    inet:setopts(Socket, [{packet, line}]),
    Result;

dispatch(_Socket, ["set", _Key, _Flags, _Exptime, _Bytes], State) ->
    {reply, <<"CLIENT_ERROR Exptime must be zero.\r\n">>, State};

dispatch(_Socket, ["delete", Key], State) ->
    case kai_coordinator:route({delete, #data{key=Key}}) of
        ok        -> {reply, <<"DELETED\r\n">>, State};
        undefined -> {reply, <<"NOT_FOUND\r\n">>, State};
        _Other    ->
            send_error_and_close(State, "Failed to delete.")
    end;

dispatch(_Socket, ["delete", Key, "0"], State) ->
    dispatch(_Socket, ["delete", Key], State);
dispatch(_Socket, ["delete", _Key, _Time], State) ->
    {reply, <<"CLIENT_ERROR Time must be zero.\r\n">>, State};

dispatch(_Socket, ["delete", _Key, _Time, "noreply"], State) ->
    {reply, <<"CLIENT_ERROR noreply not supported.\r\n">>, State};

dispatch(_Socket, ["stats"], State) ->
    Response =
        lists:map(
          fun({Name, Value}) ->
                  ["STAT " ++ atom_to_list(Name) ++ " " ++ Value ++ "\r\n"]
          end,
          kai_stat:all()
         ),
    {reply, [Response|"END\r\n"], State};

dispatch(_Socket, ["version"], State) ->
    Version =
        case application:get_key(kai, vsn) of
            {ok, V} -> V;
            _       -> "0"
        end,
    {reply, "VERSION " ++ Version ++ "\r\n", State};

dispatch(_Socket, ["quit"], _State) -> quit;

dispatch(_Socket, _Unknown, State) ->
    {reply, <<"ERROR\r\n">>, State}.

do_get(Key, State, WithCasUnique) ->
    case kai_coordinator:route({get, #data{key=Key}}) of
        Data when is_list(Data) ->
            {ok, CasUniqueInBinary} = kai_version:cas_unique(Data),
            Response = get_response(Data, WithCasUnique, CasUniqueInBinary),
            kai_stat:incr_cmd_get(),
            kai_stat:add_bytes_read(Data),
            {reply, [Response|"END\r\n"], State};
        undefined ->
            {reply, <<"END\r\n">>, State};
        _Other ->
            send_error_and_close("Failed to read.", State)
    end.

get_response(Data, WithCasUnique, CasUnique) ->
    lists:map(
      fun(Elem) ->
              Key = Elem#data.key,
              Flags = Elem#data.flags,
              Value = Elem#data.value,
              [
               io_lib:format("VALUE ~s ~s ~w", [Key, Flags, byte_size(Value)]),
               case WithCasUnique of
                   true ->
                       io_lib:format(" ~w", [cas_unique(CasUnique)]);
                   _ -> []
               end,
               "\r\n", Value, "\r\n"]
      end, Data).

recv_set_data(Socket, ["set", Key, Flags, "0", Bytes], State) ->
    case gen_tcp:recv(Socket, list_to_integer(Bytes), ?MEMCACHE_TIMEOUT) of
        {ok, Value} ->
            gen_tcp:recv(Socket, 2, ?MEMCACHE_TIMEOUT),
            case kai_coordinator:route(
                {put, #data{key=Key, flags=Flags, value=Value}}
            ) of
                ok ->
                    gen_tcp:send(Socket, <<"STORED\r\n">>),
                    kai_stat:incr_cmd_set(),
                    kai_stat:add_bytes_write(#data{value=Value}),
                    {noreply, State};
                _Other ->
                    send_error_and_close("Failed to write.", State)
            end;
        _Other ->
            {noreply, State}
    end.

send_error_and_close(Message, State) ->
    ?warning(io_lib:format("send_error_and_close/2: ~p", [Message])),
    {close, ["SERVER_ERROR ", Message, "\r\n"], State}.

cas_unique(CasUniqueInBinary) ->
    <<HashedValue:64/integer>> = CasUniqueInBinary,
    HashedValue.
