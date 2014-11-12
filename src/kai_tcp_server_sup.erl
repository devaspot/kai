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

-module(kai_tcp_server_sup).
-behaviour(supervisor).

-export([start_link/4, stop/1]).
-export([init/1]).
-export([build_monitor_name/1]).

-include("kai.hrl").

% External APIs
start_link(Name, Mod, Args, Option) ->
    supervisor:start_link(Name, ?MODULE, [Name, Mod, Args, Option]).

stop(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            exit(Pid, normal),
            ok;
        _ -> not_started
    end.

% Callbacks
init([Name, Mod, Args, Option]) ->
    case Mod:init(Args) of 
        {ok, State}    -> listen(State, Name, Mod, Option);
        {stop, Reason} -> Reason;
        Other          -> Other % 'ignore' is contained.
    end.

% Internal Functions
listen(State, Name, Mod, Option) ->
    case gen_tcp:listen(
        Option#tcp_server_option.port,
        Option#tcp_server_option.listen
    ) of
        {ok, ListenSocket} ->
            build_result(ListenSocket, State, Name, Mod, Option);
        {error, Reason} ->
            ?warning(io_lib:format("listen(~p) ~p", [Mod, {error, Reason}])),
            {stop, Reason}
    end.

build_result(ListenSocket, State, {Dest, Name}, Mod, Option) ->
    #tcp_server_option{
        max_restarts = MaxRestarts,
        time         = Time
    } = Option,
    MonitorName = build_monitor_name(Name),
    {ok, {
        {one_for_one, MaxRestarts, Time},
        [
            monitor_spec({Dest, MonitorName}) |
            acceptor_specs(
                ListenSocket, State, {Dest, Name}, MonitorName, Mod, Option
            )
        ]
    }}.

monitor_spec({Dest, MonitorName}) ->
    {
        MonitorName,
        {
            kai_tcp_server_monitor,
            start_link,
            [{Dest, MonitorName}]
        },
        permanent,
        brutal_kill,
        worker,
        []
    }.

acceptor_specs(
  ListenSocket, State, {Dest, Name}, MonitorBaseName, Mod, Option
) ->
    #tcp_server_option{
        max_processes = MaxProcesses,
        shutdown      = Shutdown
    } = Option,
    MonitorName = case Dest of
      local   -> MonitorBaseName;
      _Global -> {Dest, MonitorBaseName}
    end,
    lists:map(
        fun (N) ->
            AcceptorName = build_acceptor_name(Name, N),
            {
                AcceptorName,
                {
                    kai_tcp_server_acceptor,
                    start_link,
                    [
                        {Dest, AcceptorName},
                        ListenSocket,
                        State,
                        MonitorName,
                        Mod,
                        Option
                    ]
                },
                permanent,
                Shutdown,
                worker,
                []
            }
        end,
        lists:seq(1, MaxProcesses)
    ).

build_monitor_name(Prefix) ->
    list_to_atom(atom_to_list(Prefix) ++ "_monitor").

build_acceptor_name(Prefix, Number) ->
    list_to_atom(
        atom_to_list(Prefix) ++ "_acceptor_" ++ integer_to_list(Number)
    ).
 
