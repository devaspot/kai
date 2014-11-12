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

-module(kai_tcp_server).

-export([behaviour_info/1]).
-export([start_link/1, start_link/2, start_link/3, start_link/4]).
-export([stop/0, stop/1]).
-export([info/1, info/2]).

-include("kai.hrl").

% Behaviour Callbacks
behaviour_info(callbacks) -> [{init, 1}, {handle_call, 3}];
behaviour_info(_Other)    -> undefined.

% External APIs
start_link(Mod)       -> start_link(Mod, []).
start_link(Mod, Args) -> start_link(Mod, Args, #tcp_server_option{}).
start_link(Mod, Args, Option) ->
    start_link({local, ?MODULE}, Mod, Args, Option).
start_link(Name, Mod, Args, Option) ->
    kai_tcp_server_sup:start_link(Name, Mod, Args, Option).

stop() -> stop(?MODULE).
stop(Name) ->
    kai_tcp_server_sup:stop(Name).

info(Key) -> info(?MODULE, Key).
info(Name, Key) ->
    kai_tcp_server_monitor:info(
        kai_tcp_server_sup:build_monitor_name(Name), Key
    ).

