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

-module(kai_store).

-export([start_link/0, stop/0]).
-export([list/1, get/1, put/1, delete/1, info/1, match/1]).
-compile(export_all).
-include("kai.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    Store = kai_config:get(store),
    Module = list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Store)),
    apply(Module, start_link, [?SERVER]).

stop() ->
    gen_server:call(?SERVER, stop).
list(Bucket) ->
    gen_server:call(?SERVER, {list, Bucket}).
get(Data) ->
    gen_server:call(?SERVER, {get, Data}).
match(Data) ->
    gen_server:call(?SERVER, {match, Data}).
put(Data) ->
    gen_server:call(?SERVER, {put, Data}).
delete(Data) ->
    gen_server:call(?SERVER, {delete, Data}).
info(Name) ->
    gen_server:call(?SERVER, {info, Name}).
