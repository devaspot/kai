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

-record(data, {
    key, bucket, last_modified, vector_clocks, checksum, flags, value
}).

-record(tcp_server_option, {
    listen = [{active, false}, binary, {packet, line}, {reuseaddr, true}],
    port                    = 11211,
    max_processes           = 8,
    max_restarts            = 3,
    time                    = 60,
    shutdown                = 2000,
    accept_timeout          = infinity,
    accept_error_sleep_time = 3000,
    recv_length             = 0,
    recv_timeout            = infinity
}).

-define(error  (Data), kai_log:log(error, self(), ?FILE, ?LINE, Data)).
-define(warning(Data), kai_log:log(warning, self(), ?FILE, ?LINE, Data)).
-define(info   (Data), kai_log:log(info, self(), ?FILE, ?LINE, Data)).

%-define(debug(Data), kai_log:log(debug, self(), ?FILE, ?LINE, Data)).
-define(debug(_Data), ok).

-define(TIMEOUT, 5000).
-define(TIMER, 1000).
