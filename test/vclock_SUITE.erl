%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License.  You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(vclock_SUITE).
-compile(export_all).

-include("kai.hrl").
-include("kai_test.hrl").

all() -> [test_desceds_in_single_node].

test_desceds_in_single_node() -> [].

%% @doc Serves as both a trivial test and some example code.
test_desceds_in_single_node(_Conf) ->
    A1 = vclock:fresh(),
    A2 = vclock:increment(a, A1),
    ?assert(vclock:descends(A2,A1)),
    A3 = vclock:increment(a, A2),
    ?assert(vclock:descends(A3,A2)),
    ?assert(vclock:descends(A3,A1)),
    ok.

test_concurrent() -> [].

test_concurrent(_Conf) ->
    A1 = vclock:fresh(),
    B1 = vclock:fresh(),
    A2 = vclock:increment(a, A1),
    B2 = vclock:increment(b, B1),
    ?assertNot(vclock:descends(A2,B2)),
    ?assertNot(vclock:descends(B2,A2)),
    ok.

test_merge() -> [].

test_merge(_Conf) ->
    A1 = vclock:fresh(),
    B1 = vclock:fresh(),
    A2 = vclock:increment(a, A1),
    B2 = vclock:increment(b, B1),
    C1 = vclock:merge([A2, B2]),
    ?assert(vclock:descends(C1, A2)),
    ?assert(vclock:descends(C1, B2)),
    C2 = vclock:increment(c, C1),
    ?assertNot(vclock:descends(C2, A2)),
    ?assertNot(vclock:descends(C2, B2)),
    ?assertNot(vclock:descends(C2, C1)),
    ok.
