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

-define(NODE1, {{127,0,0,1}, 11011}).
-define(NODE2, {{127,0,0,1}, 11012}).
-define(NODE3, {{127,0,0,1}, 11013}).
-define(NODE4, {{127,0,0,1}, 11014}).

-define(INFO, [{number_of_virtual_nodes, 2}]).

-define(assert(BoolExpr),
        ((fun() ->
            case (BoolExpr) of
                true -> ok;
                __V -> .erlang:error({assertion_failed,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {expression, (??BoolExpr)},
                                       {expected, true},
                                       {value, case __V of
                                                   false -> __V;
                                                   _     -> {not_a_boolean,__V}
                                               end}]})
            end
          end)())).

-define(assertNot(BoolExpr), ?assert(not (BoolExpr))).

-define(assertEqual(Expect, Expr),
        ((fun(__X) ->
            case (Expr) of
                __X -> ok;
                __V -> .erlang:error({assertEqual_failed,
                                      [{module, ?MODULE},
                                       {line, ?LINE},
                                       {expression, (??Expr)},
                                       {expected, __X},
                                       {value, __V}]})
            end
          end)(Expect))).
