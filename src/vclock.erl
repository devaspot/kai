%% @copyright 2007-2008 Basho Technologies

%% @reference Leslie Lamport (1978). "Time, clocks, and the ordering of events in a distributed system". Communications of the ACM 21 (7): 558-565.

%% @reference Friedemann Mattern (1988). "Virtual Time and Global States of Distributed Systems". Workshop on Parallel and Distributed Algorithms: pp. 215-226

%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>

%% @doc A simple Erlang implementation of vector clocks as inspired by Lamport logical clocks.

%% Copyright 2007-2008 Basho Technologies

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%% http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(vclock).

-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([fresh/0,descends/2,merge/1,get_counter/2,get_timestamp/2,
         increment/2,all_nodes/1]).

%% @type vclock() = [vc_entry].
%% @type   vc_entry() = {node(), {counter(), timestamp()}}.
%% The timestamp is present but not used, in case a client wishes to inspect it.
%% @type   node() = term().
%% Nodes can have any term() as a name, but they must differ from each other.
%% @type   counter() = integer().
%% @type   timestamp() = integer().

%% @doc Create a brand new vclock.
%% @spec fresh() -> vclock()
fresh() ->
    [].

%% @doc Remove trivial ancestors.
%% @spec simplify(vclock()) -> vclock()
simplify(VClock) ->
    simplify(VClock, []).
simplify([], NClock) ->
    NClock;
simplify([{Node,{Ctr1,TS1}}|VClock], NClock) ->
    {Ctr2,TS2} = proplists:get_value(Node, NClock, {Ctr1,TS1}),
    {Ctr,TS} = if
                   Ctr1 > Ctr2 ->
                       {Ctr1,TS1};
                   true ->
                       {Ctr2,TS2}
               end,
    simplify(VClock, [{Node,{Ctr,TS}}|proplists:delete(Node, NClock)]).

%% @doc Return true if Va is a direct descendant of Vb, else false -- remember, a vclock is its own descendant!
%% @spec descends(Va :: vclock(), Vb :: vclock()) -> bool()
descends(_, []) ->
    %% all vclocks descend from the empty vclock
    true;
descends(Va, Vb) ->
    [{NodeB, {CtrB, _T}}|RestB] = Vb,
    CtrA =
        case proplists:get_value(NodeB, Va) of
            undefined ->
                false;
            {CA, _TSA} -> CA
        end,
    case CtrA of
        false -> false;
        _ ->
            if
                CtrA < CtrB ->
                    false;
                true ->
                    descends(Va,RestB)
            end
    end.

%% @doc Combine all VClocks in the input list into their least possible
%%      common descendant.
%% @spec merge(VClocks :: [vclock()]) -> vclock()
merge(VClocks) ->
    merge(VClocks, []).

merge([], NClock) ->
    NClock;
merge([AClock|VClocks],NClock) ->
    merge(VClocks, lists:foldl(fun(X,L) -> extend(X,L) end, NClock, AClock)).

%% @doc Get the counter value in VClock set from Node.
%% @spec get_counter(Node :: node(), VClock :: vclock()) -> counter()
get_counter(Node, VClock) ->
    case proplists:get_value(Node, VClock) of
        {Ctr, _TS} -> Ctr;
        undefined -> undefined
    end.

%% @doc Get the timestamp value in a VClock set from Node.
%% @spec get_timestamp(Node :: node(), VClock :: vclock()) -> timestamp()
get_timestamp(Node, VClock) ->
    case proplists:get_value(Node, VClock) of
        {_Ctr, TS} -> TS;
        undefined -> undefined
    end.

%% @doc Increment VClock at Node.
%% @spec increment(Node :: node(), VClock :: vclock()) -> vclock()
increment(Node, VClock) ->
    {Ctr, TS} = case proplists:get_value(Node, VClock) of
                    undefined ->
                        {1, timestamp()};
                    {C, _T} ->
                        {C + 1, timestamp()}
                end,
    extend({Node, {Ctr, TS}}, VClock).

%% @doc Reflect an update performed by Node.
%%      See increment/2 for usage.
%% @spec extend(VC_Entry :: VC_Entry, VClock :: vclock()) -> vclock()
extend({Node,{Ctr,TS}}, VClock) ->
    simplify([{Node,{Ctr,TS}}|VClock]).

%% @doc Return the list of all nodes that have ever incremented VClock.
%% @spec all_nodes(VClock :: vclock()) -> [node()]
all_nodes(VClock) ->
    [X || {X,{_,_}} <- VClock].

timestamp() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).
