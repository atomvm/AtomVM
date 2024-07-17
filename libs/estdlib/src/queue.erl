%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% SPDX-License-Identifier: Apache-2.0
%%

%
% This is shrinked down version of OTP queue module, without Okasaki API support
%

-module(queue).

%% Creation, inspection and conversion
-export([new/0, is_queue/1, is_empty/1, len/1, to_list/1, from_list/1, member/2]).
%% Original style API
-export([in/2, in_r/2, out/1, out_r/1]).
%% Less garbage style API
-export([get/1, get_r/1, peek/1, peek_r/1, drop/1, drop_r/1]).

%% Higher level API
-export([
    reverse/1,
    join/2,
    split/2,
    filter/2,
    filtermap/2,
    fold/3,
    any/2,
    all/2,
    delete/2,
    delete_r/2,
    delete_with/2,
    delete_with_r/2
]).

-export_type([queue/0, queue/1]).

%%--------------------------------------------------------------------------
%% Efficient implementation of double ended fifo queues
%%
%% Queue representation
%%
%% {RearList,FrontList}
%%
%% The first element in the queue is at the head of the FrontList
%% The last element in the queue is at the head of the RearList,
%% that is; the RearList is reversed.
%%

-opaque queue(Item) :: {list(Item), list(Item)}.

-type queue() :: queue(_).

%%--------------------------------------------------------------------------
%% Creation, inspection and conversion

% Performance:
% O(1)

%%-----------------------------------------------------------------------------
%% @returns Returns an empty queue.
%% @doc     This function returns an empty queue.
%%          This function is part of the Original API.
%% @end
%%-----------------------------------------------------------------------------
-spec new() -> queue(none()).
new() -> {[], []}.

% Performance:
% O(1)

%%-----------------------------------------------------------------------------
%% @param   Term the term to be tested
%% @returns Returns `true' if `Term' is a queue, otherwise `false'
%% @doc     Tests if `Term' is a queue and returns `true' if so, otherwise `false'.
%%          Note that the test will return `true' for a term coinciding with the
%%          representation of a queue, even when not constructed by this module.
%%          This function is part of the Original API.
%% @end
%%-----------------------------------------------------------------------------
-spec is_queue(Term :: term()) -> boolean().
is_queue({R, F}) when is_list(R), is_list(F) ->
    true;
is_queue(_) ->
    false.

% Performance:
% O(1)

%%-----------------------------------------------------------------------------
%% @param   Q the queue to be tested
%% @returns Returns `true' if `Q' is empty, otherwise `false'
%% @doc     Tests if `Q' is empty and returns `true' if so, otherwise `false'.
%%          This function is part of the Original API.
%% @end
%%-----------------------------------------------------------------------------
-spec is_empty(Q :: queue()) -> boolean().
is_empty({[], []}) ->
    true;
is_empty({In, Out}) when is_list(In), is_list(Out) ->
    false;
is_empty(Q) ->
    erlang:error(badarg, [Q]).

%% O(len(Q))
%%-----------------------------------------------------------------------------
%% @param   Q the queue whose length is to be calculated
%% @returns Returns the length of queue `Q'
%% @doc     Calculates and returns the length of queue `Q'.
%%          This function is part of the Original API.
%% @end
%%-----------------------------------------------------------------------------
-spec len(Q :: queue()) -> non_neg_integer().
len({R, F}) when is_list(R), is_list(F) ->
    length(R) + length(F);
len(Q) ->
    erlang:error(badarg, [Q]).

% Performance:
% O(len(Q))

%%-----------------------------------------------------------------------------
%% @param   Q the queue to be converted to a list
%% @returns Returns a list of the items in the queue
%% @doc     Returns a list of the items in the queue in the same order; the front
%%          item of the queue becomes the head of the list.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> List == queue:to_list(Queue).'
%%          `true'
%%          This function is part of the Original API.
%% @end
%%-----------------------------------------------------------------------------
-spec to_list(Q :: queue(Item)) -> list(Item).
to_list({In, Out}) when is_list(In), is_list(Out) ->
    Out ++ lists:reverse(In, []);
to_list(Q) ->
    erlang:error(badarg, [Q]).

% How works:
% Create queue from list
% O(length(L))

%%-----------------------------------------------------------------------------
%% @param   L the list to be converted to a queue
%% @returns Returns a queue containing the items in `L' in the same order
%% @doc     Returns a queue containing the items in `L' in the same order; the head
%%          item of the list becomes the front item of the queue.
%%          This function is part of the Original API.
%% @end
%%-----------------------------------------------------------------------------
-spec from_list(L :: list(Item)) -> queue(Item).
from_list(L) when is_list(L) ->
    f2r(L);
from_list(L) ->
    erlang:error(badarg, [L]).

% How works:
% Return true or false depending on if element is in queue
% O(length(Q)) worst case

%%-----------------------------------------------------------------------------
%% @param   Item the item to be searched in the queue
%% @param   Q the queue to be searched
%% @returns Returns `true' if `Item' matches some element in `Q', otherwise `false'
%% @doc     Returns `true' if `Item' matches some element in `Q', otherwise `false'.
%%          This function is part of the Original API.
%% @end
%%-----------------------------------------------------------------------------
-spec member(Item, Q :: queue(Item)) -> boolean().
member(X, {R, F}) when is_list(R), is_list(F) ->
    lists:member(X, R) orelse lists:member(X, F);
member(X, Q) ->
    erlang:error(badarg, [X, Q]).

%%--------------------------------------------------------------------------
%% Original style API

% How works:
% Append to tail/rear
% Put at least one element in each list, if it is cheap
% O(1)

%%-----------------------------------------------------------------------------
%% @param   Item the item that will be enqueued (inserted at the rear of the queue)
%% @param   Q1 the queue where the item will be inserted in
%% @returns Returns the queue with `Item' inserted at the rear of the queue
%% @doc     Inserts `Item' at the rear of queue `Q1'. Returns the resulting queue `Q2'.
%%          This function is part of the Original API
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue1 = queue:in(100, Queue).'
%%          `{[100,5,4,3],[1,2]}'
%%          `3> queue:to_list(Queue1).'
%%          `[1,2,3,4,5,100]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec in(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
in(X, {[_] = In, []}) ->
    {[X], In};
in(X, {In, Out}) when is_list(In), is_list(Out) ->
    {[X | In], Out};
in(X, Q) ->
    erlang:error(badarg, [X, Q]).

% How works:
% Prepend to head/front
% Put at least one element in each list, if it is cheap
% O(1)

%%-----------------------------------------------------------------------------
%% @param   Item the item that will be enqueued (inserted at the front of the queue)
%% @param   Q1 the queue where the item will be inserted in
%% @returns Returns the queue with `Item' inserted at the front of the queue
%% @doc     Inserts `Item' at the front of queue `Q1'. Returns the resulting queue `Q2'.
%%          This function is part of the Original API
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue1 = queue:in_r(100, Queue).'
%%          `{[5,4,3],[100,1,2]}'
%%          `3> queue:to_list(Queue1).'
%%          `[100,1,2,3,4,5]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec in_r(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
in_r(X, {[], [_] = F}) ->
    {F, [X]};
in_r(X, {R, F}) when is_list(R), is_list(F) ->
    {R, [X | F]};
in_r(X, Q) ->
    erlang:error(badarg, [X, Q]).

% How works:
% Take from head/front
% O(1) amortized, O(len(Q)) worst case

%%-----------------------------------------------------------------------------
%% @param   Q1 the queue from which the item will be dequeued (removed from the front)
%% @returns Returns a tuple `{{value, Item}, Q2}' where `Item' is the item removed
%%          and `Q2' is the resulting queue. If `Q1' is empty, tuple `{empty, Q1}' is returned
%% @doc     Removes the item at the front of queue `Q1'. Returns tuple
%%          `{{value, Item}, Q2}', where `Item' is the item removed and `Q2' is the
%%          resulting queue. If `Q1' is empty, tuple `{empty, Q1}' is returned.
%%          This function is part of the Original API
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> {{value, 1=Item}, Queue1} = queue:out(Queue).'
%%          `{{value,1},{[5,4,3],[2]}}'
%%          `3> queue:to_list(Queue1).'
%%          `[2,3,4,5]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec out(Q1 :: queue(Item)) ->
    {{value, Item}, Q2 :: queue(Item)}
    | {empty, Q1 :: queue(Item)}.
out({[], []} = Q) ->
    {empty, Q};
out({[V], []}) ->
    {{value, V}, {[], []}};
out({[Y | In], []}) ->
    [V | Out] = lists:reverse(In, []),
    {{value, V}, {[Y], Out}};
out({In, [V]}) when is_list(In) ->
    {{value, V}, r2f(In)};
out({In, [V | Out]}) when is_list(In) ->
    {{value, V}, {In, Out}};
out(Q) ->
    erlang:error(badarg, [Q]).

% How works:
% Take from tail/rear
% O(1) amortized, O(len(Q)) worst case

%%-----------------------------------------------------------------------------
%% @param   Q1 the queue from which the item will be dequeued (removed from the rear)
%% @returns Returns a tuple `{{value, Item}, Q2}' where `Item' is the item removed
%%          and `Q2' is the resulting queue. If `Q1' is empty, tuple `{empty, Q1}' is returned
%% @doc     Removes the item at the rear of queue `Q1'. Returns tuple `{{value, Item}, Q2}',
%%          where `Item' is the item removed and `Q2' is the new queue. If `Q1' is empty,
%%          tuple `{empty, Q1}' is returned.
%%          This function is part of the Original API
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> {{value, 5=Item}, Queue1} = queue:out_r(Queue).'
%%          `{{value,5},{[4,3],[1,2]}}'
%%          `3> queue:to_list(Queue1).'
%%          `[1,2,3,4]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec out_r(Q1 :: queue(Item)) ->
    {{value, Item}, Q2 :: queue(Item)}
    | {empty, Q1 :: queue(Item)}.
out_r({[], []} = Q) ->
    {empty, Q};
out_r({[], [V]}) ->
    {{value, V}, {[], []}};
out_r({[], [Y | Out]}) ->
    [V | In] = lists:reverse(Out, []),
    {{value, V}, {In, [Y]}};
out_r({[V], Out}) when is_list(Out) ->
    {{value, V}, f2r(Out)};
out_r({[V | In], Out}) when is_list(Out) ->
    {{value, V}, {In, Out}};
out_r(Q) ->
    erlang:error(badarg, [Q]).

%%--------------------------------------------------------------------------
%% Less garbage style API.

% How works:
% Return the first element in the queue
% O(1) since the queue is supposed to be well formed

%%-----------------------------------------------------------------------------
%% @param   Q the queue from which the first element will be returned
%% @returns Returns `Item' at the front of queue `Q'. Fails with reason `empty' if `Q' is empty
%% @doc     Returns `Item' at the front of queue `Q'.
%%          Fails with reason `empty' if `Q' is empty.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> 1 == queue:get(Queue).'
%%          `true'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get(Q :: queue(Item)) -> Item.
get({[], []} = Q) ->
    erlang:error(empty, [Q]);
get({R, F}) when is_list(R), is_list(F) ->
    get(R, F);
get(Q) ->
    erlang:error(badarg, [Q]).

-spec get(list(), list()) -> term().
get(R, [H | _]) when is_list(R) ->
    H;
get([H], []) ->
    H;
% malformed queue -> O(len(Q))
get([_ | R], []) ->
    lists:last(R).

% How works:
% Return the last element in the queue
% O(1) since the queue is supposed to be well formed

%%-----------------------------------------------------------------------------
%% @param   Q the queue from which the last element will be returned
%% @returns Returns `Item' at the rear of queue `Q'. Fails with reason `empty' if `Q' is empty
%% @doc     Returns `Item' at the rear of queue `Q'.
%%          Fails with reason `empty' if `Q' is empty.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> 5 == queue:get_r(Queue).'
%%          `true'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_r(Q :: queue(Item)) -> Item.
get_r({[], []} = Q) ->
    erlang:error(empty, [Q]);
get_r({[H | _], F}) when is_list(F) ->
    H;
get_r({[], [H]}) ->
    H;
% malformed queue -> O(len(Q))
get_r({[], [_ | F]}) ->
    lists:last(F);
get_r(Q) ->
    erlang:error(badarg, [Q]).

% How works:
% Return the first element in the queue
% O(1) since the queue is supposed to be well formed

%%-----------------------------------------------------------------------------
%% @param   Q the queue from which the first element will be returned
%% @returns Returns tuple `{value, Item}', where `Item' is the front item of `Q',
%%          or `empty' if `Q' is empty
%% @doc     Returns tuple `{value, Item}', where `Item' is the front item of `Q',
%%          or `empty' if `Q' is empty.
%%
%%          Example:
%%          `1> queue:peek(queue:new()).'
%%          `empty'
%%          `2> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `3> queue:peek(Queue).'
%%          `{value, 1}'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec peek(Q :: queue(Item)) -> empty | {value, Item}.
peek({[], []}) ->
    empty;
peek({R, [H | _]}) when is_list(R) ->
    {value, H};
peek({[H], []}) ->
    {value, H};
% malformed queue -> O(len(Q))
peek({[_ | R], []}) ->
    {value, lists:last(R)};
peek(Q) ->
    erlang:error(badarg, [Q]).

% How works:
% Return the last element in the queue
% O(1) since the queue is supposed to be well formed

%%-----------------------------------------------------------------------------
%% @param   Q the queue from which the last element will be returned
%% @returns Returns tuple `{value, Item}', where `Item' is the rear item of `Q',
%%          or `empty' if `Q' is empty
%% @doc     Returns tuple `{value, Item}', where `Item' is the rear item of `Q',
%%          or `empty' if `Q' is empty.
%%
%%          Example:
%%          `1> queue:peek_r(queue:new()).'
%%          `empty'
%%          `2> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `3> queue:peek_r(Queue).'
%%          `{value, 5}'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec peek_r(Q :: queue(Item)) -> empty | {value, Item}.
peek_r({[], []}) ->
    empty;
peek_r({[H | _], F}) when is_list(F) ->
    {value, H};
peek_r({[], [H]}) ->
    {value, H};
% malformed queue -> O(len(Q))
peek_r({[], [_ | R]}) ->
    {value, lists:last(R)};
peek_r(Q) ->
    erlang:error(badarg, [Q]).

% How works:
% Remove the first element and return resulting queue
% O(1) amortized

%%-----------------------------------------------------------------------------
%% @param   Q1 the queue from which the first element will be removed
%% @returns Returns a queue `Q2' that is the result of removing the front item from `Q1'.
%%          Fails with reason `empty' if `Q1' is empty
%% @doc     Returns a queue `Q2' that is the result of removing the front item from `Q1'.
%%          Fails with reason `empty' if `Q1' is empty.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue = queue:drop(Queue).'
%%          `{[5,4,3],[2]}'
%%          `3> queue:to_list(Queue1).'
%%          `[2,3,4,5]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec drop(Q1 :: queue(Item)) -> Q2 :: queue(Item).
drop({[], []} = Q) ->
    erlang:error(empty, [Q]);
drop({[_], []}) ->
    {[], []};
drop({[Y | R], []}) ->
    [_ | F] = lists:reverse(R, []),
    {[Y], F};
drop({R, [_]}) when is_list(R) ->
    r2f(R);
drop({R, [_ | F]}) when is_list(R) ->
    {R, F};
drop(Q) ->
    erlang:error(badarg, [Q]).

% How works:
% Remove the last element and return resulting queue
% O(1) amortized

%%-----------------------------------------------------------------------------
%% @param   Q1 the queue from which the last element will be removed
%% @returns Returns a queue `Q2' that is the result of removing the rear item from `Q1'.
%%          Fails with reason `empty' if `Q1' is empty
%% @doc     Returns a queue `Q2' that is the result of removing the rear item from `Q1'.
%%          Fails with reason `empty' if `Q1' is empty.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue = queue:drop_r(Queue).'
%%          `{[4,3],[1,2]}'
%%          `3> queue:to_list(Queue1).'
%%          `[1,2,3,4]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec drop_r(Q1 :: queue(Item)) -> Q2 :: queue(Item).
drop_r({[], []} = Q) ->
    erlang:error(empty, [Q]);
drop_r({[], [_]}) ->
    {[], []};
drop_r({[], [Y | F]}) ->
    [_ | R] = lists:reverse(F, []),
    {R, [Y]};
drop_r({[_], F}) when is_list(F) ->
    f2r(F);
drop_r({[_ | R], F}) when is_list(F) ->
    {R, F};
drop_r(Q) ->
    erlang:error(badarg, [Q]).

%%--------------------------------------------------------------------------
%% Higher level API

% How works:
% Return reversed queue
% O(1)

%%-----------------------------------------------------------------------------
%% @param   Q1 the queue to be reversed
%% @returns Returns a queue `Q2' containing the items of `Q1' in reverse order
%% @doc     Returns a queue `Q2' containing the items of `Q1' in reverse order.
%%          This function is part of the Original API
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue1 = queue:reverse(Queue).'
%%          `{[2,1],[3,4,5]}'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec reverse(Q1 :: queue(Item)) -> Q2 :: queue(Item).
reverse({R, F}) when is_list(R), is_list(F) ->
    {F, R};
reverse(Q) ->
    erlang:error(badarg, [Q]).

% How works:
% Join two queues
% Q2 empty: O(1)
% else:     O(len(Q1))

%%-----------------------------------------------------------------------------
%% @param   Q1 the first queue to be joined
%% @param   Q2 the second queue to be joined
%% @returns Returns a queue `Q3' that is the result of joining `Q1' and `Q2' with
%%          `Q1' in front of `Q2'
%% @doc     Returns a queue `Q3' that is the result of joining `Q1' and `Q2' with
%%          `Q1' in front of `Q2'.
%%          This function is part of the Original API
%%
%%          Example:
%%          `1> Queue1 = queue:from_list([1,3]).'
%%          `{[3],[1]}'
%%          `2> Queue2 = queue:from_list([2,4]).'
%%          `{[4],[2]}'
%%          `3> queue:to_list(queue:join(Queue1, Queue2)).'
%%          `[1,3,2,4]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec join(Q1 :: queue(Item), Q2 :: queue(Item)) -> Q3 :: queue(Item).
join({R, F} = Q, {[], []}) when is_list(R), is_list(F) ->
    Q;
join({[], []}, {R, F} = Q) when is_list(R), is_list(F) ->
    Q;
join({R1, F1}, {R2, F2}) when is_list(R1), is_list(F1), is_list(R2), is_list(F2) ->
    {R2, F1 ++ lists:reverse(R1, F2)};
join(Q1, Q2) ->
    erlang:error(badarg, [Q1, Q2]).

% How works:
% Split a queue in two
% N = 0..len(Q)
% O(max(N, len(Q)))

%%-----------------------------------------------------------------------------
%% @param   N the number of items to be put in the first resulting queue `Q2'
%% @param   Q1 the queue to be split
%% @returns Returns a tuple `{Q2, Q3}' where `Q2' contains the first `N' items
%%          of `Q1' and `Q3' contains the remaining items
%% @doc     Splits `Q1' in two. The `N' front items are put in `Q2' and the rest
%%          in `Q3'.
%%          This function is part of the Original API
%% @end
%%-----------------------------------------------------------------------------
-spec split(N :: non_neg_integer(), Q1 :: queue(Item)) ->
    {Q2 :: queue(Item), Q3 :: queue(Item)}.
split(0, {R, F} = Q) when is_list(R), is_list(F) ->
    {{[], []}, Q};
split(N, {R, F} = Q) when is_integer(N), N >= 1, is_list(R), is_list(F) ->
    Lf = erlang:length(F),
    % Lf >= 2
    if
        N < Lf ->
            [X | F1] = F,
            split_f1_to_r2(N - 1, R, F1, [], [X]);
        N > Lf ->
            Lr = length(R),
            M = Lr - (N - Lf),
            if
                M < 0 ->
                    erlang:error(badarg, [N, Q]);
                M > 0 ->
                    [X | R1] = R,
                    split_r1_to_f2(M - 1, R1, F, [X], []);
                % M == 0
                true ->
                    {Q, {[], []}}
            end;
        % N == Lf
        true ->
            {f2r(F), r2f(R)}
    end;
split(N, Q) ->
    erlang:error(badarg, [N, Q]).

%% Move N elements from F1 to R2
split_f1_to_r2(0, R1, F1, R2, F2) ->
    {{R2, F2}, {R1, F1}};
split_f1_to_r2(N, R1, [X | F1], R2, F2) ->
    split_f1_to_r2(N - 1, R1, F1, [X | R2], F2).

%% Move N elements from R1 to F2
split_r1_to_f2(0, R1, F1, R2, F2) ->
    {{R1, F1}, {R2, F2}};
split_r1_to_f2(N, [X | R1], F1, R2, F2) ->
    split_r1_to_f2(N - 1, R1, F1, R2, [X | F2]).

% How it works:
% filter, or rather filtermap with insert, traverses in queue order
%
% Fun(_) -> List: O(length(List) * len(Q))
% else:           O(len(Q)

%%-----------------------------------------------------------------------------
%% @param   Fun the function to be applied to each item in the queue
%% @param   Q1 the queue where the function will be applied
%% @returns Returns a queue `Q2' that is the result of calling `Fun(Item)' on all items in `Q1'
%% @doc     Returns a queue `Q2' that is the result of calling `Fun(Item)' on all items in `Q1'.
%%          If `Fun(Item)' returns `true', `Item' is copied to the result queue. If it returns
%%          `false', `Item' is not copied. If it returns a list, the list elements are inserted
%%          instead of `Item' in the result queue.
%%
%%          Example 1:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue1 = queue:filter(fun (E) -> E > 2 end, Queue).'
%%          `{[5],[3,4]}'
%%          `3> queue:to_list(Queue1).'
%%          `[3,4,5]'
%%
%%          Example 2:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue1 = queue:filter(fun (E) -> [E, E+1] end, Queue).'
%%          `{[6,5,5,4,4,3],[1,2,2,3]}'
%%          `3> queue:to_list(Queue1).'
%%          `[1,2,2,3,3,4,4,5,5,6]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec filter(Fun, Q1 :: queue(Item)) -> Q2 :: queue(Item) when
    Fun :: fun((Item) -> boolean() | list(Item)).
filter(Fun, {R0, F0}) when is_function(Fun, 1), is_list(R0), is_list(F0) ->
    F = filter_f(Fun, F0),
    R = filter_r(Fun, R0),
    if
        R =:= [] ->
            f2r(F);
        F =:= [] ->
            r2f(R);
        true ->
            {R, F}
    end;
filter(Fun, Q) ->
    erlang:error(badarg, [Fun, Q]).

%% Call Fun in head to tail order
filter_f(_, []) ->
    [];
filter_f(Fun, [X | F]) ->
    case Fun(X) of
        true ->
            [X | filter_f(Fun, F)];
        [Y] ->
            [Y | filter_f(Fun, F)];
        false ->
            filter_f(Fun, F);
        [] ->
            filter_f(Fun, F);
        L when is_list(L) ->
            L ++ filter_f(Fun, F)
    end.

%% Call Fun in reverse order, i.e tail to head
%% and reverse list result from fun to match queue order
filter_r(_, []) ->
    [];
filter_r(Fun, [X | R0]) ->
    R = filter_r(Fun, R0),
    case Fun(X) of
        true ->
            [X | R];
        [Y] ->
            [Y | R];
        false ->
            R;
        [] ->
            R;
        L when is_list(L) ->
            lists:reverse(L, R)
    end.

% How works:
% Filter and map a queue, traverses in queue order.
% Since OTP 24
% O(len(Q1))

%%-----------------------------------------------------------------------------
%% @param   Fun the function to be applied to each item in the queue
%% @param   Q1 the queue where the function will be applied
%% @returns Returns a queue `Q2' that is the result of calling `Fun(Item)' on all items in `Q1'
%% @doc     Returns a queue `Q2' that is the result of calling `Fun(Item)' on all items in `Q1'.
%%          If `Fun(Item)' returns `true', `Item' is copied to the result queue. If it returns
%%          `false', `Item' is not copied. If it returns `{true, NewItem}', the queue element
%%          at this position is replaced with `NewItem' in the result queue.
%%
%%          Example 1:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `{[5,4,3],[1,2]}'
%%          `2> Queue1 = queue:filtermap(fun (E) -> E > 2 end, Queue).'
%%          `{[5],[3,4]}'
%%          `3> queue:to_list(Queue1).'
%%          `[3,4,5]'
%%          `4> Queue1 = queue:filtermap(fun (E) -> {true, E+100} end, Queue).'
%%          `{[105,104,103],[101,102]}'
%%          `5> queue:to_list(Queue1).'
%%          `[101,102,103,104,105]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec filtermap(Fun, Q1) -> Q2 when
    Fun :: fun((Item) -> boolean() | {'true', Value}),
    Q1 :: queue(Item),
    Q2 :: queue(Item | Value),
    Item :: term(),
    Value :: term().
filtermap(Fun, {R0, F0}) when is_function(Fun, 1), is_list(R0), is_list(F0) ->
    F = lists:filtermap(Fun, F0),
    R = filtermap_r(Fun, R0),
    if
        R =:= [] ->
            f2r(F);
        F =:= [] ->
            r2f(R);
        true ->
            {R, F}
    end;
filtermap(Fun, Q) ->
    erlang:error(badarg, [Fun, Q]).

%% Call Fun in reverse order, i.e tail to head
filtermap_r(_, []) ->
    [];
filtermap_r(Fun, [X | R0]) ->
    R = filtermap_r(Fun, R0),
    case Fun(X) of
        true ->
            [X | R];
        {true, Y} ->
            [Y | R];
        false ->
            R
    end.

% How works:
% Fold a function over a queue, in queue order.
% Since OTP 24
% O(len(Q))

%%-----------------------------------------------------------------------------
%% @param   Fun the function to be applied to each item and accumulator
%% @param   Acc0 the initial accumulator value
%% @param   Q the queue over which the function will be folded
%% @returns Returns the final value of the accumulator after folding over the queue
%% @doc     Calls `Fun(Item, AccIn)' on successive items `Item' of `Queue', starting with
%%          `AccIn == Acc0'. The queue is traversed in queue order, that is, from front to
%%          rear. `Fun/2' must return a new accumulator, which is passed to the next call.
%%          The function returns the final value of the accumulator. `Acc0' is returned if
%%          the queue is empty.
%%
%%          Example:
%%          `1> queue:fold(fun(X, Sum) -> X + Sum end, 0, queue:from_list([1,2,3,4,5])).'
%%          `15'
%%          `2> queue:fold(fun(X, Prod) -> X * Prod end, 1, queue:from_list([1,2,3,4,5])).'
%%          `120'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec fold(Fun, Acc0, Q :: queue(Item)) -> Acc1 when
    Fun :: fun((Item, AccIn) -> AccOut),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term().
fold(Fun, Acc0, {R, F}) when is_function(Fun, 2), is_list(R), is_list(F) ->
    Acc1 = lists:foldl(Fun, Acc0, F),
    lists:foldr(Fun, Acc1, R);
fold(Fun, Acc0, Q) ->
    erlang:error(badarg, [Fun, Acc0, Q]).

% How works:
% Check if any item satisfies the predicate, traverse in queue order.
% Since OTP 24
% O(len(Q)) worst case

%%-----------------------------------------------------------------------------
%% @param   Pred the predicate function to apply to each item
%% @param   Q the queue to check against the predicate
%% @returns Returns `true' if `Pred(Item)' returns `true' for at least one item `Item' in
%%          `Q', otherwise `false'.
%% @doc     Returns `true' if `Pred(Item)' returns `true' for at least one item `Item' in
%%          `Q', otherwise `false'.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `2> queue:any(fun (E) -> E > 10 end, Queue).'
%%          `false'
%%          `3> queue:any(fun (E) -> E > 3 end, Queue).'
%%          `true'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec any(Pred, Q :: queue(Item)) -> boolean() when
    Pred :: fun((Item) -> boolean()).
any(Pred, {R, F}) when is_function(Pred, 1), is_list(R), is_list(F) ->
    lists:any(Pred, F) orelse
        lists:any(Pred, R);
any(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

% How works:
% Check if all items satisfy the predicate, traverse in queue order.
% Since OTP 24
% O(len(Q)) worst case

%%-----------------------------------------------------------------------------
%% @param   Pred the predicate function to apply to each item
%% @param   Q the queue to check against the predicate
%% @returns Returns `true' if `Pred(Item)' returns `true' for all items `Item' in `Q',
%%          otherwise `false'.
%% @doc     Returns `true' if `Pred(Item)' returns `true' for all items `Item' in `Q',
%%          otherwise `false'.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `2> queue:all(fun (E) -> E > 3 end, Queue).'
%%          `false'
%%          `3> queue:all(fun (E) -> E > 0 end, Queue).'
%%          `true'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec all(Pred, Q :: queue(Item)) -> boolean() when
    Pred :: fun((Item) -> boolean()).
all(Pred, {R, F}) when is_function(Pred, 1), is_list(R), is_list(F) ->
    lists:all(Pred, F) andalso
        lists:all(Pred, R);
all(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

% How works:
% Delete the first occurrence of an item in the queue, according to queue order.
% Since OTP 24
% O(len(Q1)) worst case

%%-----------------------------------------------------------------------------
%% @param   Item the item to delete from the queue
%% @param   Q1 the queue from which the item will be deleted
%% @returns Returns a copy of `Q1' where the first item matching `Item' is deleted, if there
%%          is such an item.
%% @doc     Returns a copy of `Q1' where the first item matching `Item' is deleted, if there
%%          is such an item.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5]).'
%%          `2> Queue1 = queue:delete(3, Queue).'
%%          `3> queue:member(3, Queue1).'
%%          `false'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Item, Q1) -> Q2 when
    Item :: T,
    Q1 :: queue(T),
    Q2 :: queue(T),
    T :: term().
delete(Item, {R0, F0} = Q) when is_list(R0), is_list(F0) ->
    case delete_front(Item, F0) of
        false ->
            case delete_rear(Item, R0) of
                false ->
                    Q;
                [] ->
                    f2r(F0);
                R1 ->
                    {R1, F0}
            end;
        [] ->
            r2f(R0);
        F1 ->
            {R0, F1}
    end;
delete(Item, Q) ->
    erlang:error(badarg, [Item, Q]).

% How works:
% Delete the last occurrence of an item in the queue, according to queue order.
% Since OTP 24
% O(len(Q1)) worst case

%%-----------------------------------------------------------------------------
%% @param   Item the item to delete from the queue
%% @param   Q1 the queue from which the item will be deleted
%% @returns Returns a copy of `Q1' where the last item matching `Item' is deleted, if there
%%          is such an item.
%% @doc     Returns a copy of `Q1' where the last item matching `Item' is deleted, if there
%%          is such an item.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,3,5]).'
%%          `2> Queue1 = queue:delete_r(3, Queue).'
%%          `3> queue:to_list(Queue1).'
%%          `[1,2,3,4,5]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec delete_r(Item, Q1) -> Q2 when
    Item :: T,
    Q1 :: queue(T),
    Q2 :: queue(T),
    T :: term().
delete_r(Item, {R0, F0}) when is_list(R0), is_list(F0) ->
    {F1, R1} = delete(Item, {F0, R0}),
    {R1, F1};
delete_r(Item, Q) ->
    erlang:error(badarg, [Item, Q]).

delete_front(Item, [Item | Rest]) ->
    Rest;
delete_front(Item, [X | Rest]) ->
    case delete_front(Item, Rest) of
        false -> false;
        F -> [X | F]
    end;
delete_front(_, []) ->
    false.

delete_rear(Item, [X | Rest]) ->
    case delete_rear(Item, Rest) of
        false when X =:= Item ->
            Rest;
        false ->
            false;
        R ->
            [X | R]
    end;
delete_rear(_, []) ->
    false.

% How works:
% Delete the first occurrence of an item in the queue matching a predicate, according to queue order.
% Since OTP 24
% O(len(Q1)) worst case

%%-----------------------------------------------------------------------------
%% @param   Pred the predicate function to apply to each item
%% @param   Q1 the queue from which the item will be deleted
%% @returns Returns a copy of `Q1' where the first item for which `Pred' returns `true' is
%%          deleted, if there is such an item.
%% @doc     Returns a copy of `Q1' where the first item for which `Pred' returns `true' is
%%          deleted, if there is such an item.
%%
%%          Example:
%%          `1> Queue = queue:from_list([100,1,2,3,4,5]).'
%%          `2> Queue1 = queue:delete_with(fun (E) -> E > 0, Queue).'
%%          `3> queue:to_list(Queue1).'
%%          `[1,2,3,4,5]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec delete_with(Pred, Q1) -> Q2 when
    Pred :: fun((Item) -> boolean()),
    Q1 :: queue(Item),
    Q2 :: queue(Item),
    Item :: term().
delete_with(Pred, {R0, F0} = Q) when is_function(Pred, 1), is_list(R0), is_list(F0) ->
    case delete_with_front(Pred, F0) of
        false ->
            case delete_with_rear(Pred, R0) of
                false ->
                    Q;
                [] ->
                    f2r(F0);
                R1 ->
                    {R1, F0}
            end;
        [] ->
            r2f(R0);
        F1 ->
            {R0, F1}
    end;
delete_with(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

% How works:
% Delete the last occurrence of an item in the queue matching a predicate, according to queue order.
% Since OTP 24
% O(len(Q1)) worst case

%%-----------------------------------------------------------------------------
%% @param   Pred the predicate function to apply to each item
%% @param   Q1 the queue from which the item will be deleted
%% @returns Returns a copy of `Q1' where the last item for which `Pred' returns `true' is
%%          deleted, if there is such an item.
%% @doc     Returns a copy of `Q1' where the last item for which `Pred' returns `true' is
%%          deleted, if there is such an item.
%%
%%          Example:
%%          `1> Queue = queue:from_list([1,2,3,4,5,100]).'
%%          `2> Queue1 = queue:delete_with_r(fun (E) -> E > 10, Queue).'
%%          `3> queue:to_list(Queue1).'
%%          `[1,2,3,4,5]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec delete_with_r(Pred, Q1) -> Q2 when
    Pred :: fun((Item) -> boolean()),
    Q1 :: queue(Item),
    Q2 :: queue(Item),
    Item :: term().
delete_with_r(Pred, {R0, F0}) when is_function(Pred, 1), is_list(R0), is_list(F0) ->
    {F1, R1} = delete_with(Pred, {F0, R0}),
    {R1, F1};
delete_with_r(Pred, Q) ->
    erlang:error(badarg, [Pred, Q]).

delete_with_front(Pred, [X | Rest]) ->
    case Pred(X) of
        true ->
            Rest;
        false ->
            case delete_with_front(Pred, Rest) of
                false ->
                    false;
                F ->
                    [X | F]
            end
    end;
delete_with_front(_, []) ->
    false.

delete_with_rear(Pred, [X | Rest]) ->
    case delete_with_rear(Pred, Rest) of
        false ->
            case Pred(X) of
                true ->
                    Rest;
                false ->
                    false
            end;
        R ->
            [X | R]
    end;
delete_with_rear(_, []) ->
    false.

%%--------------------------------------------------------------------------
%% Internal workers

-compile({inline, [{r2f, 1}, {f2r, 1}]}).

%% Move half of elements from R to F, if there are at least three
r2f([]) ->
    {[], []};
r2f([_] = R) ->
    {[], R};
r2f([Y, X]) ->
    {[Y], [X]};
r2f(List) ->
    {RR, FF} = lists:split(length(List) div 2, List),
    {RR, lists:reverse(FF, [])}.

%% Move half of elements from F to R, if there are enough
f2r([]) ->
    {[], []};
f2r([_] = F) ->
    {F, []};
f2r([X, Y]) ->
    {[Y], [X]};
f2r(List) ->
    {FF, RR} = lists:split(length(List) div 2, List),
    {lists:reverse(RR, []), FF}.
