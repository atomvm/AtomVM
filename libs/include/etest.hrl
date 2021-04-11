%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ASSERT_MATCH(A, B),
    case etest:assert_match(A, B) of
        ok -> ok;
        fail ->
            erlang:display({failed_assert_match, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, A, B}),
            fail
    end
).
-define(ASSERT_EQUALS(A, B),
    case etest:assert_equals(A, B) of
        ok -> ok;
        fail ->
            erlang:display({failed_assert_equals, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, A, B}),
            fail
    end
).
-define(ASSERT_TRUE(C),
    case etest:assert_true(C) of
        ok -> ok;
        fail ->
            erlang:display({failed_assert_true, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, C}),
            fail
    end
).
-define(ASSERT_FAILURE(A, E),
    case etest:assert_failure(fun() -> A end, E) of
        ok -> ok;
        fail ->
            erlang:display({failed_assert_failure, {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}, A, E}),
            fail
    end
).
