%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_system_architecture).
-export([start/0]).

start() ->
    SystemArchitecture = erlang:system_info(system_architecture),
    true = is_binary(SystemArchitecture),
    2 = count_hyphens(SystemArchitecture),
    % Ensure the OS part is indeed "zephyr"
    case binary:split(SystemArchitecture, <<"-">>, [global]) of
        [_Arch, _Vendor, <<"zephyr">>] -> ok;
        _ -> error
    end,
    ok.

count_hyphens(<<>>) -> 0;
count_hyphens(<<"-", Rest/binary>>) -> 1 + count_hyphens(Rest);
count_hyphens(<<_, Rest/binary>>) -> count_hyphens(Rest).
