%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_retention).
-export([start/0]).

start() ->
    StrictValidity = is_espressif(),
    Size = retention:size(),
    true = Size >= 16,
    ok = retention:clear(),
    ClearedValid = retention:is_valid(),
    ok = check_validity(StrictValidity, false, ClearedValid),
    Payload = <<"AtomVM-retained">>,
    ok = retention:write(1, Payload),
    WrittenValid = retention:is_valid(),
    ok = check_validity(StrictValidity, true, WrittenValid),
    {ok, Payload} = retention:read(1, byte_size(Payload)),
    {ok, <<0>>} = retention:read(0, 1),
    ok = retention:clear(),
    ok = check_validity(StrictValidity, false, retention:is_valid()),
    ok.

check_validity(true, Expected, Expected) -> ok;
check_validity(false, _Expected, Actual) when is_boolean(Actual) -> ok.

is_espressif() ->
    Architecture = erlang:system_info(system_architecture),
    case binary:split(Architecture, <<"-">>, [global]) of
        [_Architecture, Vendor | _] -> Vendor =:= <<"esp">>;
        _ -> false
    end.
