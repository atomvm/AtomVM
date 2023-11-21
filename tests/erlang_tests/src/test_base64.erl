%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_base64).

-export([start/0]).

%% erlfmt-ignore
start() ->
    %% simple tests (easy to debug)
    verify_b64(<<"">>, <<"">>),
    verify_b64(<<1, 2, 3>>, <<"AQID">>),
    verify_b64(<<1, 2, 3, 4>>, <<"AQIDBA==">>),
    verify_b64(<<1, 2, 3, 4, 5>>, <<"AQIDBAU=">>),
    verify_b64(<<1, 2, 3, 4, 5, 6>>, <<"AQIDBAUG">>),

    %% test against some random entries generated from OTP via
    %% L = [crypto:strong_rand_bytes(I) || I <- lists:seq(0,20)].
    %% [{E, base64:encode(E)} || E <- L]
    RandomEntries = [
        {<<>>,<<>>},
        {<<"Â">>,<<"wg==">>},
        {<<"µb">>,<<"tWI=">>},
        {<<"YãH">>,<<"WeNI">>},
        {<<"¢I\f<">>,<<"okkMPA==">>},
        {<<"ÿßæ´L">>,<<"/9/mtEw=">>},
        {<<252,112,131,64,138,139>>,<<"/HCDQIqL">>},
        {<<135,167,68,16,70,100,110>>,<<"h6dEEEZkbg==">>},
        {<<12,227,186,221,35,228,29,171>>,<<"DOO63SPkHas=">>},
        {<<47,158,206,66,251,170,81,134,229>>,<<"L57OQvuqUYbl">>},
        {<<226,246,35,243,215,128,215,253,125,143>>, <<"4vYj89eA1/19jw==">>},
        {<<217,65,59,149,152,174,114,216,6,137,56>>, <<"2UE7lZiuctgGiTg=">>},
        {<<252,87,132,165,80,235,132,47,235,27,132,6>>, <<"/FeEpVDrhC/rG4QG">>},
        {<<227,16,167,184,46,129,119,108,218,245,1,129,45>>,  <<"4xCnuC6Bd2za9QGBLQ==">>},
        {<<67,21,60,37,89,149,96,46,156,36,122,186,91,115>>, <<"QxU8JVmVYC6cJHq6W3M=">>},
        {<<35,235,3,216,183,47,111,48,65,37,212,86,133,132,153>>, <<"I+sD2LcvbzBBJdRWhYSZ">>},
        {<<128,94,25,153,53,246,23,93,146,130,136,191,115,204,81,25>>, <<"gF4ZmTX2F12Sgoi/c8xRGQ==">>},
        {<<180,177,165,83,215,106,14,96,96,208,219,55,52,197,78,65,4>>, <<"tLGlU9dqDmBg0Ns3NMVOQQQ=">>},
        {<<218,98,97,28,235,173,247,81,90,217,205,114,65,120,130,104,208,206>>, <<"2mJhHOut91Fa2c1yQXiCaNDO">>},
        {<<49,242,114,177,206,35,163,178,51,148,61,69,157,110,19,155,214,30,134>>,<<"MfJysc4jo7IzlD1FnW4Tm9Yehg==">>},
        {<<109,1,224,147,38,223,203,238,33,219,84,2,149,110,207,208,246,163,183,19>>,<<"bQHgkybfy+4h21QClW7P0PajtxM=">>}],
    [verify_b64(Value, ExpectedEncoding) || {Value, ExpectedEncoding} <- RandomEntries],

    %% test against some randomly generated inputs
    [verify_b64(rand_bytes(I), undefined) || I <- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,50,100,131,147,200,201,217,500,517]],

    % expected errors
    expect_error(fun() -> base64:encode(foo) end, badarg),
    expect_error(fun() -> base64:decode(foo) end, badarg),
    expect_error(fun() -> base64:decode(<<"A">>) end, badarg),
    expect_error(fun() -> base64:decode(<<"A%ID">>) end, badarg),
    expect_error(fun() -> base64:decode(<<"A ID">>) end, badarg),

    % it turns out we actually support iolists, which is kind of nice
    case erlang:system_info(machine) of
        "BEAM" ->
            expect_error(fun() -> base64:encode([<<1, 2>>, <<3, 4, 5>>, <<6>>]) end, badarith),
            expect_error(fun() -> base64:encode([<<1, 2>>, <<3, 4, 5>>, <<6>>]) end, badarith),
            expect_error(fun() -> base64:decode(["AQ", "ID", "BAUG"]) end, badarg),
            expect_error(fun() -> base64:decode(["AQ", "ID", <<"BAUG">>]) end, badarg);
        _ ->
            <<"AQIDBAUG">> = base64:encode([<<1, 2>>, <<3, 4, 5>>, <<6>>]),
            <<"AQIDBAUG">> = base64:encode([<<1, 2>>, <<3, 4, 5>>, 6]),
            <<1, 2, 3, 4, 5, 6>> = base64:decode(["AQ", "ID", "BAUG"]),
            <<1, 2, 3, 4, 5, 6>> = base64:decode(["AQ", "ID", <<"BAUG">>])
    end,
    0.

rand_bytes(I) ->
    case erlang:system_info(machine) of
        "BEAM" -> crypto:strong_rand_bytes(I);
        _ -> atomvm:rand_bytes(I)
    end.

verify_b64(Input, ExpectedEncoding) ->
    %erlang:display({Input, ExpectedEncoding}),
    Encoded = base64:encode(Input),
    Encoded = base64:encode(binary_to_list(Input)),
    %erlang:display({encoded, Encoded}),
    EncodedString = base64:encode_to_string(Input),
    EncodedString = base64:encode_to_string(binary_to_list(Input)),
    verify_value(Encoded, ExpectedEncoding),
    Decoded = base64:decode(Encoded),
    Decoded = base64:decode(binary_to_list(Encoded)),
    %erlang:display({decoded, Decoded}),
    DecodedString = base64:decode_to_string(Encoded),
    DecodedString = base64:decode_to_string(binary_to_list(Encoded)),
    Input = Decoded.

verify_value(_Value, undefined) ->
    ok;
verify_value(Value, ExpectedValue) ->
    Value = ExpectedValue.

expect_error(F, _Reason) ->
    error =
        try
            F(),
            ok
        catch
            _E:_R ->
                %% TODO E doesn't seem to match error and R doesn't seem to match Reason
                %% even through they display the same
                %% erlang:display({E, R}),
                %% E = error,
                %% R = Reason,
                error
        end.
