%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License")
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
-module(math).

-export([
    cos/1,
    acos/1,
    acosh/1,
    asin/1,
    asinh/1,
    atan/1,
    atan2/2,
    atanh/1,
    ceil/1,
    cosh/1,
    exp/1,
    floor/1,
    fmod/2,
    log/1,
    log10/1,
    log2/1,
    pow/2,
    sin/1,
    sinh/1,
    sqrt/1,
    tan/1,
    tanh/1,
    pi/0
]).

-spec cos(X :: number()) -> float().
cos(_X) ->
    throw(nif_error).

-spec acos(X :: number()) -> float().
acos(_X) ->
    throw(nif_error).

-spec acosh(X :: number()) -> float().
acosh(_X) ->
    throw(nif_error).

-spec asin(X :: number()) -> float().
asin(_X) ->
    throw(nif_error).

-spec asinh(X :: number()) -> float().
asinh(_X) ->
    throw(nif_error).

-spec atan(X :: number()) -> float().
atan(_X) ->
    throw(nif_error).

-spec atan2(Y :: number(), X :: number()) -> float().
atan2(_Y, _X) ->
    throw(nif_error).

-spec atanh(X :: number()) -> float().
atanh(_X) ->
    throw(nif_error).

-spec ceil(X :: number()) -> float().
ceil(_X) ->
    throw(nif_error).

-spec cosh(X :: number()) -> float().
cosh(_X) ->
    throw(nif_error).

-spec exp(X :: number()) -> float().
exp(_X) ->
    throw(nif_error).

-spec floor(X :: number()) -> float().
floor(_X) ->
    throw(nif_error).

-spec fmod(X :: number(), Y :: number()) -> float().
fmod(_X, _Y) ->
    throw(nif_error).

-spec log(X :: number()) -> float().
log(_X) ->
    throw(nif_error).

-spec log10(X :: number()) -> float().
log10(_X) ->
    throw(nif_error).

-spec log2(X :: number()) -> float().
log2(_X) ->
    throw(nif_error).

-spec pow(X :: number(), Y :: number()) -> float().
pow(_X, _Y) ->
    throw(nif_error).

-spec sin(X :: number()) -> float().
sin(_X) ->
    throw(nif_error).

-spec sinh(X :: number()) -> float().
sinh(_X) ->
    throw(nif_error).

-spec sqrt(X :: number()) -> float().
sqrt(_X) ->
    throw(nif_error).

-spec tan(X :: number()) -> float().
tan(_X) ->
    throw(nif_error).

-spec tanh(X :: number()) -> float().
tanh(_X) ->
    throw(nif_error).

-spec pi() -> float().
pi() ->
    3.141592653589793.
