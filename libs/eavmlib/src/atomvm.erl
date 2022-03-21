%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Fred Dushin <fred@dushin.net>
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

%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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

%%-----------------------------------------------------------------------------
%% @doc AtomVM-specific APIs
%%
%% This module contains functions that are specific to the AtomVM platform.
%% @end
%%-----------------------------------------------------------------------------
-module(atomvm).

-export([
    platform/0,
    random/0,
    rand_bytes/1,
    read_priv/2
]).

-type platform_name() ::
    generic_unix
    | esp32
    | stm32.

%%-----------------------------------------------------------------------------
%% @returns The platform name.
%% @doc     Return the platform moniker.
%%          You may use this function to uniquely identify the platform
%%          type on which your application is running.
%% @end
%%-----------------------------------------------------------------------------
-spec platform() -> platform_name().
platform() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns random 32-bit integer.
%% @doc     Returns a random 32-bit integer value.
%%          This function will use a cryptographically strong RNG if available.
%%          Otherwise, the random value is generated using a PRNG.
%% @end
%%-----------------------------------------------------------------------------
-spec random() -> integer().
random() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Len non-negative integer
%% @returns Binary containing random sequence of bytes of length Len.
%% @doc     Returns a binary containing random sequence of bytes of length Len.
%%          Supplying a negative value will result in a badarg error.
%%          This function will use a cryptographically strong RNG if available.
%%          Otherwise, the random value is generated using a PRNG.
%% @end
%%-----------------------------------------------------------------------------
-spec rand_bytes(Len :: non_neg_integer()) -> binary().
rand_bytes(_Len) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   App application name.
%% @param   Path path to the resource.
%% @returns Binary containing the resource content.
%% @doc     This function allows to fetch priv/ resources content.
%% @end
%%-----------------------------------------------------------------------------
-spec read_priv(App :: atom(), Path :: list()) -> binary().
read_priv(_App, _Path) ->
    throw(nif_error).
