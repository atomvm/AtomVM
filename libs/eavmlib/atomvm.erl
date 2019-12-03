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

%%-----------------------------------------------------------------------------
%% @doc AtomVM-specific APIs
%%
%% This module contains functions that are specific to the AtomVM platform.
%% @end
%%-----------------------------------------------------------------------------
-module(atomvm).

-export([
    platform/0,
    random/0, rand_bytes/1
]).

-type platform_name() ::
    generic_unix |
    esp32 |
    stm32.

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
-spec rand_bytes(Len::non_neg_integer()) -> binary().
rand_bytes(_Len) ->
    throw(nif_error).
