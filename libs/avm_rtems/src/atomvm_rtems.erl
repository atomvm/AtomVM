%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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
%% @doc RTEMS platform helpers.
%%
%% This module currently only exists so `avm_rtems` has a non-empty archive
%% that can be packed into `atomvmlib-rtems`. Platform-specific NIFs live in C.
%% @end
%%-----------------------------------------------------------------------------
-module(atomvm_rtems).

-export([platform/0]).

%% @doc Return the AtomVM platform atom for RTEMS.
-spec platform() -> rtems.
platform() ->
    atomvm:platform().
