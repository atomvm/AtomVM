%
% This file is part of AtomVM.
%
% Copyright 2023 Winford <winford@object.stream>
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

%% STM32F4 Family configurations
-define(STM32F401_E, #{
    rom => "ROM_512K",
    ram => "RAM_64K",
    clock => "84000000",
    flash_size_kb => "512",
    ram_size_kb => "64"
}).
-define(STM32F4_01_57_E, #{
    rom => "ROM_512K",
    ram => "RAM_128K",
    ccm => "CCM_64K",
    clock => "168000000",
    flash_size_kb => "512",
    ram_size_kb => "128"
}).
-define(STM32F4_01_57_G, #{
    rom => "ROM_1024K",
    ram => "RAM_128K",
    ccm => "CCM_64K",
    clock => "168000000",
    flash_size_kb => "1024",
    ram_size_kb => "128"
}).
-define(STM32F411_E, #{
    rom => "ROM_512K",
    ram => "RAM_128K",
    clock => "100000000",
    flash_size_kb => "512",
    ram_size_kb => "128"
}).
-define(STM32F412_E, #{
    rom => "ROM_512K",
    ram => "RAM_256K",
    clock => "100000000",
    flash_size_kb => "512",
    ram_size_kb => "256"
}).
-define(STM32F412_G, #{
    rom => "ROM_1024K",
    ram => "RAM_256K",
    clock => "100000000",
    flash_size_kb => "1024",
    ram_size_kb => "256"
}).
-define(STM32F4_12_3_G, #{
    rom => "ROM_1024K",
    ram => "RAM_256K",
    ccm => "CCM_64K",
    clock => "100000000",
    flash_size_kb => "1024",
    ram_size_kb => "256"
}).
-define(STM32F4_12_3_H, #{
    rom => "ROM_1536K",
    ram => "RAM_256K",
    ccm => "CCM_64K",
    clock => "100000000",
    flash_size_kb => "1536",
    ram_size_kb => "256"
}).
-define(STM32F4_23_79_E, #{
    rom => "ROM_512K",
    ram => "RAM_192K",
    ccm => "CCM_64K",
    clock => "180000000",
    flash_size_kb => "512",
    ram_size_kb => "192"
}).
-define(STM32F4_23_79_G, #{
    rom => "ROM_1024K",
    ram => "RAM_192K",
    ccm => "CCM_64K",
    clock => "180000000",
    flash_size_kb => "1024",
    ram_size_kb => "192"
}).
-define(STM32F4_23_79_I, #{
    rom => "ROM_2048K",
    ram => "RAM_192K",
    ccm => "CCM_64K",
    clock => "180000000",
    flash_size_kb => "2048",
    ram_size_kb => "192"
}).
-define(STM32F446_E, #{
    rom => "ROM_512K",
    ram => "RAM_128K",
    clock => "180000000",
    flash_size_kb => "512",
    ram_size_kb => "128"
}).
-define(STM32F4_67_9_E, #{
    rom => "ROM_512K",
    ram => "RAM_320K",
    ccm => "CCM_64K",
    clock => "180000000",
    flash_size_kb => "512",
    ram_size_kb => "320"
}).
-define(STM32F4_67_9_G, #{
    rom => "ROM_1024K",
    ram => "RAM_320K",
    ccm => "CCM_64K",
    clock => "180000000",
    flash_size_kb => "1024",
    ram_size_kb => "320"
}).
-define(STM32F4_67_9_I, #{
    rom => "ROM_2048K",
    ram => "RAM_320K",
    ccm => "CCM_64K",
    clock => "180000000",
    flash_size_kb => "2048",
    ram_size_kb => "320"
}).

%% STM32H7 Family configurations
-define(STM32H743_I, #{
    rom => "ROM_2048K",
    ram => "RAM_512K",
    clock => "480000000",
    flash_size_kb => "2048",
    ram_size_kb => "512"
}).
-define(STM32H743_G, #{
    rom => "ROM_1024K",
    ram => "RAM_512K",
    clock => "480000000",
    flash_size_kb => "1024",
    ram_size_kb => "512"
}).

%% STM32U5 Family configurations
-define(STM32U585_I, #{
    rom => "ROM_2048K",
    ram => "RAM_768K",
    clock => "160000000",
    flash_size_kb => "2048",
    ram_size_kb => "768"
}).

%% STM32WB Family configurations
-define(STM32WB55_G, #{
    rom => "ROM_1024K",
    ram => "RAM_256K",
    clock => "64000000",
    flash_size_kb => "1024",
    ram_size_kb => "256"
}).
-define(STM32WB55_E, #{
    rom => "ROM_512K",
    ram => "RAM_256K",
    clock => "64000000",
    flash_size_kb => "512",
    ram_size_kb => "256"
}).

%% STM32H5 Family configurations
-define(STM32H562_G, #{
    rom => "ROM_1024K",
    ram => "RAM_640K",
    clock => "250000000",
    flash_size_kb => "1024",
    ram_size_kb => "640"
}).
-define(STM32H562_I, #{
    rom => "ROM_2048K",
    ram => "RAM_640K",
    clock => "250000000",
    flash_size_kb => "2048",
    ram_size_kb => "640"
}).

%% STM32F7 Family configurations
-define(STM32F72_73_E, #{
    rom => "ROM_512K",
    ram => "RAM_256K",
    clock => "216000000",
    flash_size_kb => "512",
    ram_size_kb => "256"
}).
-define(STM32F74_75_E, #{
    rom => "ROM_512K",
    ram => "RAM_320K",
    clock => "216000000",
    flash_size_kb => "512",
    ram_size_kb => "320"
}).
-define(STM32F74_75_G, #{
    rom => "ROM_1024K",
    ram => "RAM_320K",
    clock => "216000000",
    flash_size_kb => "1024",
    ram_size_kb => "320"
}).
-define(STM32F76_77_E, #{
    rom => "ROM_512K",
    ram => "RAM_512K",
    clock => "216000000",
    flash_size_kb => "512",
    ram_size_kb => "512"
}).
-define(STM32F76_77_G, #{
    rom => "ROM_1024K",
    ram => "RAM_512K",
    clock => "216000000",
    flash_size_kb => "1024",
    ram_size_kb => "512"
}).
-define(STM32F76_77_I, #{
    rom => "ROM_2048K",
    ram => "RAM_512K",
    clock => "216000000",
    flash_size_kb => "2048",
    ram_size_kb => "512"
}).

%% STM32G0 Family configurations
-define(STM32G0B1_E, #{
    rom => "ROM_512K",
    ram => "RAM_144K",
    clock => "64000000",
    flash_size_kb => "512",
    ram_size_kb => "144"
}).
-define(STM32G071_B, #{
    rom => "ROM_128K",
    ram => "RAM_36K",
    clock => "64000000",
    flash_size_kb => "128",
    ram_size_kb => "36"
}).

%% STM32G4 Family configurations
-define(STM32G47_48_E, #{
    rom => "ROM_512K",
    ram => "RAM_128K",
    clock => "170000000",
    flash_size_kb => "512",
    ram_size_kb => "128"
}).
-define(STM32G49_4A_E, #{
    rom => "ROM_512K",
    ram => "RAM_112K",
    clock => "170000000",
    flash_size_kb => "512",
    ram_size_kb => "112"
}).

%% STM32L4 Family configurations
-define(STM32L45_46_E, #{
    rom => "ROM_512K",
    ram => "RAM_160K",
    clock => "80000000",
    flash_size_kb => "512",
    ram_size_kb => "160"
}).
-define(STM32L47_E, #{
    rom => "ROM_512K",
    ram => "RAM_128K",
    clock => "80000000",
    flash_size_kb => "512",
    ram_size_kb => "128"
}).
-define(STM32L47_48_G, #{
    rom => "ROM_1024K",
    ram => "RAM_128K",
    clock => "80000000",
    flash_size_kb => "1024",
    ram_size_kb => "128"
}).
-define(STM32L49_4A_G, #{
    rom => "ROM_1024K",
    ram => "RAM_320K",
    clock => "80000000",
    flash_size_kb => "1024",
    ram_size_kb => "320"
}).
-define(STM32L4R_4S_G, #{
    rom => "ROM_1024K",
    ram => "RAM_640K",
    clock => "120000000",
    flash_size_kb => "1024",
    ram_size_kb => "640"
}).
-define(STM32L4R_4S_I, #{
    rom => "ROM_2048K",
    ram => "RAM_640K",
    clock => "120000000",
    flash_size_kb => "2048",
    ram_size_kb => "640"
}).

%% STM32L5 Family configurations
-define(STM32L55_56_E, #{
    rom => "ROM_512K",
    ram => "RAM_256K",
    clock => "110000000",
    flash_size_kb => "512",
    ram_size_kb => "256"
}).

%% STM32F2 Family configurations
-define(STM32F2_E, #{
    rom => "ROM_512K",
    ram => "RAM_128K",
    clock => "120000000",
    flash_size_kb => "512",
    ram_size_kb => "128"
}).
-define(STM32F2_F, #{
    rom => "ROM_768K",
    ram => "RAM_128K",
    clock => "120000000",
    flash_size_kb => "768",
    ram_size_kb => "128"
}).
-define(STM32F2_G, #{
    rom => "ROM_1024K",
    ram => "RAM_128K",
    clock => "120000000",
    flash_size_kb => "1024",
    ram_size_kb => "128"
}).

%% STM32U3 Family configurations
-define(STM32U37_38_E, #{
    rom => "ROM_512K",
    ram => "RAM_256K",
    clock => "96000000",
    flash_size_kb => "512",
    ram_size_kb => "256"
}).
-define(STM32U37_38_G, #{
    rom => "ROM_1024K",
    ram => "RAM_256K",
    clock => "96000000",
    flash_size_kb => "1024",
    ram_size_kb => "256"
}).
