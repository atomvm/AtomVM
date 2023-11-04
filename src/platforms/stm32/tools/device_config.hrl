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
-define(STM32F401_E, #{rom => "ROM_512K", ram => "RAM_64K", clock => "MHZ_84"}).
-define(STM32F4_01_57_E, #{
    rom => "ROM_512K", ram => "RAM_128K", ccm => "CCM_64K", clock => "MHZ_168"
}).
-define(STM32F4_01_57_G, #{
    rom => "ROM_1024K", ram => "RAM_128K", ccm => "CCM_64K", clock => "MHZ_168"
}).
-define(STM32F411_E, #{rom => "ROM_512K", ram => "RAM_128K", clock => "MHZ_100"}).
-define(STM32F412_E, #{rom => "ROM_512K", ram => "RAM_256K", clock => "MHZ_100"}).
-define(STM32F412_G, #{rom => "ROM_1024K", ram => "RAM_256K", clock => "MHZ_100"}).
-define(STM32F4_12_3_G, #{
    rom => "ROM_1024K", ram => "RAM_256K", ccm => "CCM_64K", clock => "MHZ_100"
}).
-define(STM32F4_12_3_H, #{
    rom => "ROM_1536K", ram => "RAM_256K", ccm => "CCM_64K", clock => "MHZ_100"
}).
-define(STM32F4_23_79_E, #{
    rom => "ROM_512K", ram => "RAM_192K", ccm => "CCM_64K", clock => "MHZ_180"
}).
-define(STM32F4_23_79_G, #{
    rom => "ROM_1024K", ram => "RAM_192K", ccm => "CCM_64K", clock => "MHZ_180"
}).
-define(STM32F4_23_79_I, #{
    rom => "ROM_2048K", ram => "RAM_192K", ccm => "CCM_64K", clock => "MHZ_180"
}).
-define(STM32F446_E, #{rom => "ROM_512K", ram => "RAM_128K", clock => "MHZ_180"}).
-define(STM32F4_67_9_E, #{
    rom => "ROM_512K", ram => "RAM_320K", ccm => "CCM_64K", clock => "MHZ_180"
}).
-define(STM32F4_67_9_G, #{
    rom => "ROM_1024K", ram => "RAM_320K", ccm => "CCM_64K", clock => "MHZ_180"
}).
-define(STM32F4_67_9_I, #{
    rom => "ROM_2048K", ram => "RAM_320K", ccm => "CCM_64K", clock => "MHZ_180"
}).

%% STM32F7 Family configurations
-define(STM32F7_23_23_E, #{
    rom => "ROM_512K", ram => "RAM_192K", ccm => "CCM_64K", clock => "MHZ_216"
}).
-define(STM32F745_E, #{rom => "ROM_512K", ram => "RAM_256K", ccm => "CCM_64K", clock => "MHZ_216"}).
-define(STM32F745_G, #{rom => "ROM_1024K", ram => "RAM_256K", ccm => "CCM_64K", clock => "MHZ_216"}).
-define(STM32F765_G, #{rom => "ROM_1024K", ram => "RAM_384K", ccm => "CCM_128K", clock => "MHZ_216"}).
-define(STM32F765_I, #{rom => "ROM_2048K", ram => "RAM_384K", ccm => "CCM_128K", clock => "MHZ_216"}).
-define(STM32F7_45_6_E, #{
    rom => "ROM_512K", ram => "RAM_256K", ccm => "CCM_64K", clock => "MHZ_216"
}).
-define(STM32F7_45_6_G, #{
    rom => "ROM_1024K", ram => "RAM_256K", ccm => "CCM_64K", clock => "MHZ_216"
}).
-define(STM32F7_67_7_G, #{
    rom => "ROM_1024K", ram => "RAM_384K", ccm => "CCM_128K", clock => "MHZ_216"
}).
-define(STM32F7_67_7_I, #{
    rom => "ROM_2048K", ram => "RAM_384K", ccm => "CCM_128K", clock => "MHZ_216"
}).
-define(STM32F769_G, #{rom => "ROM_1024K", ram => "RAM_384K", ccm => "CCM_128K", clock => "MHZ_216"}).
-define(STM32F7_67_89_I, #{
    rom => "ROM_2048K", ram => "RAM_384K", ccm => "CCM_128K", clock => "MHZ_216"
}).
