%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(nested_list_size4).

-export([start/0]).

start() ->
    erts_debug:flat_size(make_nested()).

%% erlfmt-ignore
make_nested() ->
    [
     [
      [
       [
        [
         [
          [
           [
            [
             [
              [
               [
                [
                 [
                  [
                   [
                    [
                     [
                      [
                       [
                        [
                         [
                          [
                           [
                            [
                             [
                              [
                               [
                                [
                                 [
                                  [
                                   [
                                    [
                                     [[[[[[1]]]]], [[2]]]
                                    ], [[3, [a]], 30]
                                   ], [4, [40, [b]]]
                                  ], [[5, [c]], 50]
                                 ], [6, [60, [d]]]
                                ], [[7, [e]], 70]
                               ], [8, [80, [f]]]
                              ], [[9, [g]], 90]
                             ], [10, [100, [h]]]
                            ], [[11, [i]], 110]
                           ], [12, [120, [j]]]
                          ], [[13, [k]], 130]
                         ], [14, [140, [l]]]
                        ], [[15, [m]], 150]
                       ], [16, [160, [n]]]
                      ], [[17, [o]], 170]
                     ], [18, [180, [p]]]
                    ], [[19, [q]], 190]
                   ], [20, [200, [r]]]
                  ], [[21, [s]], 210]
                 ], [22, [220, [t]]]
                ], [[23, [u]], 230]
               ], [24, [240, [v]]]
              ], [[25, [w]], 250]
             ], 26
            ], 27
           ], 28
          ], 29
         ], [30]
        ], [[31]]
       ], [[[32]]]
      ], [[[[33]]]]
     ], [[[[[34]]]]]
    ].
