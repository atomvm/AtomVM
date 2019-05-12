%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 %
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

-module(spi).
-export([open/1, read_at/3, write_at/4]).

open(Param) ->
    open_port({spawn, "spi"}, Param).

read_at(SPI, Address, Len) ->
    SPI ! {self(), make_ref(), {read_at, Address, Len}},
    receive
        Ret ->
            Ret
    end.

write_at(SPI, Address, Len, Data) ->
    SPI ! {self(), make_ref(), {write_at, Address bor 16#80, Len, Data}},
    receive
        Ret ->
            Ret
    end.
