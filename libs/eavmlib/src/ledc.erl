%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2020 by Fred Dushin <fred@dushin.net>                       %
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
%% @doc LED Controller low-level APIs
%%
%% The functions in this module broadly reflect the ESP IDF-SDK LED Controller API.
%%
%% See the IDF-SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC</a>
%% documentation for more information about these APIs.
%% @end
%%-----------------------------------------------------------------------------
-module(ledc).

-export([
    timer_config/1,
    channel_config/1,
    fade_func_install/1,
    fade_func_uninstall/0,
    set_fade_with_time/4,
    set_fade_with_step/5,
    fade_start/3,
    get_duty/2,
    set_duty/3,
    update_duty/2,
    get_freq/2,
    set_freq/3,
    stop/3
]).

-include("ledc.hrl").

-type duty_resolution() :: non_neg_integer().
-type duty_resolution_cfg() :: {duty_resolution, duty_resolution()}.
-type freq_hz() :: non_neg_integer().
-type freq_hz_cfg() :: {freq_hz, freq_hz()}.
-type speed_mode() :: ?LEDC_LOW_SPEED_MODE | ?LEDC_HIGH_SPEED_MODE.
-type speed_mode_cfg() :: {speed_mode, speed_mode()}.
-type timer_num() :: non_neg_integer().
-type timer_num_cfg() :: {timer_num, timer_num()}.

-type timer_config() :: [duty_resolution_cfg() | freq_hz_cfg() | speed_mode_cfg() | timer_num_cfg()].

-type channel() :: non_neg_integer().
-type channel_cfg() :: {channel, channel()}.
-type duty() :: non_neg_integer().
-type duty_cfg() :: {duty, duty()}.
-type gpio_num() :: non_neg_integer().
-type gpio_num_cfg() :: {gpio_num, gpio_num()}.
-type hpoint() :: non_neg_integer().
-type hpoint_cfg() :: {hpoint, hpoint()}.
-type timer_sel() :: non_neg_integer().
-type timer_sel_cfg() :: {timer_sel, timer_sel()}.

-type channel_config() :: [channel_cfg() | duty_cfg() | gpio_num_cfg() | speed_mode_cfg() | hpoint_cfg() | timer_sel_cfg()].

-type ledc_error_code() :: non_neg_integer().
-type fade_mode() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% @param   Config      timer configuration
%% @returns ok | {error, ledc_error_code()}
%% @doc     LEDC timer configuration.
%%
%%          Configure LEDC timer with the given source timer/frequency(Hz)/duty_resolution.
%% @end
%%-----------------------------------------------------------------------------
-spec channel_config(Config::channel_config()) -> ok | {error, ledc_error_code()}.
channel_config(_Config) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Config      timer configuration
%% @returns ok | {error, Reason}
%% @doc     LEDC timer configuration.
%%
%%          Configure LEDC timer with the given source timer/frequency(Hz)/duty_resolution.
%% @end
%%-----------------------------------------------------------------------------
-spec timer_config(Config::timer_config()) -> ok | {error, ledc_error_code()}.
timer_config(_Config) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Flags   Flags used to allocate the interrupt. One or multiple (ORred)
%%                  ESP_INTR_FLAG_* values. See esp_intr_alloc.h for more info.
%% @returns         ok | {error, ledc_error_code()}
%% @doc     Install LEDC fade function.
%%
%%          This function will occupy interrupt of LEDC module.
%% @end
%%-----------------------------------------------------------------------------
-spec fade_func_install(Flags::non_neg_integer()) -> ok | {error, ledc_error_code()}.
fade_func_install(_Flags) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Uninstall LEDC fade function.
%% @end
%%-----------------------------------------------------------------------------
-spec fade_func_uninstall() -> ok.
fade_func_uninstall() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%%                      Note that not all targets support high speed mode.
%% @param   Channel     LEDC channel index (0-7).
%% @param   TargetDuty  Target duty of fading.(0..(2^duty_resolution-1)))
%% @param   MaxFadeTimeMs The maximum time of the fading (ms).
%% @returns         ok | {error, ledc_error_code()}
%% @doc     Set LEDC fade function, with a limited time.
%%
%%          Note. Call ledc:fade_func_install() once before calling this function.
%%          Call ledc:fade_start() after this to start fading.
%% @end
%%-----------------------------------------------------------------------------
-spec set_fade_with_time(SpeedMode::speed_mode(), Channel::channel(), TargetDuty::non_neg_integer(), MaxFadeTimeMs::non_neg_integer()) ->
    ok | {error, ledc_error_code()}.
set_fade_with_time(_SpeedMode, _Channel, _TargetDuty, _MaxFadeTimeMs) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%%                      Note that not all targets support high speed mode.
%% @param   Channel     LEDC channel index (0-7).
%% @param   TargetDuty  Target duty of fading.(0..(2^duty_resolution-1)))
%% @param   Scale       Controls the increase or decrease step scale.
%% @param   CycleNum    increase or decrease the duty every cycle_num cycles
%% @returns         ok | {error, ledc_error_code()}
%% @doc     Set LEDC fade function
%%
%%          Note. Call ledc:fade_func_install() once before calling this function.
%%          Call ledc:fade_start() after this to start fading.
%% @end
%%-----------------------------------------------------------------------------
-spec set_fade_with_step(SpeedMode::speed_mode(), Channel::channel(),
    TargetDuty::non_neg_integer(), Scale::non_neg_integer(), CycleNum::non_neg_integer()
    ) -> ok | {error, ledc_error_code()}.
set_fade_with_step(_SpeedMode, _Channel, _TargetDuty, _Scale, _CycleNum) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%%                      Note that not all targets support high speed mode.
%% @param   Channel     LEDC channel index (0-7).
%% @param   FadeMode    Whether to block until fading done.
%% @returns ok | {error, ledc_error_code()}
%% @doc     Start LEDC fading.
%%
%%          Note. Call ledc:fade_func_install() once before calling this function.
%%          Call ledc:fade_start() after this to start fading.
%% @end
%%-----------------------------------------------------------------------------
-spec fade_start(SpeedMode::speed_mode(), Channel::channel(), FadeMode::fade_mode()) ->
    ok | {error, ledc_error_code()}.
fade_start(_SpeedMode, _Channel, _FadeMode) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%%                      Note that not all targets support high speed mode.
%% @param   Channel     LEDC channel index (0-7).
%% @returns ok | {error, ledc_error_code()}
%% @doc     LEDC get duty.
%% @end
%%-----------------------------------------------------------------------------
-spec get_duty(SpeedMode::speed_mode(), Channel::channel()) -> ok | {error, ledc_error_code()}.
get_duty(_SpeedMode, _Channel) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%%                      Note that not all targets support high speed mode.
%% @param   Channel     LEDC channel index (0-7).
%% @param   Duty        Set the LEDC duty, the range of duty setting is [0, (2**duty_resolution)]
%% @returns ok | {error, ledc_error_code()}
%% @doc     LEDC set duty.
%% @end
%%-----------------------------------------------------------------------------
-spec set_duty(SpeedMode::speed_mode(), Channel::channel(), Duty::non_neg_integer()) -> ok | {error, ledc_error_code()}.
set_duty(_SpeedMode, _Channel, _Duty) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%%                      Note that not all targets support high speed mode.
%% @param   Channel     LEDC channel index (0-7).
%% @returns ok | {error, ledc_error_code()}
%% @doc     LEDC update channel parameters.
%% @end
%%-----------------------------------------------------------------------------
-spec update_duty(SpeedMode::speed_mode(), Channel::channel()) -> ok | {error, ledc_error_code()}.
update_duty(_SpeedMode, _Channel) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%% @param   TimerNum    LEDC timer index (0-3).
%% @returns ok | {error, ledc_error_code()}
%% @doc     LEDC get channel frequency (Hz)
%% @end
%%-----------------------------------------------------------------------------
-spec get_freq(SpeedMode::speed_mode(), TimerNum::timer_num()) -> ok | {error, ledc_error_code()}.
get_freq(_SpeedMode, _TimerNum) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%% @param   TimerNum    LEDC timer index (0-3).
%% @returns ok | {error, ledc_error_code()}
%% @doc     LEDC get channel frequency (Hz)
%% @end
%%-----------------------------------------------------------------------------
-spec set_freq(SpeedMode::speed_mode(), TimerNum::timer_num(), FreqHz::non_neg_integer()) -> ok | {error, ledc_error_code()}.
set_freq(_SpeedMode, _TimerNum, _FreqHz) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   SpeedMode   Select the LEDC channel group with specified speed mode.
%%                      Note that not all targets support high speed mode.
%% @param   Channel     LEDC channel index (0-7).
%% @param   IdleLevel   Set output idle level after LEDC stops.
%% @returns ok | {error, ledc_error_code()}
%% @doc     LEDC stop. Disable LEDC output, and set idle level.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(SpeedMode::speed_mode(), Channel::channel(), IdleLevel::non_neg_integer()) -> ok | {error, ledc_error_code()}.
stop(_SpeedMode, _Channel, _IdleLevel) ->
    throw(nif_error).
