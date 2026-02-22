%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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
%% @doc emscripten API
%%
%% The functions in this module broadly reflect emscripten's API and obviously
%% are only implemented for the emscripten platform.
%%
%% See Emscripten's <a href="https://emscripten.org/docs/api_reference/index.html">API documentation</a>
%% for more information about these APIs.
%%
%% The counterpart of functions defined in this module are two main Javascript
%% functions that can be used to send messages to Erlang.
%%
%% ```
%% Module.cast('some_proc', 'message')
%% await Module.call('some_proc', 'message')
%% '''
%%
%% These respectively send the following messages to Erlang process registered as `some_proc':
%% ```
%% {emscripten, {cast, <<"message">>}}
%% {emscripten, {call, Promise, <<"message">>}}
%% '''
%% Promise should be passed to `promise_resolve/1,2' or `promise_reject/1,2' as
%% documented below.
%%
%% @end
%%-----------------------------------------------------------------------------
-module(emscripten).
-export([
    run_script/1,
    run_script/2,
    promise_resolve/1,
    promise_resolve/2,
    promise_reject/1,
    promise_reject/2,

    register_keypress_callback/1,
    register_keypress_callback/2,
    register_keypress_callback/3,
    unregister_keypress_callback/1,
    register_keydown_callback/1,
    register_keydown_callback/2,
    register_keydown_callback/3,
    unregister_keydown_callback/1,
    register_keyup_callback/1,
    register_keyup_callback/2,
    register_keyup_callback/3,
    unregister_keyup_callback/1,

    register_click_callback/1,
    register_click_callback/2,
    register_click_callback/3,
    unregister_click_callback/1,
    register_mousedown_callback/1,
    register_mousedown_callback/2,
    register_mousedown_callback/3,
    unregister_mousedown_callback/1,
    register_mouseup_callback/1,
    register_mouseup_callback/2,
    register_mouseup_callback/3,
    unregister_mouseup_callback/1,
    register_dblclick_callback/1,
    register_dblclick_callback/2,
    register_dblclick_callback/3,
    unregister_dblclick_callback/1,
    register_mousemove_callback/1,
    register_mousemove_callback/2,
    register_mousemove_callback/3,
    unregister_mousemove_callback/1,
    register_mouseenter_callback/1,
    register_mouseenter_callback/2,
    register_mouseenter_callback/3,
    unregister_mouseenter_callback/1,
    register_mouseleave_callback/1,
    register_mouseleave_callback/2,
    register_mouseleave_callback/3,
    unregister_mouseleave_callback/1,
    register_mouseover_callback/1,
    register_mouseover_callback/2,
    register_mouseover_callback/3,
    unregister_mouseover_callback/1,
    register_mouseout_callback/1,
    register_mouseout_callback/2,
    register_mouseout_callback/3,
    unregister_mouseout_callback/1,

    register_wheel_callback/1,
    register_wheel_callback/2,
    register_wheel_callback/3,
    unregister_wheel_callback/1,

    register_resize_callback/1,
    register_resize_callback/2,
    register_resize_callback/3,
    unregister_resize_callback/1,
    register_scroll_callback/1,
    register_scroll_callback/2,
    register_scroll_callback/3,
    unregister_scroll_callback/1,

    register_blur_callback/1,
    register_blur_callback/2,
    register_blur_callback/3,
    unregister_blur_callback/1,
    register_focus_callback/1,
    register_focus_callback/2,
    register_focus_callback/3,
    unregister_focus_callback/1,
    register_focusin_callback/1,
    register_focusin_callback/2,
    register_focusin_callback/3,
    unregister_focusin_callback/1,
    register_focusout_callback/1,
    register_focusout_callback/2,
    register_focusout_callback/3,
    unregister_focusout_callback/1,

    register_touchstart_callback/1,
    register_touchstart_callback/2,
    register_touchstart_callback/3,
    unregister_touchstart_callback/1,
    register_touchend_callback/1,
    register_touchend_callback/2,
    register_touchend_callback/3,
    unregister_touchend_callback/1,
    register_touchmove_callback/1,
    register_touchmove_callback/2,
    register_touchmove_callback/3,
    unregister_touchmove_callback/1,
    register_touchcancel_callback/1,
    register_touchcancel_callback/2,
    register_touchcancel_callback/3,
    unregister_touchcancel_callback/1
]).

-export_type([
    promise/0,
    html5_target/0,
    listener_handle/0,
    keyboard_event/0,
    mouse_event/0,
    wheel_event/0,
    ui_event/0,
    focus_event/0,
    touch_point/0,
    touch_event/0
]).

-type run_script_opt() :: main_thread | async.
-opaque promise() :: binary().
-type html5_target() :: window | document | screen | iodata().
-opaque listener_handle() :: binary().
-type register_option() :: {use_capture, boolean()} | {prevent_default, boolean()}.
-type register_options() :: boolean() | [register_option()].
-type register_error_reason() ::
    not_supported
    | failed_not_deferred
    | invalid_target
    | unknown_target
    | failed
    | no_data
    | timed_out
    | integer().
-type register_result() ::
    {ok, listener_handle()} | {ok, listener_handle(), deferred} | {error, register_error_reason()}.

-type keyboard_event() :: #{
    timestamp := float(),
    location := integer(),
    ctrl_key := boolean(),
    shift_key := boolean(),
    alt_key := boolean(),
    meta_key := boolean(),
    repeat := boolean(),
    char_code := integer(),
    key_code := integer(),
    which := integer(),
    key := unicode:unicode_binary(),
    code := unicode:unicode_binary(),
    char_value := unicode:unicode_binary(),
    locale := unicode:unicode_binary()
}.

-type mouse_event() :: #{
    timestamp := float(),
    screen_x := integer(),
    screen_y := integer(),
    client_x := integer(),
    client_y := integer(),
    ctrl_key := boolean(),
    shift_key := boolean(),
    alt_key := boolean(),
    meta_key := boolean(),
    button := integer(),
    buttons := integer(),
    movement_x := integer(),
    movement_y := integer(),
    target_x := integer(),
    target_y := integer(),
    padding := integer()
}.

-type wheel_event() :: #{
    timestamp := float(),
    screen_x := integer(),
    screen_y := integer(),
    client_x := integer(),
    client_y := integer(),
    ctrl_key := boolean(),
    shift_key := boolean(),
    alt_key := boolean(),
    meta_key := boolean(),
    button := integer(),
    buttons := integer(),
    movement_x := integer(),
    movement_y := integer(),
    target_x := integer(),
    target_y := integer(),
    padding := integer(),
    delta_x := integer(),
    delta_y := integer(),
    delta_z := integer(),
    delta_mode := integer()
}.

-type ui_event() :: #{
    detail := integer(),
    document_body_client_width := integer(),
    document_body_client_height := integer(),
    window_inner_width := integer(),
    window_inner_height := integer(),
    window_outer_width := integer(),
    window_outer_height := integer(),
    scroll_top := integer(),
    scroll_left := integer()
}.

-type focus_event() :: #{
    node_name := unicode:unicode_binary(),
    id := unicode:unicode_binary()
}.

-type touch_point() :: #{
    identifier := integer(),
    screen_x := integer(),
    screen_y := integer(),
    client_x := integer(),
    client_y := integer(),
    page_x := integer(),
    page_y := integer(),
    is_changed := boolean(),
    on_target := boolean(),
    target_x := integer(),
    target_y := integer()
}.

-type touch_event() :: #{
    timestamp := float(),
    ctrl_key := boolean(),
    shift_key := boolean(),
    alt_key := boolean(),
    meta_key := boolean(),
    touches := [touch_point()]
}.

%% @equiv run_script(_Script, [])
-spec run_script(iodata()) -> ok.
run_script(_Script) ->
    erlang:nif_error(undefined).

%% @doc Run a script.
%% By default, the script is run in the current worker thread, which arguably
%% may not be very useful. Please note that exception handling is disabled, so
%% the script should not throw and should compile, otherwise this will crash
%% the VM.
%% @param _Script Script to run
%% @param _Options List of options. If `main_thread' is specified, the script
%% is run on the main thread. If `async', the script is run asynchronously, i.e.
%% the caller does not wait for completion. Only applies if `main_thread' is specified.
%% @returns ok
-spec run_script(_Script :: iodata(), _Options :: [run_script_opt()]) -> ok.
run_script(_Script, _Options) ->
    erlang:nif_error(undefined).

%% @equiv promise_resolve(_Promise, 0)
-spec promise_resolve(promise()) -> ok.
promise_resolve(_Promise) ->
    erlang:nif_error(undefined).

%% @doc Successfully resolve a promise with a given result.
%% A promise is currently only obtained through synchronous calls using
%% `Module.call()' javascript function.
%% If Javascript calls:
%% ```
%% await Module.call('some_proc', 'message')
%% '''
%% and if an Erlang process is registered as `some_proc`, then the process
%% will receive a message:
%% <pre><code>
%% {emscripten, {call, Promise, <<"message">>}}
%% </code></pre>
%% and the Javascript caller will wait until `promise_resolve' or `promise_reject'
%% is called. If the process doesn't exist, the promise will be rejected with
%% 'no_proc'. Likewise if the Promise is garbage collected by the Erlang VM.
%% @param _Promise Opaque promise resource
%% @param _Value Value to send to Javascript, must be an integer or a string.
-spec promise_resolve(_Promise :: promise(), _Value :: integer() | iodata()) -> ok.
promise_resolve(_Promise, _Value) ->
    erlang:nif_error(undefined).

%% @equiv promise_reject(_Promise, 0)
-spec promise_reject(promise()) -> ok.
promise_reject(_Promise) ->
    erlang:nif_error(undefined).

%% @doc Reject a promise with a given result.
%% This is similar to `promise_resolve' except the promise is rejected.
%% @param _Promise Opaque promise resource
%% @param _Value Value to send to Javascript, must be an integer or a string.
-spec promise_reject(_Promise :: promise(), _Value :: integer() | iodata()) -> ok.
promise_reject(_Promise, _Value) ->
    erlang:nif_error(undefined).

%% @equiv register_keypress_callback(_Target, [])
-spec register_keypress_callback(html5_target()) -> register_result().
register_keypress_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for keypress events.
%% This function registers with no user data and events are sent as:
%% ```
%% {emscripten, {keypress, keyboard_event()}}
%% '''
%% @see register_keypress_callback/3
-spec register_keypress_callback(html5_target(), register_options()) -> register_result().
register_keypress_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for keypress events.
%% This function registers keypress events on a given target.
%% Target can be specified as special atoms `window' for Javascript's window,
%% `document' for window.document. `screen' is also supported, but it doesn't
%% seem to work, see [https://github.com/emscripten-core/emscripten/issues/19865]
%%
%% Second parameter specifies options which can be a `boolean()' to match
%% `useCapture' in Emscripten's API. Alternatively, the option can be a proplist()
%% with `use_capture' and `prevent_default' keys. `prevent_default' determines
%% what the handler should return to Javascript, `true' meaning that the default
%% should be prevented.
%%
%% Third parameter is user data that is passed back. Indeed, when an event
%% occurs, the following message is sent to the process that registered the
%% event:
%% ```
%% {emscripten, {keypress, keyboard_event()}, UserData}
%% '''
%%
%% The function eventually returns a `listener_handle()' or an error. The
%% handler is an opaque resource that actually contains a copy of `UserData'.
%% Please note that if the calling process dies, the callback and any callback
%% for the same event on the same target are unregistered.
%%
%% @param _Target target to register keypress on
%% @param _Options options for event handling
%% @param _UserData user data passed back
-spec register_keypress_callback(
    _Target :: html5_target(), _Options :: register_options(), _UserData :: any()
) ->
    register_result().
register_keypress_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a keypress listener.
%%
%% To match Emscripten's API, this function can take a target. This function
%% unregisters every keypress listeners on the specified target.
%%
%% Alternatively, this function can take a listener_handle() returned by
%% `register_keypress_callback/1,2,3'. This will still unregister every
%% keypress listeners on the same target.
%%
%% Passing a `listener_handle()' is recommended to avoid surprises and for
%% memory efficiency. If a handle is passed, it can then be garbage collected.
%% If a handle is not passed, but the process dies, every keypress listener
%% on the same target will be unregistered, including listeners that were
%% later registered. This is a known limitation of the implementation that
%% favored avoiding memory leaks and crashes.
%%
%% @param _TargetOrHandle Target or handle
%% @returns ok or an error
-spec unregister_keypress_callback(_TargetOrHandle :: html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_keypress_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_keydown_callback(_Target, [])
-spec register_keydown_callback(html5_target()) -> register_result().
register_keydown_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for keydown events.
%% @see register_keypress_callback/2
-spec register_keydown_callback(html5_target(), register_options()) -> register_result().
register_keydown_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for keydown events.
%% @see register_keypress_callback/2
-spec register_keydown_callback(html5_target(), register_options(), any()) -> register_result().
register_keydown_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a keydown event handler.
%% @see unregister_keypress_callback/1
-spec unregister_keydown_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_keydown_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_keyup_callback(_Target, [])
-spec register_keyup_callback(html5_target()) -> register_result().
register_keyup_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for keyup events.
%% @see register_keypress_callback/2
-spec register_keyup_callback(html5_target(), register_options()) -> register_result().
register_keyup_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for keyup events.
%% @see register_keypress_callback/2
-spec register_keyup_callback(html5_target(), register_options(), any()) -> register_result().
register_keyup_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a keyup event handler.
%% @see unregister_keypress_callback/1
-spec unregister_keyup_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_keyup_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_click_callback(_Target, [])
-spec register_click_callback(html5_target()) -> register_result().
register_click_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for click events.
%% Events are sent as:
%% ```
%% {emscripten, {click, mouse_event()}}
%% '''
%% @see register_keypress_callback/2
-spec register_click_callback(html5_target(), register_options()) -> register_result().
register_click_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for click events.
%% Events are sent as:
%% ```
%% {emscripten, {click, mouse_event()}, UserData}
%% '''
%% @see register_keypress_callback/2
-spec register_click_callback(html5_target(), register_options(), any()) -> register_result().
register_click_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a click event handler.
%% @see unregister_keypress_callback/1
-spec unregister_click_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_click_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_mousedown_callback(_Target, [])
-spec register_mousedown_callback(html5_target()) -> register_result().
register_mousedown_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for mousedown events.
%% @see register_click_callback/2
-spec register_mousedown_callback(html5_target(), register_options()) -> register_result().
register_mousedown_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for mousedown events.
%% @see register_click_callback/2
-spec register_mousedown_callback(html5_target(), register_options(), any()) -> register_result().
register_mousedown_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a mousedown event handler.
%% @see unregister_click_callback/1
-spec unregister_mousedown_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_mousedown_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_mouseup_callback(_Target, [])
-spec register_mouseup_callback(html5_target()) -> register_result().
register_mouseup_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseup events.
%% @see register_click_callback/2
-spec register_mouseup_callback(html5_target(), register_options()) -> register_result().
register_mouseup_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseup events.
%% @see register_click_callback/2
-spec register_mouseup_callback(html5_target(), register_options(), any()) -> register_result().
register_mouseup_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a mouseup event handler.
%% @see unregister_click_callback/1
-spec unregister_mouseup_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_mouseup_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_dblclick_callback(_Target, [])
-spec register_dblclick_callback(html5_target()) -> register_result().
register_dblclick_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for dblclick events.
%% @see register_click_callback/2
-spec register_dblclick_callback(html5_target(), register_options()) -> register_result().
register_dblclick_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for dblclick events.
%% @see register_click_callback/2
-spec register_dblclick_callback(html5_target(), register_options(), any()) -> register_result().
register_dblclick_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a dblclick event handler.
%% @see unregister_click_callback/1
-spec unregister_dblclick_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_dblclick_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_mousemove_callback(_Target, [])
-spec register_mousemove_callback(html5_target()) -> register_result().
register_mousemove_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for mousemove events.
%% @see register_click_callback/2
-spec register_mousemove_callback(html5_target(), register_options()) -> register_result().
register_mousemove_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for mousemove events.
%% @see register_click_callback/2
-spec register_mousemove_callback(html5_target(), register_options(), any()) -> register_result().
register_mousemove_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a mousemove event handler.
%% @see unregister_click_callback/1
-spec unregister_mousemove_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_mousemove_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_mouseenter_callback(_Target, [])
-spec register_mouseenter_callback(html5_target()) -> register_result().
register_mouseenter_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseenter events.
%% @see register_click_callback/2
-spec register_mouseenter_callback(html5_target(), register_options()) -> register_result().
register_mouseenter_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseenter events.
%% @see register_click_callback/2
-spec register_mouseenter_callback(html5_target(), register_options(), any()) -> register_result().
register_mouseenter_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a mouseenter event handler.
%% @see unregister_click_callback/1
-spec unregister_mouseenter_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_mouseenter_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_mouseleave_callback(_Target, [])
-spec register_mouseleave_callback(html5_target()) -> register_result().
register_mouseleave_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseleave events.
%% @see register_click_callback/2
-spec register_mouseleave_callback(html5_target(), register_options()) -> register_result().
register_mouseleave_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseleave events.
%% @see register_click_callback/2
-spec register_mouseleave_callback(html5_target(), register_options(), any()) -> register_result().
register_mouseleave_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a mouseleave event handler.
%% @see unregister_click_callback/1
-spec unregister_mouseleave_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_mouseleave_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_mouseover_callback(_Target, [])
-spec register_mouseover_callback(html5_target()) -> register_result().
register_mouseover_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseover events.
%% @see register_click_callback/2
-spec register_mouseover_callback(html5_target(), register_options()) -> register_result().
register_mouseover_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseover events.
%% @see register_click_callback/2
-spec register_mouseover_callback(html5_target(), register_options(), any()) -> register_result().
register_mouseover_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a mouseover event handler.
%% @see unregister_click_callback/1
-spec unregister_mouseover_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_mouseover_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_mouseout_callback(_Target, [])
-spec register_mouseout_callback(html5_target()) -> register_result().
register_mouseout_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseout events.
%% @see register_click_callback/2
-spec register_mouseout_callback(html5_target(), register_options()) -> register_result().
register_mouseout_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for mouseout events.
%% @see register_click_callback/2
-spec register_mouseout_callback(html5_target(), register_options(), any()) -> register_result().
register_mouseout_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a mouseout event handler.
%% @see unregister_click_callback/1
-spec unregister_mouseout_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_mouseout_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_wheel_callback(_Target, [])
-spec register_wheel_callback(html5_target()) -> register_result().
register_wheel_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for wheel events.
%% Events are sent as:
%% ```
%% {emscripten, {wheel, wheel_event()}}
%% '''
%% @see register_keypress_callback/2
-spec register_wheel_callback(html5_target(), register_options()) -> register_result().
register_wheel_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for wheel events.
%% Events are sent as:
%% ```
%% {emscripten, {wheel, mouse_event()}, UserData}
%% '''
%% @see register_keypress_callback/2
-spec register_wheel_callback(html5_target(), register_options(), any()) -> register_result().
register_wheel_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a wheel event handler.
%% @see unregister_keypress_callback/1
-spec unregister_wheel_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_wheel_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_resize_callback(_Target, [])
-spec register_resize_callback(html5_target()) -> register_result().
register_resize_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for resize events.
%% Events are sent as:
%% ```
%% {emscripten, {resize, ui_event()}}
%% '''
%% @see register_keypress_callback/2
-spec register_resize_callback(html5_target(), register_options()) -> register_result().
register_resize_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for resize events.
%% Events are sent as:
%% ```
%% {emscripten, {resize, ui_event()}, UserData}
%% '''
%% @see register_keypress_callback/2
-spec register_resize_callback(html5_target(), register_options(), any()) -> register_result().
register_resize_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a resize event handler.
%% @see unregister_keypress_callback/1
-spec unregister_resize_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_resize_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_scroll_callback(_Target, [])
-spec register_scroll_callback(html5_target()) -> register_result().
register_scroll_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for scroll events.
%% @see register_resize_callback/2
-spec register_scroll_callback(html5_target(), register_options()) -> register_result().
register_scroll_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for scroll events.
%% @see register_resize_callback/2
-spec register_scroll_callback(html5_target(), register_options(), any()) -> register_result().
register_scroll_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a scroll event handler.
%% @see unregister_resize_callback/1
-spec unregister_scroll_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_scroll_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_blur_callback(_Target, [])
-spec register_blur_callback(html5_target()) -> register_result().
register_blur_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for blur events.
%% Events are sent as:
%% ```
%% {emscripten, {blur, focus_event()}}
%% '''
%% @see register_keypress_callback/2
-spec register_blur_callback(html5_target(), register_options()) -> register_result().
register_blur_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for blur events.
%% Events are sent as:
%% ```
%% {emscripten, {blur, focus_event()}, UserData}
%% '''
%% @see register_keypress_callback/2
-spec register_blur_callback(html5_target(), register_options(), any()) -> register_result().
register_blur_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a blur event handler.
%% @see unregister_keypress_callback/1
-spec unregister_blur_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_blur_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_focus_callback(_Target, [])
-spec register_focus_callback(html5_target()) -> register_result().
register_focus_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for focus events.
%% @see register_blur_callback/2
-spec register_focus_callback(html5_target(), register_options()) -> register_result().
register_focus_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for focus events.
%% @see register_blur_callback/2
-spec register_focus_callback(html5_target(), register_options(), any()) -> register_result().
register_focus_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a focus event handler.
%% @see unregister_blur_callback/1
-spec unregister_focus_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_focus_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_focusin_callback(_Target, [])
-spec register_focusin_callback(html5_target()) -> register_result().
register_focusin_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for focusin events.
%% @see register_blur_callback/2
-spec register_focusin_callback(html5_target(), register_options()) -> register_result().
register_focusin_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for focusin events.
%% @see register_blur_callback/2
-spec register_focusin_callback(html5_target(), register_options(), any()) -> register_result().
register_focusin_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a focusin event handler.
%% @see unregister_blur_callback/1
-spec unregister_focusin_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_focusin_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_focusout_callback(_Target, [])
-spec register_focusout_callback(html5_target()) -> register_result().
register_focusout_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for focusout events.
%% @see register_blur_callback/2
-spec register_focusout_callback(html5_target(), register_options()) -> register_result().
register_focusout_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for focusout events.
%% @see register_blur_callback/2
-spec register_focusout_callback(html5_target(), register_options(), any()) -> register_result().
register_focusout_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a focusout event handler.
%% @see unregister_blur_callback/1
-spec unregister_focusout_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_focusout_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_touchstart_callback(_Target, [])
-spec register_touchstart_callback(html5_target()) -> register_result().
register_touchstart_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for touchstart events.
%% Events are sent as:
%% ```
%% {emscripten, {touchstart, touch_event()}}
%% '''
%% @see register_keypress_callback/2
-spec register_touchstart_callback(html5_target(), register_options()) -> register_result().
register_touchstart_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for touchstart events.
%% Events are sent as:
%% ```
%% {emscripten, {touchstart, touch_event()}, UserData}
%% '''
%% @see register_keypress_callback/2
-spec register_touchstart_callback(html5_target(), register_options(), any()) -> register_result().
register_touchstart_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a touchstart event handler.
%% @see unregister_keypress_callback/1
-spec unregister_touchstart_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_touchstart_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_touchend_callback(_Target, [])
-spec register_touchend_callback(html5_target()) -> register_result().
register_touchend_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for touchend events.
%% @see register_touchstart_callback/2
-spec register_touchend_callback(html5_target(), register_options()) -> register_result().
register_touchend_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for touchend events.
%% @see register_touchstart_callback/2
-spec register_touchend_callback(html5_target(), register_options(), any()) -> register_result().
register_touchend_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a touchend event handler.
%% @see unregister_touchstart_callback/1
-spec unregister_touchend_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_touchend_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_touchmove_callback(_Target, [])
-spec register_touchmove_callback(html5_target()) -> register_result().
register_touchmove_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for touchmove events.
%% @see register_touchstart_callback/2
-spec register_touchmove_callback(html5_target(), register_options()) -> register_result().
register_touchmove_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for touchmove events.
%% @see register_touchstart_callback/2
-spec register_touchmove_callback(html5_target(), register_options(), any()) -> register_result().
register_touchmove_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a touchmove event handler.
%% @see unregister_touchstart_callback/1
-spec unregister_touchmove_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_touchmove_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).

%% @equiv register_touchcancel_callback(_Target, [])
-spec register_touchcancel_callback(html5_target()) -> register_result().
register_touchcancel_callback(_Target) ->
    erlang:nif_error(undefined).

%% @doc Register for touchcancel events.
%% @see register_touchstart_callback/2
-spec register_touchcancel_callback(html5_target(), register_options()) -> register_result().
register_touchcancel_callback(_Target, _Options) ->
    erlang:nif_error(undefined).

%% @doc Register for touchcancel events.
%% @see register_touchstart_callback/2
-spec register_touchcancel_callback(html5_target(), register_options(), any()) -> register_result().
register_touchcancel_callback(_Target, _Options, _UserData) ->
    erlang:nif_error(undefined).

%% @doc Unregister a touchcancel event handler.
%% @see unregister_touchstart_callback/1
-spec unregister_touchcancel_callback(html5_target() | listener_handle()) ->
    ok | {error, register_error_reason()}.
unregister_touchcancel_callback(_TargetOrHandle) ->
    erlang:nif_error(undefined).
