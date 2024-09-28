%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_extended_literal_large).
-export([unicode_table/1, start/0]).

start() ->
    {0, [], [], lookup_category} = unicode_table(1042),
    {0, [], {compat, [{0, 4019}, {129, 3953}, {130, 3968}]}, mn} = unicode_table(3961),
    {0, [{0, 173568}], [], lo} = unicode_table(195101),
    0.

% Derived from Erlang/OTP 27.0 unicode_util.erl which is automatically generated.
% see ../uc_spec/gen_unicode_mod.escript
unicode_table(160) ->
    {0, [], {noBreak, [{0, 32}]}, zs};
unicode_table(168) ->
    {0, [], {compat, [{0, 32}, {230, 776}]}, sk};
unicode_table(170) ->
    {0, [], {super, [{0, 97}]}, lo};
unicode_table(175) ->
    {0, [], {compat, [{0, 32}, {230, 772}]}, sk};
unicode_table(178) ->
    {0, [], {super, [{0, 50}]}, no};
unicode_table(179) ->
    {0, [], {super, [{0, 51}]}, no};
unicode_table(180) ->
    {0, [], {compat, [{0, 32}, {230, 769}]}, sk};
unicode_table(181) ->
    {0, [], {compat, [{0, 956}]}, ll};
unicode_table(184) ->
    {0, [], {compat, [{0, 32}, {202, 807}]}, sk};
unicode_table(185) ->
    {0, [], {super, [{0, 49}]}, no};
unicode_table(186) ->
    {0, [], {super, [{0, 111}]}, lo};
unicode_table(188) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 52}]}, no};
unicode_table(189) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 50}]}, no};
unicode_table(190) ->
    {0, [], {fraction, [{0, 51}, {0, 8260}, {0, 52}]}, no};
unicode_table(192) ->
    {0, [{0, 65}, {230, 768}], [], lu};
unicode_table(193) ->
    {0, [{0, 65}, {230, 769}], [], lu};
unicode_table(194) ->
    {0, [{0, 65}, {230, 770}], [], lu};
unicode_table(195) ->
    {0, [{0, 65}, {230, 771}], [], lu};
unicode_table(196) ->
    {0, [{0, 65}, {230, 776}], [], lu};
unicode_table(197) ->
    {0, [{0, 65}, {230, 778}], [], lu};
unicode_table(199) ->
    {0, [{0, 67}, {202, 807}], [], lu};
unicode_table(200) ->
    {0, [{0, 69}, {230, 768}], [], lu};
unicode_table(201) ->
    {0, [{0, 69}, {230, 769}], [], lu};
unicode_table(202) ->
    {0, [{0, 69}, {230, 770}], [], lu};
unicode_table(203) ->
    {0, [{0, 69}, {230, 776}], [], lu};
unicode_table(204) ->
    {0, [{0, 73}, {230, 768}], [], lu};
unicode_table(205) ->
    {0, [{0, 73}, {230, 769}], [], lu};
unicode_table(206) ->
    {0, [{0, 73}, {230, 770}], [], lu};
unicode_table(207) ->
    {0, [{0, 73}, {230, 776}], [], lu};
unicode_table(209) ->
    {0, [{0, 78}, {230, 771}], [], lu};
unicode_table(210) ->
    {0, [{0, 79}, {230, 768}], [], lu};
unicode_table(211) ->
    {0, [{0, 79}, {230, 769}], [], lu};
unicode_table(212) ->
    {0, [{0, 79}, {230, 770}], [], lu};
unicode_table(213) ->
    {0, [{0, 79}, {230, 771}], [], lu};
unicode_table(214) ->
    {0, [{0, 79}, {230, 776}], [], lu};
unicode_table(217) ->
    {0, [{0, 85}, {230, 768}], [], lu};
unicode_table(218) ->
    {0, [{0, 85}, {230, 769}], [], lu};
unicode_table(219) ->
    {0, [{0, 85}, {230, 770}], [], lu};
unicode_table(220) ->
    {0, [{0, 85}, {230, 776}], [], lu};
unicode_table(221) ->
    {0, [{0, 89}, {230, 769}], [], lu};
unicode_table(224) ->
    {0, [{0, 97}, {230, 768}], [], ll};
unicode_table(225) ->
    {0, [{0, 97}, {230, 769}], [], ll};
unicode_table(226) ->
    {0, [{0, 97}, {230, 770}], [], ll};
unicode_table(227) ->
    {0, [{0, 97}, {230, 771}], [], ll};
unicode_table(228) ->
    {0, [{0, 97}, {230, 776}], [], ll};
unicode_table(229) ->
    {0, [{0, 97}, {230, 778}], [], ll};
unicode_table(231) ->
    {0, [{0, 99}, {202, 807}], [], ll};
unicode_table(232) ->
    {0, [{0, 101}, {230, 768}], [], ll};
unicode_table(233) ->
    {0, [{0, 101}, {230, 769}], [], ll};
unicode_table(234) ->
    {0, [{0, 101}, {230, 770}], [], ll};
unicode_table(235) ->
    {0, [{0, 101}, {230, 776}], [], ll};
unicode_table(236) ->
    {0, [{0, 105}, {230, 768}], [], ll};
unicode_table(237) ->
    {0, [{0, 105}, {230, 769}], [], ll};
unicode_table(238) ->
    {0, [{0, 105}, {230, 770}], [], ll};
unicode_table(239) ->
    {0, [{0, 105}, {230, 776}], [], ll};
unicode_table(241) ->
    {0, [{0, 110}, {230, 771}], [], ll};
unicode_table(242) ->
    {0, [{0, 111}, {230, 768}], [], ll};
unicode_table(243) ->
    {0, [{0, 111}, {230, 769}], [], ll};
unicode_table(244) ->
    {0, [{0, 111}, {230, 770}], [], ll};
unicode_table(245) ->
    {0, [{0, 111}, {230, 771}], [], ll};
unicode_table(246) ->
    {0, [{0, 111}, {230, 776}], [], ll};
unicode_table(249) ->
    {0, [{0, 117}, {230, 768}], [], ll};
unicode_table(250) ->
    {0, [{0, 117}, {230, 769}], [], ll};
unicode_table(251) ->
    {0, [{0, 117}, {230, 770}], [], ll};
unicode_table(252) ->
    {0, [{0, 117}, {230, 776}], [], ll};
unicode_table(253) ->
    {0, [{0, 121}, {230, 769}], [], ll};
unicode_table(255) ->
    {0, [{0, 121}, {230, 776}], [], ll};
unicode_table(256) ->
    {0, [{0, 65}, {230, 772}], [], lu};
unicode_table(257) ->
    {0, [{0, 97}, {230, 772}], [], ll};
unicode_table(258) ->
    {0, [{0, 65}, {230, 774}], [], lu};
unicode_table(259) ->
    {0, [{0, 97}, {230, 774}], [], ll};
unicode_table(260) ->
    {0, [{0, 65}, {202, 808}], [], lu};
unicode_table(261) ->
    {0, [{0, 97}, {202, 808}], [], ll};
unicode_table(262) ->
    {0, [{0, 67}, {230, 769}], [], lu};
unicode_table(263) ->
    {0, [{0, 99}, {230, 769}], [], ll};
unicode_table(264) ->
    {0, [{0, 67}, {230, 770}], [], lu};
unicode_table(265) ->
    {0, [{0, 99}, {230, 770}], [], ll};
unicode_table(266) ->
    {0, [{0, 67}, {230, 775}], [], lu};
unicode_table(267) ->
    {0, [{0, 99}, {230, 775}], [], ll};
unicode_table(268) ->
    {0, [{0, 67}, {230, 780}], [], lu};
unicode_table(269) ->
    {0, [{0, 99}, {230, 780}], [], ll};
unicode_table(270) ->
    {0, [{0, 68}, {230, 780}], [], lu};
unicode_table(271) ->
    {0, [{0, 100}, {230, 780}], [], ll};
unicode_table(274) ->
    {0, [{0, 69}, {230, 772}], [], lu};
unicode_table(275) ->
    {0, [{0, 101}, {230, 772}], [], ll};
unicode_table(276) ->
    {0, [{0, 69}, {230, 774}], [], lu};
unicode_table(277) ->
    {0, [{0, 101}, {230, 774}], [], ll};
unicode_table(278) ->
    {0, [{0, 69}, {230, 775}], [], lu};
unicode_table(279) ->
    {0, [{0, 101}, {230, 775}], [], ll};
unicode_table(280) ->
    {0, [{0, 69}, {202, 808}], [], lu};
unicode_table(281) ->
    {0, [{0, 101}, {202, 808}], [], ll};
unicode_table(282) ->
    {0, [{0, 69}, {230, 780}], [], lu};
unicode_table(283) ->
    {0, [{0, 101}, {230, 780}], [], ll};
unicode_table(284) ->
    {0, [{0, 71}, {230, 770}], [], lu};
unicode_table(285) ->
    {0, [{0, 103}, {230, 770}], [], ll};
unicode_table(286) ->
    {0, [{0, 71}, {230, 774}], [], lu};
unicode_table(287) ->
    {0, [{0, 103}, {230, 774}], [], ll};
unicode_table(288) ->
    {0, [{0, 71}, {230, 775}], [], lu};
unicode_table(289) ->
    {0, [{0, 103}, {230, 775}], [], ll};
unicode_table(290) ->
    {0, [{0, 71}, {202, 807}], [], lu};
unicode_table(291) ->
    {0, [{0, 103}, {202, 807}], [], ll};
unicode_table(292) ->
    {0, [{0, 72}, {230, 770}], [], lu};
unicode_table(293) ->
    {0, [{0, 104}, {230, 770}], [], ll};
unicode_table(296) ->
    {0, [{0, 73}, {230, 771}], [], lu};
unicode_table(297) ->
    {0, [{0, 105}, {230, 771}], [], ll};
unicode_table(298) ->
    {0, [{0, 73}, {230, 772}], [], lu};
unicode_table(299) ->
    {0, [{0, 105}, {230, 772}], [], ll};
unicode_table(300) ->
    {0, [{0, 73}, {230, 774}], [], lu};
unicode_table(301) ->
    {0, [{0, 105}, {230, 774}], [], ll};
unicode_table(302) ->
    {0, [{0, 73}, {202, 808}], [], lu};
unicode_table(303) ->
    {0, [{0, 105}, {202, 808}], [], ll};
unicode_table(304) ->
    {0, [{0, 73}, {230, 775}], [], lu};
unicode_table(306) ->
    {0, [], {compat, [{0, 73}, {0, 74}]}, lu};
unicode_table(307) ->
    {0, [], {compat, [{0, 105}, {0, 106}]}, ll};
unicode_table(308) ->
    {0, [{0, 74}, {230, 770}], [], lu};
unicode_table(309) ->
    {0, [{0, 106}, {230, 770}], [], ll};
unicode_table(310) ->
    {0, [{0, 75}, {202, 807}], [], lu};
unicode_table(311) ->
    {0, [{0, 107}, {202, 807}], [], ll};
unicode_table(313) ->
    {0, [{0, 76}, {230, 769}], [], lu};
unicode_table(314) ->
    {0, [{0, 108}, {230, 769}], [], ll};
unicode_table(315) ->
    {0, [{0, 76}, {202, 807}], [], lu};
unicode_table(316) ->
    {0, [{0, 108}, {202, 807}], [], ll};
unicode_table(317) ->
    {0, [{0, 76}, {230, 780}], [], lu};
unicode_table(318) ->
    {0, [{0, 108}, {230, 780}], [], ll};
unicode_table(319) ->
    {0, [], {compat, [{0, 76}, {0, 183}]}, lu};
unicode_table(320) ->
    {0, [], {compat, [{0, 108}, {0, 183}]}, ll};
unicode_table(323) ->
    {0, [{0, 78}, {230, 769}], [], lu};
unicode_table(324) ->
    {0, [{0, 110}, {230, 769}], [], ll};
unicode_table(325) ->
    {0, [{0, 78}, {202, 807}], [], lu};
unicode_table(326) ->
    {0, [{0, 110}, {202, 807}], [], ll};
unicode_table(327) ->
    {0, [{0, 78}, {230, 780}], [], lu};
unicode_table(328) ->
    {0, [{0, 110}, {230, 780}], [], ll};
unicode_table(329) ->
    {0, [], {compat, [{0, 700}, {0, 110}]}, ll};
unicode_table(332) ->
    {0, [{0, 79}, {230, 772}], [], lu};
unicode_table(333) ->
    {0, [{0, 111}, {230, 772}], [], ll};
unicode_table(334) ->
    {0, [{0, 79}, {230, 774}], [], lu};
unicode_table(335) ->
    {0, [{0, 111}, {230, 774}], [], ll};
unicode_table(336) ->
    {0, [{0, 79}, {230, 779}], [], lu};
unicode_table(337) ->
    {0, [{0, 111}, {230, 779}], [], ll};
unicode_table(340) ->
    {0, [{0, 82}, {230, 769}], [], lu};
unicode_table(341) ->
    {0, [{0, 114}, {230, 769}], [], ll};
unicode_table(342) ->
    {0, [{0, 82}, {202, 807}], [], lu};
unicode_table(343) ->
    {0, [{0, 114}, {202, 807}], [], ll};
unicode_table(344) ->
    {0, [{0, 82}, {230, 780}], [], lu};
unicode_table(345) ->
    {0, [{0, 114}, {230, 780}], [], ll};
unicode_table(346) ->
    {0, [{0, 83}, {230, 769}], [], lu};
unicode_table(347) ->
    {0, [{0, 115}, {230, 769}], [], ll};
unicode_table(348) ->
    {0, [{0, 83}, {230, 770}], [], lu};
unicode_table(349) ->
    {0, [{0, 115}, {230, 770}], [], ll};
unicode_table(350) ->
    {0, [{0, 83}, {202, 807}], [], lu};
unicode_table(351) ->
    {0, [{0, 115}, {202, 807}], [], ll};
unicode_table(352) ->
    {0, [{0, 83}, {230, 780}], [], lu};
unicode_table(353) ->
    {0, [{0, 115}, {230, 780}], [], ll};
unicode_table(354) ->
    {0, [{0, 84}, {202, 807}], [], lu};
unicode_table(355) ->
    {0, [{0, 116}, {202, 807}], [], ll};
unicode_table(356) ->
    {0, [{0, 84}, {230, 780}], [], lu};
unicode_table(357) ->
    {0, [{0, 116}, {230, 780}], [], ll};
unicode_table(360) ->
    {0, [{0, 85}, {230, 771}], [], lu};
unicode_table(361) ->
    {0, [{0, 117}, {230, 771}], [], ll};
unicode_table(362) ->
    {0, [{0, 85}, {230, 772}], [], lu};
unicode_table(363) ->
    {0, [{0, 117}, {230, 772}], [], ll};
unicode_table(364) ->
    {0, [{0, 85}, {230, 774}], [], lu};
unicode_table(365) ->
    {0, [{0, 117}, {230, 774}], [], ll};
unicode_table(366) ->
    {0, [{0, 85}, {230, 778}], [], lu};
unicode_table(367) ->
    {0, [{0, 117}, {230, 778}], [], ll};
unicode_table(368) ->
    {0, [{0, 85}, {230, 779}], [], lu};
unicode_table(369) ->
    {0, [{0, 117}, {230, 779}], [], ll};
unicode_table(370) ->
    {0, [{0, 85}, {202, 808}], [], lu};
unicode_table(371) ->
    {0, [{0, 117}, {202, 808}], [], ll};
unicode_table(372) ->
    {0, [{0, 87}, {230, 770}], [], lu};
unicode_table(373) ->
    {0, [{0, 119}, {230, 770}], [], ll};
unicode_table(374) ->
    {0, [{0, 89}, {230, 770}], [], lu};
unicode_table(375) ->
    {0, [{0, 121}, {230, 770}], [], ll};
unicode_table(376) ->
    {0, [{0, 89}, {230, 776}], [], lu};
unicode_table(377) ->
    {0, [{0, 90}, {230, 769}], [], lu};
unicode_table(378) ->
    {0, [{0, 122}, {230, 769}], [], ll};
unicode_table(379) ->
    {0, [{0, 90}, {230, 775}], [], lu};
unicode_table(380) ->
    {0, [{0, 122}, {230, 775}], [], ll};
unicode_table(381) ->
    {0, [{0, 90}, {230, 780}], [], lu};
unicode_table(382) ->
    {0, [{0, 122}, {230, 780}], [], ll};
unicode_table(383) ->
    {0, [], {compat, [{0, 115}]}, ll};
unicode_table(416) ->
    {0, [{0, 79}, {216, 795}], [], lu};
unicode_table(417) ->
    {0, [{0, 111}, {216, 795}], [], ll};
unicode_table(431) ->
    {0, [{0, 85}, {216, 795}], [], lu};
unicode_table(432) ->
    {0, [{0, 117}, {216, 795}], [], ll};
unicode_table(452) ->
    {0, [], {compat, [{0, 68}, {0, 90}, {230, 780}]}, lu};
unicode_table(453) ->
    {0, [], {compat, [{0, 68}, {0, 122}, {230, 780}]}, lt};
unicode_table(454) ->
    {0, [], {compat, [{0, 100}, {0, 122}, {230, 780}]}, ll};
unicode_table(455) ->
    {0, [], {compat, [{0, 76}, {0, 74}]}, lu};
unicode_table(456) ->
    {0, [], {compat, [{0, 76}, {0, 106}]}, lt};
unicode_table(457) ->
    {0, [], {compat, [{0, 108}, {0, 106}]}, ll};
unicode_table(458) ->
    {0, [], {compat, [{0, 78}, {0, 74}]}, lu};
unicode_table(459) ->
    {0, [], {compat, [{0, 78}, {0, 106}]}, lt};
unicode_table(460) ->
    {0, [], {compat, [{0, 110}, {0, 106}]}, ll};
unicode_table(461) ->
    {0, [{0, 65}, {230, 780}], [], lu};
unicode_table(462) ->
    {0, [{0, 97}, {230, 780}], [], ll};
unicode_table(463) ->
    {0, [{0, 73}, {230, 780}], [], lu};
unicode_table(464) ->
    {0, [{0, 105}, {230, 780}], [], ll};
unicode_table(465) ->
    {0, [{0, 79}, {230, 780}], [], lu};
unicode_table(466) ->
    {0, [{0, 111}, {230, 780}], [], ll};
unicode_table(467) ->
    {0, [{0, 85}, {230, 780}], [], lu};
unicode_table(468) ->
    {0, [{0, 117}, {230, 780}], [], ll};
unicode_table(469) ->
    {0, [{0, 85}, {230, 776}, {230, 772}], [], lu};
unicode_table(470) ->
    {0, [{0, 117}, {230, 776}, {230, 772}], [], ll};
unicode_table(471) ->
    {0, [{0, 85}, {230, 776}, {230, 769}], [], lu};
unicode_table(472) ->
    {0, [{0, 117}, {230, 776}, {230, 769}], [], ll};
unicode_table(473) ->
    {0, [{0, 85}, {230, 776}, {230, 780}], [], lu};
unicode_table(474) ->
    {0, [{0, 117}, {230, 776}, {230, 780}], [], ll};
unicode_table(475) ->
    {0, [{0, 85}, {230, 776}, {230, 768}], [], lu};
unicode_table(476) ->
    {0, [{0, 117}, {230, 776}, {230, 768}], [], ll};
unicode_table(478) ->
    {0, [{0, 65}, {230, 776}, {230, 772}], [], lu};
unicode_table(479) ->
    {0, [{0, 97}, {230, 776}, {230, 772}], [], ll};
unicode_table(480) ->
    {0, [{0, 65}, {230, 775}, {230, 772}], [], lu};
unicode_table(481) ->
    {0, [{0, 97}, {230, 775}, {230, 772}], [], ll};
unicode_table(482) ->
    {0, [{0, 198}, {230, 772}], [], lu};
unicode_table(483) ->
    {0, [{0, 230}, {230, 772}], [], ll};
unicode_table(486) ->
    {0, [{0, 71}, {230, 780}], [], lu};
unicode_table(487) ->
    {0, [{0, 103}, {230, 780}], [], ll};
unicode_table(488) ->
    {0, [{0, 75}, {230, 780}], [], lu};
unicode_table(489) ->
    {0, [{0, 107}, {230, 780}], [], ll};
unicode_table(490) ->
    {0, [{0, 79}, {202, 808}], [], lu};
unicode_table(491) ->
    {0, [{0, 111}, {202, 808}], [], ll};
unicode_table(492) ->
    {0, [{0, 79}, {202, 808}, {230, 772}], [], lu};
unicode_table(493) ->
    {0, [{0, 111}, {202, 808}, {230, 772}], [], ll};
unicode_table(494) ->
    {0, [{0, 439}, {230, 780}], [], lu};
unicode_table(495) ->
    {0, [{0, 658}, {230, 780}], [], ll};
unicode_table(496) ->
    {0, [{0, 106}, {230, 780}], [], ll};
unicode_table(497) ->
    {0, [], {compat, [{0, 68}, {0, 90}]}, lu};
unicode_table(498) ->
    {0, [], {compat, [{0, 68}, {0, 122}]}, lt};
unicode_table(499) ->
    {0, [], {compat, [{0, 100}, {0, 122}]}, ll};
unicode_table(500) ->
    {0, [{0, 71}, {230, 769}], [], lu};
unicode_table(501) ->
    {0, [{0, 103}, {230, 769}], [], ll};
unicode_table(504) ->
    {0, [{0, 78}, {230, 768}], [], lu};
unicode_table(505) ->
    {0, [{0, 110}, {230, 768}], [], ll};
unicode_table(506) ->
    {0, [{0, 65}, {230, 778}, {230, 769}], [], lu};
unicode_table(507) ->
    {0, [{0, 97}, {230, 778}, {230, 769}], [], ll};
unicode_table(508) ->
    {0, [{0, 198}, {230, 769}], [], lu};
unicode_table(509) ->
    {0, [{0, 230}, {230, 769}], [], ll};
unicode_table(510) ->
    {0, [{0, 216}, {230, 769}], [], lu};
unicode_table(511) ->
    {0, [{0, 248}, {230, 769}], [], ll};
unicode_table(512) ->
    {0, [{0, 65}, {230, 783}], [], lu};
unicode_table(513) ->
    {0, [{0, 97}, {230, 783}], [], ll};
unicode_table(514) ->
    {0, [{0, 65}, {230, 785}], [], lu};
unicode_table(515) ->
    {0, [{0, 97}, {230, 785}], [], ll};
unicode_table(516) ->
    {0, [{0, 69}, {230, 783}], [], lu};
unicode_table(517) ->
    {0, [{0, 101}, {230, 783}], [], ll};
unicode_table(518) ->
    {0, [{0, 69}, {230, 785}], [], lu};
unicode_table(519) ->
    {0, [{0, 101}, {230, 785}], [], ll};
unicode_table(520) ->
    {0, [{0, 73}, {230, 783}], [], lu};
unicode_table(521) ->
    {0, [{0, 105}, {230, 783}], [], ll};
unicode_table(522) ->
    {0, [{0, 73}, {230, 785}], [], lu};
unicode_table(523) ->
    {0, [{0, 105}, {230, 785}], [], ll};
unicode_table(524) ->
    {0, [{0, 79}, {230, 783}], [], lu};
unicode_table(525) ->
    {0, [{0, 111}, {230, 783}], [], ll};
unicode_table(526) ->
    {0, [{0, 79}, {230, 785}], [], lu};
unicode_table(527) ->
    {0, [{0, 111}, {230, 785}], [], ll};
unicode_table(528) ->
    {0, [{0, 82}, {230, 783}], [], lu};
unicode_table(529) ->
    {0, [{0, 114}, {230, 783}], [], ll};
unicode_table(530) ->
    {0, [{0, 82}, {230, 785}], [], lu};
unicode_table(531) ->
    {0, [{0, 114}, {230, 785}], [], ll};
unicode_table(532) ->
    {0, [{0, 85}, {230, 783}], [], lu};
unicode_table(533) ->
    {0, [{0, 117}, {230, 783}], [], ll};
unicode_table(534) ->
    {0, [{0, 85}, {230, 785}], [], lu};
unicode_table(535) ->
    {0, [{0, 117}, {230, 785}], [], ll};
unicode_table(536) ->
    {0, [{0, 83}, {220, 806}], [], lu};
unicode_table(537) ->
    {0, [{0, 115}, {220, 806}], [], ll};
unicode_table(538) ->
    {0, [{0, 84}, {220, 806}], [], lu};
unicode_table(539) ->
    {0, [{0, 116}, {220, 806}], [], ll};
unicode_table(542) ->
    {0, [{0, 72}, {230, 780}], [], lu};
unicode_table(543) ->
    {0, [{0, 104}, {230, 780}], [], ll};
unicode_table(550) ->
    {0, [{0, 65}, {230, 775}], [], lu};
unicode_table(551) ->
    {0, [{0, 97}, {230, 775}], [], ll};
unicode_table(552) ->
    {0, [{0, 69}, {202, 807}], [], lu};
unicode_table(553) ->
    {0, [{0, 101}, {202, 807}], [], ll};
unicode_table(554) ->
    {0, [{0, 79}, {230, 776}, {230, 772}], [], lu};
unicode_table(555) ->
    {0, [{0, 111}, {230, 776}, {230, 772}], [], ll};
unicode_table(556) ->
    {0, [{0, 79}, {230, 771}, {230, 772}], [], lu};
unicode_table(557) ->
    {0, [{0, 111}, {230, 771}, {230, 772}], [], ll};
unicode_table(558) ->
    {0, [{0, 79}, {230, 775}], [], lu};
unicode_table(559) ->
    {0, [{0, 111}, {230, 775}], [], ll};
unicode_table(560) ->
    {0, [{0, 79}, {230, 775}, {230, 772}], [], lu};
unicode_table(561) ->
    {0, [{0, 111}, {230, 775}, {230, 772}], [], ll};
unicode_table(562) ->
    {0, [{0, 89}, {230, 772}], [], lu};
unicode_table(563) ->
    {0, [{0, 121}, {230, 772}], [], ll};
unicode_table(688) ->
    {0, [], {super, [{0, 104}]}, lm};
unicode_table(689) ->
    {0, [], {super, [{0, 614}]}, lm};
unicode_table(690) ->
    {0, [], {super, [{0, 106}]}, lm};
unicode_table(691) ->
    {0, [], {super, [{0, 114}]}, lm};
unicode_table(692) ->
    {0, [], {super, [{0, 633}]}, lm};
unicode_table(693) ->
    {0, [], {super, [{0, 635}]}, lm};
unicode_table(694) ->
    {0, [], {super, [{0, 641}]}, lm};
unicode_table(695) ->
    {0, [], {super, [{0, 119}]}, lm};
unicode_table(696) ->
    {0, [], {super, [{0, 121}]}, lm};
unicode_table(728) ->
    {0, [], {compat, [{0, 32}, {230, 774}]}, sk};
unicode_table(729) ->
    {0, [], {compat, [{0, 32}, {230, 775}]}, sk};
unicode_table(730) ->
    {0, [], {compat, [{0, 32}, {230, 778}]}, sk};
unicode_table(731) ->
    {0, [], {compat, [{0, 32}, {202, 808}]}, sk};
unicode_table(732) ->
    {0, [], {compat, [{0, 32}, {230, 771}]}, sk};
unicode_table(733) ->
    {0, [], {compat, [{0, 32}, {230, 779}]}, sk};
unicode_table(736) ->
    {0, [], {super, [{0, 611}]}, lm};
unicode_table(737) ->
    {0, [], {super, [{0, 108}]}, lm};
unicode_table(738) ->
    {0, [], {super, [{0, 115}]}, lm};
unicode_table(739) ->
    {0, [], {super, [{0, 120}]}, lm};
unicode_table(740) ->
    {0, [], {super, [{0, 661}]}, lm};
unicode_table(768) ->
    {230, [], [], mn};
unicode_table(769) ->
    {230, [], [], mn};
unicode_table(770) ->
    {230, [], [], mn};
unicode_table(771) ->
    {230, [], [], mn};
unicode_table(772) ->
    {230, [], [], mn};
unicode_table(773) ->
    {230, [], [], mn};
unicode_table(774) ->
    {230, [], [], mn};
unicode_table(775) ->
    {230, [], [], mn};
unicode_table(776) ->
    {230, [], [], mn};
unicode_table(777) ->
    {230, [], [], mn};
unicode_table(778) ->
    {230, [], [], mn};
unicode_table(779) ->
    {230, [], [], mn};
unicode_table(780) ->
    {230, [], [], mn};
unicode_table(781) ->
    {230, [], [], mn};
unicode_table(782) ->
    {230, [], [], mn};
unicode_table(783) ->
    {230, [], [], mn};
unicode_table(784) ->
    {230, [], [], mn};
unicode_table(785) ->
    {230, [], [], mn};
unicode_table(786) ->
    {230, [], [], mn};
unicode_table(787) ->
    {230, [], [], mn};
unicode_table(788) ->
    {230, [], [], mn};
unicode_table(789) ->
    {232, [], [], mn};
unicode_table(790) ->
    {220, [], [], mn};
unicode_table(791) ->
    {220, [], [], mn};
unicode_table(792) ->
    {220, [], [], mn};
unicode_table(793) ->
    {220, [], [], mn};
unicode_table(794) ->
    {232, [], [], mn};
unicode_table(795) ->
    {216, [], [], mn};
unicode_table(796) ->
    {220, [], [], mn};
unicode_table(797) ->
    {220, [], [], mn};
unicode_table(798) ->
    {220, [], [], mn};
unicode_table(799) ->
    {220, [], [], mn};
unicode_table(800) ->
    {220, [], [], mn};
unicode_table(801) ->
    {202, [], [], mn};
unicode_table(802) ->
    {202, [], [], mn};
unicode_table(803) ->
    {220, [], [], mn};
unicode_table(804) ->
    {220, [], [], mn};
unicode_table(805) ->
    {220, [], [], mn};
unicode_table(806) ->
    {220, [], [], mn};
unicode_table(807) ->
    {202, [], [], mn};
unicode_table(808) ->
    {202, [], [], mn};
unicode_table(809) ->
    {220, [], [], mn};
unicode_table(810) ->
    {220, [], [], mn};
unicode_table(811) ->
    {220, [], [], mn};
unicode_table(812) ->
    {220, [], [], mn};
unicode_table(813) ->
    {220, [], [], mn};
unicode_table(814) ->
    {220, [], [], mn};
unicode_table(815) ->
    {220, [], [], mn};
unicode_table(816) ->
    {220, [], [], mn};
unicode_table(817) ->
    {220, [], [], mn};
unicode_table(818) ->
    {220, [], [], mn};
unicode_table(819) ->
    {220, [], [], mn};
unicode_table(820) ->
    {1, [], [], mn};
unicode_table(821) ->
    {1, [], [], mn};
unicode_table(822) ->
    {1, [], [], mn};
unicode_table(823) ->
    {1, [], [], mn};
unicode_table(824) ->
    {1, [], [], mn};
unicode_table(825) ->
    {220, [], [], mn};
unicode_table(826) ->
    {220, [], [], mn};
unicode_table(827) ->
    {220, [], [], mn};
unicode_table(828) ->
    {220, [], [], mn};
unicode_table(829) ->
    {230, [], [], mn};
unicode_table(830) ->
    {230, [], [], mn};
unicode_table(831) ->
    {230, [], [], mn};
unicode_table(832) ->
    {230, [{230, 768}], [], mn};
unicode_table(833) ->
    {230, [{230, 769}], [], mn};
unicode_table(834) ->
    {230, [], [], mn};
unicode_table(835) ->
    {230, [{230, 787}], [], mn};
unicode_table(836) ->
    {230, [{230, 776}, {230, 769}], [], mn};
unicode_table(837) ->
    {240, [], [], mn};
unicode_table(838) ->
    {230, [], [], mn};
unicode_table(839) ->
    {220, [], [], mn};
unicode_table(840) ->
    {220, [], [], mn};
unicode_table(841) ->
    {220, [], [], mn};
unicode_table(842) ->
    {230, [], [], mn};
unicode_table(843) ->
    {230, [], [], mn};
unicode_table(844) ->
    {230, [], [], mn};
unicode_table(845) ->
    {220, [], [], mn};
unicode_table(846) ->
    {220, [], [], mn};
unicode_table(848) ->
    {230, [], [], mn};
unicode_table(849) ->
    {230, [], [], mn};
unicode_table(850) ->
    {230, [], [], mn};
unicode_table(851) ->
    {220, [], [], mn};
unicode_table(852) ->
    {220, [], [], mn};
unicode_table(853) ->
    {220, [], [], mn};
unicode_table(854) ->
    {220, [], [], mn};
unicode_table(855) ->
    {230, [], [], mn};
unicode_table(856) ->
    {232, [], [], mn};
unicode_table(857) ->
    {220, [], [], mn};
unicode_table(858) ->
    {220, [], [], mn};
unicode_table(859) ->
    {230, [], [], mn};
unicode_table(860) ->
    {233, [], [], mn};
unicode_table(861) ->
    {234, [], [], mn};
unicode_table(862) ->
    {234, [], [], mn};
unicode_table(863) ->
    {233, [], [], mn};
unicode_table(864) ->
    {234, [], [], mn};
unicode_table(865) ->
    {234, [], [], mn};
unicode_table(866) ->
    {233, [], [], mn};
unicode_table(867) ->
    {230, [], [], mn};
unicode_table(868) ->
    {230, [], [], mn};
unicode_table(869) ->
    {230, [], [], mn};
unicode_table(870) ->
    {230, [], [], mn};
unicode_table(871) ->
    {230, [], [], mn};
unicode_table(872) ->
    {230, [], [], mn};
unicode_table(873) ->
    {230, [], [], mn};
unicode_table(874) ->
    {230, [], [], mn};
unicode_table(875) ->
    {230, [], [], mn};
unicode_table(876) ->
    {230, [], [], mn};
unicode_table(877) ->
    {230, [], [], mn};
unicode_table(878) ->
    {230, [], [], mn};
unicode_table(879) ->
    {230, [], [], mn};
unicode_table(884) ->
    {0, [{0, 697}], [], lm};
unicode_table(890) ->
    {0, [], {compat, [{0, 32}, {240, 837}]}, lm};
unicode_table(894) ->
    {0, [{0, 59}], [], po};
unicode_table(900) ->
    {0, [], {compat, [{0, 32}, {230, 769}]}, sk};
unicode_table(901) ->
    {0, [{0, 168}, {230, 769}], {compat, [{0, 32}, {230, 776}, {230, 769}]}, sk};
unicode_table(902) ->
    {0, [{0, 913}, {230, 769}], [], lu};
unicode_table(903) ->
    {0, [{0, 183}], [], po};
unicode_table(904) ->
    {0, [{0, 917}, {230, 769}], [], lu};
unicode_table(905) ->
    {0, [{0, 919}, {230, 769}], [], lu};
unicode_table(906) ->
    {0, [{0, 921}, {230, 769}], [], lu};
unicode_table(908) ->
    {0, [{0, 927}, {230, 769}], [], lu};
unicode_table(910) ->
    {0, [{0, 933}, {230, 769}], [], lu};
unicode_table(911) ->
    {0, [{0, 937}, {230, 769}], [], lu};
unicode_table(912) ->
    {0, [{0, 953}, {230, 776}, {230, 769}], [], ll};
unicode_table(938) ->
    {0, [{0, 921}, {230, 776}], [], lu};
unicode_table(939) ->
    {0, [{0, 933}, {230, 776}], [], lu};
unicode_table(940) ->
    {0, [{0, 945}, {230, 769}], [], ll};
unicode_table(941) ->
    {0, [{0, 949}, {230, 769}], [], ll};
unicode_table(942) ->
    {0, [{0, 951}, {230, 769}], [], ll};
unicode_table(943) ->
    {0, [{0, 953}, {230, 769}], [], ll};
unicode_table(944) ->
    {0, [{0, 965}, {230, 776}, {230, 769}], [], ll};
unicode_table(970) ->
    {0, [{0, 953}, {230, 776}], [], ll};
unicode_table(971) ->
    {0, [{0, 965}, {230, 776}], [], ll};
unicode_table(972) ->
    {0, [{0, 959}, {230, 769}], [], ll};
unicode_table(973) ->
    {0, [{0, 965}, {230, 769}], [], ll};
unicode_table(974) ->
    {0, [{0, 969}, {230, 769}], [], ll};
unicode_table(976) ->
    {0, [], {compat, [{0, 946}]}, ll};
unicode_table(977) ->
    {0, [], {compat, [{0, 952}]}, ll};
unicode_table(978) ->
    {0, [], {compat, [{0, 933}]}, lu};
unicode_table(979) ->
    {0, [{0, 978}, {230, 769}], {compat, [{0, 933}, {230, 769}]}, lu};
unicode_table(980) ->
    {0, [{0, 978}, {230, 776}], {compat, [{0, 933}, {230, 776}]}, lu};
unicode_table(981) ->
    {0, [], {compat, [{0, 966}]}, ll};
unicode_table(982) ->
    {0, [], {compat, [{0, 960}]}, ll};
unicode_table(1008) ->
    {0, [], {compat, [{0, 954}]}, ll};
unicode_table(1009) ->
    {0, [], {compat, [{0, 961}]}, ll};
unicode_table(1010) ->
    {0, [], {compat, [{0, 962}]}, ll};
unicode_table(1012) ->
    {0, [], {compat, [{0, 920}]}, lu};
unicode_table(1013) ->
    {0, [], {compat, [{0, 949}]}, ll};
unicode_table(1017) ->
    {0, [], {compat, [{0, 931}]}, lu};
unicode_table(1024) ->
    {0, [{0, 1045}, {230, 768}], [], lu};
unicode_table(1025) ->
    {0, [{0, 1045}, {230, 776}], [], lu};
unicode_table(1027) ->
    {0, [{0, 1043}, {230, 769}], [], lu};
unicode_table(1031) ->
    {0, [{0, 1030}, {230, 776}], [], lu};
unicode_table(1036) ->
    {0, [{0, 1050}, {230, 769}], [], lu};
unicode_table(1037) ->
    {0, [{0, 1048}, {230, 768}], [], lu};
unicode_table(1038) ->
    {0, [{0, 1059}, {230, 774}], [], lu};
unicode_table(1049) ->
    {0, [{0, 1048}, {230, 774}], [], lu};
unicode_table(1081) ->
    {0, [{0, 1080}, {230, 774}], [], ll};
unicode_table(1104) ->
    {0, [{0, 1077}, {230, 768}], [], ll};
unicode_table(1105) ->
    {0, [{0, 1077}, {230, 776}], [], ll};
unicode_table(1107) ->
    {0, [{0, 1075}, {230, 769}], [], ll};
unicode_table(1111) ->
    {0, [{0, 1110}, {230, 776}], [], ll};
unicode_table(1116) ->
    {0, [{0, 1082}, {230, 769}], [], ll};
unicode_table(1117) ->
    {0, [{0, 1080}, {230, 768}], [], ll};
unicode_table(1118) ->
    {0, [{0, 1091}, {230, 774}], [], ll};
unicode_table(1142) ->
    {0, [{0, 1140}, {230, 783}], [], lu};
unicode_table(1143) ->
    {0, [{0, 1141}, {230, 783}], [], ll};
unicode_table(1155) ->
    {230, [], [], mn};
unicode_table(1156) ->
    {230, [], [], mn};
unicode_table(1157) ->
    {230, [], [], mn};
unicode_table(1158) ->
    {230, [], [], mn};
unicode_table(1159) ->
    {230, [], [], mn};
unicode_table(1217) ->
    {0, [{0, 1046}, {230, 774}], [], lu};
unicode_table(1218) ->
    {0, [{0, 1078}, {230, 774}], [], ll};
unicode_table(1232) ->
    {0, [{0, 1040}, {230, 774}], [], lu};
unicode_table(1233) ->
    {0, [{0, 1072}, {230, 774}], [], ll};
unicode_table(1234) ->
    {0, [{0, 1040}, {230, 776}], [], lu};
unicode_table(1235) ->
    {0, [{0, 1072}, {230, 776}], [], ll};
unicode_table(1238) ->
    {0, [{0, 1045}, {230, 774}], [], lu};
unicode_table(1239) ->
    {0, [{0, 1077}, {230, 774}], [], ll};
unicode_table(1242) ->
    {0, [{0, 1240}, {230, 776}], [], lu};
unicode_table(1243) ->
    {0, [{0, 1241}, {230, 776}], [], ll};
unicode_table(1244) ->
    {0, [{0, 1046}, {230, 776}], [], lu};
unicode_table(1245) ->
    {0, [{0, 1078}, {230, 776}], [], ll};
unicode_table(1246) ->
    {0, [{0, 1047}, {230, 776}], [], lu};
unicode_table(1247) ->
    {0, [{0, 1079}, {230, 776}], [], ll};
unicode_table(1250) ->
    {0, [{0, 1048}, {230, 772}], [], lu};
unicode_table(1251) ->
    {0, [{0, 1080}, {230, 772}], [], ll};
unicode_table(1252) ->
    {0, [{0, 1048}, {230, 776}], [], lu};
unicode_table(1253) ->
    {0, [{0, 1080}, {230, 776}], [], ll};
unicode_table(1254) ->
    {0, [{0, 1054}, {230, 776}], [], lu};
unicode_table(1255) ->
    {0, [{0, 1086}, {230, 776}], [], ll};
unicode_table(1258) ->
    {0, [{0, 1256}, {230, 776}], [], lu};
unicode_table(1259) ->
    {0, [{0, 1257}, {230, 776}], [], ll};
unicode_table(1260) ->
    {0, [{0, 1069}, {230, 776}], [], lu};
unicode_table(1261) ->
    {0, [{0, 1101}, {230, 776}], [], ll};
unicode_table(1262) ->
    {0, [{0, 1059}, {230, 772}], [], lu};
unicode_table(1263) ->
    {0, [{0, 1091}, {230, 772}], [], ll};
unicode_table(1264) ->
    {0, [{0, 1059}, {230, 776}], [], lu};
unicode_table(1265) ->
    {0, [{0, 1091}, {230, 776}], [], ll};
unicode_table(1266) ->
    {0, [{0, 1059}, {230, 779}], [], lu};
unicode_table(1267) ->
    {0, [{0, 1091}, {230, 779}], [], ll};
unicode_table(1268) ->
    {0, [{0, 1063}, {230, 776}], [], lu};
unicode_table(1269) ->
    {0, [{0, 1095}, {230, 776}], [], ll};
unicode_table(1272) ->
    {0, [{0, 1067}, {230, 776}], [], lu};
unicode_table(1273) ->
    {0, [{0, 1099}, {230, 776}], [], ll};
unicode_table(1415) ->
    {0, [], {compat, [{0, 1381}, {0, 1410}]}, ll};
unicode_table(1425) ->
    {220, [], [], mn};
unicode_table(1426) ->
    {230, [], [], mn};
unicode_table(1427) ->
    {230, [], [], mn};
unicode_table(1428) ->
    {230, [], [], mn};
unicode_table(1429) ->
    {230, [], [], mn};
unicode_table(1430) ->
    {220, [], [], mn};
unicode_table(1431) ->
    {230, [], [], mn};
unicode_table(1432) ->
    {230, [], [], mn};
unicode_table(1433) ->
    {230, [], [], mn};
unicode_table(1434) ->
    {222, [], [], mn};
unicode_table(1435) ->
    {220, [], [], mn};
unicode_table(1436) ->
    {230, [], [], mn};
unicode_table(1437) ->
    {230, [], [], mn};
unicode_table(1438) ->
    {230, [], [], mn};
unicode_table(1439) ->
    {230, [], [], mn};
unicode_table(1440) ->
    {230, [], [], mn};
unicode_table(1441) ->
    {230, [], [], mn};
unicode_table(1442) ->
    {220, [], [], mn};
unicode_table(1443) ->
    {220, [], [], mn};
unicode_table(1444) ->
    {220, [], [], mn};
unicode_table(1445) ->
    {220, [], [], mn};
unicode_table(1446) ->
    {220, [], [], mn};
unicode_table(1447) ->
    {220, [], [], mn};
unicode_table(1448) ->
    {230, [], [], mn};
unicode_table(1449) ->
    {230, [], [], mn};
unicode_table(1450) ->
    {220, [], [], mn};
unicode_table(1451) ->
    {230, [], [], mn};
unicode_table(1452) ->
    {230, [], [], mn};
unicode_table(1453) ->
    {222, [], [], mn};
unicode_table(1454) ->
    {228, [], [], mn};
unicode_table(1455) ->
    {230, [], [], mn};
unicode_table(1456) ->
    {10, [], [], mn};
unicode_table(1457) ->
    {11, [], [], mn};
unicode_table(1458) ->
    {12, [], [], mn};
unicode_table(1459) ->
    {13, [], [], mn};
unicode_table(1460) ->
    {14, [], [], mn};
unicode_table(1461) ->
    {15, [], [], mn};
unicode_table(1462) ->
    {16, [], [], mn};
unicode_table(1463) ->
    {17, [], [], mn};
unicode_table(1464) ->
    {18, [], [], mn};
unicode_table(1465) ->
    {19, [], [], mn};
unicode_table(1466) ->
    {19, [], [], mn};
unicode_table(1467) ->
    {20, [], [], mn};
unicode_table(1468) ->
    {21, [], [], mn};
unicode_table(1469) ->
    {22, [], [], mn};
unicode_table(1471) ->
    {23, [], [], mn};
unicode_table(1473) ->
    {24, [], [], mn};
unicode_table(1474) ->
    {25, [], [], mn};
unicode_table(1476) ->
    {230, [], [], mn};
unicode_table(1477) ->
    {220, [], [], mn};
unicode_table(1479) ->
    {18, [], [], mn};
unicode_table(1552) ->
    {230, [], [], mn};
unicode_table(1553) ->
    {230, [], [], mn};
unicode_table(1554) ->
    {230, [], [], mn};
unicode_table(1555) ->
    {230, [], [], mn};
unicode_table(1556) ->
    {230, [], [], mn};
unicode_table(1557) ->
    {230, [], [], mn};
unicode_table(1558) ->
    {230, [], [], mn};
unicode_table(1559) ->
    {230, [], [], mn};
unicode_table(1560) ->
    {30, [], [], mn};
unicode_table(1561) ->
    {31, [], [], mn};
unicode_table(1562) ->
    {32, [], [], mn};
unicode_table(1570) ->
    {0, [{0, 1575}, {230, 1619}], [], lo};
unicode_table(1571) ->
    {0, [{0, 1575}, {230, 1620}], [], lo};
unicode_table(1572) ->
    {0, [{0, 1608}, {230, 1620}], [], lo};
unicode_table(1573) ->
    {0, [{0, 1575}, {220, 1621}], [], lo};
unicode_table(1574) ->
    {0, [{0, 1610}, {230, 1620}], [], lo};
unicode_table(1611) ->
    {27, [], [], mn};
unicode_table(1612) ->
    {28, [], [], mn};
unicode_table(1613) ->
    {29, [], [], mn};
unicode_table(1614) ->
    {30, [], [], mn};
unicode_table(1615) ->
    {31, [], [], mn};
unicode_table(1616) ->
    {32, [], [], mn};
unicode_table(1617) ->
    {33, [], [], mn};
unicode_table(1618) ->
    {34, [], [], mn};
unicode_table(1619) ->
    {230, [], [], mn};
unicode_table(1620) ->
    {230, [], [], mn};
unicode_table(1621) ->
    {220, [], [], mn};
unicode_table(1622) ->
    {220, [], [], mn};
unicode_table(1623) ->
    {230, [], [], mn};
unicode_table(1624) ->
    {230, [], [], mn};
unicode_table(1625) ->
    {230, [], [], mn};
unicode_table(1626) ->
    {230, [], [], mn};
unicode_table(1627) ->
    {230, [], [], mn};
unicode_table(1628) ->
    {220, [], [], mn};
unicode_table(1629) ->
    {230, [], [], mn};
unicode_table(1630) ->
    {230, [], [], mn};
unicode_table(1631) ->
    {220, [], [], mn};
unicode_table(1648) ->
    {35, [], [], mn};
unicode_table(1653) ->
    {0, [], {compat, [{0, 1575}, {0, 1652}]}, lo};
unicode_table(1654) ->
    {0, [], {compat, [{0, 1608}, {0, 1652}]}, lo};
unicode_table(1655) ->
    {0, [], {compat, [{0, 1735}, {0, 1652}]}, lo};
unicode_table(1656) ->
    {0, [], {compat, [{0, 1610}, {0, 1652}]}, lo};
unicode_table(1728) ->
    {0, [{0, 1749}, {230, 1620}], [], lo};
unicode_table(1730) ->
    {0, [{0, 1729}, {230, 1620}], [], lo};
unicode_table(1747) ->
    {0, [{0, 1746}, {230, 1620}], [], lo};
unicode_table(1750) ->
    {230, [], [], mn};
unicode_table(1751) ->
    {230, [], [], mn};
unicode_table(1752) ->
    {230, [], [], mn};
unicode_table(1753) ->
    {230, [], [], mn};
unicode_table(1754) ->
    {230, [], [], mn};
unicode_table(1755) ->
    {230, [], [], mn};
unicode_table(1756) ->
    {230, [], [], mn};
unicode_table(1759) ->
    {230, [], [], mn};
unicode_table(1760) ->
    {230, [], [], mn};
unicode_table(1761) ->
    {230, [], [], mn};
unicode_table(1762) ->
    {230, [], [], mn};
unicode_table(1763) ->
    {220, [], [], mn};
unicode_table(1764) ->
    {230, [], [], mn};
unicode_table(1767) ->
    {230, [], [], mn};
unicode_table(1768) ->
    {230, [], [], mn};
unicode_table(1770) ->
    {220, [], [], mn};
unicode_table(1771) ->
    {230, [], [], mn};
unicode_table(1772) ->
    {230, [], [], mn};
unicode_table(1773) ->
    {220, [], [], mn};
unicode_table(1809) ->
    {36, [], [], mn};
unicode_table(1840) ->
    {230, [], [], mn};
unicode_table(1841) ->
    {220, [], [], mn};
unicode_table(1842) ->
    {230, [], [], mn};
unicode_table(1843) ->
    {230, [], [], mn};
unicode_table(1844) ->
    {220, [], [], mn};
unicode_table(1845) ->
    {230, [], [], mn};
unicode_table(1846) ->
    {230, [], [], mn};
unicode_table(1847) ->
    {220, [], [], mn};
unicode_table(1848) ->
    {220, [], [], mn};
unicode_table(1849) ->
    {220, [], [], mn};
unicode_table(1850) ->
    {230, [], [], mn};
unicode_table(1851) ->
    {220, [], [], mn};
unicode_table(1852) ->
    {220, [], [], mn};
unicode_table(1853) ->
    {230, [], [], mn};
unicode_table(1854) ->
    {220, [], [], mn};
unicode_table(1855) ->
    {230, [], [], mn};
unicode_table(1856) ->
    {230, [], [], mn};
unicode_table(1857) ->
    {230, [], [], mn};
unicode_table(1858) ->
    {220, [], [], mn};
unicode_table(1859) ->
    {230, [], [], mn};
unicode_table(1860) ->
    {220, [], [], mn};
unicode_table(1861) ->
    {230, [], [], mn};
unicode_table(1862) ->
    {220, [], [], mn};
unicode_table(1863) ->
    {230, [], [], mn};
unicode_table(1864) ->
    {220, [], [], mn};
unicode_table(1865) ->
    {230, [], [], mn};
unicode_table(1866) ->
    {230, [], [], mn};
unicode_table(2027) ->
    {230, [], [], mn};
unicode_table(2028) ->
    {230, [], [], mn};
unicode_table(2029) ->
    {230, [], [], mn};
unicode_table(2030) ->
    {230, [], [], mn};
unicode_table(2031) ->
    {230, [], [], mn};
unicode_table(2032) ->
    {230, [], [], mn};
unicode_table(2033) ->
    {230, [], [], mn};
unicode_table(2034) ->
    {220, [], [], mn};
unicode_table(2035) ->
    {230, [], [], mn};
unicode_table(2045) ->
    {220, [], [], mn};
unicode_table(2070) ->
    {230, [], [], mn};
unicode_table(2071) ->
    {230, [], [], mn};
unicode_table(2072) ->
    {230, [], [], mn};
unicode_table(2073) ->
    {230, [], [], mn};
unicode_table(2075) ->
    {230, [], [], mn};
unicode_table(2076) ->
    {230, [], [], mn};
unicode_table(2077) ->
    {230, [], [], mn};
unicode_table(2078) ->
    {230, [], [], mn};
unicode_table(2079) ->
    {230, [], [], mn};
unicode_table(2080) ->
    {230, [], [], mn};
unicode_table(2081) ->
    {230, [], [], mn};
unicode_table(2082) ->
    {230, [], [], mn};
unicode_table(2083) ->
    {230, [], [], mn};
unicode_table(2085) ->
    {230, [], [], mn};
unicode_table(2086) ->
    {230, [], [], mn};
unicode_table(2087) ->
    {230, [], [], mn};
unicode_table(2089) ->
    {230, [], [], mn};
unicode_table(2090) ->
    {230, [], [], mn};
unicode_table(2091) ->
    {230, [], [], mn};
unicode_table(2092) ->
    {230, [], [], mn};
unicode_table(2093) ->
    {230, [], [], mn};
unicode_table(2137) ->
    {220, [], [], mn};
unicode_table(2138) ->
    {220, [], [], mn};
unicode_table(2139) ->
    {220, [], [], mn};
unicode_table(2200) ->
    {230, [], [], mn};
unicode_table(2201) ->
    {220, [], [], mn};
unicode_table(2202) ->
    {220, [], [], mn};
unicode_table(2203) ->
    {220, [], [], mn};
unicode_table(2204) ->
    {230, [], [], mn};
unicode_table(2205) ->
    {230, [], [], mn};
unicode_table(2206) ->
    {230, [], [], mn};
unicode_table(2207) ->
    {230, [], [], mn};
unicode_table(2250) ->
    {230, [], [], mn};
unicode_table(2251) ->
    {230, [], [], mn};
unicode_table(2252) ->
    {230, [], [], mn};
unicode_table(2253) ->
    {230, [], [], mn};
unicode_table(2254) ->
    {230, [], [], mn};
unicode_table(2255) ->
    {220, [], [], mn};
unicode_table(2256) ->
    {220, [], [], mn};
unicode_table(2257) ->
    {220, [], [], mn};
unicode_table(2258) ->
    {220, [], [], mn};
unicode_table(2259) ->
    {220, [], [], mn};
unicode_table(2260) ->
    {230, [], [], mn};
unicode_table(2261) ->
    {230, [], [], mn};
unicode_table(2262) ->
    {230, [], [], mn};
unicode_table(2263) ->
    {230, [], [], mn};
unicode_table(2264) ->
    {230, [], [], mn};
unicode_table(2265) ->
    {230, [], [], mn};
unicode_table(2266) ->
    {230, [], [], mn};
unicode_table(2267) ->
    {230, [], [], mn};
unicode_table(2268) ->
    {230, [], [], mn};
unicode_table(2269) ->
    {230, [], [], mn};
unicode_table(2270) ->
    {230, [], [], mn};
unicode_table(2271) ->
    {230, [], [], mn};
unicode_table(2272) ->
    {230, [], [], mn};
unicode_table(2273) ->
    {230, [], [], mn};
unicode_table(2275) ->
    {220, [], [], mn};
unicode_table(2276) ->
    {230, [], [], mn};
unicode_table(2277) ->
    {230, [], [], mn};
unicode_table(2278) ->
    {220, [], [], mn};
unicode_table(2279) ->
    {230, [], [], mn};
unicode_table(2280) ->
    {230, [], [], mn};
unicode_table(2281) ->
    {220, [], [], mn};
unicode_table(2282) ->
    {230, [], [], mn};
unicode_table(2283) ->
    {230, [], [], mn};
unicode_table(2284) ->
    {230, [], [], mn};
unicode_table(2285) ->
    {220, [], [], mn};
unicode_table(2286) ->
    {220, [], [], mn};
unicode_table(2287) ->
    {220, [], [], mn};
unicode_table(2288) ->
    {27, [], [], mn};
unicode_table(2289) ->
    {28, [], [], mn};
unicode_table(2290) ->
    {29, [], [], mn};
unicode_table(2291) ->
    {230, [], [], mn};
unicode_table(2292) ->
    {230, [], [], mn};
unicode_table(2293) ->
    {230, [], [], mn};
unicode_table(2294) ->
    {220, [], [], mn};
unicode_table(2295) ->
    {230, [], [], mn};
unicode_table(2296) ->
    {230, [], [], mn};
unicode_table(2297) ->
    {220, [], [], mn};
unicode_table(2298) ->
    {220, [], [], mn};
unicode_table(2299) ->
    {230, [], [], mn};
unicode_table(2300) ->
    {230, [], [], mn};
unicode_table(2301) ->
    {230, [], [], mn};
unicode_table(2302) ->
    {230, [], [], mn};
unicode_table(2303) ->
    {230, [], [], mn};
unicode_table(2345) ->
    {0, [{0, 2344}, {7, 2364}], [], lo};
unicode_table(2353) ->
    {0, [{0, 2352}, {7, 2364}], [], lo};
unicode_table(2356) ->
    {0, [{0, 2355}, {7, 2364}], [], lo};
unicode_table(2364) ->
    {7, [], [], mn};
unicode_table(2381) ->
    {9, [], [], mn};
unicode_table(2385) ->
    {230, [], [], mn};
unicode_table(2386) ->
    {220, [], [], mn};
unicode_table(2387) ->
    {230, [], [], mn};
unicode_table(2388) ->
    {230, [], [], mn};
unicode_table(2392) ->
    {0, [{0, 2325}, {7, 2364}], [], lo};
unicode_table(2393) ->
    {0, [{0, 2326}, {7, 2364}], [], lo};
unicode_table(2394) ->
    {0, [{0, 2327}, {7, 2364}], [], lo};
unicode_table(2395) ->
    {0, [{0, 2332}, {7, 2364}], [], lo};
unicode_table(2396) ->
    {0, [{0, 2337}, {7, 2364}], [], lo};
unicode_table(2397) ->
    {0, [{0, 2338}, {7, 2364}], [], lo};
unicode_table(2398) ->
    {0, [{0, 2347}, {7, 2364}], [], lo};
unicode_table(2399) ->
    {0, [{0, 2351}, {7, 2364}], [], lo};
unicode_table(2492) ->
    {7, [], [], mn};
unicode_table(2507) ->
    {0, [{0, 2503}, {0, 2494}], [], mc};
unicode_table(2508) ->
    {0, [{0, 2503}, {0, 2519}], [], mc};
unicode_table(2509) ->
    {9, [], [], mn};
unicode_table(2524) ->
    {0, [{0, 2465}, {7, 2492}], [], lo};
unicode_table(2525) ->
    {0, [{0, 2466}, {7, 2492}], [], lo};
unicode_table(2527) ->
    {0, [{0, 2479}, {7, 2492}], [], lo};
unicode_table(2558) ->
    {230, [], [], mn};
unicode_table(2611) ->
    {0, [{0, 2610}, {7, 2620}], [], lo};
unicode_table(2614) ->
    {0, [{0, 2616}, {7, 2620}], [], lo};
unicode_table(2620) ->
    {7, [], [], mn};
unicode_table(2637) ->
    {9, [], [], mn};
unicode_table(2649) ->
    {0, [{0, 2582}, {7, 2620}], [], lo};
unicode_table(2650) ->
    {0, [{0, 2583}, {7, 2620}], [], lo};
unicode_table(2651) ->
    {0, [{0, 2588}, {7, 2620}], [], lo};
unicode_table(2654) ->
    {0, [{0, 2603}, {7, 2620}], [], lo};
unicode_table(2748) ->
    {7, [], [], mn};
unicode_table(2765) ->
    {9, [], [], mn};
unicode_table(2876) ->
    {7, [], [], mn};
unicode_table(2888) ->
    {0, [{0, 2887}, {0, 2902}], [], mc};
unicode_table(2891) ->
    {0, [{0, 2887}, {0, 2878}], [], mc};
unicode_table(2892) ->
    {0, [{0, 2887}, {0, 2903}], [], mc};
unicode_table(2893) ->
    {9, [], [], mn};
unicode_table(2908) ->
    {0, [{0, 2849}, {7, 2876}], [], lo};
unicode_table(2909) ->
    {0, [{0, 2850}, {7, 2876}], [], lo};
unicode_table(2964) ->
    {0, [{0, 2962}, {0, 3031}], [], lo};
unicode_table(3018) ->
    {0, [{0, 3014}, {0, 3006}], [], mc};
unicode_table(3019) ->
    {0, [{0, 3015}, {0, 3006}], [], mc};
unicode_table(3020) ->
    {0, [{0, 3014}, {0, 3031}], [], mc};
unicode_table(3021) ->
    {9, [], [], mn};
unicode_table(3132) ->
    {7, [], [], mn};
unicode_table(3144) ->
    {0, [{0, 3142}, {91, 3158}], [], mn};
unicode_table(3149) ->
    {9, [], [], mn};
unicode_table(3157) ->
    {84, [], [], mn};
unicode_table(3158) ->
    {91, [], [], mn};
unicode_table(3260) ->
    {7, [], [], mn};
unicode_table(3264) ->
    {0, [{0, 3263}, {0, 3285}], [], mc};
unicode_table(3271) ->
    {0, [{0, 3270}, {0, 3285}], [], mc};
unicode_table(3272) ->
    {0, [{0, 3270}, {0, 3286}], [], mc};
unicode_table(3274) ->
    {0, [{0, 3270}, {0, 3266}], [], mc};
unicode_table(3275) ->
    {0, [{0, 3270}, {0, 3266}, {0, 3285}], [], mc};
unicode_table(3277) ->
    {9, [], [], mn};
unicode_table(3387) ->
    {9, [], [], mn};
unicode_table(3388) ->
    {9, [], [], mn};
unicode_table(3402) ->
    {0, [{0, 3398}, {0, 3390}], [], mc};
unicode_table(3403) ->
    {0, [{0, 3399}, {0, 3390}], [], mc};
unicode_table(3404) ->
    {0, [{0, 3398}, {0, 3415}], [], mc};
unicode_table(3405) ->
    {9, [], [], mn};
unicode_table(3530) ->
    {9, [], [], mn};
unicode_table(3546) ->
    {0, [{0, 3545}, {9, 3530}], [], mc};
unicode_table(3548) ->
    {0, [{0, 3545}, {0, 3535}], [], mc};
unicode_table(3549) ->
    {0, [{0, 3545}, {0, 3535}, {9, 3530}], [], mc};
unicode_table(3550) ->
    {0, [{0, 3545}, {0, 3551}], [], mc};
unicode_table(3635) ->
    {0, [], {compat, [{0, 3661}, {0, 3634}]}, lo};
unicode_table(3640) ->
    {103, [], [], mn};
unicode_table(3641) ->
    {103, [], [], mn};
unicode_table(3642) ->
    {9, [], [], mn};
unicode_table(3656) ->
    {107, [], [], mn};
unicode_table(3657) ->
    {107, [], [], mn};
unicode_table(3658) ->
    {107, [], [], mn};
unicode_table(3659) ->
    {107, [], [], mn};
unicode_table(3763) ->
    {0, [], {compat, [{0, 3789}, {0, 3762}]}, lo};
unicode_table(3768) ->
    {118, [], [], mn};
unicode_table(3769) ->
    {118, [], [], mn};
unicode_table(3770) ->
    {9, [], [], mn};
unicode_table(3784) ->
    {122, [], [], mn};
unicode_table(3785) ->
    {122, [], [], mn};
unicode_table(3786) ->
    {122, [], [], mn};
unicode_table(3787) ->
    {122, [], [], mn};
unicode_table(3804) ->
    {0, [], {compat, [{0, 3755}, {0, 3737}]}, lo};
unicode_table(3805) ->
    {0, [], {compat, [{0, 3755}, {0, 3745}]}, lo};
unicode_table(3852) ->
    {0, [], {noBreak, [{0, 3851}]}, po};
unicode_table(3864) ->
    {220, [], [], mn};
unicode_table(3865) ->
    {220, [], [], mn};
unicode_table(3893) ->
    {220, [], [], mn};
unicode_table(3895) ->
    {220, [], [], mn};
unicode_table(3897) ->
    {216, [], [], mn};
unicode_table(3907) ->
    {0, [{0, 3906}, {0, 4023}], [], lo};
unicode_table(3917) ->
    {0, [{0, 3916}, {0, 4023}], [], lo};
unicode_table(3922) ->
    {0, [{0, 3921}, {0, 4023}], [], lo};
unicode_table(3927) ->
    {0, [{0, 3926}, {0, 4023}], [], lo};
unicode_table(3932) ->
    {0, [{0, 3931}, {0, 4023}], [], lo};
unicode_table(3945) ->
    {0, [{0, 3904}, {0, 4021}], [], lo};
unicode_table(3953) ->
    {129, [], [], mn};
unicode_table(3954) ->
    {130, [], [], mn};
unicode_table(3955) ->
    {0, [{129, 3953}, {130, 3954}], [], mn};
unicode_table(3956) ->
    {132, [], [], mn};
unicode_table(3957) ->
    {0, [{129, 3953}, {132, 3956}], [], mn};
unicode_table(3958) ->
    {0, [{0, 4018}, {130, 3968}], [], mn};
unicode_table(3959) ->
    {0, [], {compat, [{0, 4018}, {129, 3953}, {130, 3968}]}, mn};
unicode_table(3960) ->
    {0, [{0, 4019}, {130, 3968}], [], mn};
unicode_table(3961) ->
    {0, [], {compat, [{0, 4019}, {129, 3953}, {130, 3968}]}, mn};
unicode_table(3962) ->
    {130, [], [], mn};
unicode_table(3963) ->
    {130, [], [], mn};
unicode_table(3964) ->
    {130, [], [], mn};
unicode_table(3965) ->
    {130, [], [], mn};
unicode_table(3968) ->
    {130, [], [], mn};
unicode_table(3969) ->
    {0, [{129, 3953}, {130, 3968}], [], mn};
unicode_table(3970) ->
    {230, [], [], mn};
unicode_table(3971) ->
    {230, [], [], mn};
unicode_table(3972) ->
    {9, [], [], mn};
unicode_table(3974) ->
    {230, [], [], mn};
unicode_table(3975) ->
    {230, [], [], mn};
unicode_table(3987) ->
    {0, [{0, 3986}, {0, 4023}], [], mn};
unicode_table(3997) ->
    {0, [{0, 3996}, {0, 4023}], [], mn};
unicode_table(4002) ->
    {0, [{0, 4001}, {0, 4023}], [], mn};
unicode_table(4007) ->
    {0, [{0, 4006}, {0, 4023}], [], mn};
unicode_table(4012) ->
    {0, [{0, 4011}, {0, 4023}], [], mn};
unicode_table(4025) ->
    {0, [{0, 3984}, {0, 4021}], [], mn};
unicode_table(4038) ->
    {220, [], [], mn};
unicode_table(4134) ->
    {0, [{0, 4133}, {0, 4142}], [], lo};
unicode_table(4151) ->
    {7, [], [], mn};
unicode_table(4153) ->
    {9, [], [], mn};
unicode_table(4154) ->
    {9, [], [], mn};
unicode_table(4237) ->
    {220, [], [], mn};
unicode_table(4348) ->
    {0, [], {super, [{0, 4316}]}, lm};
unicode_table(4957) ->
    {230, [], [], mn};
unicode_table(4958) ->
    {230, [], [], mn};
unicode_table(4959) ->
    {230, [], [], mn};
unicode_table(5908) ->
    {9, [], [], mn};
unicode_table(5909) ->
    {9, [], [], mc};
unicode_table(5940) ->
    {9, [], [], mc};
unicode_table(6098) ->
    {9, [], [], mn};
unicode_table(6109) ->
    {230, [], [], mn};
unicode_table(6313) ->
    {228, [], [], mn};
unicode_table(6457) ->
    {222, [], [], mn};
unicode_table(6458) ->
    {230, [], [], mn};
unicode_table(6459) ->
    {220, [], [], mn};
unicode_table(6679) ->
    {230, [], [], mn};
unicode_table(6680) ->
    {220, [], [], mn};
unicode_table(6752) ->
    {9, [], [], mn};
unicode_table(6773) ->
    {230, [], [], mn};
unicode_table(6774) ->
    {230, [], [], mn};
unicode_table(6775) ->
    {230, [], [], mn};
unicode_table(6776) ->
    {230, [], [], mn};
unicode_table(6777) ->
    {230, [], [], mn};
unicode_table(6778) ->
    {230, [], [], mn};
unicode_table(6779) ->
    {230, [], [], mn};
unicode_table(6780) ->
    {230, [], [], mn};
unicode_table(6783) ->
    {220, [], [], mn};
unicode_table(6832) ->
    {230, [], [], mn};
unicode_table(6833) ->
    {230, [], [], mn};
unicode_table(6834) ->
    {230, [], [], mn};
unicode_table(6835) ->
    {230, [], [], mn};
unicode_table(6836) ->
    {230, [], [], mn};
unicode_table(6837) ->
    {220, [], [], mn};
unicode_table(6838) ->
    {220, [], [], mn};
unicode_table(6839) ->
    {220, [], [], mn};
unicode_table(6840) ->
    {220, [], [], mn};
unicode_table(6841) ->
    {220, [], [], mn};
unicode_table(6842) ->
    {220, [], [], mn};
unicode_table(6843) ->
    {230, [], [], mn};
unicode_table(6844) ->
    {230, [], [], mn};
unicode_table(6845) ->
    {220, [], [], mn};
unicode_table(6847) ->
    {220, [], [], mn};
unicode_table(6848) ->
    {220, [], [], mn};
unicode_table(6849) ->
    {230, [], [], mn};
unicode_table(6850) ->
    {230, [], [], mn};
unicode_table(6851) ->
    {220, [], [], mn};
unicode_table(6852) ->
    {220, [], [], mn};
unicode_table(6853) ->
    {230, [], [], mn};
unicode_table(6854) ->
    {230, [], [], mn};
unicode_table(6855) ->
    {230, [], [], mn};
unicode_table(6856) ->
    {230, [], [], mn};
unicode_table(6857) ->
    {230, [], [], mn};
unicode_table(6858) ->
    {220, [], [], mn};
unicode_table(6859) ->
    {230, [], [], mn};
unicode_table(6860) ->
    {230, [], [], mn};
unicode_table(6861) ->
    {230, [], [], mn};
unicode_table(6862) ->
    {230, [], [], mn};
unicode_table(6918) ->
    {0, [{0, 6917}, {0, 6965}], [], lo};
unicode_table(6920) ->
    {0, [{0, 6919}, {0, 6965}], [], lo};
unicode_table(6922) ->
    {0, [{0, 6921}, {0, 6965}], [], lo};
unicode_table(6924) ->
    {0, [{0, 6923}, {0, 6965}], [], lo};
unicode_table(6926) ->
    {0, [{0, 6925}, {0, 6965}], [], lo};
unicode_table(6930) ->
    {0, [{0, 6929}, {0, 6965}], [], lo};
unicode_table(6964) ->
    {7, [], [], mn};
unicode_table(6971) ->
    {0, [{0, 6970}, {0, 6965}], [], mc};
unicode_table(6973) ->
    {0, [{0, 6972}, {0, 6965}], [], mc};
unicode_table(6976) ->
    {0, [{0, 6974}, {0, 6965}], [], mc};
unicode_table(6977) ->
    {0, [{0, 6975}, {0, 6965}], [], mc};
unicode_table(6979) ->
    {0, [{0, 6978}, {0, 6965}], [], mc};
unicode_table(6980) ->
    {9, [], [], mc};
unicode_table(7019) ->
    {230, [], [], mn};
unicode_table(7020) ->
    {220, [], [], mn};
unicode_table(7021) ->
    {230, [], [], mn};
unicode_table(7022) ->
    {230, [], [], mn};
unicode_table(7023) ->
    {230, [], [], mn};
unicode_table(7024) ->
    {230, [], [], mn};
unicode_table(7025) ->
    {230, [], [], mn};
unicode_table(7026) ->
    {230, [], [], mn};
unicode_table(7027) ->
    {230, [], [], mn};
unicode_table(7082) ->
    {9, [], [], mc};
unicode_table(7083) ->
    {9, [], [], mn};
unicode_table(7142) ->
    {7, [], [], mn};
unicode_table(7154) ->
    {9, [], [], mc};
unicode_table(7155) ->
    {9, [], [], mc};
unicode_table(7223) ->
    {7, [], [], mn};
unicode_table(7376) ->
    {230, [], [], mn};
unicode_table(7377) ->
    {230, [], [], mn};
unicode_table(7378) ->
    {230, [], [], mn};
unicode_table(7380) ->
    {1, [], [], mn};
unicode_table(7381) ->
    {220, [], [], mn};
unicode_table(7382) ->
    {220, [], [], mn};
unicode_table(7383) ->
    {220, [], [], mn};
unicode_table(7384) ->
    {220, [], [], mn};
unicode_table(7385) ->
    {220, [], [], mn};
unicode_table(7386) ->
    {230, [], [], mn};
unicode_table(7387) ->
    {230, [], [], mn};
unicode_table(7388) ->
    {220, [], [], mn};
unicode_table(7389) ->
    {220, [], [], mn};
unicode_table(7390) ->
    {220, [], [], mn};
unicode_table(7391) ->
    {220, [], [], mn};
unicode_table(7392) ->
    {230, [], [], mn};
unicode_table(7394) ->
    {1, [], [], mn};
unicode_table(7395) ->
    {1, [], [], mn};
unicode_table(7396) ->
    {1, [], [], mn};
unicode_table(7397) ->
    {1, [], [], mn};
unicode_table(7398) ->
    {1, [], [], mn};
unicode_table(7399) ->
    {1, [], [], mn};
unicode_table(7400) ->
    {1, [], [], mn};
unicode_table(7405) ->
    {220, [], [], mn};
unicode_table(7412) ->
    {230, [], [], mn};
unicode_table(7416) ->
    {230, [], [], mn};
unicode_table(7417) ->
    {230, [], [], mn};
unicode_table(7468) ->
    {0, [], {super, [{0, 65}]}, lm};
unicode_table(7469) ->
    {0, [], {super, [{0, 198}]}, lm};
unicode_table(7470) ->
    {0, [], {super, [{0, 66}]}, lm};
unicode_table(7472) ->
    {0, [], {super, [{0, 68}]}, lm};
unicode_table(7473) ->
    {0, [], {super, [{0, 69}]}, lm};
unicode_table(7474) ->
    {0, [], {super, [{0, 398}]}, lm};
unicode_table(7475) ->
    {0, [], {super, [{0, 71}]}, lm};
unicode_table(7476) ->
    {0, [], {super, [{0, 72}]}, lm};
unicode_table(7477) ->
    {0, [], {super, [{0, 73}]}, lm};
unicode_table(7478) ->
    {0, [], {super, [{0, 74}]}, lm};
unicode_table(7479) ->
    {0, [], {super, [{0, 75}]}, lm};
unicode_table(7480) ->
    {0, [], {super, [{0, 76}]}, lm};
unicode_table(7481) ->
    {0, [], {super, [{0, 77}]}, lm};
unicode_table(7482) ->
    {0, [], {super, [{0, 78}]}, lm};
unicode_table(7484) ->
    {0, [], {super, [{0, 79}]}, lm};
unicode_table(7485) ->
    {0, [], {super, [{0, 546}]}, lm};
unicode_table(7486) ->
    {0, [], {super, [{0, 80}]}, lm};
unicode_table(7487) ->
    {0, [], {super, [{0, 82}]}, lm};
unicode_table(7488) ->
    {0, [], {super, [{0, 84}]}, lm};
unicode_table(7489) ->
    {0, [], {super, [{0, 85}]}, lm};
unicode_table(7490) ->
    {0, [], {super, [{0, 87}]}, lm};
unicode_table(7491) ->
    {0, [], {super, [{0, 97}]}, lm};
unicode_table(7492) ->
    {0, [], {super, [{0, 592}]}, lm};
unicode_table(7493) ->
    {0, [], {super, [{0, 593}]}, lm};
unicode_table(7494) ->
    {0, [], {super, [{0, 7426}]}, lm};
unicode_table(7495) ->
    {0, [], {super, [{0, 98}]}, lm};
unicode_table(7496) ->
    {0, [], {super, [{0, 100}]}, lm};
unicode_table(7497) ->
    {0, [], {super, [{0, 101}]}, lm};
unicode_table(7498) ->
    {0, [], {super, [{0, 601}]}, lm};
unicode_table(7499) ->
    {0, [], {super, [{0, 603}]}, lm};
unicode_table(7500) ->
    {0, [], {super, [{0, 604}]}, lm};
unicode_table(7501) ->
    {0, [], {super, [{0, 103}]}, lm};
unicode_table(7503) ->
    {0, [], {super, [{0, 107}]}, lm};
unicode_table(7504) ->
    {0, [], {super, [{0, 109}]}, lm};
unicode_table(7505) ->
    {0, [], {super, [{0, 331}]}, lm};
unicode_table(7506) ->
    {0, [], {super, [{0, 111}]}, lm};
unicode_table(7507) ->
    {0, [], {super, [{0, 596}]}, lm};
unicode_table(7508) ->
    {0, [], {super, [{0, 7446}]}, lm};
unicode_table(7509) ->
    {0, [], {super, [{0, 7447}]}, lm};
unicode_table(7510) ->
    {0, [], {super, [{0, 112}]}, lm};
unicode_table(7511) ->
    {0, [], {super, [{0, 116}]}, lm};
unicode_table(7512) ->
    {0, [], {super, [{0, 117}]}, lm};
unicode_table(7513) ->
    {0, [], {super, [{0, 7453}]}, lm};
unicode_table(7514) ->
    {0, [], {super, [{0, 623}]}, lm};
unicode_table(7515) ->
    {0, [], {super, [{0, 118}]}, lm};
unicode_table(7516) ->
    {0, [], {super, [{0, 7461}]}, lm};
unicode_table(7517) ->
    {0, [], {super, [{0, 946}]}, lm};
unicode_table(7518) ->
    {0, [], {super, [{0, 947}]}, lm};
unicode_table(7519) ->
    {0, [], {super, [{0, 948}]}, lm};
unicode_table(7520) ->
    {0, [], {super, [{0, 966}]}, lm};
unicode_table(7521) ->
    {0, [], {super, [{0, 967}]}, lm};
unicode_table(7522) ->
    {0, [], {sub, [{0, 105}]}, lm};
unicode_table(7523) ->
    {0, [], {sub, [{0, 114}]}, lm};
unicode_table(7524) ->
    {0, [], {sub, [{0, 117}]}, lm};
unicode_table(7525) ->
    {0, [], {sub, [{0, 118}]}, lm};
unicode_table(7526) ->
    {0, [], {sub, [{0, 946}]}, lm};
unicode_table(7527) ->
    {0, [], {sub, [{0, 947}]}, lm};
unicode_table(7528) ->
    {0, [], {sub, [{0, 961}]}, lm};
unicode_table(7529) ->
    {0, [], {sub, [{0, 966}]}, lm};
unicode_table(7530) ->
    {0, [], {sub, [{0, 967}]}, lm};
unicode_table(7544) ->
    {0, [], {super, [{0, 1085}]}, lm};
unicode_table(7579) ->
    {0, [], {super, [{0, 594}]}, lm};
unicode_table(7580) ->
    {0, [], {super, [{0, 99}]}, lm};
unicode_table(7581) ->
    {0, [], {super, [{0, 597}]}, lm};
unicode_table(7582) ->
    {0, [], {super, [{0, 240}]}, lm};
unicode_table(7583) ->
    {0, [], {super, [{0, 604}]}, lm};
unicode_table(7584) ->
    {0, [], {super, [{0, 102}]}, lm};
unicode_table(7585) ->
    {0, [], {super, [{0, 607}]}, lm};
unicode_table(7586) ->
    {0, [], {super, [{0, 609}]}, lm};
unicode_table(7587) ->
    {0, [], {super, [{0, 613}]}, lm};
unicode_table(7588) ->
    {0, [], {super, [{0, 616}]}, lm};
unicode_table(7589) ->
    {0, [], {super, [{0, 617}]}, lm};
unicode_table(7590) ->
    {0, [], {super, [{0, 618}]}, lm};
unicode_table(7591) ->
    {0, [], {super, [{0, 7547}]}, lm};
unicode_table(7592) ->
    {0, [], {super, [{0, 669}]}, lm};
unicode_table(7593) ->
    {0, [], {super, [{0, 621}]}, lm};
unicode_table(7594) ->
    {0, [], {super, [{0, 7557}]}, lm};
unicode_table(7595) ->
    {0, [], {super, [{0, 671}]}, lm};
unicode_table(7596) ->
    {0, [], {super, [{0, 625}]}, lm};
unicode_table(7597) ->
    {0, [], {super, [{0, 624}]}, lm};
unicode_table(7598) ->
    {0, [], {super, [{0, 626}]}, lm};
unicode_table(7599) ->
    {0, [], {super, [{0, 627}]}, lm};
unicode_table(7600) ->
    {0, [], {super, [{0, 628}]}, lm};
unicode_table(7601) ->
    {0, [], {super, [{0, 629}]}, lm};
unicode_table(7602) ->
    {0, [], {super, [{0, 632}]}, lm};
unicode_table(7603) ->
    {0, [], {super, [{0, 642}]}, lm};
unicode_table(7604) ->
    {0, [], {super, [{0, 643}]}, lm};
unicode_table(7605) ->
    {0, [], {super, [{0, 427}]}, lm};
unicode_table(7606) ->
    {0, [], {super, [{0, 649}]}, lm};
unicode_table(7607) ->
    {0, [], {super, [{0, 650}]}, lm};
unicode_table(7608) ->
    {0, [], {super, [{0, 7452}]}, lm};
unicode_table(7609) ->
    {0, [], {super, [{0, 651}]}, lm};
unicode_table(7610) ->
    {0, [], {super, [{0, 652}]}, lm};
unicode_table(7611) ->
    {0, [], {super, [{0, 122}]}, lm};
unicode_table(7612) ->
    {0, [], {super, [{0, 656}]}, lm};
unicode_table(7613) ->
    {0, [], {super, [{0, 657}]}, lm};
unicode_table(7614) ->
    {0, [], {super, [{0, 658}]}, lm};
unicode_table(7615) ->
    {0, [], {super, [{0, 952}]}, lm};
unicode_table(7616) ->
    {230, [], [], mn};
unicode_table(7617) ->
    {230, [], [], mn};
unicode_table(7618) ->
    {220, [], [], mn};
unicode_table(7619) ->
    {230, [], [], mn};
unicode_table(7620) ->
    {230, [], [], mn};
unicode_table(7621) ->
    {230, [], [], mn};
unicode_table(7622) ->
    {230, [], [], mn};
unicode_table(7623) ->
    {230, [], [], mn};
unicode_table(7624) ->
    {230, [], [], mn};
unicode_table(7625) ->
    {230, [], [], mn};
unicode_table(7626) ->
    {220, [], [], mn};
unicode_table(7627) ->
    {230, [], [], mn};
unicode_table(7628) ->
    {230, [], [], mn};
unicode_table(7629) ->
    {234, [], [], mn};
unicode_table(7630) ->
    {214, [], [], mn};
unicode_table(7631) ->
    {220, [], [], mn};
unicode_table(7632) ->
    {202, [], [], mn};
unicode_table(7633) ->
    {230, [], [], mn};
unicode_table(7634) ->
    {230, [], [], mn};
unicode_table(7635) ->
    {230, [], [], mn};
unicode_table(7636) ->
    {230, [], [], mn};
unicode_table(7637) ->
    {230, [], [], mn};
unicode_table(7638) ->
    {230, [], [], mn};
unicode_table(7639) ->
    {230, [], [], mn};
unicode_table(7640) ->
    {230, [], [], mn};
unicode_table(7641) ->
    {230, [], [], mn};
unicode_table(7642) ->
    {230, [], [], mn};
unicode_table(7643) ->
    {230, [], [], mn};
unicode_table(7644) ->
    {230, [], [], mn};
unicode_table(7645) ->
    {230, [], [], mn};
unicode_table(7646) ->
    {230, [], [], mn};
unicode_table(7647) ->
    {230, [], [], mn};
unicode_table(7648) ->
    {230, [], [], mn};
unicode_table(7649) ->
    {230, [], [], mn};
unicode_table(7650) ->
    {230, [], [], mn};
unicode_table(7651) ->
    {230, [], [], mn};
unicode_table(7652) ->
    {230, [], [], mn};
unicode_table(7653) ->
    {230, [], [], mn};
unicode_table(7654) ->
    {230, [], [], mn};
unicode_table(7655) ->
    {230, [], [], mn};
unicode_table(7656) ->
    {230, [], [], mn};
unicode_table(7657) ->
    {230, [], [], mn};
unicode_table(7658) ->
    {230, [], [], mn};
unicode_table(7659) ->
    {230, [], [], mn};
unicode_table(7660) ->
    {230, [], [], mn};
unicode_table(7661) ->
    {230, [], [], mn};
unicode_table(7662) ->
    {230, [], [], mn};
unicode_table(7663) ->
    {230, [], [], mn};
unicode_table(7664) ->
    {230, [], [], mn};
unicode_table(7665) ->
    {230, [], [], mn};
unicode_table(7666) ->
    {230, [], [], mn};
unicode_table(7667) ->
    {230, [], [], mn};
unicode_table(7668) ->
    {230, [], [], mn};
unicode_table(7669) ->
    {230, [], [], mn};
unicode_table(7670) ->
    {232, [], [], mn};
unicode_table(7671) ->
    {228, [], [], mn};
unicode_table(7672) ->
    {228, [], [], mn};
unicode_table(7673) ->
    {220, [], [], mn};
unicode_table(7674) ->
    {218, [], [], mn};
unicode_table(7675) ->
    {230, [], [], mn};
unicode_table(7676) ->
    {233, [], [], mn};
unicode_table(7677) ->
    {220, [], [], mn};
unicode_table(7678) ->
    {230, [], [], mn};
unicode_table(7679) ->
    {220, [], [], mn};
unicode_table(7680) ->
    {0, [{0, 65}, {220, 805}], [], lu};
unicode_table(7681) ->
    {0, [{0, 97}, {220, 805}], [], ll};
unicode_table(7682) ->
    {0, [{0, 66}, {230, 775}], [], lu};
unicode_table(7683) ->
    {0, [{0, 98}, {230, 775}], [], ll};
unicode_table(7684) ->
    {0, [{0, 66}, {220, 803}], [], lu};
unicode_table(7685) ->
    {0, [{0, 98}, {220, 803}], [], ll};
unicode_table(7686) ->
    {0, [{0, 66}, {220, 817}], [], lu};
unicode_table(7687) ->
    {0, [{0, 98}, {220, 817}], [], ll};
unicode_table(7688) ->
    {0, [{0, 67}, {202, 807}, {230, 769}], [], lu};
unicode_table(7689) ->
    {0, [{0, 99}, {202, 807}, {230, 769}], [], ll};
unicode_table(7690) ->
    {0, [{0, 68}, {230, 775}], [], lu};
unicode_table(7691) ->
    {0, [{0, 100}, {230, 775}], [], ll};
unicode_table(7692) ->
    {0, [{0, 68}, {220, 803}], [], lu};
unicode_table(7693) ->
    {0, [{0, 100}, {220, 803}], [], ll};
unicode_table(7694) ->
    {0, [{0, 68}, {220, 817}], [], lu};
unicode_table(7695) ->
    {0, [{0, 100}, {220, 817}], [], ll};
unicode_table(7696) ->
    {0, [{0, 68}, {202, 807}], [], lu};
unicode_table(7697) ->
    {0, [{0, 100}, {202, 807}], [], ll};
unicode_table(7698) ->
    {0, [{0, 68}, {220, 813}], [], lu};
unicode_table(7699) ->
    {0, [{0, 100}, {220, 813}], [], ll};
unicode_table(7700) ->
    {0, [{0, 69}, {230, 772}, {230, 768}], [], lu};
unicode_table(7701) ->
    {0, [{0, 101}, {230, 772}, {230, 768}], [], ll};
unicode_table(7702) ->
    {0, [{0, 69}, {230, 772}, {230, 769}], [], lu};
unicode_table(7703) ->
    {0, [{0, 101}, {230, 772}, {230, 769}], [], ll};
unicode_table(7704) ->
    {0, [{0, 69}, {220, 813}], [], lu};
unicode_table(7705) ->
    {0, [{0, 101}, {220, 813}], [], ll};
unicode_table(7706) ->
    {0, [{0, 69}, {220, 816}], [], lu};
unicode_table(7707) ->
    {0, [{0, 101}, {220, 816}], [], ll};
unicode_table(7708) ->
    {0, [{0, 69}, {202, 807}, {230, 774}], [], lu};
unicode_table(7709) ->
    {0, [{0, 101}, {202, 807}, {230, 774}], [], ll};
unicode_table(7710) ->
    {0, [{0, 70}, {230, 775}], [], lu};
unicode_table(7711) ->
    {0, [{0, 102}, {230, 775}], [], ll};
unicode_table(7712) ->
    {0, [{0, 71}, {230, 772}], [], lu};
unicode_table(7713) ->
    {0, [{0, 103}, {230, 772}], [], ll};
unicode_table(7714) ->
    {0, [{0, 72}, {230, 775}], [], lu};
unicode_table(7715) ->
    {0, [{0, 104}, {230, 775}], [], ll};
unicode_table(7716) ->
    {0, [{0, 72}, {220, 803}], [], lu};
unicode_table(7717) ->
    {0, [{0, 104}, {220, 803}], [], ll};
unicode_table(7718) ->
    {0, [{0, 72}, {230, 776}], [], lu};
unicode_table(7719) ->
    {0, [{0, 104}, {230, 776}], [], ll};
unicode_table(7720) ->
    {0, [{0, 72}, {202, 807}], [], lu};
unicode_table(7721) ->
    {0, [{0, 104}, {202, 807}], [], ll};
unicode_table(7722) ->
    {0, [{0, 72}, {220, 814}], [], lu};
unicode_table(7723) ->
    {0, [{0, 104}, {220, 814}], [], ll};
unicode_table(7724) ->
    {0, [{0, 73}, {220, 816}], [], lu};
unicode_table(7725) ->
    {0, [{0, 105}, {220, 816}], [], ll};
unicode_table(7726) ->
    {0, [{0, 73}, {230, 776}, {230, 769}], [], lu};
unicode_table(7727) ->
    {0, [{0, 105}, {230, 776}, {230, 769}], [], ll};
unicode_table(7728) ->
    {0, [{0, 75}, {230, 769}], [], lu};
unicode_table(7729) ->
    {0, [{0, 107}, {230, 769}], [], ll};
unicode_table(7730) ->
    {0, [{0, 75}, {220, 803}], [], lu};
unicode_table(7731) ->
    {0, [{0, 107}, {220, 803}], [], ll};
unicode_table(7732) ->
    {0, [{0, 75}, {220, 817}], [], lu};
unicode_table(7733) ->
    {0, [{0, 107}, {220, 817}], [], ll};
unicode_table(7734) ->
    {0, [{0, 76}, {220, 803}], [], lu};
unicode_table(7735) ->
    {0, [{0, 108}, {220, 803}], [], ll};
unicode_table(7736) ->
    {0, [{0, 76}, {220, 803}, {230, 772}], [], lu};
unicode_table(7737) ->
    {0, [{0, 108}, {220, 803}, {230, 772}], [], ll};
unicode_table(7738) ->
    {0, [{0, 76}, {220, 817}], [], lu};
unicode_table(7739) ->
    {0, [{0, 108}, {220, 817}], [], ll};
unicode_table(7740) ->
    {0, [{0, 76}, {220, 813}], [], lu};
unicode_table(7741) ->
    {0, [{0, 108}, {220, 813}], [], ll};
unicode_table(7742) ->
    {0, [{0, 77}, {230, 769}], [], lu};
unicode_table(7743) ->
    {0, [{0, 109}, {230, 769}], [], ll};
unicode_table(7744) ->
    {0, [{0, 77}, {230, 775}], [], lu};
unicode_table(7745) ->
    {0, [{0, 109}, {230, 775}], [], ll};
unicode_table(7746) ->
    {0, [{0, 77}, {220, 803}], [], lu};
unicode_table(7747) ->
    {0, [{0, 109}, {220, 803}], [], ll};
unicode_table(7748) ->
    {0, [{0, 78}, {230, 775}], [], lu};
unicode_table(7749) ->
    {0, [{0, 110}, {230, 775}], [], ll};
unicode_table(7750) ->
    {0, [{0, 78}, {220, 803}], [], lu};
unicode_table(7751) ->
    {0, [{0, 110}, {220, 803}], [], ll};
unicode_table(7752) ->
    {0, [{0, 78}, {220, 817}], [], lu};
unicode_table(7753) ->
    {0, [{0, 110}, {220, 817}], [], ll};
unicode_table(7754) ->
    {0, [{0, 78}, {220, 813}], [], lu};
unicode_table(7755) ->
    {0, [{0, 110}, {220, 813}], [], ll};
unicode_table(7756) ->
    {0, [{0, 79}, {230, 771}, {230, 769}], [], lu};
unicode_table(7757) ->
    {0, [{0, 111}, {230, 771}, {230, 769}], [], ll};
unicode_table(7758) ->
    {0, [{0, 79}, {230, 771}, {230, 776}], [], lu};
unicode_table(7759) ->
    {0, [{0, 111}, {230, 771}, {230, 776}], [], ll};
unicode_table(7760) ->
    {0, [{0, 79}, {230, 772}, {230, 768}], [], lu};
unicode_table(7761) ->
    {0, [{0, 111}, {230, 772}, {230, 768}], [], ll};
unicode_table(7762) ->
    {0, [{0, 79}, {230, 772}, {230, 769}], [], lu};
unicode_table(7763) ->
    {0, [{0, 111}, {230, 772}, {230, 769}], [], ll};
unicode_table(7764) ->
    {0, [{0, 80}, {230, 769}], [], lu};
unicode_table(7765) ->
    {0, [{0, 112}, {230, 769}], [], ll};
unicode_table(7766) ->
    {0, [{0, 80}, {230, 775}], [], lu};
unicode_table(7767) ->
    {0, [{0, 112}, {230, 775}], [], ll};
unicode_table(7768) ->
    {0, [{0, 82}, {230, 775}], [], lu};
unicode_table(7769) ->
    {0, [{0, 114}, {230, 775}], [], ll};
unicode_table(7770) ->
    {0, [{0, 82}, {220, 803}], [], lu};
unicode_table(7771) ->
    {0, [{0, 114}, {220, 803}], [], ll};
unicode_table(7772) ->
    {0, [{0, 82}, {220, 803}, {230, 772}], [], lu};
unicode_table(7773) ->
    {0, [{0, 114}, {220, 803}, {230, 772}], [], ll};
unicode_table(7774) ->
    {0, [{0, 82}, {220, 817}], [], lu};
unicode_table(7775) ->
    {0, [{0, 114}, {220, 817}], [], ll};
unicode_table(7776) ->
    {0, [{0, 83}, {230, 775}], [], lu};
unicode_table(7777) ->
    {0, [{0, 115}, {230, 775}], [], ll};
unicode_table(7778) ->
    {0, [{0, 83}, {220, 803}], [], lu};
unicode_table(7779) ->
    {0, [{0, 115}, {220, 803}], [], ll};
unicode_table(7780) ->
    {0, [{0, 83}, {230, 769}, {230, 775}], [], lu};
unicode_table(7781) ->
    {0, [{0, 115}, {230, 769}, {230, 775}], [], ll};
unicode_table(7782) ->
    {0, [{0, 83}, {230, 780}, {230, 775}], [], lu};
unicode_table(7783) ->
    {0, [{0, 115}, {230, 780}, {230, 775}], [], ll};
unicode_table(7784) ->
    {0, [{0, 83}, {220, 803}, {230, 775}], [], lu};
unicode_table(7785) ->
    {0, [{0, 115}, {220, 803}, {230, 775}], [], ll};
unicode_table(7786) ->
    {0, [{0, 84}, {230, 775}], [], lu};
unicode_table(7787) ->
    {0, [{0, 116}, {230, 775}], [], ll};
unicode_table(7788) ->
    {0, [{0, 84}, {220, 803}], [], lu};
unicode_table(7789) ->
    {0, [{0, 116}, {220, 803}], [], ll};
unicode_table(7790) ->
    {0, [{0, 84}, {220, 817}], [], lu};
unicode_table(7791) ->
    {0, [{0, 116}, {220, 817}], [], ll};
unicode_table(7792) ->
    {0, [{0, 84}, {220, 813}], [], lu};
unicode_table(7793) ->
    {0, [{0, 116}, {220, 813}], [], ll};
unicode_table(7794) ->
    {0, [{0, 85}, {220, 804}], [], lu};
unicode_table(7795) ->
    {0, [{0, 117}, {220, 804}], [], ll};
unicode_table(7796) ->
    {0, [{0, 85}, {220, 816}], [], lu};
unicode_table(7797) ->
    {0, [{0, 117}, {220, 816}], [], ll};
unicode_table(7798) ->
    {0, [{0, 85}, {220, 813}], [], lu};
unicode_table(7799) ->
    {0, [{0, 117}, {220, 813}], [], ll};
unicode_table(7800) ->
    {0, [{0, 85}, {230, 771}, {230, 769}], [], lu};
unicode_table(7801) ->
    {0, [{0, 117}, {230, 771}, {230, 769}], [], ll};
unicode_table(7802) ->
    {0, [{0, 85}, {230, 772}, {230, 776}], [], lu};
unicode_table(7803) ->
    {0, [{0, 117}, {230, 772}, {230, 776}], [], ll};
unicode_table(7804) ->
    {0, [{0, 86}, {230, 771}], [], lu};
unicode_table(7805) ->
    {0, [{0, 118}, {230, 771}], [], ll};
unicode_table(7806) ->
    {0, [{0, 86}, {220, 803}], [], lu};
unicode_table(7807) ->
    {0, [{0, 118}, {220, 803}], [], ll};
unicode_table(7808) ->
    {0, [{0, 87}, {230, 768}], [], lu};
unicode_table(7809) ->
    {0, [{0, 119}, {230, 768}], [], ll};
unicode_table(7810) ->
    {0, [{0, 87}, {230, 769}], [], lu};
unicode_table(7811) ->
    {0, [{0, 119}, {230, 769}], [], ll};
unicode_table(7812) ->
    {0, [{0, 87}, {230, 776}], [], lu};
unicode_table(7813) ->
    {0, [{0, 119}, {230, 776}], [], ll};
unicode_table(7814) ->
    {0, [{0, 87}, {230, 775}], [], lu};
unicode_table(7815) ->
    {0, [{0, 119}, {230, 775}], [], ll};
unicode_table(7816) ->
    {0, [{0, 87}, {220, 803}], [], lu};
unicode_table(7817) ->
    {0, [{0, 119}, {220, 803}], [], ll};
unicode_table(7818) ->
    {0, [{0, 88}, {230, 775}], [], lu};
unicode_table(7819) ->
    {0, [{0, 120}, {230, 775}], [], ll};
unicode_table(7820) ->
    {0, [{0, 88}, {230, 776}], [], lu};
unicode_table(7821) ->
    {0, [{0, 120}, {230, 776}], [], ll};
unicode_table(7822) ->
    {0, [{0, 89}, {230, 775}], [], lu};
unicode_table(7823) ->
    {0, [{0, 121}, {230, 775}], [], ll};
unicode_table(7824) ->
    {0, [{0, 90}, {230, 770}], [], lu};
unicode_table(7825) ->
    {0, [{0, 122}, {230, 770}], [], ll};
unicode_table(7826) ->
    {0, [{0, 90}, {220, 803}], [], lu};
unicode_table(7827) ->
    {0, [{0, 122}, {220, 803}], [], ll};
unicode_table(7828) ->
    {0, [{0, 90}, {220, 817}], [], lu};
unicode_table(7829) ->
    {0, [{0, 122}, {220, 817}], [], ll};
unicode_table(7830) ->
    {0, [{0, 104}, {220, 817}], [], ll};
unicode_table(7831) ->
    {0, [{0, 116}, {230, 776}], [], ll};
unicode_table(7832) ->
    {0, [{0, 119}, {230, 778}], [], ll};
unicode_table(7833) ->
    {0, [{0, 121}, {230, 778}], [], ll};
unicode_table(7834) ->
    {0, [], {compat, [{0, 97}, {0, 702}]}, ll};
unicode_table(7835) ->
    {0, [{0, 383}, {230, 775}], {compat, [{0, 115}, {230, 775}]}, ll};
unicode_table(7840) ->
    {0, [{0, 65}, {220, 803}], [], lu};
unicode_table(7841) ->
    {0, [{0, 97}, {220, 803}], [], ll};
unicode_table(7842) ->
    {0, [{0, 65}, {230, 777}], [], lu};
unicode_table(7843) ->
    {0, [{0, 97}, {230, 777}], [], ll};
unicode_table(7844) ->
    {0, [{0, 65}, {230, 770}, {230, 769}], [], lu};
unicode_table(7845) ->
    {0, [{0, 97}, {230, 770}, {230, 769}], [], ll};
unicode_table(7846) ->
    {0, [{0, 65}, {230, 770}, {230, 768}], [], lu};
unicode_table(7847) ->
    {0, [{0, 97}, {230, 770}, {230, 768}], [], ll};
unicode_table(7848) ->
    {0, [{0, 65}, {230, 770}, {230, 777}], [], lu};
unicode_table(7849) ->
    {0, [{0, 97}, {230, 770}, {230, 777}], [], ll};
unicode_table(7850) ->
    {0, [{0, 65}, {230, 770}, {230, 771}], [], lu};
unicode_table(7851) ->
    {0, [{0, 97}, {230, 770}, {230, 771}], [], ll};
unicode_table(7852) ->
    {0, [{0, 65}, {220, 803}, {230, 770}], [], lu};
unicode_table(7853) ->
    {0, [{0, 97}, {220, 803}, {230, 770}], [], ll};
unicode_table(7854) ->
    {0, [{0, 65}, {230, 774}, {230, 769}], [], lu};
unicode_table(7855) ->
    {0, [{0, 97}, {230, 774}, {230, 769}], [], ll};
unicode_table(7856) ->
    {0, [{0, 65}, {230, 774}, {230, 768}], [], lu};
unicode_table(7857) ->
    {0, [{0, 97}, {230, 774}, {230, 768}], [], ll};
unicode_table(7858) ->
    {0, [{0, 65}, {230, 774}, {230, 777}], [], lu};
unicode_table(7859) ->
    {0, [{0, 97}, {230, 774}, {230, 777}], [], ll};
unicode_table(7860) ->
    {0, [{0, 65}, {230, 774}, {230, 771}], [], lu};
unicode_table(7861) ->
    {0, [{0, 97}, {230, 774}, {230, 771}], [], ll};
unicode_table(7862) ->
    {0, [{0, 65}, {220, 803}, {230, 774}], [], lu};
unicode_table(7863) ->
    {0, [{0, 97}, {220, 803}, {230, 774}], [], ll};
unicode_table(7864) ->
    {0, [{0, 69}, {220, 803}], [], lu};
unicode_table(7865) ->
    {0, [{0, 101}, {220, 803}], [], ll};
unicode_table(7866) ->
    {0, [{0, 69}, {230, 777}], [], lu};
unicode_table(7867) ->
    {0, [{0, 101}, {230, 777}], [], ll};
unicode_table(7868) ->
    {0, [{0, 69}, {230, 771}], [], lu};
unicode_table(7869) ->
    {0, [{0, 101}, {230, 771}], [], ll};
unicode_table(7870) ->
    {0, [{0, 69}, {230, 770}, {230, 769}], [], lu};
unicode_table(7871) ->
    {0, [{0, 101}, {230, 770}, {230, 769}], [], ll};
unicode_table(7872) ->
    {0, [{0, 69}, {230, 770}, {230, 768}], [], lu};
unicode_table(7873) ->
    {0, [{0, 101}, {230, 770}, {230, 768}], [], ll};
unicode_table(7874) ->
    {0, [{0, 69}, {230, 770}, {230, 777}], [], lu};
unicode_table(7875) ->
    {0, [{0, 101}, {230, 770}, {230, 777}], [], ll};
unicode_table(7876) ->
    {0, [{0, 69}, {230, 770}, {230, 771}], [], lu};
unicode_table(7877) ->
    {0, [{0, 101}, {230, 770}, {230, 771}], [], ll};
unicode_table(7878) ->
    {0, [{0, 69}, {220, 803}, {230, 770}], [], lu};
unicode_table(7879) ->
    {0, [{0, 101}, {220, 803}, {230, 770}], [], ll};
unicode_table(7880) ->
    {0, [{0, 73}, {230, 777}], [], lu};
unicode_table(7881) ->
    {0, [{0, 105}, {230, 777}], [], ll};
unicode_table(7882) ->
    {0, [{0, 73}, {220, 803}], [], lu};
unicode_table(7883) ->
    {0, [{0, 105}, {220, 803}], [], ll};
unicode_table(7884) ->
    {0, [{0, 79}, {220, 803}], [], lu};
unicode_table(7885) ->
    {0, [{0, 111}, {220, 803}], [], ll};
unicode_table(7886) ->
    {0, [{0, 79}, {230, 777}], [], lu};
unicode_table(7887) ->
    {0, [{0, 111}, {230, 777}], [], ll};
unicode_table(7888) ->
    {0, [{0, 79}, {230, 770}, {230, 769}], [], lu};
unicode_table(7889) ->
    {0, [{0, 111}, {230, 770}, {230, 769}], [], ll};
unicode_table(7890) ->
    {0, [{0, 79}, {230, 770}, {230, 768}], [], lu};
unicode_table(7891) ->
    {0, [{0, 111}, {230, 770}, {230, 768}], [], ll};
unicode_table(7892) ->
    {0, [{0, 79}, {230, 770}, {230, 777}], [], lu};
unicode_table(7893) ->
    {0, [{0, 111}, {230, 770}, {230, 777}], [], ll};
unicode_table(7894) ->
    {0, [{0, 79}, {230, 770}, {230, 771}], [], lu};
unicode_table(7895) ->
    {0, [{0, 111}, {230, 770}, {230, 771}], [], ll};
unicode_table(7896) ->
    {0, [{0, 79}, {220, 803}, {230, 770}], [], lu};
unicode_table(7897) ->
    {0, [{0, 111}, {220, 803}, {230, 770}], [], ll};
unicode_table(7898) ->
    {0, [{0, 79}, {216, 795}, {230, 769}], [], lu};
unicode_table(7899) ->
    {0, [{0, 111}, {216, 795}, {230, 769}], [], ll};
unicode_table(7900) ->
    {0, [{0, 79}, {216, 795}, {230, 768}], [], lu};
unicode_table(7901) ->
    {0, [{0, 111}, {216, 795}, {230, 768}], [], ll};
unicode_table(7902) ->
    {0, [{0, 79}, {216, 795}, {230, 777}], [], lu};
unicode_table(7903) ->
    {0, [{0, 111}, {216, 795}, {230, 777}], [], ll};
unicode_table(7904) ->
    {0, [{0, 79}, {216, 795}, {230, 771}], [], lu};
unicode_table(7905) ->
    {0, [{0, 111}, {216, 795}, {230, 771}], [], ll};
unicode_table(7906) ->
    {0, [{0, 79}, {216, 795}, {220, 803}], [], lu};
unicode_table(7907) ->
    {0, [{0, 111}, {216, 795}, {220, 803}], [], ll};
unicode_table(7908) ->
    {0, [{0, 85}, {220, 803}], [], lu};
unicode_table(7909) ->
    {0, [{0, 117}, {220, 803}], [], ll};
unicode_table(7910) ->
    {0, [{0, 85}, {230, 777}], [], lu};
unicode_table(7911) ->
    {0, [{0, 117}, {230, 777}], [], ll};
unicode_table(7912) ->
    {0, [{0, 85}, {216, 795}, {230, 769}], [], lu};
unicode_table(7913) ->
    {0, [{0, 117}, {216, 795}, {230, 769}], [], ll};
unicode_table(7914) ->
    {0, [{0, 85}, {216, 795}, {230, 768}], [], lu};
unicode_table(7915) ->
    {0, [{0, 117}, {216, 795}, {230, 768}], [], ll};
unicode_table(7916) ->
    {0, [{0, 85}, {216, 795}, {230, 777}], [], lu};
unicode_table(7917) ->
    {0, [{0, 117}, {216, 795}, {230, 777}], [], ll};
unicode_table(7918) ->
    {0, [{0, 85}, {216, 795}, {230, 771}], [], lu};
unicode_table(7919) ->
    {0, [{0, 117}, {216, 795}, {230, 771}], [], ll};
unicode_table(7920) ->
    {0, [{0, 85}, {216, 795}, {220, 803}], [], lu};
unicode_table(7921) ->
    {0, [{0, 117}, {216, 795}, {220, 803}], [], ll};
unicode_table(7922) ->
    {0, [{0, 89}, {230, 768}], [], lu};
unicode_table(7923) ->
    {0, [{0, 121}, {230, 768}], [], ll};
unicode_table(7924) ->
    {0, [{0, 89}, {220, 803}], [], lu};
unicode_table(7925) ->
    {0, [{0, 121}, {220, 803}], [], ll};
unicode_table(7926) ->
    {0, [{0, 89}, {230, 777}], [], lu};
unicode_table(7927) ->
    {0, [{0, 121}, {230, 777}], [], ll};
unicode_table(7928) ->
    {0, [{0, 89}, {230, 771}], [], lu};
unicode_table(7929) ->
    {0, [{0, 121}, {230, 771}], [], ll};
unicode_table(7936) ->
    {0, [{0, 945}, {230, 787}], [], ll};
unicode_table(7937) ->
    {0, [{0, 945}, {230, 788}], [], ll};
unicode_table(7938) ->
    {0, [{0, 945}, {230, 787}, {230, 768}], [], ll};
unicode_table(7939) ->
    {0, [{0, 945}, {230, 788}, {230, 768}], [], ll};
unicode_table(7940) ->
    {0, [{0, 945}, {230, 787}, {230, 769}], [], ll};
unicode_table(7941) ->
    {0, [{0, 945}, {230, 788}, {230, 769}], [], ll};
unicode_table(7942) ->
    {0, [{0, 945}, {230, 787}, {230, 834}], [], ll};
unicode_table(7943) ->
    {0, [{0, 945}, {230, 788}, {230, 834}], [], ll};
unicode_table(7944) ->
    {0, [{0, 913}, {230, 787}], [], lu};
unicode_table(7945) ->
    {0, [{0, 913}, {230, 788}], [], lu};
unicode_table(7946) ->
    {0, [{0, 913}, {230, 787}, {230, 768}], [], lu};
unicode_table(7947) ->
    {0, [{0, 913}, {230, 788}, {230, 768}], [], lu};
unicode_table(7948) ->
    {0, [{0, 913}, {230, 787}, {230, 769}], [], lu};
unicode_table(7949) ->
    {0, [{0, 913}, {230, 788}, {230, 769}], [], lu};
unicode_table(7950) ->
    {0, [{0, 913}, {230, 787}, {230, 834}], [], lu};
unicode_table(7951) ->
    {0, [{0, 913}, {230, 788}, {230, 834}], [], lu};
unicode_table(7952) ->
    {0, [{0, 949}, {230, 787}], [], ll};
unicode_table(7953) ->
    {0, [{0, 949}, {230, 788}], [], ll};
unicode_table(7954) ->
    {0, [{0, 949}, {230, 787}, {230, 768}], [], ll};
unicode_table(7955) ->
    {0, [{0, 949}, {230, 788}, {230, 768}], [], ll};
unicode_table(7956) ->
    {0, [{0, 949}, {230, 787}, {230, 769}], [], ll};
unicode_table(7957) ->
    {0, [{0, 949}, {230, 788}, {230, 769}], [], ll};
unicode_table(7960) ->
    {0, [{0, 917}, {230, 787}], [], lu};
unicode_table(7961) ->
    {0, [{0, 917}, {230, 788}], [], lu};
unicode_table(7962) ->
    {0, [{0, 917}, {230, 787}, {230, 768}], [], lu};
unicode_table(7963) ->
    {0, [{0, 917}, {230, 788}, {230, 768}], [], lu};
unicode_table(7964) ->
    {0, [{0, 917}, {230, 787}, {230, 769}], [], lu};
unicode_table(7965) ->
    {0, [{0, 917}, {230, 788}, {230, 769}], [], lu};
unicode_table(7968) ->
    {0, [{0, 951}, {230, 787}], [], ll};
unicode_table(7969) ->
    {0, [{0, 951}, {230, 788}], [], ll};
unicode_table(7970) ->
    {0, [{0, 951}, {230, 787}, {230, 768}], [], ll};
unicode_table(7971) ->
    {0, [{0, 951}, {230, 788}, {230, 768}], [], ll};
unicode_table(7972) ->
    {0, [{0, 951}, {230, 787}, {230, 769}], [], ll};
unicode_table(7973) ->
    {0, [{0, 951}, {230, 788}, {230, 769}], [], ll};
unicode_table(7974) ->
    {0, [{0, 951}, {230, 787}, {230, 834}], [], ll};
unicode_table(7975) ->
    {0, [{0, 951}, {230, 788}, {230, 834}], [], ll};
unicode_table(7976) ->
    {0, [{0, 919}, {230, 787}], [], lu};
unicode_table(7977) ->
    {0, [{0, 919}, {230, 788}], [], lu};
unicode_table(7978) ->
    {0, [{0, 919}, {230, 787}, {230, 768}], [], lu};
unicode_table(7979) ->
    {0, [{0, 919}, {230, 788}, {230, 768}], [], lu};
unicode_table(7980) ->
    {0, [{0, 919}, {230, 787}, {230, 769}], [], lu};
unicode_table(7981) ->
    {0, [{0, 919}, {230, 788}, {230, 769}], [], lu};
unicode_table(7982) ->
    {0, [{0, 919}, {230, 787}, {230, 834}], [], lu};
unicode_table(7983) ->
    {0, [{0, 919}, {230, 788}, {230, 834}], [], lu};
unicode_table(7984) ->
    {0, [{0, 953}, {230, 787}], [], ll};
unicode_table(7985) ->
    {0, [{0, 953}, {230, 788}], [], ll};
unicode_table(7986) ->
    {0, [{0, 953}, {230, 787}, {230, 768}], [], ll};
unicode_table(7987) ->
    {0, [{0, 953}, {230, 788}, {230, 768}], [], ll};
unicode_table(7988) ->
    {0, [{0, 953}, {230, 787}, {230, 769}], [], ll};
unicode_table(7989) ->
    {0, [{0, 953}, {230, 788}, {230, 769}], [], ll};
unicode_table(7990) ->
    {0, [{0, 953}, {230, 787}, {230, 834}], [], ll};
unicode_table(7991) ->
    {0, [{0, 953}, {230, 788}, {230, 834}], [], ll};
unicode_table(7992) ->
    {0, [{0, 921}, {230, 787}], [], lu};
unicode_table(7993) ->
    {0, [{0, 921}, {230, 788}], [], lu};
unicode_table(7994) ->
    {0, [{0, 921}, {230, 787}, {230, 768}], [], lu};
unicode_table(7995) ->
    {0, [{0, 921}, {230, 788}, {230, 768}], [], lu};
unicode_table(7996) ->
    {0, [{0, 921}, {230, 787}, {230, 769}], [], lu};
unicode_table(7997) ->
    {0, [{0, 921}, {230, 788}, {230, 769}], [], lu};
unicode_table(7998) ->
    {0, [{0, 921}, {230, 787}, {230, 834}], [], lu};
unicode_table(7999) ->
    {0, [{0, 921}, {230, 788}, {230, 834}], [], lu};
unicode_table(8000) ->
    {0, [{0, 959}, {230, 787}], [], ll};
unicode_table(8001) ->
    {0, [{0, 959}, {230, 788}], [], ll};
unicode_table(8002) ->
    {0, [{0, 959}, {230, 787}, {230, 768}], [], ll};
unicode_table(8003) ->
    {0, [{0, 959}, {230, 788}, {230, 768}], [], ll};
unicode_table(8004) ->
    {0, [{0, 959}, {230, 787}, {230, 769}], [], ll};
unicode_table(8005) ->
    {0, [{0, 959}, {230, 788}, {230, 769}], [], ll};
unicode_table(8008) ->
    {0, [{0, 927}, {230, 787}], [], lu};
unicode_table(8009) ->
    {0, [{0, 927}, {230, 788}], [], lu};
unicode_table(8010) ->
    {0, [{0, 927}, {230, 787}, {230, 768}], [], lu};
unicode_table(8011) ->
    {0, [{0, 927}, {230, 788}, {230, 768}], [], lu};
unicode_table(8012) ->
    {0, [{0, 927}, {230, 787}, {230, 769}], [], lu};
unicode_table(8013) ->
    {0, [{0, 927}, {230, 788}, {230, 769}], [], lu};
unicode_table(8016) ->
    {0, [{0, 965}, {230, 787}], [], ll};
unicode_table(8017) ->
    {0, [{0, 965}, {230, 788}], [], ll};
unicode_table(8018) ->
    {0, [{0, 965}, {230, 787}, {230, 768}], [], ll};
unicode_table(8019) ->
    {0, [{0, 965}, {230, 788}, {230, 768}], [], ll};
unicode_table(8020) ->
    {0, [{0, 965}, {230, 787}, {230, 769}], [], ll};
unicode_table(8021) ->
    {0, [{0, 965}, {230, 788}, {230, 769}], [], ll};
unicode_table(8022) ->
    {0, [{0, 965}, {230, 787}, {230, 834}], [], ll};
unicode_table(8023) ->
    {0, [{0, 965}, {230, 788}, {230, 834}], [], ll};
unicode_table(8025) ->
    {0, [{0, 933}, {230, 788}], [], lu};
unicode_table(8027) ->
    {0, [{0, 933}, {230, 788}, {230, 768}], [], lu};
unicode_table(8029) ->
    {0, [{0, 933}, {230, 788}, {230, 769}], [], lu};
unicode_table(8031) ->
    {0, [{0, 933}, {230, 788}, {230, 834}], [], lu};
unicode_table(8032) ->
    {0, [{0, 969}, {230, 787}], [], ll};
unicode_table(8033) ->
    {0, [{0, 969}, {230, 788}], [], ll};
unicode_table(8034) ->
    {0, [{0, 969}, {230, 787}, {230, 768}], [], ll};
unicode_table(8035) ->
    {0, [{0, 969}, {230, 788}, {230, 768}], [], ll};
unicode_table(8036) ->
    {0, [{0, 969}, {230, 787}, {230, 769}], [], ll};
unicode_table(8037) ->
    {0, [{0, 969}, {230, 788}, {230, 769}], [], ll};
unicode_table(8038) ->
    {0, [{0, 969}, {230, 787}, {230, 834}], [], ll};
unicode_table(8039) ->
    {0, [{0, 969}, {230, 788}, {230, 834}], [], ll};
unicode_table(8040) ->
    {0, [{0, 937}, {230, 787}], [], lu};
unicode_table(8041) ->
    {0, [{0, 937}, {230, 788}], [], lu};
unicode_table(8042) ->
    {0, [{0, 937}, {230, 787}, {230, 768}], [], lu};
unicode_table(8043) ->
    {0, [{0, 937}, {230, 788}, {230, 768}], [], lu};
unicode_table(8044) ->
    {0, [{0, 937}, {230, 787}, {230, 769}], [], lu};
unicode_table(8045) ->
    {0, [{0, 937}, {230, 788}, {230, 769}], [], lu};
unicode_table(8046) ->
    {0, [{0, 937}, {230, 787}, {230, 834}], [], lu};
unicode_table(8047) ->
    {0, [{0, 937}, {230, 788}, {230, 834}], [], lu};
unicode_table(8048) ->
    {0, [{0, 945}, {230, 768}], [], ll};
unicode_table(8049) ->
    {0, [{0, 945}, {230, 769}], [], ll};
unicode_table(8050) ->
    {0, [{0, 949}, {230, 768}], [], ll};
unicode_table(8051) ->
    {0, [{0, 949}, {230, 769}], [], ll};
unicode_table(8052) ->
    {0, [{0, 951}, {230, 768}], [], ll};
unicode_table(8053) ->
    {0, [{0, 951}, {230, 769}], [], ll};
unicode_table(8054) ->
    {0, [{0, 953}, {230, 768}], [], ll};
unicode_table(8055) ->
    {0, [{0, 953}, {230, 769}], [], ll};
unicode_table(8056) ->
    {0, [{0, 959}, {230, 768}], [], ll};
unicode_table(8057) ->
    {0, [{0, 959}, {230, 769}], [], ll};
unicode_table(8058) ->
    {0, [{0, 965}, {230, 768}], [], ll};
unicode_table(8059) ->
    {0, [{0, 965}, {230, 769}], [], ll};
unicode_table(8060) ->
    {0, [{0, 969}, {230, 768}], [], ll};
unicode_table(8061) ->
    {0, [{0, 969}, {230, 769}], [], ll};
unicode_table(8064) ->
    {0, [{0, 945}, {230, 787}, {240, 837}], [], ll};
unicode_table(8065) ->
    {0, [{0, 945}, {230, 788}, {240, 837}], [], ll};
unicode_table(8066) ->
    {0, [{0, 945}, {230, 787}, {230, 768}, {240, 837}], [], ll};
unicode_table(8067) ->
    {0, [{0, 945}, {230, 788}, {230, 768}, {240, 837}], [], ll};
unicode_table(8068) ->
    {0, [{0, 945}, {230, 787}, {230, 769}, {240, 837}], [], ll};
unicode_table(8069) ->
    {0, [{0, 945}, {230, 788}, {230, 769}, {240, 837}], [], ll};
unicode_table(8070) ->
    {0, [{0, 945}, {230, 787}, {230, 834}, {240, 837}], [], ll};
unicode_table(8071) ->
    {0, [{0, 945}, {230, 788}, {230, 834}, {240, 837}], [], ll};
unicode_table(8072) ->
    {0, [{0, 913}, {230, 787}, {240, 837}], [], lt};
unicode_table(8073) ->
    {0, [{0, 913}, {230, 788}, {240, 837}], [], lt};
unicode_table(8074) ->
    {0, [{0, 913}, {230, 787}, {230, 768}, {240, 837}], [], lt};
unicode_table(8075) ->
    {0, [{0, 913}, {230, 788}, {230, 768}, {240, 837}], [], lt};
unicode_table(8076) ->
    {0, [{0, 913}, {230, 787}, {230, 769}, {240, 837}], [], lt};
unicode_table(8077) ->
    {0, [{0, 913}, {230, 788}, {230, 769}, {240, 837}], [], lt};
unicode_table(8078) ->
    {0, [{0, 913}, {230, 787}, {230, 834}, {240, 837}], [], lt};
unicode_table(8079) ->
    {0, [{0, 913}, {230, 788}, {230, 834}, {240, 837}], [], lt};
unicode_table(8080) ->
    {0, [{0, 951}, {230, 787}, {240, 837}], [], ll};
unicode_table(8081) ->
    {0, [{0, 951}, {230, 788}, {240, 837}], [], ll};
unicode_table(8082) ->
    {0, [{0, 951}, {230, 787}, {230, 768}, {240, 837}], [], ll};
unicode_table(8083) ->
    {0, [{0, 951}, {230, 788}, {230, 768}, {240, 837}], [], ll};
unicode_table(8084) ->
    {0, [{0, 951}, {230, 787}, {230, 769}, {240, 837}], [], ll};
unicode_table(8085) ->
    {0, [{0, 951}, {230, 788}, {230, 769}, {240, 837}], [], ll};
unicode_table(8086) ->
    {0, [{0, 951}, {230, 787}, {230, 834}, {240, 837}], [], ll};
unicode_table(8087) ->
    {0, [{0, 951}, {230, 788}, {230, 834}, {240, 837}], [], ll};
unicode_table(8088) ->
    {0, [{0, 919}, {230, 787}, {240, 837}], [], lt};
unicode_table(8089) ->
    {0, [{0, 919}, {230, 788}, {240, 837}], [], lt};
unicode_table(8090) ->
    {0, [{0, 919}, {230, 787}, {230, 768}, {240, 837}], [], lt};
unicode_table(8091) ->
    {0, [{0, 919}, {230, 788}, {230, 768}, {240, 837}], [], lt};
unicode_table(8092) ->
    {0, [{0, 919}, {230, 787}, {230, 769}, {240, 837}], [], lt};
unicode_table(8093) ->
    {0, [{0, 919}, {230, 788}, {230, 769}, {240, 837}], [], lt};
unicode_table(8094) ->
    {0, [{0, 919}, {230, 787}, {230, 834}, {240, 837}], [], lt};
unicode_table(8095) ->
    {0, [{0, 919}, {230, 788}, {230, 834}, {240, 837}], [], lt};
unicode_table(8096) ->
    {0, [{0, 969}, {230, 787}, {240, 837}], [], ll};
unicode_table(8097) ->
    {0, [{0, 969}, {230, 788}, {240, 837}], [], ll};
unicode_table(8098) ->
    {0, [{0, 969}, {230, 787}, {230, 768}, {240, 837}], [], ll};
unicode_table(8099) ->
    {0, [{0, 969}, {230, 788}, {230, 768}, {240, 837}], [], ll};
unicode_table(8100) ->
    {0, [{0, 969}, {230, 787}, {230, 769}, {240, 837}], [], ll};
unicode_table(8101) ->
    {0, [{0, 969}, {230, 788}, {230, 769}, {240, 837}], [], ll};
unicode_table(8102) ->
    {0, [{0, 969}, {230, 787}, {230, 834}, {240, 837}], [], ll};
unicode_table(8103) ->
    {0, [{0, 969}, {230, 788}, {230, 834}, {240, 837}], [], ll};
unicode_table(8104) ->
    {0, [{0, 937}, {230, 787}, {240, 837}], [], lt};
unicode_table(8105) ->
    {0, [{0, 937}, {230, 788}, {240, 837}], [], lt};
unicode_table(8106) ->
    {0, [{0, 937}, {230, 787}, {230, 768}, {240, 837}], [], lt};
unicode_table(8107) ->
    {0, [{0, 937}, {230, 788}, {230, 768}, {240, 837}], [], lt};
unicode_table(8108) ->
    {0, [{0, 937}, {230, 787}, {230, 769}, {240, 837}], [], lt};
unicode_table(8109) ->
    {0, [{0, 937}, {230, 788}, {230, 769}, {240, 837}], [], lt};
unicode_table(8110) ->
    {0, [{0, 937}, {230, 787}, {230, 834}, {240, 837}], [], lt};
unicode_table(8111) ->
    {0, [{0, 937}, {230, 788}, {230, 834}, {240, 837}], [], lt};
unicode_table(8112) ->
    {0, [{0, 945}, {230, 774}], [], ll};
unicode_table(8113) ->
    {0, [{0, 945}, {230, 772}], [], ll};
unicode_table(8114) ->
    {0, [{0, 945}, {230, 768}, {240, 837}], [], ll};
unicode_table(8115) ->
    {0, [{0, 945}, {240, 837}], [], ll};
unicode_table(8116) ->
    {0, [{0, 945}, {230, 769}, {240, 837}], [], ll};
unicode_table(8118) ->
    {0, [{0, 945}, {230, 834}], [], ll};
unicode_table(8119) ->
    {0, [{0, 945}, {230, 834}, {240, 837}], [], ll};
unicode_table(8120) ->
    {0, [{0, 913}, {230, 774}], [], lu};
unicode_table(8121) ->
    {0, [{0, 913}, {230, 772}], [], lu};
unicode_table(8122) ->
    {0, [{0, 913}, {230, 768}], [], lu};
unicode_table(8123) ->
    {0, [{0, 913}, {230, 769}], [], lu};
unicode_table(8124) ->
    {0, [{0, 913}, {240, 837}], [], lt};
unicode_table(8125) ->
    {0, [], {compat, [{0, 32}, {230, 787}]}, sk};
unicode_table(8126) ->
    {0, [{0, 953}], [], ll};
unicode_table(8127) ->
    {0, [], {compat, [{0, 32}, {230, 787}]}, sk};
unicode_table(8128) ->
    {0, [], {compat, [{0, 32}, {230, 834}]}, sk};
unicode_table(8129) ->
    {0, [{0, 168}, {230, 834}], {compat, [{0, 32}, {230, 776}, {230, 834}]}, sk};
unicode_table(8130) ->
    {0, [{0, 951}, {230, 768}, {240, 837}], [], ll};
unicode_table(8131) ->
    {0, [{0, 951}, {240, 837}], [], ll};
unicode_table(8132) ->
    {0, [{0, 951}, {230, 769}, {240, 837}], [], ll};
unicode_table(8134) ->
    {0, [{0, 951}, {230, 834}], [], ll};
unicode_table(8135) ->
    {0, [{0, 951}, {230, 834}, {240, 837}], [], ll};
unicode_table(8136) ->
    {0, [{0, 917}, {230, 768}], [], lu};
unicode_table(8137) ->
    {0, [{0, 917}, {230, 769}], [], lu};
unicode_table(8138) ->
    {0, [{0, 919}, {230, 768}], [], lu};
unicode_table(8139) ->
    {0, [{0, 919}, {230, 769}], [], lu};
unicode_table(8140) ->
    {0, [{0, 919}, {240, 837}], [], lt};
unicode_table(8141) ->
    {0, [{0, 8127}, {230, 768}], {compat, [{0, 32}, {230, 787}, {230, 768}]}, sk};
unicode_table(8142) ->
    {0, [{0, 8127}, {230, 769}], {compat, [{0, 32}, {230, 787}, {230, 769}]}, sk};
unicode_table(8143) ->
    {0, [{0, 8127}, {230, 834}], {compat, [{0, 32}, {230, 787}, {230, 834}]}, sk};
unicode_table(8144) ->
    {0, [{0, 953}, {230, 774}], [], ll};
unicode_table(8145) ->
    {0, [{0, 953}, {230, 772}], [], ll};
unicode_table(8146) ->
    {0, [{0, 953}, {230, 776}, {230, 768}], [], ll};
unicode_table(8147) ->
    {0, [{0, 953}, {230, 776}, {230, 769}], [], ll};
unicode_table(8150) ->
    {0, [{0, 953}, {230, 834}], [], ll};
unicode_table(8151) ->
    {0, [{0, 953}, {230, 776}, {230, 834}], [], ll};
unicode_table(8152) ->
    {0, [{0, 921}, {230, 774}], [], lu};
unicode_table(8153) ->
    {0, [{0, 921}, {230, 772}], [], lu};
unicode_table(8154) ->
    {0, [{0, 921}, {230, 768}], [], lu};
unicode_table(8155) ->
    {0, [{0, 921}, {230, 769}], [], lu};
unicode_table(8157) ->
    {0, [{0, 8190}, {230, 768}], {compat, [{0, 32}, {230, 788}, {230, 768}]}, sk};
unicode_table(8158) ->
    {0, [{0, 8190}, {230, 769}], {compat, [{0, 32}, {230, 788}, {230, 769}]}, sk};
unicode_table(8159) ->
    {0, [{0, 8190}, {230, 834}], {compat, [{0, 32}, {230, 788}, {230, 834}]}, sk};
unicode_table(8160) ->
    {0, [{0, 965}, {230, 774}], [], ll};
unicode_table(8161) ->
    {0, [{0, 965}, {230, 772}], [], ll};
unicode_table(8162) ->
    {0, [{0, 965}, {230, 776}, {230, 768}], [], ll};
unicode_table(8163) ->
    {0, [{0, 965}, {230, 776}, {230, 769}], [], ll};
unicode_table(8164) ->
    {0, [{0, 961}, {230, 787}], [], ll};
unicode_table(8165) ->
    {0, [{0, 961}, {230, 788}], [], ll};
unicode_table(8166) ->
    {0, [{0, 965}, {230, 834}], [], ll};
unicode_table(8167) ->
    {0, [{0, 965}, {230, 776}, {230, 834}], [], ll};
unicode_table(8168) ->
    {0, [{0, 933}, {230, 774}], [], lu};
unicode_table(8169) ->
    {0, [{0, 933}, {230, 772}], [], lu};
unicode_table(8170) ->
    {0, [{0, 933}, {230, 768}], [], lu};
unicode_table(8171) ->
    {0, [{0, 933}, {230, 769}], [], lu};
unicode_table(8172) ->
    {0, [{0, 929}, {230, 788}], [], lu};
unicode_table(8173) ->
    {0, [{0, 168}, {230, 768}], {compat, [{0, 32}, {230, 776}, {230, 768}]}, sk};
unicode_table(8174) ->
    {0, [{0, 168}, {230, 769}], {compat, [{0, 32}, {230, 776}, {230, 769}]}, sk};
unicode_table(8175) ->
    {0, [{0, 96}], [], sk};
unicode_table(8178) ->
    {0, [{0, 969}, {230, 768}, {240, 837}], [], ll};
unicode_table(8179) ->
    {0, [{0, 969}, {240, 837}], [], ll};
unicode_table(8180) ->
    {0, [{0, 969}, {230, 769}, {240, 837}], [], ll};
unicode_table(8182) ->
    {0, [{0, 969}, {230, 834}], [], ll};
unicode_table(8183) ->
    {0, [{0, 969}, {230, 834}, {240, 837}], [], ll};
unicode_table(8184) ->
    {0, [{0, 927}, {230, 768}], [], lu};
unicode_table(8185) ->
    {0, [{0, 927}, {230, 769}], [], lu};
unicode_table(8186) ->
    {0, [{0, 937}, {230, 768}], [], lu};
unicode_table(8187) ->
    {0, [{0, 937}, {230, 769}], [], lu};
unicode_table(8188) ->
    {0, [{0, 937}, {240, 837}], [], lt};
unicode_table(8189) ->
    {0, [{0, 180}], {compat, [{0, 32}, {230, 769}]}, sk};
unicode_table(8190) ->
    {0, [], {compat, [{0, 32}, {230, 788}]}, sk};
unicode_table(8192) ->
    {0, [{0, 8194}], {compat, [{0, 32}]}, zs};
unicode_table(8193) ->
    {0, [{0, 8195}], {compat, [{0, 32}]}, zs};
unicode_table(8194) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8195) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8196) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8197) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8198) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8199) ->
    {0, [], {noBreak, [{0, 32}]}, zs};
unicode_table(8200) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8201) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8202) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8209) ->
    {0, [], {noBreak, [{0, 8208}]}, pd};
unicode_table(8215) ->
    {0, [], {compat, [{0, 32}, {220, 819}]}, po};
unicode_table(8228) ->
    {0, [], {compat, [{0, 46}]}, po};
unicode_table(8229) ->
    {0, [], {compat, [{0, 46}, {0, 46}]}, po};
unicode_table(8230) ->
    {0, [], {compat, [{0, 46}, {0, 46}, {0, 46}]}, po};
unicode_table(8239) ->
    {0, [], {noBreak, [{0, 32}]}, zs};
unicode_table(8243) ->
    {0, [], {compat, [{0, 8242}, {0, 8242}]}, po};
unicode_table(8244) ->
    {0, [], {compat, [{0, 8242}, {0, 8242}, {0, 8242}]}, po};
unicode_table(8246) ->
    {0, [], {compat, [{0, 8245}, {0, 8245}]}, po};
unicode_table(8247) ->
    {0, [], {compat, [{0, 8245}, {0, 8245}, {0, 8245}]}, po};
unicode_table(8252) ->
    {0, [], {compat, [{0, 33}, {0, 33}]}, po};
unicode_table(8254) ->
    {0, [], {compat, [{0, 32}, {230, 773}]}, po};
unicode_table(8263) ->
    {0, [], {compat, [{0, 63}, {0, 63}]}, po};
unicode_table(8264) ->
    {0, [], {compat, [{0, 63}, {0, 33}]}, po};
unicode_table(8265) ->
    {0, [], {compat, [{0, 33}, {0, 63}]}, po};
unicode_table(8279) ->
    {0, [], {compat, [{0, 8242}, {0, 8242}, {0, 8242}, {0, 8242}]}, po};
unicode_table(8287) ->
    {0, [], {compat, [{0, 32}]}, zs};
unicode_table(8304) ->
    {0, [], {super, [{0, 48}]}, no};
unicode_table(8305) ->
    {0, [], {super, [{0, 105}]}, lm};
unicode_table(8308) ->
    {0, [], {super, [{0, 52}]}, no};
unicode_table(8309) ->
    {0, [], {super, [{0, 53}]}, no};
unicode_table(8310) ->
    {0, [], {super, [{0, 54}]}, no};
unicode_table(8311) ->
    {0, [], {super, [{0, 55}]}, no};
unicode_table(8312) ->
    {0, [], {super, [{0, 56}]}, no};
unicode_table(8313) ->
    {0, [], {super, [{0, 57}]}, no};
unicode_table(8314) ->
    {0, [], {super, [{0, 43}]}, sm};
unicode_table(8315) ->
    {0, [], {super, [{0, 8722}]}, sm};
unicode_table(8316) ->
    {0, [], {super, [{0, 61}]}, sm};
unicode_table(8317) ->
    {0, [], {super, [{0, 40}]}, ps};
unicode_table(8318) ->
    {0, [], {super, [{0, 41}]}, pe};
unicode_table(8319) ->
    {0, [], {super, [{0, 110}]}, lm};
unicode_table(8320) ->
    {0, [], {sub, [{0, 48}]}, no};
unicode_table(8321) ->
    {0, [], {sub, [{0, 49}]}, no};
unicode_table(8322) ->
    {0, [], {sub, [{0, 50}]}, no};
unicode_table(8323) ->
    {0, [], {sub, [{0, 51}]}, no};
unicode_table(8324) ->
    {0, [], {sub, [{0, 52}]}, no};
unicode_table(8325) ->
    {0, [], {sub, [{0, 53}]}, no};
unicode_table(8326) ->
    {0, [], {sub, [{0, 54}]}, no};
unicode_table(8327) ->
    {0, [], {sub, [{0, 55}]}, no};
unicode_table(8328) ->
    {0, [], {sub, [{0, 56}]}, no};
unicode_table(8329) ->
    {0, [], {sub, [{0, 57}]}, no};
unicode_table(8330) ->
    {0, [], {sub, [{0, 43}]}, sm};
unicode_table(8331) ->
    {0, [], {sub, [{0, 8722}]}, sm};
unicode_table(8332) ->
    {0, [], {sub, [{0, 61}]}, sm};
unicode_table(8333) ->
    {0, [], {sub, [{0, 40}]}, ps};
unicode_table(8334) ->
    {0, [], {sub, [{0, 41}]}, pe};
unicode_table(8336) ->
    {0, [], {sub, [{0, 97}]}, lm};
unicode_table(8337) ->
    {0, [], {sub, [{0, 101}]}, lm};
unicode_table(8338) ->
    {0, [], {sub, [{0, 111}]}, lm};
unicode_table(8339) ->
    {0, [], {sub, [{0, 120}]}, lm};
unicode_table(8340) ->
    {0, [], {sub, [{0, 601}]}, lm};
unicode_table(8341) ->
    {0, [], {sub, [{0, 104}]}, lm};
unicode_table(8342) ->
    {0, [], {sub, [{0, 107}]}, lm};
unicode_table(8343) ->
    {0, [], {sub, [{0, 108}]}, lm};
unicode_table(8344) ->
    {0, [], {sub, [{0, 109}]}, lm};
unicode_table(8345) ->
    {0, [], {sub, [{0, 110}]}, lm};
unicode_table(8346) ->
    {0, [], {sub, [{0, 112}]}, lm};
unicode_table(8347) ->
    {0, [], {sub, [{0, 115}]}, lm};
unicode_table(8348) ->
    {0, [], {sub, [{0, 116}]}, lm};
unicode_table(8360) ->
    {0, [], {compat, [{0, 82}, {0, 115}]}, sc};
unicode_table(8400) ->
    {230, [], [], mn};
unicode_table(8401) ->
    {230, [], [], mn};
unicode_table(8402) ->
    {1, [], [], mn};
unicode_table(8403) ->
    {1, [], [], mn};
unicode_table(8404) ->
    {230, [], [], mn};
unicode_table(8405) ->
    {230, [], [], mn};
unicode_table(8406) ->
    {230, [], [], mn};
unicode_table(8407) ->
    {230, [], [], mn};
unicode_table(8408) ->
    {1, [], [], mn};
unicode_table(8409) ->
    {1, [], [], mn};
unicode_table(8410) ->
    {1, [], [], mn};
unicode_table(8411) ->
    {230, [], [], mn};
unicode_table(8412) ->
    {230, [], [], mn};
unicode_table(8417) ->
    {230, [], [], mn};
unicode_table(8421) ->
    {1, [], [], mn};
unicode_table(8422) ->
    {1, [], [], mn};
unicode_table(8423) ->
    {230, [], [], mn};
unicode_table(8424) ->
    {220, [], [], mn};
unicode_table(8425) ->
    {230, [], [], mn};
unicode_table(8426) ->
    {1, [], [], mn};
unicode_table(8427) ->
    {1, [], [], mn};
unicode_table(8428) ->
    {220, [], [], mn};
unicode_table(8429) ->
    {220, [], [], mn};
unicode_table(8430) ->
    {220, [], [], mn};
unicode_table(8431) ->
    {220, [], [], mn};
unicode_table(8432) ->
    {230, [], [], mn};
unicode_table(8448) ->
    {0, [], {compat, [{0, 97}, {0, 47}, {0, 99}]}, so};
unicode_table(8449) ->
    {0, [], {compat, [{0, 97}, {0, 47}, {0, 115}]}, so};
unicode_table(8450) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(8451) ->
    {0, [], {compat, [{0, 176}, {0, 67}]}, so};
unicode_table(8453) ->
    {0, [], {compat, [{0, 99}, {0, 47}, {0, 111}]}, so};
unicode_table(8454) ->
    {0, [], {compat, [{0, 99}, {0, 47}, {0, 117}]}, so};
unicode_table(8455) ->
    {0, [], {compat, [{0, 400}]}, lu};
unicode_table(8457) ->
    {0, [], {compat, [{0, 176}, {0, 70}]}, so};
unicode_table(8458) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(8459) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(8460) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(8461) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(8462) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(8463) ->
    {0, [], {font, [{0, 295}]}, ll};
unicode_table(8464) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(8465) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(8466) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(8467) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(8469) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(8470) ->
    {0, [], {compat, [{0, 78}, {0, 111}]}, so};
unicode_table(8473) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(8474) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(8475) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(8476) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(8477) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(8480) ->
    {0, [], {super, [{0, 83}, {0, 77}]}, so};
unicode_table(8481) ->
    {0, [], {compat, [{0, 84}, {0, 69}, {0, 76}]}, so};
unicode_table(8482) ->
    {0, [], {super, [{0, 84}, {0, 77}]}, so};
unicode_table(8484) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(8486) ->
    {0, [{0, 937}], [], lu};
unicode_table(8488) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(8490) ->
    {0, [{0, 75}], [], lu};
unicode_table(8491) ->
    {0, [{0, 65}, {230, 778}], [], lu};
unicode_table(8492) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(8493) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(8495) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(8496) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(8497) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(8499) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(8500) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(8501) ->
    {0, [], {compat, [{0, 1488}]}, lo};
unicode_table(8502) ->
    {0, [], {compat, [{0, 1489}]}, lo};
unicode_table(8503) ->
    {0, [], {compat, [{0, 1490}]}, lo};
unicode_table(8504) ->
    {0, [], {compat, [{0, 1491}]}, lo};
unicode_table(8505) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(8507) ->
    {0, [], {compat, [{0, 70}, {0, 65}, {0, 88}]}, so};
unicode_table(8508) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(8509) ->
    {0, [], {font, [{0, 947}]}, ll};
unicode_table(8510) ->
    {0, [], {font, [{0, 915}]}, lu};
unicode_table(8511) ->
    {0, [], {font, [{0, 928}]}, lu};
unicode_table(8512) ->
    {0, [], {font, [{0, 8721}]}, sm};
unicode_table(8517) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(8518) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(8519) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(8520) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(8521) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(8528) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 55}]}, no};
unicode_table(8529) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 57}]}, no};
unicode_table(8530) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 49}, {0, 48}]}, no};
unicode_table(8531) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 51}]}, no};
unicode_table(8532) ->
    {0, [], {fraction, [{0, 50}, {0, 8260}, {0, 51}]}, no};
unicode_table(8533) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 53}]}, no};
unicode_table(8534) ->
    {0, [], {fraction, [{0, 50}, {0, 8260}, {0, 53}]}, no};
unicode_table(8535) ->
    {0, [], {fraction, [{0, 51}, {0, 8260}, {0, 53}]}, no};
unicode_table(8536) ->
    {0, [], {fraction, [{0, 52}, {0, 8260}, {0, 53}]}, no};
unicode_table(8537) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 54}]}, no};
unicode_table(8538) ->
    {0, [], {fraction, [{0, 53}, {0, 8260}, {0, 54}]}, no};
unicode_table(8539) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}, {0, 56}]}, no};
unicode_table(8540) ->
    {0, [], {fraction, [{0, 51}, {0, 8260}, {0, 56}]}, no};
unicode_table(8541) ->
    {0, [], {fraction, [{0, 53}, {0, 8260}, {0, 56}]}, no};
unicode_table(8542) ->
    {0, [], {fraction, [{0, 55}, {0, 8260}, {0, 56}]}, no};
unicode_table(8543) ->
    {0, [], {fraction, [{0, 49}, {0, 8260}]}, no};
unicode_table(8544) ->
    {0, [], {compat, [{0, 73}]}, nl};
unicode_table(8545) ->
    {0, [], {compat, [{0, 73}, {0, 73}]}, nl};
unicode_table(8546) ->
    {0, [], {compat, [{0, 73}, {0, 73}, {0, 73}]}, nl};
unicode_table(8547) ->
    {0, [], {compat, [{0, 73}, {0, 86}]}, nl};
unicode_table(8548) ->
    {0, [], {compat, [{0, 86}]}, nl};
unicode_table(8549) ->
    {0, [], {compat, [{0, 86}, {0, 73}]}, nl};
unicode_table(8550) ->
    {0, [], {compat, [{0, 86}, {0, 73}, {0, 73}]}, nl};
unicode_table(8551) ->
    {0, [], {compat, [{0, 86}, {0, 73}, {0, 73}, {0, 73}]}, nl};
unicode_table(8552) ->
    {0, [], {compat, [{0, 73}, {0, 88}]}, nl};
unicode_table(8553) ->
    {0, [], {compat, [{0, 88}]}, nl};
unicode_table(8554) ->
    {0, [], {compat, [{0, 88}, {0, 73}]}, nl};
unicode_table(8555) ->
    {0, [], {compat, [{0, 88}, {0, 73}, {0, 73}]}, nl};
unicode_table(8556) ->
    {0, [], {compat, [{0, 76}]}, nl};
unicode_table(8557) ->
    {0, [], {compat, [{0, 67}]}, nl};
unicode_table(8558) ->
    {0, [], {compat, [{0, 68}]}, nl};
unicode_table(8559) ->
    {0, [], {compat, [{0, 77}]}, nl};
unicode_table(8560) ->
    {0, [], {compat, [{0, 105}]}, nl};
unicode_table(8561) ->
    {0, [], {compat, [{0, 105}, {0, 105}]}, nl};
unicode_table(8562) ->
    {0, [], {compat, [{0, 105}, {0, 105}, {0, 105}]}, nl};
unicode_table(8563) ->
    {0, [], {compat, [{0, 105}, {0, 118}]}, nl};
unicode_table(8564) ->
    {0, [], {compat, [{0, 118}]}, nl};
unicode_table(8565) ->
    {0, [], {compat, [{0, 118}, {0, 105}]}, nl};
unicode_table(8566) ->
    {0, [], {compat, [{0, 118}, {0, 105}, {0, 105}]}, nl};
unicode_table(8567) ->
    {0, [], {compat, [{0, 118}, {0, 105}, {0, 105}, {0, 105}]}, nl};
unicode_table(8568) ->
    {0, [], {compat, [{0, 105}, {0, 120}]}, nl};
unicode_table(8569) ->
    {0, [], {compat, [{0, 120}]}, nl};
unicode_table(8570) ->
    {0, [], {compat, [{0, 120}, {0, 105}]}, nl};
unicode_table(8571) ->
    {0, [], {compat, [{0, 120}, {0, 105}, {0, 105}]}, nl};
unicode_table(8572) ->
    {0, [], {compat, [{0, 108}]}, nl};
unicode_table(8573) ->
    {0, [], {compat, [{0, 99}]}, nl};
unicode_table(8574) ->
    {0, [], {compat, [{0, 100}]}, nl};
unicode_table(8575) ->
    {0, [], {compat, [{0, 109}]}, nl};
unicode_table(8585) ->
    {0, [], {fraction, [{0, 48}, {0, 8260}, {0, 51}]}, no};
unicode_table(8602) ->
    {0, [{0, 8592}, {1, 824}], [], sm};
unicode_table(8603) ->
    {0, [{0, 8594}, {1, 824}], [], sm};
unicode_table(8622) ->
    {0, [{0, 8596}, {1, 824}], [], sm};
unicode_table(8653) ->
    {0, [{0, 8656}, {1, 824}], [], so};
unicode_table(8654) ->
    {0, [{0, 8660}, {1, 824}], [], sm};
unicode_table(8655) ->
    {0, [{0, 8658}, {1, 824}], [], sm};
unicode_table(8708) ->
    {0, [{0, 8707}, {1, 824}], [], sm};
unicode_table(8713) ->
    {0, [{0, 8712}, {1, 824}], [], sm};
unicode_table(8716) ->
    {0, [{0, 8715}, {1, 824}], [], sm};
unicode_table(8740) ->
    {0, [{0, 8739}, {1, 824}], [], sm};
unicode_table(8742) ->
    {0, [{0, 8741}, {1, 824}], [], sm};
unicode_table(8748) ->
    {0, [], {compat, [{0, 8747}, {0, 8747}]}, sm};
unicode_table(8749) ->
    {0, [], {compat, [{0, 8747}, {0, 8747}, {0, 8747}]}, sm};
unicode_table(8751) ->
    {0, [], {compat, [{0, 8750}, {0, 8750}]}, sm};
unicode_table(8752) ->
    {0, [], {compat, [{0, 8750}, {0, 8750}, {0, 8750}]}, sm};
unicode_table(8769) ->
    {0, [{0, 8764}, {1, 824}], [], sm};
unicode_table(8772) ->
    {0, [{0, 8771}, {1, 824}], [], sm};
unicode_table(8775) ->
    {0, [{0, 8773}, {1, 824}], [], sm};
unicode_table(8777) ->
    {0, [{0, 8776}, {1, 824}], [], sm};
unicode_table(8800) ->
    {0, [{0, 61}, {1, 824}], [], sm};
unicode_table(8802) ->
    {0, [{0, 8801}, {1, 824}], [], sm};
unicode_table(8813) ->
    {0, [{0, 8781}, {1, 824}], [], sm};
unicode_table(8814) ->
    {0, [{0, 60}, {1, 824}], [], sm};
unicode_table(8815) ->
    {0, [{0, 62}, {1, 824}], [], sm};
unicode_table(8816) ->
    {0, [{0, 8804}, {1, 824}], [], sm};
unicode_table(8817) ->
    {0, [{0, 8805}, {1, 824}], [], sm};
unicode_table(8820) ->
    {0, [{0, 8818}, {1, 824}], [], sm};
unicode_table(8821) ->
    {0, [{0, 8819}, {1, 824}], [], sm};
unicode_table(8824) ->
    {0, [{0, 8822}, {1, 824}], [], sm};
unicode_table(8825) ->
    {0, [{0, 8823}, {1, 824}], [], sm};
unicode_table(8832) ->
    {0, [{0, 8826}, {1, 824}], [], sm};
unicode_table(8833) ->
    {0, [{0, 8827}, {1, 824}], [], sm};
unicode_table(8836) ->
    {0, [{0, 8834}, {1, 824}], [], sm};
unicode_table(8837) ->
    {0, [{0, 8835}, {1, 824}], [], sm};
unicode_table(8840) ->
    {0, [{0, 8838}, {1, 824}], [], sm};
unicode_table(8841) ->
    {0, [{0, 8839}, {1, 824}], [], sm};
unicode_table(8876) ->
    {0, [{0, 8866}, {1, 824}], [], sm};
unicode_table(8877) ->
    {0, [{0, 8872}, {1, 824}], [], sm};
unicode_table(8878) ->
    {0, [{0, 8873}, {1, 824}], [], sm};
unicode_table(8879) ->
    {0, [{0, 8875}, {1, 824}], [], sm};
unicode_table(8928) ->
    {0, [{0, 8828}, {1, 824}], [], sm};
unicode_table(8929) ->
    {0, [{0, 8829}, {1, 824}], [], sm};
unicode_table(8930) ->
    {0, [{0, 8849}, {1, 824}], [], sm};
unicode_table(8931) ->
    {0, [{0, 8850}, {1, 824}], [], sm};
unicode_table(8938) ->
    {0, [{0, 8882}, {1, 824}], [], sm};
unicode_table(8939) ->
    {0, [{0, 8883}, {1, 824}], [], sm};
unicode_table(8940) ->
    {0, [{0, 8884}, {1, 824}], [], sm};
unicode_table(8941) ->
    {0, [{0, 8885}, {1, 824}], [], sm};
unicode_table(9001) ->
    {0, [{0, 12296}], [], ps};
unicode_table(9002) ->
    {0, [{0, 12297}], [], pe};
unicode_table(9312) ->
    {0, [], {circle, [{0, 49}]}, no};
unicode_table(9313) ->
    {0, [], {circle, [{0, 50}]}, no};
unicode_table(9314) ->
    {0, [], {circle, [{0, 51}]}, no};
unicode_table(9315) ->
    {0, [], {circle, [{0, 52}]}, no};
unicode_table(9316) ->
    {0, [], {circle, [{0, 53}]}, no};
unicode_table(9317) ->
    {0, [], {circle, [{0, 54}]}, no};
unicode_table(9318) ->
    {0, [], {circle, [{0, 55}]}, no};
unicode_table(9319) ->
    {0, [], {circle, [{0, 56}]}, no};
unicode_table(9320) ->
    {0, [], {circle, [{0, 57}]}, no};
unicode_table(9321) ->
    {0, [], {circle, [{0, 49}, {0, 48}]}, no};
unicode_table(9322) ->
    {0, [], {circle, [{0, 49}, {0, 49}]}, no};
unicode_table(9323) ->
    {0, [], {circle, [{0, 49}, {0, 50}]}, no};
unicode_table(9324) ->
    {0, [], {circle, [{0, 49}, {0, 51}]}, no};
unicode_table(9325) ->
    {0, [], {circle, [{0, 49}, {0, 52}]}, no};
unicode_table(9326) ->
    {0, [], {circle, [{0, 49}, {0, 53}]}, no};
unicode_table(9327) ->
    {0, [], {circle, [{0, 49}, {0, 54}]}, no};
unicode_table(9328) ->
    {0, [], {circle, [{0, 49}, {0, 55}]}, no};
unicode_table(9329) ->
    {0, [], {circle, [{0, 49}, {0, 56}]}, no};
unicode_table(9330) ->
    {0, [], {circle, [{0, 49}, {0, 57}]}, no};
unicode_table(9331) ->
    {0, [], {circle, [{0, 50}, {0, 48}]}, no};
unicode_table(9332) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 41}]}, no};
unicode_table(9333) ->
    {0, [], {compat, [{0, 40}, {0, 50}, {0, 41}]}, no};
unicode_table(9334) ->
    {0, [], {compat, [{0, 40}, {0, 51}, {0, 41}]}, no};
unicode_table(9335) ->
    {0, [], {compat, [{0, 40}, {0, 52}, {0, 41}]}, no};
unicode_table(9336) ->
    {0, [], {compat, [{0, 40}, {0, 53}, {0, 41}]}, no};
unicode_table(9337) ->
    {0, [], {compat, [{0, 40}, {0, 54}, {0, 41}]}, no};
unicode_table(9338) ->
    {0, [], {compat, [{0, 40}, {0, 55}, {0, 41}]}, no};
unicode_table(9339) ->
    {0, [], {compat, [{0, 40}, {0, 56}, {0, 41}]}, no};
unicode_table(9340) ->
    {0, [], {compat, [{0, 40}, {0, 57}, {0, 41}]}, no};
unicode_table(9341) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 48}, {0, 41}]}, no};
unicode_table(9342) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 49}, {0, 41}]}, no};
unicode_table(9343) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 50}, {0, 41}]}, no};
unicode_table(9344) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 51}, {0, 41}]}, no};
unicode_table(9345) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 52}, {0, 41}]}, no};
unicode_table(9346) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 53}, {0, 41}]}, no};
unicode_table(9347) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 54}, {0, 41}]}, no};
unicode_table(9348) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 55}, {0, 41}]}, no};
unicode_table(9349) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 56}, {0, 41}]}, no};
unicode_table(9350) ->
    {0, [], {compat, [{0, 40}, {0, 49}, {0, 57}, {0, 41}]}, no};
unicode_table(9351) ->
    {0, [], {compat, [{0, 40}, {0, 50}, {0, 48}, {0, 41}]}, no};
unicode_table(9352) ->
    {0, [], {compat, [{0, 49}, {0, 46}]}, no};
unicode_table(9353) ->
    {0, [], {compat, [{0, 50}, {0, 46}]}, no};
unicode_table(9354) ->
    {0, [], {compat, [{0, 51}, {0, 46}]}, no};
unicode_table(9355) ->
    {0, [], {compat, [{0, 52}, {0, 46}]}, no};
unicode_table(9356) ->
    {0, [], {compat, [{0, 53}, {0, 46}]}, no};
unicode_table(9357) ->
    {0, [], {compat, [{0, 54}, {0, 46}]}, no};
unicode_table(9358) ->
    {0, [], {compat, [{0, 55}, {0, 46}]}, no};
unicode_table(9359) ->
    {0, [], {compat, [{0, 56}, {0, 46}]}, no};
unicode_table(9360) ->
    {0, [], {compat, [{0, 57}, {0, 46}]}, no};
unicode_table(9361) ->
    {0, [], {compat, [{0, 49}, {0, 48}, {0, 46}]}, no};
unicode_table(9362) ->
    {0, [], {compat, [{0, 49}, {0, 49}, {0, 46}]}, no};
unicode_table(9363) ->
    {0, [], {compat, [{0, 49}, {0, 50}, {0, 46}]}, no};
unicode_table(9364) ->
    {0, [], {compat, [{0, 49}, {0, 51}, {0, 46}]}, no};
unicode_table(9365) ->
    {0, [], {compat, [{0, 49}, {0, 52}, {0, 46}]}, no};
unicode_table(9366) ->
    {0, [], {compat, [{0, 49}, {0, 53}, {0, 46}]}, no};
unicode_table(9367) ->
    {0, [], {compat, [{0, 49}, {0, 54}, {0, 46}]}, no};
unicode_table(9368) ->
    {0, [], {compat, [{0, 49}, {0, 55}, {0, 46}]}, no};
unicode_table(9369) ->
    {0, [], {compat, [{0, 49}, {0, 56}, {0, 46}]}, no};
unicode_table(9370) ->
    {0, [], {compat, [{0, 49}, {0, 57}, {0, 46}]}, no};
unicode_table(9371) ->
    {0, [], {compat, [{0, 50}, {0, 48}, {0, 46}]}, no};
unicode_table(9372) ->
    {0, [], {compat, [{0, 40}, {0, 97}, {0, 41}]}, so};
unicode_table(9373) ->
    {0, [], {compat, [{0, 40}, {0, 98}, {0, 41}]}, so};
unicode_table(9374) ->
    {0, [], {compat, [{0, 40}, {0, 99}, {0, 41}]}, so};
unicode_table(9375) ->
    {0, [], {compat, [{0, 40}, {0, 100}, {0, 41}]}, so};
unicode_table(9376) ->
    {0, [], {compat, [{0, 40}, {0, 101}, {0, 41}]}, so};
unicode_table(9377) ->
    {0, [], {compat, [{0, 40}, {0, 102}, {0, 41}]}, so};
unicode_table(9378) ->
    {0, [], {compat, [{0, 40}, {0, 103}, {0, 41}]}, so};
unicode_table(9379) ->
    {0, [], {compat, [{0, 40}, {0, 104}, {0, 41}]}, so};
unicode_table(9380) ->
    {0, [], {compat, [{0, 40}, {0, 105}, {0, 41}]}, so};
unicode_table(9381) ->
    {0, [], {compat, [{0, 40}, {0, 106}, {0, 41}]}, so};
unicode_table(9382) ->
    {0, [], {compat, [{0, 40}, {0, 107}, {0, 41}]}, so};
unicode_table(9383) ->
    {0, [], {compat, [{0, 40}, {0, 108}, {0, 41}]}, so};
unicode_table(9384) ->
    {0, [], {compat, [{0, 40}, {0, 109}, {0, 41}]}, so};
unicode_table(9385) ->
    {0, [], {compat, [{0, 40}, {0, 110}, {0, 41}]}, so};
unicode_table(9386) ->
    {0, [], {compat, [{0, 40}, {0, 111}, {0, 41}]}, so};
unicode_table(9387) ->
    {0, [], {compat, [{0, 40}, {0, 112}, {0, 41}]}, so};
unicode_table(9388) ->
    {0, [], {compat, [{0, 40}, {0, 113}, {0, 41}]}, so};
unicode_table(9389) ->
    {0, [], {compat, [{0, 40}, {0, 114}, {0, 41}]}, so};
unicode_table(9390) ->
    {0, [], {compat, [{0, 40}, {0, 115}, {0, 41}]}, so};
unicode_table(9391) ->
    {0, [], {compat, [{0, 40}, {0, 116}, {0, 41}]}, so};
unicode_table(9392) ->
    {0, [], {compat, [{0, 40}, {0, 117}, {0, 41}]}, so};
unicode_table(9393) ->
    {0, [], {compat, [{0, 40}, {0, 118}, {0, 41}]}, so};
unicode_table(9394) ->
    {0, [], {compat, [{0, 40}, {0, 119}, {0, 41}]}, so};
unicode_table(9395) ->
    {0, [], {compat, [{0, 40}, {0, 120}, {0, 41}]}, so};
unicode_table(9396) ->
    {0, [], {compat, [{0, 40}, {0, 121}, {0, 41}]}, so};
unicode_table(9397) ->
    {0, [], {compat, [{0, 40}, {0, 122}, {0, 41}]}, so};
unicode_table(9398) ->
    {0, [], {circle, [{0, 65}]}, so};
unicode_table(9399) ->
    {0, [], {circle, [{0, 66}]}, so};
unicode_table(9400) ->
    {0, [], {circle, [{0, 67}]}, so};
unicode_table(9401) ->
    {0, [], {circle, [{0, 68}]}, so};
unicode_table(9402) ->
    {0, [], {circle, [{0, 69}]}, so};
unicode_table(9403) ->
    {0, [], {circle, [{0, 70}]}, so};
unicode_table(9404) ->
    {0, [], {circle, [{0, 71}]}, so};
unicode_table(9405) ->
    {0, [], {circle, [{0, 72}]}, so};
unicode_table(9406) ->
    {0, [], {circle, [{0, 73}]}, so};
unicode_table(9407) ->
    {0, [], {circle, [{0, 74}]}, so};
unicode_table(9408) ->
    {0, [], {circle, [{0, 75}]}, so};
unicode_table(9409) ->
    {0, [], {circle, [{0, 76}]}, so};
unicode_table(9410) ->
    {0, [], {circle, [{0, 77}]}, so};
unicode_table(9411) ->
    {0, [], {circle, [{0, 78}]}, so};
unicode_table(9412) ->
    {0, [], {circle, [{0, 79}]}, so};
unicode_table(9413) ->
    {0, [], {circle, [{0, 80}]}, so};
unicode_table(9414) ->
    {0, [], {circle, [{0, 81}]}, so};
unicode_table(9415) ->
    {0, [], {circle, [{0, 82}]}, so};
unicode_table(9416) ->
    {0, [], {circle, [{0, 83}]}, so};
unicode_table(9417) ->
    {0, [], {circle, [{0, 84}]}, so};
unicode_table(9418) ->
    {0, [], {circle, [{0, 85}]}, so};
unicode_table(9419) ->
    {0, [], {circle, [{0, 86}]}, so};
unicode_table(9420) ->
    {0, [], {circle, [{0, 87}]}, so};
unicode_table(9421) ->
    {0, [], {circle, [{0, 88}]}, so};
unicode_table(9422) ->
    {0, [], {circle, [{0, 89}]}, so};
unicode_table(9423) ->
    {0, [], {circle, [{0, 90}]}, so};
unicode_table(9424) ->
    {0, [], {circle, [{0, 97}]}, so};
unicode_table(9425) ->
    {0, [], {circle, [{0, 98}]}, so};
unicode_table(9426) ->
    {0, [], {circle, [{0, 99}]}, so};
unicode_table(9427) ->
    {0, [], {circle, [{0, 100}]}, so};
unicode_table(9428) ->
    {0, [], {circle, [{0, 101}]}, so};
unicode_table(9429) ->
    {0, [], {circle, [{0, 102}]}, so};
unicode_table(9430) ->
    {0, [], {circle, [{0, 103}]}, so};
unicode_table(9431) ->
    {0, [], {circle, [{0, 104}]}, so};
unicode_table(9432) ->
    {0, [], {circle, [{0, 105}]}, so};
unicode_table(9433) ->
    {0, [], {circle, [{0, 106}]}, so};
unicode_table(9434) ->
    {0, [], {circle, [{0, 107}]}, so};
unicode_table(9435) ->
    {0, [], {circle, [{0, 108}]}, so};
unicode_table(9436) ->
    {0, [], {circle, [{0, 109}]}, so};
unicode_table(9437) ->
    {0, [], {circle, [{0, 110}]}, so};
unicode_table(9438) ->
    {0, [], {circle, [{0, 111}]}, so};
unicode_table(9439) ->
    {0, [], {circle, [{0, 112}]}, so};
unicode_table(9440) ->
    {0, [], {circle, [{0, 113}]}, so};
unicode_table(9441) ->
    {0, [], {circle, [{0, 114}]}, so};
unicode_table(9442) ->
    {0, [], {circle, [{0, 115}]}, so};
unicode_table(9443) ->
    {0, [], {circle, [{0, 116}]}, so};
unicode_table(9444) ->
    {0, [], {circle, [{0, 117}]}, so};
unicode_table(9445) ->
    {0, [], {circle, [{0, 118}]}, so};
unicode_table(9446) ->
    {0, [], {circle, [{0, 119}]}, so};
unicode_table(9447) ->
    {0, [], {circle, [{0, 120}]}, so};
unicode_table(9448) ->
    {0, [], {circle, [{0, 121}]}, so};
unicode_table(9449) ->
    {0, [], {circle, [{0, 122}]}, so};
unicode_table(9450) ->
    {0, [], {circle, [{0, 48}]}, no};
unicode_table(10764) ->
    {0, [], {compat, [{0, 8747}, {0, 8747}, {0, 8747}, {0, 8747}]}, sm};
unicode_table(10868) ->
    {0, [], {compat, [{0, 58}, {0, 58}, {0, 61}]}, sm};
unicode_table(10869) ->
    {0, [], {compat, [{0, 61}, {0, 61}]}, sm};
unicode_table(10870) ->
    {0, [], {compat, [{0, 61}, {0, 61}, {0, 61}]}, sm};
unicode_table(10972) ->
    {0, [{0, 10973}, {1, 824}], [], sm};
unicode_table(11388) ->
    {0, [], {sub, [{0, 106}]}, lm};
unicode_table(11389) ->
    {0, [], {super, [{0, 86}]}, lm};
unicode_table(11503) ->
    {230, [], [], mn};
unicode_table(11504) ->
    {230, [], [], mn};
unicode_table(11505) ->
    {230, [], [], mn};
unicode_table(11631) ->
    {0, [], {super, [{0, 11617}]}, lm};
unicode_table(11647) ->
    {9, [], [], mn};
unicode_table(11744) ->
    {230, [], [], mn};
unicode_table(11745) ->
    {230, [], [], mn};
unicode_table(11746) ->
    {230, [], [], mn};
unicode_table(11747) ->
    {230, [], [], mn};
unicode_table(11748) ->
    {230, [], [], mn};
unicode_table(11749) ->
    {230, [], [], mn};
unicode_table(11750) ->
    {230, [], [], mn};
unicode_table(11751) ->
    {230, [], [], mn};
unicode_table(11752) ->
    {230, [], [], mn};
unicode_table(11753) ->
    {230, [], [], mn};
unicode_table(11754) ->
    {230, [], [], mn};
unicode_table(11755) ->
    {230, [], [], mn};
unicode_table(11756) ->
    {230, [], [], mn};
unicode_table(11757) ->
    {230, [], [], mn};
unicode_table(11758) ->
    {230, [], [], mn};
unicode_table(11759) ->
    {230, [], [], mn};
unicode_table(11760) ->
    {230, [], [], mn};
unicode_table(11761) ->
    {230, [], [], mn};
unicode_table(11762) ->
    {230, [], [], mn};
unicode_table(11763) ->
    {230, [], [], mn};
unicode_table(11764) ->
    {230, [], [], mn};
unicode_table(11765) ->
    {230, [], [], mn};
unicode_table(11766) ->
    {230, [], [], mn};
unicode_table(11767) ->
    {230, [], [], mn};
unicode_table(11768) ->
    {230, [], [], mn};
unicode_table(11769) ->
    {230, [], [], mn};
unicode_table(11770) ->
    {230, [], [], mn};
unicode_table(11771) ->
    {230, [], [], mn};
unicode_table(11772) ->
    {230, [], [], mn};
unicode_table(11773) ->
    {230, [], [], mn};
unicode_table(11774) ->
    {230, [], [], mn};
unicode_table(11775) ->
    {230, [], [], mn};
unicode_table(11935) ->
    {0, [], {compat, [{0, 27597}]}, so};
unicode_table(12019) ->
    {0, [], {compat, [{0, 40863}]}, so};
unicode_table(12032) ->
    {0, [], {compat, [{0, 19968}]}, so};
unicode_table(12033) ->
    {0, [], {compat, [{0, 20008}]}, so};
unicode_table(12034) ->
    {0, [], {compat, [{0, 20022}]}, so};
unicode_table(12035) ->
    {0, [], {compat, [{0, 20031}]}, so};
unicode_table(12036) ->
    {0, [], {compat, [{0, 20057}]}, so};
unicode_table(12037) ->
    {0, [], {compat, [{0, 20101}]}, so};
unicode_table(12038) ->
    {0, [], {compat, [{0, 20108}]}, so};
unicode_table(12039) ->
    {0, [], {compat, [{0, 20128}]}, so};
unicode_table(12040) ->
    {0, [], {compat, [{0, 20154}]}, so};
unicode_table(12041) ->
    {0, [], {compat, [{0, 20799}]}, so};
unicode_table(12042) ->
    {0, [], {compat, [{0, 20837}]}, so};
unicode_table(12043) ->
    {0, [], {compat, [{0, 20843}]}, so};
unicode_table(12044) ->
    {0, [], {compat, [{0, 20866}]}, so};
unicode_table(12045) ->
    {0, [], {compat, [{0, 20886}]}, so};
unicode_table(12046) ->
    {0, [], {compat, [{0, 20907}]}, so};
unicode_table(12047) ->
    {0, [], {compat, [{0, 20960}]}, so};
unicode_table(12048) ->
    {0, [], {compat, [{0, 20981}]}, so};
unicode_table(12049) ->
    {0, [], {compat, [{0, 20992}]}, so};
unicode_table(12050) ->
    {0, [], {compat, [{0, 21147}]}, so};
unicode_table(12051) ->
    {0, [], {compat, [{0, 21241}]}, so};
unicode_table(12052) ->
    {0, [], {compat, [{0, 21269}]}, so};
unicode_table(12053) ->
    {0, [], {compat, [{0, 21274}]}, so};
unicode_table(12054) ->
    {0, [], {compat, [{0, 21304}]}, so};
unicode_table(12055) ->
    {0, [], {compat, [{0, 21313}]}, so};
unicode_table(12056) ->
    {0, [], {compat, [{0, 21340}]}, so};
unicode_table(12057) ->
    {0, [], {compat, [{0, 21353}]}, so};
unicode_table(12058) ->
    {0, [], {compat, [{0, 21378}]}, so};
unicode_table(12059) ->
    {0, [], {compat, [{0, 21430}]}, so};
unicode_table(12060) ->
    {0, [], {compat, [{0, 21448}]}, so};
unicode_table(12061) ->
    {0, [], {compat, [{0, 21475}]}, so};
unicode_table(12062) ->
    {0, [], {compat, [{0, 22231}]}, so};
unicode_table(12063) ->
    {0, [], {compat, [{0, 22303}]}, so};
unicode_table(12064) ->
    {0, [], {compat, [{0, 22763}]}, so};
unicode_table(12065) ->
    {0, [], {compat, [{0, 22786}]}, so};
unicode_table(12066) ->
    {0, [], {compat, [{0, 22794}]}, so};
unicode_table(12067) ->
    {0, [], {compat, [{0, 22805}]}, so};
unicode_table(12068) ->
    {0, [], {compat, [{0, 22823}]}, so};
unicode_table(12069) ->
    {0, [], {compat, [{0, 22899}]}, so};
unicode_table(12070) ->
    {0, [], {compat, [{0, 23376}]}, so};
unicode_table(12071) ->
    {0, [], {compat, [{0, 23424}]}, so};
unicode_table(12072) ->
    {0, [], {compat, [{0, 23544}]}, so};
unicode_table(12073) ->
    {0, [], {compat, [{0, 23567}]}, so};
unicode_table(12074) ->
    {0, [], {compat, [{0, 23586}]}, so};
unicode_table(12075) ->
    {0, [], {compat, [{0, 23608}]}, so};
unicode_table(12076) ->
    {0, [], {compat, [{0, 23662}]}, so};
unicode_table(12077) ->
    {0, [], {compat, [{0, 23665}]}, so};
unicode_table(12078) ->
    {0, [], {compat, [{0, 24027}]}, so};
unicode_table(12079) ->
    {0, [], {compat, [{0, 24037}]}, so};
unicode_table(12080) ->
    {0, [], {compat, [{0, 24049}]}, so};
unicode_table(12081) ->
    {0, [], {compat, [{0, 24062}]}, so};
unicode_table(12082) ->
    {0, [], {compat, [{0, 24178}]}, so};
unicode_table(12083) ->
    {0, [], {compat, [{0, 24186}]}, so};
unicode_table(12084) ->
    {0, [], {compat, [{0, 24191}]}, so};
unicode_table(12085) ->
    {0, [], {compat, [{0, 24308}]}, so};
unicode_table(12086) ->
    {0, [], {compat, [{0, 24318}]}, so};
unicode_table(12087) ->
    {0, [], {compat, [{0, 24331}]}, so};
unicode_table(12088) ->
    {0, [], {compat, [{0, 24339}]}, so};
unicode_table(12089) ->
    {0, [], {compat, [{0, 24400}]}, so};
unicode_table(12090) ->
    {0, [], {compat, [{0, 24417}]}, so};
unicode_table(12091) ->
    {0, [], {compat, [{0, 24435}]}, so};
unicode_table(12092) ->
    {0, [], {compat, [{0, 24515}]}, so};
unicode_table(12093) ->
    {0, [], {compat, [{0, 25096}]}, so};
unicode_table(12094) ->
    {0, [], {compat, [{0, 25142}]}, so};
unicode_table(12095) ->
    {0, [], {compat, [{0, 25163}]}, so};
unicode_table(12096) ->
    {0, [], {compat, [{0, 25903}]}, so};
unicode_table(12097) ->
    {0, [], {compat, [{0, 25908}]}, so};
unicode_table(12098) ->
    {0, [], {compat, [{0, 25991}]}, so};
unicode_table(12099) ->
    {0, [], {compat, [{0, 26007}]}, so};
unicode_table(12100) ->
    {0, [], {compat, [{0, 26020}]}, so};
unicode_table(12101) ->
    {0, [], {compat, [{0, 26041}]}, so};
unicode_table(12102) ->
    {0, [], {compat, [{0, 26080}]}, so};
unicode_table(12103) ->
    {0, [], {compat, [{0, 26085}]}, so};
unicode_table(12104) ->
    {0, [], {compat, [{0, 26352}]}, so};
unicode_table(12105) ->
    {0, [], {compat, [{0, 26376}]}, so};
unicode_table(12106) ->
    {0, [], {compat, [{0, 26408}]}, so};
unicode_table(12107) ->
    {0, [], {compat, [{0, 27424}]}, so};
unicode_table(12108) ->
    {0, [], {compat, [{0, 27490}]}, so};
unicode_table(12109) ->
    {0, [], {compat, [{0, 27513}]}, so};
unicode_table(12110) ->
    {0, [], {compat, [{0, 27571}]}, so};
unicode_table(12111) ->
    {0, [], {compat, [{0, 27595}]}, so};
unicode_table(12112) ->
    {0, [], {compat, [{0, 27604}]}, so};
unicode_table(12113) ->
    {0, [], {compat, [{0, 27611}]}, so};
unicode_table(12114) ->
    {0, [], {compat, [{0, 27663}]}, so};
unicode_table(12115) ->
    {0, [], {compat, [{0, 27668}]}, so};
unicode_table(12116) ->
    {0, [], {compat, [{0, 27700}]}, so};
unicode_table(12117) ->
    {0, [], {compat, [{0, 28779}]}, so};
unicode_table(12118) ->
    {0, [], {compat, [{0, 29226}]}, so};
unicode_table(12119) ->
    {0, [], {compat, [{0, 29238}]}, so};
unicode_table(12120) ->
    {0, [], {compat, [{0, 29243}]}, so};
unicode_table(12121) ->
    {0, [], {compat, [{0, 29247}]}, so};
unicode_table(12122) ->
    {0, [], {compat, [{0, 29255}]}, so};
unicode_table(12123) ->
    {0, [], {compat, [{0, 29273}]}, so};
unicode_table(12124) ->
    {0, [], {compat, [{0, 29275}]}, so};
unicode_table(12125) ->
    {0, [], {compat, [{0, 29356}]}, so};
unicode_table(12126) ->
    {0, [], {compat, [{0, 29572}]}, so};
unicode_table(12127) ->
    {0, [], {compat, [{0, 29577}]}, so};
unicode_table(12128) ->
    {0, [], {compat, [{0, 29916}]}, so};
unicode_table(12129) ->
    {0, [], {compat, [{0, 29926}]}, so};
unicode_table(12130) ->
    {0, [], {compat, [{0, 29976}]}, so};
unicode_table(12131) ->
    {0, [], {compat, [{0, 29983}]}, so};
unicode_table(12132) ->
    {0, [], {compat, [{0, 29992}]}, so};
unicode_table(12133) ->
    {0, [], {compat, [{0, 30000}]}, so};
unicode_table(12134) ->
    {0, [], {compat, [{0, 30091}]}, so};
unicode_table(12135) ->
    {0, [], {compat, [{0, 30098}]}, so};
unicode_table(12136) ->
    {0, [], {compat, [{0, 30326}]}, so};
unicode_table(12137) ->
    {0, [], {compat, [{0, 30333}]}, so};
unicode_table(12138) ->
    {0, [], {compat, [{0, 30382}]}, so};
unicode_table(12139) ->
    {0, [], {compat, [{0, 30399}]}, so};
unicode_table(12140) ->
    {0, [], {compat, [{0, 30446}]}, so};
unicode_table(12141) ->
    {0, [], {compat, [{0, 30683}]}, so};
unicode_table(12142) ->
    {0, [], {compat, [{0, 30690}]}, so};
unicode_table(12143) ->
    {0, [], {compat, [{0, 30707}]}, so};
unicode_table(12144) ->
    {0, [], {compat, [{0, 31034}]}, so};
unicode_table(12145) ->
    {0, [], {compat, [{0, 31160}]}, so};
unicode_table(12146) ->
    {0, [], {compat, [{0, 31166}]}, so};
unicode_table(12147) ->
    {0, [], {compat, [{0, 31348}]}, so};
unicode_table(12148) ->
    {0, [], {compat, [{0, 31435}]}, so};
unicode_table(12149) ->
    {0, [], {compat, [{0, 31481}]}, so};
unicode_table(12150) ->
    {0, [], {compat, [{0, 31859}]}, so};
unicode_table(12151) ->
    {0, [], {compat, [{0, 31992}]}, so};
unicode_table(12152) ->
    {0, [], {compat, [{0, 32566}]}, so};
unicode_table(12153) ->
    {0, [], {compat, [{0, 32593}]}, so};
unicode_table(12154) ->
    {0, [], {compat, [{0, 32650}]}, so};
unicode_table(12155) ->
    {0, [], {compat, [{0, 32701}]}, so};
unicode_table(12156) ->
    {0, [], {compat, [{0, 32769}]}, so};
unicode_table(12157) ->
    {0, [], {compat, [{0, 32780}]}, so};
unicode_table(12158) ->
    {0, [], {compat, [{0, 32786}]}, so};
unicode_table(12159) ->
    {0, [], {compat, [{0, 32819}]}, so};
unicode_table(12160) ->
    {0, [], {compat, [{0, 32895}]}, so};
unicode_table(12161) ->
    {0, [], {compat, [{0, 32905}]}, so};
unicode_table(12162) ->
    {0, [], {compat, [{0, 33251}]}, so};
unicode_table(12163) ->
    {0, [], {compat, [{0, 33258}]}, so};
unicode_table(12164) ->
    {0, [], {compat, [{0, 33267}]}, so};
unicode_table(12165) ->
    {0, [], {compat, [{0, 33276}]}, so};
unicode_table(12166) ->
    {0, [], {compat, [{0, 33292}]}, so};
unicode_table(12167) ->
    {0, [], {compat, [{0, 33307}]}, so};
unicode_table(12168) ->
    {0, [], {compat, [{0, 33311}]}, so};
unicode_table(12169) ->
    {0, [], {compat, [{0, 33390}]}, so};
unicode_table(12170) ->
    {0, [], {compat, [{0, 33394}]}, so};
unicode_table(12171) ->
    {0, [], {compat, [{0, 33400}]}, so};
unicode_table(12172) ->
    {0, [], {compat, [{0, 34381}]}, so};
unicode_table(12173) ->
    {0, [], {compat, [{0, 34411}]}, so};
unicode_table(12174) ->
    {0, [], {compat, [{0, 34880}]}, so};
unicode_table(12175) ->
    {0, [], {compat, [{0, 34892}]}, so};
unicode_table(12176) ->
    {0, [], {compat, [{0, 34915}]}, so};
unicode_table(12177) ->
    {0, [], {compat, [{0, 35198}]}, so};
unicode_table(12178) ->
    {0, [], {compat, [{0, 35211}]}, so};
unicode_table(12179) ->
    {0, [], {compat, [{0, 35282}]}, so};
unicode_table(12180) ->
    {0, [], {compat, [{0, 35328}]}, so};
unicode_table(12181) ->
    {0, [], {compat, [{0, 35895}]}, so};
unicode_table(12182) ->
    {0, [], {compat, [{0, 35910}]}, so};
unicode_table(12183) ->
    {0, [], {compat, [{0, 35925}]}, so};
unicode_table(12184) ->
    {0, [], {compat, [{0, 35960}]}, so};
unicode_table(12185) ->
    {0, [], {compat, [{0, 35997}]}, so};
unicode_table(12186) ->
    {0, [], {compat, [{0, 36196}]}, so};
unicode_table(12187) ->
    {0, [], {compat, [{0, 36208}]}, so};
unicode_table(12188) ->
    {0, [], {compat, [{0, 36275}]}, so};
unicode_table(12189) ->
    {0, [], {compat, [{0, 36523}]}, so};
unicode_table(12190) ->
    {0, [], {compat, [{0, 36554}]}, so};
unicode_table(12191) ->
    {0, [], {compat, [{0, 36763}]}, so};
unicode_table(12192) ->
    {0, [], {compat, [{0, 36784}]}, so};
unicode_table(12193) ->
    {0, [], {compat, [{0, 36789}]}, so};
unicode_table(12194) ->
    {0, [], {compat, [{0, 37009}]}, so};
unicode_table(12195) ->
    {0, [], {compat, [{0, 37193}]}, so};
unicode_table(12196) ->
    {0, [], {compat, [{0, 37318}]}, so};
unicode_table(12197) ->
    {0, [], {compat, [{0, 37324}]}, so};
unicode_table(12198) ->
    {0, [], {compat, [{0, 37329}]}, so};
unicode_table(12199) ->
    {0, [], {compat, [{0, 38263}]}, so};
unicode_table(12200) ->
    {0, [], {compat, [{0, 38272}]}, so};
unicode_table(12201) ->
    {0, [], {compat, [{0, 38428}]}, so};
unicode_table(12202) ->
    {0, [], {compat, [{0, 38582}]}, so};
unicode_table(12203) ->
    {0, [], {compat, [{0, 38585}]}, so};
unicode_table(12204) ->
    {0, [], {compat, [{0, 38632}]}, so};
unicode_table(12205) ->
    {0, [], {compat, [{0, 38737}]}, so};
unicode_table(12206) ->
    {0, [], {compat, [{0, 38750}]}, so};
unicode_table(12207) ->
    {0, [], {compat, [{0, 38754}]}, so};
unicode_table(12208) ->
    {0, [], {compat, [{0, 38761}]}, so};
unicode_table(12209) ->
    {0, [], {compat, [{0, 38859}]}, so};
unicode_table(12210) ->
    {0, [], {compat, [{0, 38893}]}, so};
unicode_table(12211) ->
    {0, [], {compat, [{0, 38899}]}, so};
unicode_table(12212) ->
    {0, [], {compat, [{0, 38913}]}, so};
unicode_table(12213) ->
    {0, [], {compat, [{0, 39080}]}, so};
unicode_table(12214) ->
    {0, [], {compat, [{0, 39131}]}, so};
unicode_table(12215) ->
    {0, [], {compat, [{0, 39135}]}, so};
unicode_table(12216) ->
    {0, [], {compat, [{0, 39318}]}, so};
unicode_table(12217) ->
    {0, [], {compat, [{0, 39321}]}, so};
unicode_table(12218) ->
    {0, [], {compat, [{0, 39340}]}, so};
unicode_table(12219) ->
    {0, [], {compat, [{0, 39592}]}, so};
unicode_table(12220) ->
    {0, [], {compat, [{0, 39640}]}, so};
unicode_table(12221) ->
    {0, [], {compat, [{0, 39647}]}, so};
unicode_table(12222) ->
    {0, [], {compat, [{0, 39717}]}, so};
unicode_table(12223) ->
    {0, [], {compat, [{0, 39727}]}, so};
unicode_table(12224) ->
    {0, [], {compat, [{0, 39730}]}, so};
unicode_table(12225) ->
    {0, [], {compat, [{0, 39740}]}, so};
unicode_table(12226) ->
    {0, [], {compat, [{0, 39770}]}, so};
unicode_table(12227) ->
    {0, [], {compat, [{0, 40165}]}, so};
unicode_table(12228) ->
    {0, [], {compat, [{0, 40565}]}, so};
unicode_table(12229) ->
    {0, [], {compat, [{0, 40575}]}, so};
unicode_table(12230) ->
    {0, [], {compat, [{0, 40613}]}, so};
unicode_table(12231) ->
    {0, [], {compat, [{0, 40635}]}, so};
unicode_table(12232) ->
    {0, [], {compat, [{0, 40643}]}, so};
unicode_table(12233) ->
    {0, [], {compat, [{0, 40653}]}, so};
unicode_table(12234) ->
    {0, [], {compat, [{0, 40657}]}, so};
unicode_table(12235) ->
    {0, [], {compat, [{0, 40697}]}, so};
unicode_table(12236) ->
    {0, [], {compat, [{0, 40701}]}, so};
unicode_table(12237) ->
    {0, [], {compat, [{0, 40718}]}, so};
unicode_table(12238) ->
    {0, [], {compat, [{0, 40723}]}, so};
unicode_table(12239) ->
    {0, [], {compat, [{0, 40736}]}, so};
unicode_table(12240) ->
    {0, [], {compat, [{0, 40763}]}, so};
unicode_table(12241) ->
    {0, [], {compat, [{0, 40778}]}, so};
unicode_table(12242) ->
    {0, [], {compat, [{0, 40786}]}, so};
unicode_table(12243) ->
    {0, [], {compat, [{0, 40845}]}, so};
unicode_table(12244) ->
    {0, [], {compat, [{0, 40860}]}, so};
unicode_table(12245) ->
    {0, [], {compat, [{0, 40864}]}, so};
unicode_table(12288) ->
    {0, [], {wide, [{0, 32}]}, zs};
unicode_table(12330) ->
    {218, [], [], mn};
unicode_table(12331) ->
    {228, [], [], mn};
unicode_table(12332) ->
    {232, [], [], mn};
unicode_table(12333) ->
    {222, [], [], mn};
unicode_table(12334) ->
    {224, [], [], mc};
unicode_table(12335) ->
    {224, [], [], mc};
unicode_table(12342) ->
    {0, [], {compat, [{0, 12306}]}, so};
unicode_table(12344) ->
    {0, [], {compat, [{0, 21313}]}, nl};
unicode_table(12345) ->
    {0, [], {compat, [{0, 21316}]}, nl};
unicode_table(12346) ->
    {0, [], {compat, [{0, 21317}]}, nl};
unicode_table(12364) ->
    {0, [{0, 12363}, {8, 12441}], [], lo};
unicode_table(12366) ->
    {0, [{0, 12365}, {8, 12441}], [], lo};
unicode_table(12368) ->
    {0, [{0, 12367}, {8, 12441}], [], lo};
unicode_table(12370) ->
    {0, [{0, 12369}, {8, 12441}], [], lo};
unicode_table(12372) ->
    {0, [{0, 12371}, {8, 12441}], [], lo};
unicode_table(12374) ->
    {0, [{0, 12373}, {8, 12441}], [], lo};
unicode_table(12376) ->
    {0, [{0, 12375}, {8, 12441}], [], lo};
unicode_table(12378) ->
    {0, [{0, 12377}, {8, 12441}], [], lo};
unicode_table(12380) ->
    {0, [{0, 12379}, {8, 12441}], [], lo};
unicode_table(12382) ->
    {0, [{0, 12381}, {8, 12441}], [], lo};
unicode_table(12384) ->
    {0, [{0, 12383}, {8, 12441}], [], lo};
unicode_table(12386) ->
    {0, [{0, 12385}, {8, 12441}], [], lo};
unicode_table(12389) ->
    {0, [{0, 12388}, {8, 12441}], [], lo};
unicode_table(12391) ->
    {0, [{0, 12390}, {8, 12441}], [], lo};
unicode_table(12393) ->
    {0, [{0, 12392}, {8, 12441}], [], lo};
unicode_table(12400) ->
    {0, [{0, 12399}, {8, 12441}], [], lo};
unicode_table(12401) ->
    {0, [{0, 12399}, {8, 12442}], [], lo};
unicode_table(12403) ->
    {0, [{0, 12402}, {8, 12441}], [], lo};
unicode_table(12404) ->
    {0, [{0, 12402}, {8, 12442}], [], lo};
unicode_table(12406) ->
    {0, [{0, 12405}, {8, 12441}], [], lo};
unicode_table(12407) ->
    {0, [{0, 12405}, {8, 12442}], [], lo};
unicode_table(12409) ->
    {0, [{0, 12408}, {8, 12441}], [], lo};
unicode_table(12410) ->
    {0, [{0, 12408}, {8, 12442}], [], lo};
unicode_table(12412) ->
    {0, [{0, 12411}, {8, 12441}], [], lo};
unicode_table(12413) ->
    {0, [{0, 12411}, {8, 12442}], [], lo};
unicode_table(12436) ->
    {0, [{0, 12358}, {8, 12441}], [], lo};
unicode_table(12441) ->
    {8, [], [], mn};
unicode_table(12442) ->
    {8, [], [], mn};
unicode_table(12443) ->
    {0, [], {compat, [{0, 32}, {8, 12441}]}, sk};
unicode_table(12444) ->
    {0, [], {compat, [{0, 32}, {8, 12442}]}, sk};
unicode_table(12446) ->
    {0, [{0, 12445}, {8, 12441}], [], lm};
unicode_table(12447) ->
    {0, [], {vertical, [{0, 12424}, {0, 12426}]}, lo};
unicode_table(12460) ->
    {0, [{0, 12459}, {8, 12441}], [], lo};
unicode_table(12462) ->
    {0, [{0, 12461}, {8, 12441}], [], lo};
unicode_table(12464) ->
    {0, [{0, 12463}, {8, 12441}], [], lo};
unicode_table(12466) ->
    {0, [{0, 12465}, {8, 12441}], [], lo};
unicode_table(12468) ->
    {0, [{0, 12467}, {8, 12441}], [], lo};
unicode_table(12470) ->
    {0, [{0, 12469}, {8, 12441}], [], lo};
unicode_table(12472) ->
    {0, [{0, 12471}, {8, 12441}], [], lo};
unicode_table(12474) ->
    {0, [{0, 12473}, {8, 12441}], [], lo};
unicode_table(12476) ->
    {0, [{0, 12475}, {8, 12441}], [], lo};
unicode_table(12478) ->
    {0, [{0, 12477}, {8, 12441}], [], lo};
unicode_table(12480) ->
    {0, [{0, 12479}, {8, 12441}], [], lo};
unicode_table(12482) ->
    {0, [{0, 12481}, {8, 12441}], [], lo};
unicode_table(12485) ->
    {0, [{0, 12484}, {8, 12441}], [], lo};
unicode_table(12487) ->
    {0, [{0, 12486}, {8, 12441}], [], lo};
unicode_table(12489) ->
    {0, [{0, 12488}, {8, 12441}], [], lo};
unicode_table(12496) ->
    {0, [{0, 12495}, {8, 12441}], [], lo};
unicode_table(12497) ->
    {0, [{0, 12495}, {8, 12442}], [], lo};
unicode_table(12499) ->
    {0, [{0, 12498}, {8, 12441}], [], lo};
unicode_table(12500) ->
    {0, [{0, 12498}, {8, 12442}], [], lo};
unicode_table(12502) ->
    {0, [{0, 12501}, {8, 12441}], [], lo};
unicode_table(12503) ->
    {0, [{0, 12501}, {8, 12442}], [], lo};
unicode_table(12505) ->
    {0, [{0, 12504}, {8, 12441}], [], lo};
unicode_table(12506) ->
    {0, [{0, 12504}, {8, 12442}], [], lo};
unicode_table(12508) ->
    {0, [{0, 12507}, {8, 12441}], [], lo};
unicode_table(12509) ->
    {0, [{0, 12507}, {8, 12442}], [], lo};
unicode_table(12532) ->
    {0, [{0, 12454}, {8, 12441}], [], lo};
unicode_table(12535) ->
    {0, [{0, 12527}, {8, 12441}], [], lo};
unicode_table(12536) ->
    {0, [{0, 12528}, {8, 12441}], [], lo};
unicode_table(12537) ->
    {0, [{0, 12529}, {8, 12441}], [], lo};
unicode_table(12538) ->
    {0, [{0, 12530}, {8, 12441}], [], lo};
unicode_table(12542) ->
    {0, [{0, 12541}, {8, 12441}], [], lm};
unicode_table(12543) ->
    {0, [], {vertical, [{0, 12467}, {0, 12488}]}, lo};
unicode_table(12593) ->
    {0, [], {compat, [{0, 4352}]}, lo};
unicode_table(12594) ->
    {0, [], {compat, [{0, 4353}]}, lo};
unicode_table(12595) ->
    {0, [], {compat, [{0, 4522}]}, lo};
unicode_table(12596) ->
    {0, [], {compat, [{0, 4354}]}, lo};
unicode_table(12597) ->
    {0, [], {compat, [{0, 4524}]}, lo};
unicode_table(12598) ->
    {0, [], {compat, [{0, 4525}]}, lo};
unicode_table(12599) ->
    {0, [], {compat, [{0, 4355}]}, lo};
unicode_table(12600) ->
    {0, [], {compat, [{0, 4356}]}, lo};
unicode_table(12601) ->
    {0, [], {compat, [{0, 4357}]}, lo};
unicode_table(12602) ->
    {0, [], {compat, [{0, 4528}]}, lo};
unicode_table(12603) ->
    {0, [], {compat, [{0, 4529}]}, lo};
unicode_table(12604) ->
    {0, [], {compat, [{0, 4530}]}, lo};
unicode_table(12605) ->
    {0, [], {compat, [{0, 4531}]}, lo};
unicode_table(12606) ->
    {0, [], {compat, [{0, 4532}]}, lo};
unicode_table(12607) ->
    {0, [], {compat, [{0, 4533}]}, lo};
unicode_table(12608) ->
    {0, [], {compat, [{0, 4378}]}, lo};
unicode_table(12609) ->
    {0, [], {compat, [{0, 4358}]}, lo};
unicode_table(12610) ->
    {0, [], {compat, [{0, 4359}]}, lo};
unicode_table(12611) ->
    {0, [], {compat, [{0, 4360}]}, lo};
unicode_table(12612) ->
    {0, [], {compat, [{0, 4385}]}, lo};
unicode_table(12613) ->
    {0, [], {compat, [{0, 4361}]}, lo};
unicode_table(12614) ->
    {0, [], {compat, [{0, 4362}]}, lo};
unicode_table(12615) ->
    {0, [], {compat, [{0, 4363}]}, lo};
unicode_table(12616) ->
    {0, [], {compat, [{0, 4364}]}, lo};
unicode_table(12617) ->
    {0, [], {compat, [{0, 4365}]}, lo};
unicode_table(12618) ->
    {0, [], {compat, [{0, 4366}]}, lo};
unicode_table(12619) ->
    {0, [], {compat, [{0, 4367}]}, lo};
unicode_table(12620) ->
    {0, [], {compat, [{0, 4368}]}, lo};
unicode_table(12621) ->
    {0, [], {compat, [{0, 4369}]}, lo};
unicode_table(12622) ->
    {0, [], {compat, [{0, 4370}]}, lo};
unicode_table(12623) ->
    {0, [], {compat, [{0, 4449}]}, lo};
unicode_table(12624) ->
    {0, [], {compat, [{0, 4450}]}, lo};
unicode_table(12625) ->
    {0, [], {compat, [{0, 4451}]}, lo};
unicode_table(12626) ->
    {0, [], {compat, [{0, 4452}]}, lo};
unicode_table(12627) ->
    {0, [], {compat, [{0, 4453}]}, lo};
unicode_table(12628) ->
    {0, [], {compat, [{0, 4454}]}, lo};
unicode_table(12629) ->
    {0, [], {compat, [{0, 4455}]}, lo};
unicode_table(12630) ->
    {0, [], {compat, [{0, 4456}]}, lo};
unicode_table(12631) ->
    {0, [], {compat, [{0, 4457}]}, lo};
unicode_table(12632) ->
    {0, [], {compat, [{0, 4458}]}, lo};
unicode_table(12633) ->
    {0, [], {compat, [{0, 4459}]}, lo};
unicode_table(12634) ->
    {0, [], {compat, [{0, 4460}]}, lo};
unicode_table(12635) ->
    {0, [], {compat, [{0, 4461}]}, lo};
unicode_table(12636) ->
    {0, [], {compat, [{0, 4462}]}, lo};
unicode_table(12637) ->
    {0, [], {compat, [{0, 4463}]}, lo};
unicode_table(12638) ->
    {0, [], {compat, [{0, 4464}]}, lo};
unicode_table(12639) ->
    {0, [], {compat, [{0, 4465}]}, lo};
unicode_table(12640) ->
    {0, [], {compat, [{0, 4466}]}, lo};
unicode_table(12641) ->
    {0, [], {compat, [{0, 4467}]}, lo};
unicode_table(12642) ->
    {0, [], {compat, [{0, 4468}]}, lo};
unicode_table(12643) ->
    {0, [], {compat, [{0, 4469}]}, lo};
unicode_table(12644) ->
    {0, [], {compat, [{0, 4448}]}, lo};
unicode_table(12645) ->
    {0, [], {compat, [{0, 4372}]}, lo};
unicode_table(12646) ->
    {0, [], {compat, [{0, 4373}]}, lo};
unicode_table(12647) ->
    {0, [], {compat, [{0, 4551}]}, lo};
unicode_table(12648) ->
    {0, [], {compat, [{0, 4552}]}, lo};
unicode_table(12649) ->
    {0, [], {compat, [{0, 4556}]}, lo};
unicode_table(12650) ->
    {0, [], {compat, [{0, 4558}]}, lo};
unicode_table(12651) ->
    {0, [], {compat, [{0, 4563}]}, lo};
unicode_table(12652) ->
    {0, [], {compat, [{0, 4567}]}, lo};
unicode_table(12653) ->
    {0, [], {compat, [{0, 4569}]}, lo};
unicode_table(12654) ->
    {0, [], {compat, [{0, 4380}]}, lo};
unicode_table(12655) ->
    {0, [], {compat, [{0, 4573}]}, lo};
unicode_table(12656) ->
    {0, [], {compat, [{0, 4575}]}, lo};
unicode_table(12657) ->
    {0, [], {compat, [{0, 4381}]}, lo};
unicode_table(12658) ->
    {0, [], {compat, [{0, 4382}]}, lo};
unicode_table(12659) ->
    {0, [], {compat, [{0, 4384}]}, lo};
unicode_table(12660) ->
    {0, [], {compat, [{0, 4386}]}, lo};
unicode_table(12661) ->
    {0, [], {compat, [{0, 4387}]}, lo};
unicode_table(12662) ->
    {0, [], {compat, [{0, 4391}]}, lo};
unicode_table(12663) ->
    {0, [], {compat, [{0, 4393}]}, lo};
unicode_table(12664) ->
    {0, [], {compat, [{0, 4395}]}, lo};
unicode_table(12665) ->
    {0, [], {compat, [{0, 4396}]}, lo};
unicode_table(12666) ->
    {0, [], {compat, [{0, 4397}]}, lo};
unicode_table(12667) ->
    {0, [], {compat, [{0, 4398}]}, lo};
unicode_table(12668) ->
    {0, [], {compat, [{0, 4399}]}, lo};
unicode_table(12669) ->
    {0, [], {compat, [{0, 4402}]}, lo};
unicode_table(12670) ->
    {0, [], {compat, [{0, 4406}]}, lo};
unicode_table(12671) ->
    {0, [], {compat, [{0, 4416}]}, lo};
unicode_table(12672) ->
    {0, [], {compat, [{0, 4423}]}, lo};
unicode_table(12673) ->
    {0, [], {compat, [{0, 4428}]}, lo};
unicode_table(12674) ->
    {0, [], {compat, [{0, 4593}]}, lo};
unicode_table(12675) ->
    {0, [], {compat, [{0, 4594}]}, lo};
unicode_table(12676) ->
    {0, [], {compat, [{0, 4439}]}, lo};
unicode_table(12677) ->
    {0, [], {compat, [{0, 4440}]}, lo};
unicode_table(12678) ->
    {0, [], {compat, [{0, 4441}]}, lo};
unicode_table(12679) ->
    {0, [], {compat, [{0, 4484}]}, lo};
unicode_table(12680) ->
    {0, [], {compat, [{0, 4485}]}, lo};
unicode_table(12681) ->
    {0, [], {compat, [{0, 4488}]}, lo};
unicode_table(12682) ->
    {0, [], {compat, [{0, 4497}]}, lo};
unicode_table(12683) ->
    {0, [], {compat, [{0, 4498}]}, lo};
unicode_table(12684) ->
    {0, [], {compat, [{0, 4500}]}, lo};
unicode_table(12685) ->
    {0, [], {compat, [{0, 4510}]}, lo};
unicode_table(12686) ->
    {0, [], {compat, [{0, 4513}]}, lo};
unicode_table(12690) ->
    {0, [], {super, [{0, 19968}]}, no};
unicode_table(12691) ->
    {0, [], {super, [{0, 20108}]}, no};
unicode_table(12692) ->
    {0, [], {super, [{0, 19977}]}, no};
unicode_table(12693) ->
    {0, [], {super, [{0, 22235}]}, no};
unicode_table(12694) ->
    {0, [], {super, [{0, 19978}]}, so};
unicode_table(12695) ->
    {0, [], {super, [{0, 20013}]}, so};
unicode_table(12696) ->
    {0, [], {super, [{0, 19979}]}, so};
unicode_table(12697) ->
    {0, [], {super, [{0, 30002}]}, so};
unicode_table(12698) ->
    {0, [], {super, [{0, 20057}]}, so};
unicode_table(12699) ->
    {0, [], {super, [{0, 19993}]}, so};
unicode_table(12700) ->
    {0, [], {super, [{0, 19969}]}, so};
unicode_table(12701) ->
    {0, [], {super, [{0, 22825}]}, so};
unicode_table(12702) ->
    {0, [], {super, [{0, 22320}]}, so};
unicode_table(12703) ->
    {0, [], {super, [{0, 20154}]}, so};
unicode_table(12800) ->
    {0, [], {compat, [{0, 40}, {0, 4352}, {0, 41}]}, so};
unicode_table(12801) ->
    {0, [], {compat, [{0, 40}, {0, 4354}, {0, 41}]}, so};
unicode_table(12802) ->
    {0, [], {compat, [{0, 40}, {0, 4355}, {0, 41}]}, so};
unicode_table(12803) ->
    {0, [], {compat, [{0, 40}, {0, 4357}, {0, 41}]}, so};
unicode_table(12804) ->
    {0, [], {compat, [{0, 40}, {0, 4358}, {0, 41}]}, so};
unicode_table(12805) ->
    {0, [], {compat, [{0, 40}, {0, 4359}, {0, 41}]}, so};
unicode_table(12806) ->
    {0, [], {compat, [{0, 40}, {0, 4361}, {0, 41}]}, so};
unicode_table(12807) ->
    {0, [], {compat, [{0, 40}, {0, 4363}, {0, 41}]}, so};
unicode_table(12808) ->
    {0, [], {compat, [{0, 40}, {0, 4364}, {0, 41}]}, so};
unicode_table(12809) ->
    {0, [], {compat, [{0, 40}, {0, 4366}, {0, 41}]}, so};
unicode_table(12810) ->
    {0, [], {compat, [{0, 40}, {0, 4367}, {0, 41}]}, so};
unicode_table(12811) ->
    {0, [], {compat, [{0, 40}, {0, 4368}, {0, 41}]}, so};
unicode_table(12812) ->
    {0, [], {compat, [{0, 40}, {0, 4369}, {0, 41}]}, so};
unicode_table(12813) ->
    {0, [], {compat, [{0, 40}, {0, 4370}, {0, 41}]}, so};
unicode_table(12814) ->
    {0, [], {compat, [{0, 40}, {0, 4352}, {0, 4449}, {0, 41}]}, so};
unicode_table(12815) ->
    {0, [], {compat, [{0, 40}, {0, 4354}, {0, 4449}, {0, 41}]}, so};
unicode_table(12816) ->
    {0, [], {compat, [{0, 40}, {0, 4355}, {0, 4449}, {0, 41}]}, so};
unicode_table(12817) ->
    {0, [], {compat, [{0, 40}, {0, 4357}, {0, 4449}, {0, 41}]}, so};
unicode_table(12818) ->
    {0, [], {compat, [{0, 40}, {0, 4358}, {0, 4449}, {0, 41}]}, so};
unicode_table(12819) ->
    {0, [], {compat, [{0, 40}, {0, 4359}, {0, 4449}, {0, 41}]}, so};
unicode_table(12820) ->
    {0, [], {compat, [{0, 40}, {0, 4361}, {0, 4449}, {0, 41}]}, so};
unicode_table(12821) ->
    {0, [], {compat, [{0, 40}, {0, 4363}, {0, 4449}, {0, 41}]}, so};
unicode_table(12822) ->
    {0, [], {compat, [{0, 40}, {0, 4364}, {0, 4449}, {0, 41}]}, so};
unicode_table(12823) ->
    {0, [], {compat, [{0, 40}, {0, 4366}, {0, 4449}, {0, 41}]}, so};
unicode_table(12824) ->
    {0, [], {compat, [{0, 40}, {0, 4367}, {0, 4449}, {0, 41}]}, so};
unicode_table(12825) ->
    {0, [], {compat, [{0, 40}, {0, 4368}, {0, 4449}, {0, 41}]}, so};
unicode_table(12826) ->
    {0, [], {compat, [{0, 40}, {0, 4369}, {0, 4449}, {0, 41}]}, so};
unicode_table(12827) ->
    {0, [], {compat, [{0, 40}, {0, 4370}, {0, 4449}, {0, 41}]}, so};
unicode_table(12828) ->
    {0, [], {compat, [{0, 40}, {0, 4364}, {0, 4462}, {0, 41}]}, so};
unicode_table(12829) ->
    {0, [], {compat, [{0, 40}, {0, 4363}, {0, 4457}, {0, 4364}, {0, 4453}, {0, 4523}, {0, 41}]},
        so};
unicode_table(12830) ->
    {0, [], {compat, [{0, 40}, {0, 4363}, {0, 4457}, {0, 4370}, {0, 4462}, {0, 41}]}, so};
unicode_table(12832) ->
    {0, [], {compat, [{0, 40}, {0, 19968}, {0, 41}]}, no};
unicode_table(12833) ->
    {0, [], {compat, [{0, 40}, {0, 20108}, {0, 41}]}, no};
unicode_table(12834) ->
    {0, [], {compat, [{0, 40}, {0, 19977}, {0, 41}]}, no};
unicode_table(12835) ->
    {0, [], {compat, [{0, 40}, {0, 22235}, {0, 41}]}, no};
unicode_table(12836) ->
    {0, [], {compat, [{0, 40}, {0, 20116}, {0, 41}]}, no};
unicode_table(12837) ->
    {0, [], {compat, [{0, 40}, {0, 20845}, {0, 41}]}, no};
unicode_table(12838) ->
    {0, [], {compat, [{0, 40}, {0, 19971}, {0, 41}]}, no};
unicode_table(12839) ->
    {0, [], {compat, [{0, 40}, {0, 20843}, {0, 41}]}, no};
unicode_table(12840) ->
    {0, [], {compat, [{0, 40}, {0, 20061}, {0, 41}]}, no};
unicode_table(12841) ->
    {0, [], {compat, [{0, 40}, {0, 21313}, {0, 41}]}, no};
unicode_table(12842) ->
    {0, [], {compat, [{0, 40}, {0, 26376}, {0, 41}]}, so};
unicode_table(12843) ->
    {0, [], {compat, [{0, 40}, {0, 28779}, {0, 41}]}, so};
unicode_table(12844) ->
    {0, [], {compat, [{0, 40}, {0, 27700}, {0, 41}]}, so};
unicode_table(12845) ->
    {0, [], {compat, [{0, 40}, {0, 26408}, {0, 41}]}, so};
unicode_table(12846) ->
    {0, [], {compat, [{0, 40}, {0, 37329}, {0, 41}]}, so};
unicode_table(12847) ->
    {0, [], {compat, [{0, 40}, {0, 22303}, {0, 41}]}, so};
unicode_table(12848) ->
    {0, [], {compat, [{0, 40}, {0, 26085}, {0, 41}]}, so};
unicode_table(12849) ->
    {0, [], {compat, [{0, 40}, {0, 26666}, {0, 41}]}, so};
unicode_table(12850) ->
    {0, [], {compat, [{0, 40}, {0, 26377}, {0, 41}]}, so};
unicode_table(12851) ->
    {0, [], {compat, [{0, 40}, {0, 31038}, {0, 41}]}, so};
unicode_table(12852) ->
    {0, [], {compat, [{0, 40}, {0, 21517}, {0, 41}]}, so};
unicode_table(12853) ->
    {0, [], {compat, [{0, 40}, {0, 29305}, {0, 41}]}, so};
unicode_table(12854) ->
    {0, [], {compat, [{0, 40}, {0, 36001}, {0, 41}]}, so};
unicode_table(12855) ->
    {0, [], {compat, [{0, 40}, {0, 31069}, {0, 41}]}, so};
unicode_table(12856) ->
    {0, [], {compat, [{0, 40}, {0, 21172}, {0, 41}]}, so};
unicode_table(12857) ->
    {0, [], {compat, [{0, 40}, {0, 20195}, {0, 41}]}, so};
unicode_table(12858) ->
    {0, [], {compat, [{0, 40}, {0, 21628}, {0, 41}]}, so};
unicode_table(12859) ->
    {0, [], {compat, [{0, 40}, {0, 23398}, {0, 41}]}, so};
unicode_table(12860) ->
    {0, [], {compat, [{0, 40}, {0, 30435}, {0, 41}]}, so};
unicode_table(12861) ->
    {0, [], {compat, [{0, 40}, {0, 20225}, {0, 41}]}, so};
unicode_table(12862) ->
    {0, [], {compat, [{0, 40}, {0, 36039}, {0, 41}]}, so};
unicode_table(12863) ->
    {0, [], {compat, [{0, 40}, {0, 21332}, {0, 41}]}, so};
unicode_table(12864) ->
    {0, [], {compat, [{0, 40}, {0, 31085}, {0, 41}]}, so};
unicode_table(12865) ->
    {0, [], {compat, [{0, 40}, {0, 20241}, {0, 41}]}, so};
unicode_table(12866) ->
    {0, [], {compat, [{0, 40}, {0, 33258}, {0, 41}]}, so};
unicode_table(12867) ->
    {0, [], {compat, [{0, 40}, {0, 33267}, {0, 41}]}, so};
unicode_table(12868) ->
    {0, [], {circle, [{0, 21839}]}, so};
unicode_table(12869) ->
    {0, [], {circle, [{0, 24188}]}, so};
unicode_table(12870) ->
    {0, [], {circle, [{0, 25991}]}, so};
unicode_table(12871) ->
    {0, [], {circle, [{0, 31631}]}, so};
unicode_table(12880) ->
    {0, [], {square, [{0, 80}, {0, 84}, {0, 69}]}, so};
unicode_table(12881) ->
    {0, [], {circle, [{0, 50}, {0, 49}]}, no};
unicode_table(12882) ->
    {0, [], {circle, [{0, 50}, {0, 50}]}, no};
unicode_table(12883) ->
    {0, [], {circle, [{0, 50}, {0, 51}]}, no};
unicode_table(12884) ->
    {0, [], {circle, [{0, 50}, {0, 52}]}, no};
unicode_table(12885) ->
    {0, [], {circle, [{0, 50}, {0, 53}]}, no};
unicode_table(12886) ->
    {0, [], {circle, [{0, 50}, {0, 54}]}, no};
unicode_table(12887) ->
    {0, [], {circle, [{0, 50}, {0, 55}]}, no};
unicode_table(12888) ->
    {0, [], {circle, [{0, 50}, {0, 56}]}, no};
unicode_table(12889) ->
    {0, [], {circle, [{0, 50}, {0, 57}]}, no};
unicode_table(12890) ->
    {0, [], {circle, [{0, 51}, {0, 48}]}, no};
unicode_table(12891) ->
    {0, [], {circle, [{0, 51}, {0, 49}]}, no};
unicode_table(12892) ->
    {0, [], {circle, [{0, 51}, {0, 50}]}, no};
unicode_table(12893) ->
    {0, [], {circle, [{0, 51}, {0, 51}]}, no};
unicode_table(12894) ->
    {0, [], {circle, [{0, 51}, {0, 52}]}, no};
unicode_table(12895) ->
    {0, [], {circle, [{0, 51}, {0, 53}]}, no};
unicode_table(12896) ->
    {0, [], {circle, [{0, 4352}]}, so};
unicode_table(12897) ->
    {0, [], {circle, [{0, 4354}]}, so};
unicode_table(12898) ->
    {0, [], {circle, [{0, 4355}]}, so};
unicode_table(12899) ->
    {0, [], {circle, [{0, 4357}]}, so};
unicode_table(12900) ->
    {0, [], {circle, [{0, 4358}]}, so};
unicode_table(12901) ->
    {0, [], {circle, [{0, 4359}]}, so};
unicode_table(12902) ->
    {0, [], {circle, [{0, 4361}]}, so};
unicode_table(12903) ->
    {0, [], {circle, [{0, 4363}]}, so};
unicode_table(12904) ->
    {0, [], {circle, [{0, 4364}]}, so};
unicode_table(12905) ->
    {0, [], {circle, [{0, 4366}]}, so};
unicode_table(12906) ->
    {0, [], {circle, [{0, 4367}]}, so};
unicode_table(12907) ->
    {0, [], {circle, [{0, 4368}]}, so};
unicode_table(12908) ->
    {0, [], {circle, [{0, 4369}]}, so};
unicode_table(12909) ->
    {0, [], {circle, [{0, 4370}]}, so};
unicode_table(12910) ->
    {0, [], {circle, [{0, 4352}, {0, 4449}]}, so};
unicode_table(12911) ->
    {0, [], {circle, [{0, 4354}, {0, 4449}]}, so};
unicode_table(12912) ->
    {0, [], {circle, [{0, 4355}, {0, 4449}]}, so};
unicode_table(12913) ->
    {0, [], {circle, [{0, 4357}, {0, 4449}]}, so};
unicode_table(12914) ->
    {0, [], {circle, [{0, 4358}, {0, 4449}]}, so};
unicode_table(12915) ->
    {0, [], {circle, [{0, 4359}, {0, 4449}]}, so};
unicode_table(12916) ->
    {0, [], {circle, [{0, 4361}, {0, 4449}]}, so};
unicode_table(12917) ->
    {0, [], {circle, [{0, 4363}, {0, 4449}]}, so};
unicode_table(12918) ->
    {0, [], {circle, [{0, 4364}, {0, 4449}]}, so};
unicode_table(12919) ->
    {0, [], {circle, [{0, 4366}, {0, 4449}]}, so};
unicode_table(12920) ->
    {0, [], {circle, [{0, 4367}, {0, 4449}]}, so};
unicode_table(12921) ->
    {0, [], {circle, [{0, 4368}, {0, 4449}]}, so};
unicode_table(12922) ->
    {0, [], {circle, [{0, 4369}, {0, 4449}]}, so};
unicode_table(12923) ->
    {0, [], {circle, [{0, 4370}, {0, 4449}]}, so};
unicode_table(12924) ->
    {0, [], {circle, [{0, 4366}, {0, 4449}, {0, 4535}, {0, 4352}, {0, 4457}]}, so};
unicode_table(12925) ->
    {0, [], {circle, [{0, 4364}, {0, 4462}, {0, 4363}, {0, 4468}]}, so};
unicode_table(12926) ->
    {0, [], {circle, [{0, 4363}, {0, 4462}]}, so};
unicode_table(12928) ->
    {0, [], {circle, [{0, 19968}]}, no};
unicode_table(12929) ->
    {0, [], {circle, [{0, 20108}]}, no};
unicode_table(12930) ->
    {0, [], {circle, [{0, 19977}]}, no};
unicode_table(12931) ->
    {0, [], {circle, [{0, 22235}]}, no};
unicode_table(12932) ->
    {0, [], {circle, [{0, 20116}]}, no};
unicode_table(12933) ->
    {0, [], {circle, [{0, 20845}]}, no};
unicode_table(12934) ->
    {0, [], {circle, [{0, 19971}]}, no};
unicode_table(12935) ->
    {0, [], {circle, [{0, 20843}]}, no};
unicode_table(12936) ->
    {0, [], {circle, [{0, 20061}]}, no};
unicode_table(12937) ->
    {0, [], {circle, [{0, 21313}]}, no};
unicode_table(12938) ->
    {0, [], {circle, [{0, 26376}]}, so};
unicode_table(12939) ->
    {0, [], {circle, [{0, 28779}]}, so};
unicode_table(12940) ->
    {0, [], {circle, [{0, 27700}]}, so};
unicode_table(12941) ->
    {0, [], {circle, [{0, 26408}]}, so};
unicode_table(12942) ->
    {0, [], {circle, [{0, 37329}]}, so};
unicode_table(12943) ->
    {0, [], {circle, [{0, 22303}]}, so};
unicode_table(12944) ->
    {0, [], {circle, [{0, 26085}]}, so};
unicode_table(12945) ->
    {0, [], {circle, [{0, 26666}]}, so};
unicode_table(12946) ->
    {0, [], {circle, [{0, 26377}]}, so};
unicode_table(12947) ->
    {0, [], {circle, [{0, 31038}]}, so};
unicode_table(12948) ->
    {0, [], {circle, [{0, 21517}]}, so};
unicode_table(12949) ->
    {0, [], {circle, [{0, 29305}]}, so};
unicode_table(12950) ->
    {0, [], {circle, [{0, 36001}]}, so};
unicode_table(12951) ->
    {0, [], {circle, [{0, 31069}]}, so};
unicode_table(12952) ->
    {0, [], {circle, [{0, 21172}]}, so};
unicode_table(12953) ->
    {0, [], {circle, [{0, 31192}]}, so};
unicode_table(12954) ->
    {0, [], {circle, [{0, 30007}]}, so};
unicode_table(12955) ->
    {0, [], {circle, [{0, 22899}]}, so};
unicode_table(12956) ->
    {0, [], {circle, [{0, 36969}]}, so};
unicode_table(12957) ->
    {0, [], {circle, [{0, 20778}]}, so};
unicode_table(12958) ->
    {0, [], {circle, [{0, 21360}]}, so};
unicode_table(12959) ->
    {0, [], {circle, [{0, 27880}]}, so};
unicode_table(12960) ->
    {0, [], {circle, [{0, 38917}]}, so};
unicode_table(12961) ->
    {0, [], {circle, [{0, 20241}]}, so};
unicode_table(12962) ->
    {0, [], {circle, [{0, 20889}]}, so};
unicode_table(12963) ->
    {0, [], {circle, [{0, 27491}]}, so};
unicode_table(12964) ->
    {0, [], {circle, [{0, 19978}]}, so};
unicode_table(12965) ->
    {0, [], {circle, [{0, 20013}]}, so};
unicode_table(12966) ->
    {0, [], {circle, [{0, 19979}]}, so};
unicode_table(12967) ->
    {0, [], {circle, [{0, 24038}]}, so};
unicode_table(12968) ->
    {0, [], {circle, [{0, 21491}]}, so};
unicode_table(12969) ->
    {0, [], {circle, [{0, 21307}]}, so};
unicode_table(12970) ->
    {0, [], {circle, [{0, 23447}]}, so};
unicode_table(12971) ->
    {0, [], {circle, [{0, 23398}]}, so};
unicode_table(12972) ->
    {0, [], {circle, [{0, 30435}]}, so};
unicode_table(12973) ->
    {0, [], {circle, [{0, 20225}]}, so};
unicode_table(12974) ->
    {0, [], {circle, [{0, 36039}]}, so};
unicode_table(12975) ->
    {0, [], {circle, [{0, 21332}]}, so};
unicode_table(12976) ->
    {0, [], {circle, [{0, 22812}]}, so};
unicode_table(12977) ->
    {0, [], {circle, [{0, 51}, {0, 54}]}, no};
unicode_table(12978) ->
    {0, [], {circle, [{0, 51}, {0, 55}]}, no};
unicode_table(12979) ->
    {0, [], {circle, [{0, 51}, {0, 56}]}, no};
unicode_table(12980) ->
    {0, [], {circle, [{0, 51}, {0, 57}]}, no};
unicode_table(12981) ->
    {0, [], {circle, [{0, 52}, {0, 48}]}, no};
unicode_table(12982) ->
    {0, [], {circle, [{0, 52}, {0, 49}]}, no};
unicode_table(12983) ->
    {0, [], {circle, [{0, 52}, {0, 50}]}, no};
unicode_table(12984) ->
    {0, [], {circle, [{0, 52}, {0, 51}]}, no};
unicode_table(12985) ->
    {0, [], {circle, [{0, 52}, {0, 52}]}, no};
unicode_table(12986) ->
    {0, [], {circle, [{0, 52}, {0, 53}]}, no};
unicode_table(12987) ->
    {0, [], {circle, [{0, 52}, {0, 54}]}, no};
unicode_table(12988) ->
    {0, [], {circle, [{0, 52}, {0, 55}]}, no};
unicode_table(12989) ->
    {0, [], {circle, [{0, 52}, {0, 56}]}, no};
unicode_table(12990) ->
    {0, [], {circle, [{0, 52}, {0, 57}]}, no};
unicode_table(12991) ->
    {0, [], {circle, [{0, 53}, {0, 48}]}, no};
unicode_table(12992) ->
    {0, [], {compat, [{0, 49}, {0, 26376}]}, so};
unicode_table(12993) ->
    {0, [], {compat, [{0, 50}, {0, 26376}]}, so};
unicode_table(12994) ->
    {0, [], {compat, [{0, 51}, {0, 26376}]}, so};
unicode_table(12995) ->
    {0, [], {compat, [{0, 52}, {0, 26376}]}, so};
unicode_table(12996) ->
    {0, [], {compat, [{0, 53}, {0, 26376}]}, so};
unicode_table(12997) ->
    {0, [], {compat, [{0, 54}, {0, 26376}]}, so};
unicode_table(12998) ->
    {0, [], {compat, [{0, 55}, {0, 26376}]}, so};
unicode_table(12999) ->
    {0, [], {compat, [{0, 56}, {0, 26376}]}, so};
unicode_table(13000) ->
    {0, [], {compat, [{0, 57}, {0, 26376}]}, so};
unicode_table(13001) ->
    {0, [], {compat, [{0, 49}, {0, 48}, {0, 26376}]}, so};
unicode_table(13002) ->
    {0, [], {compat, [{0, 49}, {0, 49}, {0, 26376}]}, so};
unicode_table(13003) ->
    {0, [], {compat, [{0, 49}, {0, 50}, {0, 26376}]}, so};
unicode_table(13004) ->
    {0, [], {square, [{0, 72}, {0, 103}]}, so};
unicode_table(13005) ->
    {0, [], {square, [{0, 101}, {0, 114}, {0, 103}]}, so};
unicode_table(13006) ->
    {0, [], {square, [{0, 101}, {0, 86}]}, so};
unicode_table(13007) ->
    {0, [], {square, [{0, 76}, {0, 84}, {0, 68}]}, so};
unicode_table(13008) ->
    {0, [], {circle, [{0, 12450}]}, so};
unicode_table(13009) ->
    {0, [], {circle, [{0, 12452}]}, so};
unicode_table(13010) ->
    {0, [], {circle, [{0, 12454}]}, so};
unicode_table(13011) ->
    {0, [], {circle, [{0, 12456}]}, so};
unicode_table(13012) ->
    {0, [], {circle, [{0, 12458}]}, so};
unicode_table(13013) ->
    {0, [], {circle, [{0, 12459}]}, so};
unicode_table(13014) ->
    {0, [], {circle, [{0, 12461}]}, so};
unicode_table(13015) ->
    {0, [], {circle, [{0, 12463}]}, so};
unicode_table(13016) ->
    {0, [], {circle, [{0, 12465}]}, so};
unicode_table(13017) ->
    {0, [], {circle, [{0, 12467}]}, so};
unicode_table(13018) ->
    {0, [], {circle, [{0, 12469}]}, so};
unicode_table(13019) ->
    {0, [], {circle, [{0, 12471}]}, so};
unicode_table(13020) ->
    {0, [], {circle, [{0, 12473}]}, so};
unicode_table(13021) ->
    {0, [], {circle, [{0, 12475}]}, so};
unicode_table(13022) ->
    {0, [], {circle, [{0, 12477}]}, so};
unicode_table(13023) ->
    {0, [], {circle, [{0, 12479}]}, so};
unicode_table(13024) ->
    {0, [], {circle, [{0, 12481}]}, so};
unicode_table(13025) ->
    {0, [], {circle, [{0, 12484}]}, so};
unicode_table(13026) ->
    {0, [], {circle, [{0, 12486}]}, so};
unicode_table(13027) ->
    {0, [], {circle, [{0, 12488}]}, so};
unicode_table(13028) ->
    {0, [], {circle, [{0, 12490}]}, so};
unicode_table(13029) ->
    {0, [], {circle, [{0, 12491}]}, so};
unicode_table(13030) ->
    {0, [], {circle, [{0, 12492}]}, so};
unicode_table(13031) ->
    {0, [], {circle, [{0, 12493}]}, so};
unicode_table(13032) ->
    {0, [], {circle, [{0, 12494}]}, so};
unicode_table(13033) ->
    {0, [], {circle, [{0, 12495}]}, so};
unicode_table(13034) ->
    {0, [], {circle, [{0, 12498}]}, so};
unicode_table(13035) ->
    {0, [], {circle, [{0, 12501}]}, so};
unicode_table(13036) ->
    {0, [], {circle, [{0, 12504}]}, so};
unicode_table(13037) ->
    {0, [], {circle, [{0, 12507}]}, so};
unicode_table(13038) ->
    {0, [], {circle, [{0, 12510}]}, so};
unicode_table(13039) ->
    {0, [], {circle, [{0, 12511}]}, so};
unicode_table(13040) ->
    {0, [], {circle, [{0, 12512}]}, so};
unicode_table(13041) ->
    {0, [], {circle, [{0, 12513}]}, so};
unicode_table(13042) ->
    {0, [], {circle, [{0, 12514}]}, so};
unicode_table(13043) ->
    {0, [], {circle, [{0, 12516}]}, so};
unicode_table(13044) ->
    {0, [], {circle, [{0, 12518}]}, so};
unicode_table(13045) ->
    {0, [], {circle, [{0, 12520}]}, so};
unicode_table(13046) ->
    {0, [], {circle, [{0, 12521}]}, so};
unicode_table(13047) ->
    {0, [], {circle, [{0, 12522}]}, so};
unicode_table(13048) ->
    {0, [], {circle, [{0, 12523}]}, so};
unicode_table(13049) ->
    {0, [], {circle, [{0, 12524}]}, so};
unicode_table(13050) ->
    {0, [], {circle, [{0, 12525}]}, so};
unicode_table(13051) ->
    {0, [], {circle, [{0, 12527}]}, so};
unicode_table(13052) ->
    {0, [], {circle, [{0, 12528}]}, so};
unicode_table(13053) ->
    {0, [], {circle, [{0, 12529}]}, so};
unicode_table(13054) ->
    {0, [], {circle, [{0, 12530}]}, so};
unicode_table(13055) ->
    {0, [], {square, [{0, 20196}, {0, 21644}]}, so};
unicode_table(13056) ->
    {0, [], {square, [{0, 12450}, {0, 12495}, {8, 12442}, {0, 12540}, {0, 12488}]}, so};
unicode_table(13057) ->
    {0, [], {square, [{0, 12450}, {0, 12523}, {0, 12501}, {0, 12449}]}, so};
unicode_table(13058) ->
    {0, [], {square, [{0, 12450}, {0, 12531}, {0, 12504}, {8, 12442}, {0, 12450}]}, so};
unicode_table(13059) ->
    {0, [], {square, [{0, 12450}, {0, 12540}, {0, 12523}]}, so};
unicode_table(13060) ->
    {0, [], {square, [{0, 12452}, {0, 12491}, {0, 12531}, {0, 12463}, {8, 12441}]}, so};
unicode_table(13061) ->
    {0, [], {square, [{0, 12452}, {0, 12531}, {0, 12481}]}, so};
unicode_table(13062) ->
    {0, [], {square, [{0, 12454}, {0, 12457}, {0, 12531}]}, so};
unicode_table(13063) ->
    {0, [], {square, [{0, 12456}, {0, 12473}, {0, 12463}, {0, 12540}, {0, 12488}, {8, 12441}]}, so};
unicode_table(13064) ->
    {0, [], {square, [{0, 12456}, {0, 12540}, {0, 12459}, {0, 12540}]}, so};
unicode_table(13065) ->
    {0, [], {square, [{0, 12458}, {0, 12531}, {0, 12473}]}, so};
unicode_table(13066) ->
    {0, [], {square, [{0, 12458}, {0, 12540}, {0, 12512}]}, so};
unicode_table(13067) ->
    {0, [], {square, [{0, 12459}, {0, 12452}, {0, 12522}]}, so};
unicode_table(13068) ->
    {0, [], {square, [{0, 12459}, {0, 12521}, {0, 12483}, {0, 12488}]}, so};
unicode_table(13069) ->
    {0, [], {square, [{0, 12459}, {0, 12525}, {0, 12522}, {0, 12540}]}, so};
unicode_table(13070) ->
    {0, [], {square, [{0, 12459}, {8, 12441}, {0, 12525}, {0, 12531}]}, so};
unicode_table(13071) ->
    {0, [], {square, [{0, 12459}, {8, 12441}, {0, 12531}, {0, 12510}]}, so};
unicode_table(13072) ->
    {0, [], {square, [{0, 12461}, {8, 12441}, {0, 12459}, {8, 12441}]}, so};
unicode_table(13073) ->
    {0, [], {square, [{0, 12461}, {8, 12441}, {0, 12491}, {0, 12540}]}, so};
unicode_table(13074) ->
    {0, [], {square, [{0, 12461}, {0, 12517}, {0, 12522}, {0, 12540}]}, so};
unicode_table(13075) ->
    {0, [], {square, [{0, 12461}, {8, 12441}, {0, 12523}, {0, 12479}, {8, 12441}, {0, 12540}]}, so};
unicode_table(13076) ->
    {0, [], {square, [{0, 12461}, {0, 12525}]}, so};
unicode_table(13077) ->
    {0, [], {square, [{0, 12461}, {0, 12525}, {0, 12463}, {8, 12441}, {0, 12521}, {0, 12512}]}, so};
unicode_table(13078) ->
    {0, [], {square, [{0, 12461}, {0, 12525}, {0, 12513}, {0, 12540}, {0, 12488}, {0, 12523}]}, so};
unicode_table(13079) ->
    {0, [], {square, [{0, 12461}, {0, 12525}, {0, 12527}, {0, 12483}, {0, 12488}]}, so};
unicode_table(13080) ->
    {0, [], {square, [{0, 12463}, {8, 12441}, {0, 12521}, {0, 12512}]}, so};
unicode_table(13081) ->
    {0, [], {square, [{0, 12463}, {8, 12441}, {0, 12521}, {0, 12512}, {0, 12488}, {0, 12531}]}, so};
unicode_table(13082) ->
    {0, [], {square, [{0, 12463}, {0, 12523}, {0, 12475}, {8, 12441}, {0, 12452}, {0, 12525}]}, so};
unicode_table(13083) ->
    {0, [], {square, [{0, 12463}, {0, 12525}, {0, 12540}, {0, 12493}]}, so};
unicode_table(13084) ->
    {0, [], {square, [{0, 12465}, {0, 12540}, {0, 12473}]}, so};
unicode_table(13085) ->
    {0, [], {square, [{0, 12467}, {0, 12523}, {0, 12490}]}, so};
unicode_table(13086) ->
    {0, [], {square, [{0, 12467}, {0, 12540}, {0, 12507}, {8, 12442}]}, so};
unicode_table(13087) ->
    {0, [], {square, [{0, 12469}, {0, 12452}, {0, 12463}, {0, 12523}]}, so};
unicode_table(13088) ->
    {0, [], {square, [{0, 12469}, {0, 12531}, {0, 12481}, {0, 12540}, {0, 12512}]}, so};
unicode_table(13089) ->
    {0, [], {square, [{0, 12471}, {0, 12522}, {0, 12531}, {0, 12463}, {8, 12441}]}, so};
unicode_table(13090) ->
    {0, [], {square, [{0, 12475}, {0, 12531}, {0, 12481}]}, so};
unicode_table(13091) ->
    {0, [], {square, [{0, 12475}, {0, 12531}, {0, 12488}]}, so};
unicode_table(13092) ->
    {0, [], {square, [{0, 12479}, {8, 12441}, {0, 12540}, {0, 12473}]}, so};
unicode_table(13093) ->
    {0, [], {square, [{0, 12486}, {8, 12441}, {0, 12471}]}, so};
unicode_table(13094) ->
    {0, [], {square, [{0, 12488}, {8, 12441}, {0, 12523}]}, so};
unicode_table(13095) ->
    {0, [], {square, [{0, 12488}, {0, 12531}]}, so};
unicode_table(13096) ->
    {0, [], {square, [{0, 12490}, {0, 12494}]}, so};
unicode_table(13097) ->
    {0, [], {square, [{0, 12494}, {0, 12483}, {0, 12488}]}, so};
unicode_table(13098) ->
    {0, [], {square, [{0, 12495}, {0, 12452}, {0, 12484}]}, so};
unicode_table(13099) ->
    {0, [], {square, [{0, 12495}, {8, 12442}, {0, 12540}, {0, 12475}, {0, 12531}, {0, 12488}]}, so};
unicode_table(13100) ->
    {0, [], {square, [{0, 12495}, {8, 12442}, {0, 12540}, {0, 12484}]}, so};
unicode_table(13101) ->
    {0, [], {square, [{0, 12495}, {8, 12441}, {0, 12540}, {0, 12524}, {0, 12523}]}, so};
unicode_table(13102) ->
    {0, [], {square, [{0, 12498}, {8, 12442}, {0, 12450}, {0, 12473}, {0, 12488}, {0, 12523}]}, so};
unicode_table(13103) ->
    {0, [], {square, [{0, 12498}, {8, 12442}, {0, 12463}, {0, 12523}]}, so};
unicode_table(13104) ->
    {0, [], {square, [{0, 12498}, {8, 12442}, {0, 12467}]}, so};
unicode_table(13105) ->
    {0, [], {square, [{0, 12498}, {8, 12441}, {0, 12523}]}, so};
unicode_table(13106) ->
    {0, [], {square, [{0, 12501}, {0, 12449}, {0, 12521}, {0, 12483}, {0, 12488}, {8, 12441}]}, so};
unicode_table(13107) ->
    {0, [], {square, [{0, 12501}, {0, 12451}, {0, 12540}, {0, 12488}]}, so};
unicode_table(13108) ->
    {0, [], {square, [{0, 12501}, {8, 12441}, {0, 12483}, {0, 12471}, {0, 12455}, {0, 12523}]}, so};
unicode_table(13109) ->
    {0, [], {square, [{0, 12501}, {0, 12521}, {0, 12531}]}, so};
unicode_table(13110) ->
    {0, [], {square, [{0, 12504}, {0, 12463}, {0, 12479}, {0, 12540}, {0, 12523}]}, so};
unicode_table(13111) ->
    {0, [], {square, [{0, 12504}, {8, 12442}, {0, 12477}]}, so};
unicode_table(13112) ->
    {0, [], {square, [{0, 12504}, {8, 12442}, {0, 12491}, {0, 12498}]}, so};
unicode_table(13113) ->
    {0, [], {square, [{0, 12504}, {0, 12523}, {0, 12484}]}, so};
unicode_table(13114) ->
    {0, [], {square, [{0, 12504}, {8, 12442}, {0, 12531}, {0, 12473}]}, so};
unicode_table(13115) ->
    {0, [], {square, [{0, 12504}, {8, 12442}, {0, 12540}, {0, 12471}, {8, 12441}]}, so};
unicode_table(13116) ->
    {0, [], {square, [{0, 12504}, {8, 12441}, {0, 12540}, {0, 12479}]}, so};
unicode_table(13117) ->
    {0, [], {square, [{0, 12507}, {8, 12442}, {0, 12452}, {0, 12531}, {0, 12488}]}, so};
unicode_table(13118) ->
    {0, [], {square, [{0, 12507}, {8, 12441}, {0, 12523}, {0, 12488}]}, so};
unicode_table(13119) ->
    {0, [], {square, [{0, 12507}, {0, 12531}]}, so};
unicode_table(13120) ->
    {0, [], {square, [{0, 12507}, {8, 12442}, {0, 12531}, {0, 12488}, {8, 12441}]}, so};
unicode_table(13121) ->
    {0, [], {square, [{0, 12507}, {0, 12540}, {0, 12523}]}, so};
unicode_table(13122) ->
    {0, [], {square, [{0, 12507}, {0, 12540}, {0, 12531}]}, so};
unicode_table(13123) ->
    {0, [], {square, [{0, 12510}, {0, 12452}, {0, 12463}, {0, 12525}]}, so};
unicode_table(13124) ->
    {0, [], {square, [{0, 12510}, {0, 12452}, {0, 12523}]}, so};
unicode_table(13125) ->
    {0, [], {square, [{0, 12510}, {0, 12483}, {0, 12495}]}, so};
unicode_table(13126) ->
    {0, [], {square, [{0, 12510}, {0, 12523}, {0, 12463}]}, so};
unicode_table(13127) ->
    {0, [], {square, [{0, 12510}, {0, 12531}, {0, 12471}, {0, 12519}, {0, 12531}]}, so};
unicode_table(13128) ->
    {0, [], {square, [{0, 12511}, {0, 12463}, {0, 12525}, {0, 12531}]}, so};
unicode_table(13129) ->
    {0, [], {square, [{0, 12511}, {0, 12522}]}, so};
unicode_table(13130) ->
    {0, [], {square, [{0, 12511}, {0, 12522}, {0, 12495}, {8, 12441}, {0, 12540}, {0, 12523}]}, so};
unicode_table(13131) ->
    {0, [], {square, [{0, 12513}, {0, 12459}, {8, 12441}]}, so};
unicode_table(13132) ->
    {0, [], {square, [{0, 12513}, {0, 12459}, {8, 12441}, {0, 12488}, {0, 12531}]}, so};
unicode_table(13133) ->
    {0, [], {square, [{0, 12513}, {0, 12540}, {0, 12488}, {0, 12523}]}, so};
unicode_table(13134) ->
    {0, [], {square, [{0, 12516}, {0, 12540}, {0, 12488}, {8, 12441}]}, so};
unicode_table(13135) ->
    {0, [], {square, [{0, 12516}, {0, 12540}, {0, 12523}]}, so};
unicode_table(13136) ->
    {0, [], {square, [{0, 12518}, {0, 12450}, {0, 12531}]}, so};
unicode_table(13137) ->
    {0, [], {square, [{0, 12522}, {0, 12483}, {0, 12488}, {0, 12523}]}, so};
unicode_table(13138) ->
    {0, [], {square, [{0, 12522}, {0, 12521}]}, so};
unicode_table(13139) ->
    {0, [], {square, [{0, 12523}, {0, 12498}, {8, 12442}, {0, 12540}]}, so};
unicode_table(13140) ->
    {0, [], {square, [{0, 12523}, {0, 12540}, {0, 12501}, {8, 12441}, {0, 12523}]}, so};
unicode_table(13141) ->
    {0, [], {square, [{0, 12524}, {0, 12512}]}, so};
unicode_table(13142) ->
    {0, [], {square, [{0, 12524}, {0, 12531}, {0, 12488}, {0, 12465}, {8, 12441}, {0, 12531}]}, so};
unicode_table(13143) ->
    {0, [], {square, [{0, 12527}, {0, 12483}, {0, 12488}]}, so};
unicode_table(13144) ->
    {0, [], {compat, [{0, 48}, {0, 28857}]}, so};
unicode_table(13145) ->
    {0, [], {compat, [{0, 49}, {0, 28857}]}, so};
unicode_table(13146) ->
    {0, [], {compat, [{0, 50}, {0, 28857}]}, so};
unicode_table(13147) ->
    {0, [], {compat, [{0, 51}, {0, 28857}]}, so};
unicode_table(13148) ->
    {0, [], {compat, [{0, 52}, {0, 28857}]}, so};
unicode_table(13149) ->
    {0, [], {compat, [{0, 53}, {0, 28857}]}, so};
unicode_table(13150) ->
    {0, [], {compat, [{0, 54}, {0, 28857}]}, so};
unicode_table(13151) ->
    {0, [], {compat, [{0, 55}, {0, 28857}]}, so};
unicode_table(13152) ->
    {0, [], {compat, [{0, 56}, {0, 28857}]}, so};
unicode_table(13153) ->
    {0, [], {compat, [{0, 57}, {0, 28857}]}, so};
unicode_table(13154) ->
    {0, [], {compat, [{0, 49}, {0, 48}, {0, 28857}]}, so};
unicode_table(13155) ->
    {0, [], {compat, [{0, 49}, {0, 49}, {0, 28857}]}, so};
unicode_table(13156) ->
    {0, [], {compat, [{0, 49}, {0, 50}, {0, 28857}]}, so};
unicode_table(13157) ->
    {0, [], {compat, [{0, 49}, {0, 51}, {0, 28857}]}, so};
unicode_table(13158) ->
    {0, [], {compat, [{0, 49}, {0, 52}, {0, 28857}]}, so};
unicode_table(13159) ->
    {0, [], {compat, [{0, 49}, {0, 53}, {0, 28857}]}, so};
unicode_table(13160) ->
    {0, [], {compat, [{0, 49}, {0, 54}, {0, 28857}]}, so};
unicode_table(13161) ->
    {0, [], {compat, [{0, 49}, {0, 55}, {0, 28857}]}, so};
unicode_table(13162) ->
    {0, [], {compat, [{0, 49}, {0, 56}, {0, 28857}]}, so};
unicode_table(13163) ->
    {0, [], {compat, [{0, 49}, {0, 57}, {0, 28857}]}, so};
unicode_table(13164) ->
    {0, [], {compat, [{0, 50}, {0, 48}, {0, 28857}]}, so};
unicode_table(13165) ->
    {0, [], {compat, [{0, 50}, {0, 49}, {0, 28857}]}, so};
unicode_table(13166) ->
    {0, [], {compat, [{0, 50}, {0, 50}, {0, 28857}]}, so};
unicode_table(13167) ->
    {0, [], {compat, [{0, 50}, {0, 51}, {0, 28857}]}, so};
unicode_table(13168) ->
    {0, [], {compat, [{0, 50}, {0, 52}, {0, 28857}]}, so};
unicode_table(13169) ->
    {0, [], {square, [{0, 104}, {0, 80}, {0, 97}]}, so};
unicode_table(13170) ->
    {0, [], {square, [{0, 100}, {0, 97}]}, so};
unicode_table(13171) ->
    {0, [], {square, [{0, 65}, {0, 85}]}, so};
unicode_table(13172) ->
    {0, [], {square, [{0, 98}, {0, 97}, {0, 114}]}, so};
unicode_table(13173) ->
    {0, [], {square, [{0, 111}, {0, 86}]}, so};
unicode_table(13174) ->
    {0, [], {square, [{0, 112}, {0, 99}]}, so};
unicode_table(13175) ->
    {0, [], {square, [{0, 100}, {0, 109}]}, so};
unicode_table(13176) ->
    {0, [], {square, [{0, 100}, {0, 109}, {0, 50}]}, so};
unicode_table(13177) ->
    {0, [], {square, [{0, 100}, {0, 109}, {0, 51}]}, so};
unicode_table(13178) ->
    {0, [], {square, [{0, 73}, {0, 85}]}, so};
unicode_table(13179) ->
    {0, [], {square, [{0, 24179}, {0, 25104}]}, so};
unicode_table(13180) ->
    {0, [], {square, [{0, 26157}, {0, 21644}]}, so};
unicode_table(13181) ->
    {0, [], {square, [{0, 22823}, {0, 27491}]}, so};
unicode_table(13182) ->
    {0, [], {square, [{0, 26126}, {0, 27835}]}, so};
unicode_table(13183) ->
    {0, [], {square, [{0, 26666}, {0, 24335}, {0, 20250}, {0, 31038}]}, so};
unicode_table(13184) ->
    {0, [], {square, [{0, 112}, {0, 65}]}, so};
unicode_table(13185) ->
    {0, [], {square, [{0, 110}, {0, 65}]}, so};
unicode_table(13186) ->
    {0, [], {square, [{0, 956}, {0, 65}]}, so};
unicode_table(13187) ->
    {0, [], {square, [{0, 109}, {0, 65}]}, so};
unicode_table(13188) ->
    {0, [], {square, [{0, 107}, {0, 65}]}, so};
unicode_table(13189) ->
    {0, [], {square, [{0, 75}, {0, 66}]}, so};
unicode_table(13190) ->
    {0, [], {square, [{0, 77}, {0, 66}]}, so};
unicode_table(13191) ->
    {0, [], {square, [{0, 71}, {0, 66}]}, so};
unicode_table(13192) ->
    {0, [], {square, [{0, 99}, {0, 97}, {0, 108}]}, so};
unicode_table(13193) ->
    {0, [], {square, [{0, 107}, {0, 99}, {0, 97}, {0, 108}]}, so};
unicode_table(13194) ->
    {0, [], {square, [{0, 112}, {0, 70}]}, so};
unicode_table(13195) ->
    {0, [], {square, [{0, 110}, {0, 70}]}, so};
unicode_table(13196) ->
    {0, [], {square, [{0, 956}, {0, 70}]}, so};
unicode_table(13197) ->
    {0, [], {square, [{0, 956}, {0, 103}]}, so};
unicode_table(13198) ->
    {0, [], {square, [{0, 109}, {0, 103}]}, so};
unicode_table(13199) ->
    {0, [], {square, [{0, 107}, {0, 103}]}, so};
unicode_table(13200) ->
    {0, [], {square, [{0, 72}, {0, 122}]}, so};
unicode_table(13201) ->
    {0, [], {square, [{0, 107}, {0, 72}, {0, 122}]}, so};
unicode_table(13202) ->
    {0, [], {square, [{0, 77}, {0, 72}, {0, 122}]}, so};
unicode_table(13203) ->
    {0, [], {square, [{0, 71}, {0, 72}, {0, 122}]}, so};
unicode_table(13204) ->
    {0, [], {square, [{0, 84}, {0, 72}, {0, 122}]}, so};
unicode_table(13205) ->
    {0, [], {square, [{0, 956}, {0, 108}]}, so};
unicode_table(13206) ->
    {0, [], {square, [{0, 109}, {0, 108}]}, so};
unicode_table(13207) ->
    {0, [], {square, [{0, 100}, {0, 108}]}, so};
unicode_table(13208) ->
    {0, [], {square, [{0, 107}, {0, 108}]}, so};
unicode_table(13209) ->
    {0, [], {square, [{0, 102}, {0, 109}]}, so};
unicode_table(13210) ->
    {0, [], {square, [{0, 110}, {0, 109}]}, so};
unicode_table(13211) ->
    {0, [], {square, [{0, 956}, {0, 109}]}, so};
unicode_table(13212) ->
    {0, [], {square, [{0, 109}, {0, 109}]}, so};
unicode_table(13213) ->
    {0, [], {square, [{0, 99}, {0, 109}]}, so};
unicode_table(13214) ->
    {0, [], {square, [{0, 107}, {0, 109}]}, so};
unicode_table(13215) ->
    {0, [], {square, [{0, 109}, {0, 109}, {0, 50}]}, so};
unicode_table(13216) ->
    {0, [], {square, [{0, 99}, {0, 109}, {0, 50}]}, so};
unicode_table(13217) ->
    {0, [], {square, [{0, 109}, {0, 50}]}, so};
unicode_table(13218) ->
    {0, [], {square, [{0, 107}, {0, 109}, {0, 50}]}, so};
unicode_table(13219) ->
    {0, [], {square, [{0, 109}, {0, 109}, {0, 51}]}, so};
unicode_table(13220) ->
    {0, [], {square, [{0, 99}, {0, 109}, {0, 51}]}, so};
unicode_table(13221) ->
    {0, [], {square, [{0, 109}, {0, 51}]}, so};
unicode_table(13222) ->
    {0, [], {square, [{0, 107}, {0, 109}, {0, 51}]}, so};
unicode_table(13223) ->
    {0, [], {square, [{0, 109}, {0, 8725}, {0, 115}]}, so};
unicode_table(13224) ->
    {0, [], {square, [{0, 109}, {0, 8725}, {0, 115}, {0, 50}]}, so};
unicode_table(13225) ->
    {0, [], {square, [{0, 80}, {0, 97}]}, so};
unicode_table(13226) ->
    {0, [], {square, [{0, 107}, {0, 80}, {0, 97}]}, so};
unicode_table(13227) ->
    {0, [], {square, [{0, 77}, {0, 80}, {0, 97}]}, so};
unicode_table(13228) ->
    {0, [], {square, [{0, 71}, {0, 80}, {0, 97}]}, so};
unicode_table(13229) ->
    {0, [], {square, [{0, 114}, {0, 97}, {0, 100}]}, so};
unicode_table(13230) ->
    {0, [], {square, [{0, 114}, {0, 97}, {0, 100}, {0, 8725}, {0, 115}]}, so};
unicode_table(13231) ->
    {0, [], {square, [{0, 114}, {0, 97}, {0, 100}, {0, 8725}, {0, 115}, {0, 50}]}, so};
unicode_table(13232) ->
    {0, [], {square, [{0, 112}, {0, 115}]}, so};
unicode_table(13233) ->
    {0, [], {square, [{0, 110}, {0, 115}]}, so};
unicode_table(13234) ->
    {0, [], {square, [{0, 956}, {0, 115}]}, so};
unicode_table(13235) ->
    {0, [], {square, [{0, 109}, {0, 115}]}, so};
unicode_table(13236) ->
    {0, [], {square, [{0, 112}, {0, 86}]}, so};
unicode_table(13237) ->
    {0, [], {square, [{0, 110}, {0, 86}]}, so};
unicode_table(13238) ->
    {0, [], {square, [{0, 956}, {0, 86}]}, so};
unicode_table(13239) ->
    {0, [], {square, [{0, 109}, {0, 86}]}, so};
unicode_table(13240) ->
    {0, [], {square, [{0, 107}, {0, 86}]}, so};
unicode_table(13241) ->
    {0, [], {square, [{0, 77}, {0, 86}]}, so};
unicode_table(13242) ->
    {0, [], {square, [{0, 112}, {0, 87}]}, so};
unicode_table(13243) ->
    {0, [], {square, [{0, 110}, {0, 87}]}, so};
unicode_table(13244) ->
    {0, [], {square, [{0, 956}, {0, 87}]}, so};
unicode_table(13245) ->
    {0, [], {square, [{0, 109}, {0, 87}]}, so};
unicode_table(13246) ->
    {0, [], {square, [{0, 107}, {0, 87}]}, so};
unicode_table(13247) ->
    {0, [], {square, [{0, 77}, {0, 87}]}, so};
unicode_table(13248) ->
    {0, [], {square, [{0, 107}, {0, 937}]}, so};
unicode_table(13249) ->
    {0, [], {square, [{0, 77}, {0, 937}]}, so};
unicode_table(13250) ->
    {0, [], {square, [{0, 97}, {0, 46}, {0, 109}, {0, 46}]}, so};
unicode_table(13251) ->
    {0, [], {square, [{0, 66}, {0, 113}]}, so};
unicode_table(13252) ->
    {0, [], {square, [{0, 99}, {0, 99}]}, so};
unicode_table(13253) ->
    {0, [], {square, [{0, 99}, {0, 100}]}, so};
unicode_table(13254) ->
    {0, [], {square, [{0, 67}, {0, 8725}, {0, 107}, {0, 103}]}, so};
unicode_table(13255) ->
    {0, [], {square, [{0, 67}, {0, 111}, {0, 46}]}, so};
unicode_table(13256) ->
    {0, [], {square, [{0, 100}, {0, 66}]}, so};
unicode_table(13257) ->
    {0, [], {square, [{0, 71}, {0, 121}]}, so};
unicode_table(13258) ->
    {0, [], {square, [{0, 104}, {0, 97}]}, so};
unicode_table(13259) ->
    {0, [], {square, [{0, 72}, {0, 80}]}, so};
unicode_table(13260) ->
    {0, [], {square, [{0, 105}, {0, 110}]}, so};
unicode_table(13261) ->
    {0, [], {square, [{0, 75}, {0, 75}]}, so};
unicode_table(13262) ->
    {0, [], {square, [{0, 75}, {0, 77}]}, so};
unicode_table(13263) ->
    {0, [], {square, [{0, 107}, {0, 116}]}, so};
unicode_table(13264) ->
    {0, [], {square, [{0, 108}, {0, 109}]}, so};
unicode_table(13265) ->
    {0, [], {square, [{0, 108}, {0, 110}]}, so};
unicode_table(13266) ->
    {0, [], {square, [{0, 108}, {0, 111}, {0, 103}]}, so};
unicode_table(13267) ->
    {0, [], {square, [{0, 108}, {0, 120}]}, so};
unicode_table(13268) ->
    {0, [], {square, [{0, 109}, {0, 98}]}, so};
unicode_table(13269) ->
    {0, [], {square, [{0, 109}, {0, 105}, {0, 108}]}, so};
unicode_table(13270) ->
    {0, [], {square, [{0, 109}, {0, 111}, {0, 108}]}, so};
unicode_table(13271) ->
    {0, [], {square, [{0, 80}, {0, 72}]}, so};
unicode_table(13272) ->
    {0, [], {square, [{0, 112}, {0, 46}, {0, 109}, {0, 46}]}, so};
unicode_table(13273) ->
    {0, [], {square, [{0, 80}, {0, 80}, {0, 77}]}, so};
unicode_table(13274) ->
    {0, [], {square, [{0, 80}, {0, 82}]}, so};
unicode_table(13275) ->
    {0, [], {square, [{0, 115}, {0, 114}]}, so};
unicode_table(13276) ->
    {0, [], {square, [{0, 83}, {0, 118}]}, so};
unicode_table(13277) ->
    {0, [], {square, [{0, 87}, {0, 98}]}, so};
unicode_table(13278) ->
    {0, [], {square, [{0, 86}, {0, 8725}, {0, 109}]}, so};
unicode_table(13279) ->
    {0, [], {square, [{0, 65}, {0, 8725}, {0, 109}]}, so};
unicode_table(13280) ->
    {0, [], {compat, [{0, 49}, {0, 26085}]}, so};
unicode_table(13281) ->
    {0, [], {compat, [{0, 50}, {0, 26085}]}, so};
unicode_table(13282) ->
    {0, [], {compat, [{0, 51}, {0, 26085}]}, so};
unicode_table(13283) ->
    {0, [], {compat, [{0, 52}, {0, 26085}]}, so};
unicode_table(13284) ->
    {0, [], {compat, [{0, 53}, {0, 26085}]}, so};
unicode_table(13285) ->
    {0, [], {compat, [{0, 54}, {0, 26085}]}, so};
unicode_table(13286) ->
    {0, [], {compat, [{0, 55}, {0, 26085}]}, so};
unicode_table(13287) ->
    {0, [], {compat, [{0, 56}, {0, 26085}]}, so};
unicode_table(13288) ->
    {0, [], {compat, [{0, 57}, {0, 26085}]}, so};
unicode_table(13289) ->
    {0, [], {compat, [{0, 49}, {0, 48}, {0, 26085}]}, so};
unicode_table(13290) ->
    {0, [], {compat, [{0, 49}, {0, 49}, {0, 26085}]}, so};
unicode_table(13291) ->
    {0, [], {compat, [{0, 49}, {0, 50}, {0, 26085}]}, so};
unicode_table(13292) ->
    {0, [], {compat, [{0, 49}, {0, 51}, {0, 26085}]}, so};
unicode_table(13293) ->
    {0, [], {compat, [{0, 49}, {0, 52}, {0, 26085}]}, so};
unicode_table(13294) ->
    {0, [], {compat, [{0, 49}, {0, 53}, {0, 26085}]}, so};
unicode_table(13295) ->
    {0, [], {compat, [{0, 49}, {0, 54}, {0, 26085}]}, so};
unicode_table(13296) ->
    {0, [], {compat, [{0, 49}, {0, 55}, {0, 26085}]}, so};
unicode_table(13297) ->
    {0, [], {compat, [{0, 49}, {0, 56}, {0, 26085}]}, so};
unicode_table(13298) ->
    {0, [], {compat, [{0, 49}, {0, 57}, {0, 26085}]}, so};
unicode_table(13299) ->
    {0, [], {compat, [{0, 50}, {0, 48}, {0, 26085}]}, so};
unicode_table(13300) ->
    {0, [], {compat, [{0, 50}, {0, 49}, {0, 26085}]}, so};
unicode_table(13301) ->
    {0, [], {compat, [{0, 50}, {0, 50}, {0, 26085}]}, so};
unicode_table(13302) ->
    {0, [], {compat, [{0, 50}, {0, 51}, {0, 26085}]}, so};
unicode_table(13303) ->
    {0, [], {compat, [{0, 50}, {0, 52}, {0, 26085}]}, so};
unicode_table(13304) ->
    {0, [], {compat, [{0, 50}, {0, 53}, {0, 26085}]}, so};
unicode_table(13305) ->
    {0, [], {compat, [{0, 50}, {0, 54}, {0, 26085}]}, so};
unicode_table(13306) ->
    {0, [], {compat, [{0, 50}, {0, 55}, {0, 26085}]}, so};
unicode_table(13307) ->
    {0, [], {compat, [{0, 50}, {0, 56}, {0, 26085}]}, so};
unicode_table(13308) ->
    {0, [], {compat, [{0, 50}, {0, 57}, {0, 26085}]}, so};
unicode_table(13309) ->
    {0, [], {compat, [{0, 51}, {0, 48}, {0, 26085}]}, so};
unicode_table(13310) ->
    {0, [], {compat, [{0, 51}, {0, 49}, {0, 26085}]}, so};
unicode_table(13311) ->
    {0, [], {square, [{0, 103}, {0, 97}, {0, 108}]}, so};
unicode_table(42607) ->
    {230, [], [], mn};
unicode_table(42612) ->
    {230, [], [], mn};
unicode_table(42613) ->
    {230, [], [], mn};
unicode_table(42614) ->
    {230, [], [], mn};
unicode_table(42615) ->
    {230, [], [], mn};
unicode_table(42616) ->
    {230, [], [], mn};
unicode_table(42617) ->
    {230, [], [], mn};
unicode_table(42618) ->
    {230, [], [], mn};
unicode_table(42619) ->
    {230, [], [], mn};
unicode_table(42620) ->
    {230, [], [], mn};
unicode_table(42621) ->
    {230, [], [], mn};
unicode_table(42652) ->
    {0, [], {super, [{0, 1098}]}, lm};
unicode_table(42653) ->
    {0, [], {super, [{0, 1100}]}, lm};
unicode_table(42654) ->
    {230, [], [], mn};
unicode_table(42655) ->
    {230, [], [], mn};
unicode_table(42736) ->
    {230, [], [], mn};
unicode_table(42737) ->
    {230, [], [], mn};
unicode_table(42864) ->
    {0, [], {super, [{0, 42863}]}, lm};
unicode_table(42994) ->
    {0, [], {super, [{0, 67}]}, lm};
unicode_table(42995) ->
    {0, [], {super, [{0, 70}]}, lm};
unicode_table(42996) ->
    {0, [], {super, [{0, 81}]}, lm};
unicode_table(43000) ->
    {0, [], {super, [{0, 294}]}, lm};
unicode_table(43001) ->
    {0, [], {super, [{0, 339}]}, lm};
unicode_table(43014) ->
    {9, [], [], mn};
unicode_table(43052) ->
    {9, [], [], mn};
unicode_table(43204) ->
    {9, [], [], mn};
unicode_table(43232) ->
    {230, [], [], mn};
unicode_table(43233) ->
    {230, [], [], mn};
unicode_table(43234) ->
    {230, [], [], mn};
unicode_table(43235) ->
    {230, [], [], mn};
unicode_table(43236) ->
    {230, [], [], mn};
unicode_table(43237) ->
    {230, [], [], mn};
unicode_table(43238) ->
    {230, [], [], mn};
unicode_table(43239) ->
    {230, [], [], mn};
unicode_table(43240) ->
    {230, [], [], mn};
unicode_table(43241) ->
    {230, [], [], mn};
unicode_table(43242) ->
    {230, [], [], mn};
unicode_table(43243) ->
    {230, [], [], mn};
unicode_table(43244) ->
    {230, [], [], mn};
unicode_table(43245) ->
    {230, [], [], mn};
unicode_table(43246) ->
    {230, [], [], mn};
unicode_table(43247) ->
    {230, [], [], mn};
unicode_table(43248) ->
    {230, [], [], mn};
unicode_table(43249) ->
    {230, [], [], mn};
unicode_table(43307) ->
    {220, [], [], mn};
unicode_table(43308) ->
    {220, [], [], mn};
unicode_table(43309) ->
    {220, [], [], mn};
unicode_table(43347) ->
    {9, [], [], mc};
unicode_table(43443) ->
    {7, [], [], mn};
unicode_table(43456) ->
    {9, [], [], mc};
unicode_table(43696) ->
    {230, [], [], mn};
unicode_table(43698) ->
    {230, [], [], mn};
unicode_table(43699) ->
    {230, [], [], mn};
unicode_table(43700) ->
    {220, [], [], mn};
unicode_table(43703) ->
    {230, [], [], mn};
unicode_table(43704) ->
    {230, [], [], mn};
unicode_table(43710) ->
    {230, [], [], mn};
unicode_table(43711) ->
    {230, [], [], mn};
unicode_table(43713) ->
    {230, [], [], mn};
unicode_table(43766) ->
    {9, [], [], mn};
unicode_table(43868) ->
    {0, [], {super, [{0, 42791}]}, lm};
unicode_table(43869) ->
    {0, [], {super, [{0, 43831}]}, lm};
unicode_table(43870) ->
    {0, [], {super, [{0, 619}]}, lm};
unicode_table(43871) ->
    {0, [], {super, [{0, 43858}]}, lm};
unicode_table(43881) ->
    {0, [], {super, [{0, 653}]}, lm};
unicode_table(44013) ->
    {9, [], [], mn};
unicode_table(63744) ->
    {0, [{0, 35912}], [], lo};
unicode_table(63745) ->
    {0, [{0, 26356}], [], lo};
unicode_table(63746) ->
    {0, [{0, 36554}], [], lo};
unicode_table(63747) ->
    {0, [{0, 36040}], [], lo};
unicode_table(63748) ->
    {0, [{0, 28369}], [], lo};
unicode_table(63749) ->
    {0, [{0, 20018}], [], lo};
unicode_table(63750) ->
    {0, [{0, 21477}], [], lo};
unicode_table(63751) ->
    {0, [{0, 40860}], [], lo};
unicode_table(63752) ->
    {0, [{0, 40860}], [], lo};
unicode_table(63753) ->
    {0, [{0, 22865}], [], lo};
unicode_table(63754) ->
    {0, [{0, 37329}], [], lo};
unicode_table(63755) ->
    {0, [{0, 21895}], [], lo};
unicode_table(63756) ->
    {0, [{0, 22856}], [], lo};
unicode_table(63757) ->
    {0, [{0, 25078}], [], lo};
unicode_table(63758) ->
    {0, [{0, 30313}], [], lo};
unicode_table(63759) ->
    {0, [{0, 32645}], [], lo};
unicode_table(63760) ->
    {0, [{0, 34367}], [], lo};
unicode_table(63761) ->
    {0, [{0, 34746}], [], lo};
unicode_table(63762) ->
    {0, [{0, 35064}], [], lo};
unicode_table(63763) ->
    {0, [{0, 37007}], [], lo};
unicode_table(63764) ->
    {0, [{0, 27138}], [], lo};
unicode_table(63765) ->
    {0, [{0, 27931}], [], lo};
unicode_table(63766) ->
    {0, [{0, 28889}], [], lo};
unicode_table(63767) ->
    {0, [{0, 29662}], [], lo};
unicode_table(63768) ->
    {0, [{0, 33853}], [], lo};
unicode_table(63769) ->
    {0, [{0, 37226}], [], lo};
unicode_table(63770) ->
    {0, [{0, 39409}], [], lo};
unicode_table(63771) ->
    {0, [{0, 20098}], [], lo};
unicode_table(63772) ->
    {0, [{0, 21365}], [], lo};
unicode_table(63773) ->
    {0, [{0, 27396}], [], lo};
unicode_table(63774) ->
    {0, [{0, 29211}], [], lo};
unicode_table(63775) ->
    {0, [{0, 34349}], [], lo};
unicode_table(63776) ->
    {0, [{0, 40478}], [], lo};
unicode_table(63777) ->
    {0, [{0, 23888}], [], lo};
unicode_table(63778) ->
    {0, [{0, 28651}], [], lo};
unicode_table(63779) ->
    {0, [{0, 34253}], [], lo};
unicode_table(63780) ->
    {0, [{0, 35172}], [], lo};
unicode_table(63781) ->
    {0, [{0, 25289}], [], lo};
unicode_table(63782) ->
    {0, [{0, 33240}], [], lo};
unicode_table(63783) ->
    {0, [{0, 34847}], [], lo};
unicode_table(63784) ->
    {0, [{0, 24266}], [], lo};
unicode_table(63785) ->
    {0, [{0, 26391}], [], lo};
unicode_table(63786) ->
    {0, [{0, 28010}], [], lo};
unicode_table(63787) ->
    {0, [{0, 29436}], [], lo};
unicode_table(63788) ->
    {0, [{0, 37070}], [], lo};
unicode_table(63789) ->
    {0, [{0, 20358}], [], lo};
unicode_table(63790) ->
    {0, [{0, 20919}], [], lo};
unicode_table(63791) ->
    {0, [{0, 21214}], [], lo};
unicode_table(63792) ->
    {0, [{0, 25796}], [], lo};
unicode_table(63793) ->
    {0, [{0, 27347}], [], lo};
unicode_table(63794) ->
    {0, [{0, 29200}], [], lo};
unicode_table(63795) ->
    {0, [{0, 30439}], [], lo};
unicode_table(63796) ->
    {0, [{0, 32769}], [], lo};
unicode_table(63797) ->
    {0, [{0, 34310}], [], lo};
unicode_table(63798) ->
    {0, [{0, 34396}], [], lo};
unicode_table(63799) ->
    {0, [{0, 36335}], [], lo};
unicode_table(63800) ->
    {0, [{0, 38706}], [], lo};
unicode_table(63801) ->
    {0, [{0, 39791}], [], lo};
unicode_table(63802) ->
    {0, [{0, 40442}], [], lo};
unicode_table(63803) ->
    {0, [{0, 30860}], [], lo};
unicode_table(63804) ->
    {0, [{0, 31103}], [], lo};
unicode_table(63805) ->
    {0, [{0, 32160}], [], lo};
unicode_table(63806) ->
    {0, [{0, 33737}], [], lo};
unicode_table(63807) ->
    {0, [{0, 37636}], [], lo};
unicode_table(63808) ->
    {0, [{0, 40575}], [], lo};
unicode_table(63809) ->
    {0, [{0, 35542}], [], lo};
unicode_table(63810) ->
    {0, [{0, 22751}], [], lo};
unicode_table(63811) ->
    {0, [{0, 24324}], [], lo};
unicode_table(63812) ->
    {0, [{0, 31840}], [], lo};
unicode_table(63813) ->
    {0, [{0, 32894}], [], lo};
unicode_table(63814) ->
    {0, [{0, 29282}], [], lo};
unicode_table(63815) ->
    {0, [{0, 30922}], [], lo};
unicode_table(63816) ->
    {0, [{0, 36034}], [], lo};
unicode_table(63817) ->
    {0, [{0, 38647}], [], lo};
unicode_table(63818) ->
    {0, [{0, 22744}], [], lo};
unicode_table(63819) ->
    {0, [{0, 23650}], [], lo};
unicode_table(63820) ->
    {0, [{0, 27155}], [], lo};
unicode_table(63821) ->
    {0, [{0, 28122}], [], lo};
unicode_table(63822) ->
    {0, [{0, 28431}], [], lo};
unicode_table(63823) ->
    {0, [{0, 32047}], [], lo};
unicode_table(63824) ->
    {0, [{0, 32311}], [], lo};
unicode_table(63825) ->
    {0, [{0, 38475}], [], lo};
unicode_table(63826) ->
    {0, [{0, 21202}], [], lo};
unicode_table(63827) ->
    {0, [{0, 32907}], [], lo};
unicode_table(63828) ->
    {0, [{0, 20956}], [], lo};
unicode_table(63829) ->
    {0, [{0, 20940}], [], lo};
unicode_table(63830) ->
    {0, [{0, 31260}], [], lo};
unicode_table(63831) ->
    {0, [{0, 32190}], [], lo};
unicode_table(63832) ->
    {0, [{0, 33777}], [], lo};
unicode_table(63833) ->
    {0, [{0, 38517}], [], lo};
unicode_table(63834) ->
    {0, [{0, 35712}], [], lo};
unicode_table(63835) ->
    {0, [{0, 25295}], [], lo};
unicode_table(63836) ->
    {0, [{0, 27138}], [], lo};
unicode_table(63837) ->
    {0, [{0, 35582}], [], lo};
unicode_table(63838) ->
    {0, [{0, 20025}], [], lo};
unicode_table(63839) ->
    {0, [{0, 23527}], [], lo};
unicode_table(63840) ->
    {0, [{0, 24594}], [], lo};
unicode_table(63841) ->
    {0, [{0, 29575}], [], lo};
unicode_table(63842) ->
    {0, [{0, 30064}], [], lo};
unicode_table(63843) ->
    {0, [{0, 21271}], [], lo};
unicode_table(63844) ->
    {0, [{0, 30971}], [], lo};
unicode_table(63845) ->
    {0, [{0, 20415}], [], lo};
unicode_table(63846) ->
    {0, [{0, 24489}], [], lo};
unicode_table(63847) ->
    {0, [{0, 19981}], [], lo};
unicode_table(63848) ->
    {0, [{0, 27852}], [], lo};
unicode_table(63849) ->
    {0, [{0, 25976}], [], lo};
unicode_table(63850) ->
    {0, [{0, 32034}], [], lo};
unicode_table(63851) ->
    {0, [{0, 21443}], [], lo};
unicode_table(63852) ->
    {0, [{0, 22622}], [], lo};
unicode_table(63853) ->
    {0, [{0, 30465}], [], lo};
unicode_table(63854) ->
    {0, [{0, 33865}], [], lo};
unicode_table(63855) ->
    {0, [{0, 35498}], [], lo};
unicode_table(63856) ->
    {0, [{0, 27578}], [], lo};
unicode_table(63857) ->
    {0, [{0, 36784}], [], lo};
unicode_table(63858) ->
    {0, [{0, 27784}], [], lo};
unicode_table(63859) ->
    {0, [{0, 25342}], [], lo};
unicode_table(63860) ->
    {0, [{0, 33509}], [], lo};
unicode_table(63861) ->
    {0, [{0, 25504}], [], lo};
unicode_table(63862) ->
    {0, [{0, 30053}], [], lo};
unicode_table(63863) ->
    {0, [{0, 20142}], [], lo};
unicode_table(63864) ->
    {0, [{0, 20841}], [], lo};
unicode_table(63865) ->
    {0, [{0, 20937}], [], lo};
unicode_table(63866) ->
    {0, [{0, 26753}], [], lo};
unicode_table(63867) ->
    {0, [{0, 31975}], [], lo};
unicode_table(63868) ->
    {0, [{0, 33391}], [], lo};
unicode_table(63869) ->
    {0, [{0, 35538}], [], lo};
unicode_table(63870) ->
    {0, [{0, 37327}], [], lo};
unicode_table(63871) ->
    {0, [{0, 21237}], [], lo};
unicode_table(63872) ->
    {0, [{0, 21570}], [], lo};
unicode_table(63873) ->
    {0, [{0, 22899}], [], lo};
unicode_table(63874) ->
    {0, [{0, 24300}], [], lo};
unicode_table(63875) ->
    {0, [{0, 26053}], [], lo};
unicode_table(63876) ->
    {0, [{0, 28670}], [], lo};
unicode_table(63877) ->
    {0, [{0, 31018}], [], lo};
unicode_table(63878) ->
    {0, [{0, 38317}], [], lo};
unicode_table(63879) ->
    {0, [{0, 39530}], [], lo};
unicode_table(63880) ->
    {0, [{0, 40599}], [], lo};
unicode_table(63881) ->
    {0, [{0, 40654}], [], lo};
unicode_table(63882) ->
    {0, [{0, 21147}], [], lo};
unicode_table(63883) ->
    {0, [{0, 26310}], [], lo};
unicode_table(63884) ->
    {0, [{0, 27511}], [], lo};
unicode_table(63885) ->
    {0, [{0, 36706}], [], lo};
unicode_table(63886) ->
    {0, [{0, 24180}], [], lo};
unicode_table(63887) ->
    {0, [{0, 24976}], [], lo};
unicode_table(63888) ->
    {0, [{0, 25088}], [], lo};
unicode_table(63889) ->
    {0, [{0, 25754}], [], lo};
unicode_table(63890) ->
    {0, [{0, 28451}], [], lo};
unicode_table(63891) ->
    {0, [{0, 29001}], [], lo};
unicode_table(63892) ->
    {0, [{0, 29833}], [], lo};
unicode_table(63893) ->
    {0, [{0, 31178}], [], lo};
unicode_table(63894) ->
    {0, [{0, 32244}], [], lo};
unicode_table(63895) ->
    {0, [{0, 32879}], [], lo};
unicode_table(63896) ->
    {0, [{0, 36646}], [], lo};
unicode_table(63897) ->
    {0, [{0, 34030}], [], lo};
unicode_table(63898) ->
    {0, [{0, 36899}], [], lo};
unicode_table(63899) ->
    {0, [{0, 37706}], [], lo};
unicode_table(63900) ->
    {0, [{0, 21015}], [], lo};
unicode_table(63901) ->
    {0, [{0, 21155}], [], lo};
unicode_table(63902) ->
    {0, [{0, 21693}], [], lo};
unicode_table(63903) ->
    {0, [{0, 28872}], [], lo};
unicode_table(63904) ->
    {0, [{0, 35010}], [], lo};
unicode_table(63905) ->
    {0, [{0, 35498}], [], lo};
unicode_table(63906) ->
    {0, [{0, 24265}], [], lo};
unicode_table(63907) ->
    {0, [{0, 24565}], [], lo};
unicode_table(63908) ->
    {0, [{0, 25467}], [], lo};
unicode_table(63909) ->
    {0, [{0, 27566}], [], lo};
unicode_table(63910) ->
    {0, [{0, 31806}], [], lo};
unicode_table(63911) ->
    {0, [{0, 29557}], [], lo};
unicode_table(63912) ->
    {0, [{0, 20196}], [], lo};
unicode_table(63913) ->
    {0, [{0, 22265}], [], lo};
unicode_table(63914) ->
    {0, [{0, 23527}], [], lo};
unicode_table(63915) ->
    {0, [{0, 23994}], [], lo};
unicode_table(63916) ->
    {0, [{0, 24604}], [], lo};
unicode_table(63917) ->
    {0, [{0, 29618}], [], lo};
unicode_table(63918) ->
    {0, [{0, 29801}], [], lo};
unicode_table(63919) ->
    {0, [{0, 32666}], [], lo};
unicode_table(63920) ->
    {0, [{0, 32838}], [], lo};
unicode_table(63921) ->
    {0, [{0, 37428}], [], lo};
unicode_table(63922) ->
    {0, [{0, 38646}], [], lo};
unicode_table(63923) ->
    {0, [{0, 38728}], [], lo};
unicode_table(63924) ->
    {0, [{0, 38936}], [], lo};
unicode_table(63925) ->
    {0, [{0, 20363}], [], lo};
unicode_table(63926) ->
    {0, [{0, 31150}], [], lo};
unicode_table(63927) ->
    {0, [{0, 37300}], [], lo};
unicode_table(63928) ->
    {0, [{0, 38584}], [], lo};
unicode_table(63929) ->
    {0, [{0, 24801}], [], lo};
unicode_table(63930) ->
    {0, [{0, 20102}], [], lo};
unicode_table(63931) ->
    {0, [{0, 20698}], [], lo};
unicode_table(63932) ->
    {0, [{0, 23534}], [], lo};
unicode_table(63933) ->
    {0, [{0, 23615}], [], lo};
unicode_table(63934) ->
    {0, [{0, 26009}], [], lo};
unicode_table(63935) ->
    {0, [{0, 27138}], [], lo};
unicode_table(63936) ->
    {0, [{0, 29134}], [], lo};
unicode_table(63937) ->
    {0, [{0, 30274}], [], lo};
unicode_table(63938) ->
    {0, [{0, 34044}], [], lo};
unicode_table(63939) ->
    {0, [{0, 36988}], [], lo};
unicode_table(63940) ->
    {0, [{0, 40845}], [], lo};
unicode_table(63941) ->
    {0, [{0, 26248}], [], lo};
unicode_table(63942) ->
    {0, [{0, 38446}], [], lo};
unicode_table(63943) ->
    {0, [{0, 21129}], [], lo};
unicode_table(63944) ->
    {0, [{0, 26491}], [], lo};
unicode_table(63945) ->
    {0, [{0, 26611}], [], lo};
unicode_table(63946) ->
    {0, [{0, 27969}], [], lo};
unicode_table(63947) ->
    {0, [{0, 28316}], [], lo};
unicode_table(63948) ->
    {0, [{0, 29705}], [], lo};
unicode_table(63949) ->
    {0, [{0, 30041}], [], lo};
unicode_table(63950) ->
    {0, [{0, 30827}], [], lo};
unicode_table(63951) ->
    {0, [{0, 32016}], [], lo};
unicode_table(63952) ->
    {0, [{0, 39006}], [], lo};
unicode_table(63953) ->
    {0, [{0, 20845}], [], lo};
unicode_table(63954) ->
    {0, [{0, 25134}], [], lo};
unicode_table(63955) ->
    {0, [{0, 38520}], [], lo};
unicode_table(63956) ->
    {0, [{0, 20523}], [], lo};
unicode_table(63957) ->
    {0, [{0, 23833}], [], lo};
unicode_table(63958) ->
    {0, [{0, 28138}], [], lo};
unicode_table(63959) ->
    {0, [{0, 36650}], [], lo};
unicode_table(63960) ->
    {0, [{0, 24459}], [], lo};
unicode_table(63961) ->
    {0, [{0, 24900}], [], lo};
unicode_table(63962) ->
    {0, [{0, 26647}], [], lo};
unicode_table(63963) ->
    {0, [{0, 29575}], [], lo};
unicode_table(63964) ->
    {0, [{0, 38534}], [], lo};
unicode_table(63965) ->
    {0, [{0, 21033}], [], lo};
unicode_table(63966) ->
    {0, [{0, 21519}], [], lo};
unicode_table(63967) ->
    {0, [{0, 23653}], [], lo};
unicode_table(63968) ->
    {0, [{0, 26131}], [], lo};
unicode_table(63969) ->
    {0, [{0, 26446}], [], lo};
unicode_table(63970) ->
    {0, [{0, 26792}], [], lo};
unicode_table(63971) ->
    {0, [{0, 27877}], [], lo};
unicode_table(63972) ->
    {0, [{0, 29702}], [], lo};
unicode_table(63973) ->
    {0, [{0, 30178}], [], lo};
unicode_table(63974) ->
    {0, [{0, 32633}], [], lo};
unicode_table(63975) ->
    {0, [{0, 35023}], [], lo};
unicode_table(63976) ->
    {0, [{0, 35041}], [], lo};
unicode_table(63977) ->
    {0, [{0, 37324}], [], lo};
unicode_table(63978) ->
    {0, [{0, 38626}], [], lo};
unicode_table(63979) ->
    {0, [{0, 21311}], [], lo};
unicode_table(63980) ->
    {0, [{0, 28346}], [], lo};
unicode_table(63981) ->
    {0, [{0, 21533}], [], lo};
unicode_table(63982) ->
    {0, [{0, 29136}], [], lo};
unicode_table(63983) ->
    {0, [{0, 29848}], [], lo};
unicode_table(63984) ->
    {0, [{0, 34298}], [], lo};
unicode_table(63985) ->
    {0, [{0, 38563}], [], lo};
unicode_table(63986) ->
    {0, [{0, 40023}], [], lo};
unicode_table(63987) ->
    {0, [{0, 40607}], [], lo};
unicode_table(63988) ->
    {0, [{0, 26519}], [], lo};
unicode_table(63989) ->
    {0, [{0, 28107}], [], lo};
unicode_table(63990) ->
    {0, [{0, 33256}], [], lo};
unicode_table(63991) ->
    {0, [{0, 31435}], [], lo};
unicode_table(63992) ->
    {0, [{0, 31520}], [], lo};
unicode_table(63993) ->
    {0, [{0, 31890}], [], lo};
unicode_table(63994) ->
    {0, [{0, 29376}], [], lo};
unicode_table(63995) ->
    {0, [{0, 28825}], [], lo};
unicode_table(63996) ->
    {0, [{0, 35672}], [], lo};
unicode_table(63997) ->
    {0, [{0, 20160}], [], lo};
unicode_table(63998) ->
    {0, [{0, 33590}], [], lo};
unicode_table(63999) ->
    {0, [{0, 21050}], [], lo};
unicode_table(64000) ->
    {0, [{0, 20999}], [], lo};
unicode_table(64001) ->
    {0, [{0, 24230}], [], lo};
unicode_table(64002) ->
    {0, [{0, 25299}], [], lo};
unicode_table(64003) ->
    {0, [{0, 31958}], [], lo};
unicode_table(64004) ->
    {0, [{0, 23429}], [], lo};
unicode_table(64005) ->
    {0, [{0, 27934}], [], lo};
unicode_table(64006) ->
    {0, [{0, 26292}], [], lo};
unicode_table(64007) ->
    {0, [{0, 36667}], [], lo};
unicode_table(64008) ->
    {0, [{0, 34892}], [], lo};
unicode_table(64009) ->
    {0, [{0, 38477}], [], lo};
unicode_table(64010) ->
    {0, [{0, 35211}], [], lo};
unicode_table(64011) ->
    {0, [{0, 24275}], [], lo};
unicode_table(64012) ->
    {0, [{0, 20800}], [], lo};
unicode_table(64013) ->
    {0, [{0, 21952}], [], lo};
unicode_table(64016) ->
    {0, [{0, 22618}], [], lo};
unicode_table(64018) ->
    {0, [{0, 26228}], [], lo};
unicode_table(64021) ->
    {0, [{0, 20958}], [], lo};
unicode_table(64022) ->
    {0, [{0, 29482}], [], lo};
unicode_table(64023) ->
    {0, [{0, 30410}], [], lo};
unicode_table(64024) ->
    {0, [{0, 31036}], [], lo};
unicode_table(64025) ->
    {0, [{0, 31070}], [], lo};
unicode_table(64026) ->
    {0, [{0, 31077}], [], lo};
unicode_table(64027) ->
    {0, [{0, 31119}], [], lo};
unicode_table(64028) ->
    {0, [{0, 38742}], [], lo};
unicode_table(64029) ->
    {0, [{0, 31934}], [], lo};
unicode_table(64030) ->
    {0, [{0, 32701}], [], lo};
unicode_table(64032) ->
    {0, [{0, 34322}], [], lo};
unicode_table(64034) ->
    {0, [{0, 35576}], [], lo};
unicode_table(64037) ->
    {0, [{0, 36920}], [], lo};
unicode_table(64038) ->
    {0, [{0, 37117}], [], lo};
unicode_table(64042) ->
    {0, [{0, 39151}], [], lo};
unicode_table(64043) ->
    {0, [{0, 39164}], [], lo};
unicode_table(64044) ->
    {0, [{0, 39208}], [], lo};
unicode_table(64045) ->
    {0, [{0, 40372}], [], lo};
unicode_table(64046) ->
    {0, [{0, 37086}], [], lo};
unicode_table(64047) ->
    {0, [{0, 38583}], [], lo};
unicode_table(64048) ->
    {0, [{0, 20398}], [], lo};
unicode_table(64049) ->
    {0, [{0, 20711}], [], lo};
unicode_table(64050) ->
    {0, [{0, 20813}], [], lo};
unicode_table(64051) ->
    {0, [{0, 21193}], [], lo};
unicode_table(64052) ->
    {0, [{0, 21220}], [], lo};
unicode_table(64053) ->
    {0, [{0, 21329}], [], lo};
unicode_table(64054) ->
    {0, [{0, 21917}], [], lo};
unicode_table(64055) ->
    {0, [{0, 22022}], [], lo};
unicode_table(64056) ->
    {0, [{0, 22120}], [], lo};
unicode_table(64057) ->
    {0, [{0, 22592}], [], lo};
unicode_table(64058) ->
    {0, [{0, 22696}], [], lo};
unicode_table(64059) ->
    {0, [{0, 23652}], [], lo};
unicode_table(64060) ->
    {0, [{0, 23662}], [], lo};
unicode_table(64061) ->
    {0, [{0, 24724}], [], lo};
unicode_table(64062) ->
    {0, [{0, 24936}], [], lo};
unicode_table(64063) ->
    {0, [{0, 24974}], [], lo};
unicode_table(64064) ->
    {0, [{0, 25074}], [], lo};
unicode_table(64065) ->
    {0, [{0, 25935}], [], lo};
unicode_table(64066) ->
    {0, [{0, 26082}], [], lo};
unicode_table(64067) ->
    {0, [{0, 26257}], [], lo};
unicode_table(64068) ->
    {0, [{0, 26757}], [], lo};
unicode_table(64069) ->
    {0, [{0, 28023}], [], lo};
unicode_table(64070) ->
    {0, [{0, 28186}], [], lo};
unicode_table(64071) ->
    {0, [{0, 28450}], [], lo};
unicode_table(64072) ->
    {0, [{0, 29038}], [], lo};
unicode_table(64073) ->
    {0, [{0, 29227}], [], lo};
unicode_table(64074) ->
    {0, [{0, 29730}], [], lo};
unicode_table(64075) ->
    {0, [{0, 30865}], [], lo};
unicode_table(64076) ->
    {0, [{0, 31038}], [], lo};
unicode_table(64077) ->
    {0, [{0, 31049}], [], lo};
unicode_table(64078) ->
    {0, [{0, 31048}], [], lo};
unicode_table(64079) ->
    {0, [{0, 31056}], [], lo};
unicode_table(64080) ->
    {0, [{0, 31062}], [], lo};
unicode_table(64081) ->
    {0, [{0, 31069}], [], lo};
unicode_table(64082) ->
    {0, [{0, 31117}], [], lo};
unicode_table(64083) ->
    {0, [{0, 31118}], [], lo};
unicode_table(64084) ->
    {0, [{0, 31296}], [], lo};
unicode_table(64085) ->
    {0, [{0, 31361}], [], lo};
unicode_table(64086) ->
    {0, [{0, 31680}], [], lo};
unicode_table(64087) ->
    {0, [{0, 32244}], [], lo};
unicode_table(64088) ->
    {0, [{0, 32265}], [], lo};
unicode_table(64089) ->
    {0, [{0, 32321}], [], lo};
unicode_table(64090) ->
    {0, [{0, 32626}], [], lo};
unicode_table(64091) ->
    {0, [{0, 32773}], [], lo};
unicode_table(64092) ->
    {0, [{0, 33261}], [], lo};
unicode_table(64093) ->
    {0, [{0, 33401}], [], lo};
unicode_table(64094) ->
    {0, [{0, 33401}], [], lo};
unicode_table(64095) ->
    {0, [{0, 33879}], [], lo};
unicode_table(64096) ->
    {0, [{0, 35088}], [], lo};
unicode_table(64097) ->
    {0, [{0, 35222}], [], lo};
unicode_table(64098) ->
    {0, [{0, 35585}], [], lo};
unicode_table(64099) ->
    {0, [{0, 35641}], [], lo};
unicode_table(64100) ->
    {0, [{0, 36051}], [], lo};
unicode_table(64101) ->
    {0, [{0, 36104}], [], lo};
unicode_table(64102) ->
    {0, [{0, 36790}], [], lo};
unicode_table(64103) ->
    {0, [{0, 36920}], [], lo};
unicode_table(64104) ->
    {0, [{0, 38627}], [], lo};
unicode_table(64105) ->
    {0, [{0, 38911}], [], lo};
unicode_table(64106) ->
    {0, [{0, 38971}], [], lo};
unicode_table(64107) ->
    {0, [{0, 24693}], [], lo};
unicode_table(64108) ->
    {0, [{0, 148206}], [], lo};
unicode_table(64109) ->
    {0, [{0, 33304}], [], lo};
unicode_table(64112) ->
    {0, [{0, 20006}], [], lo};
unicode_table(64113) ->
    {0, [{0, 20917}], [], lo};
unicode_table(64114) ->
    {0, [{0, 20840}], [], lo};
unicode_table(64115) ->
    {0, [{0, 20352}], [], lo};
unicode_table(64116) ->
    {0, [{0, 20805}], [], lo};
unicode_table(64117) ->
    {0, [{0, 20864}], [], lo};
unicode_table(64118) ->
    {0, [{0, 21191}], [], lo};
unicode_table(64119) ->
    {0, [{0, 21242}], [], lo};
unicode_table(64120) ->
    {0, [{0, 21917}], [], lo};
unicode_table(64121) ->
    {0, [{0, 21845}], [], lo};
unicode_table(64122) ->
    {0, [{0, 21913}], [], lo};
unicode_table(64123) ->
    {0, [{0, 21986}], [], lo};
unicode_table(64124) ->
    {0, [{0, 22618}], [], lo};
unicode_table(64125) ->
    {0, [{0, 22707}], [], lo};
unicode_table(64126) ->
    {0, [{0, 22852}], [], lo};
unicode_table(64127) ->
    {0, [{0, 22868}], [], lo};
unicode_table(64128) ->
    {0, [{0, 23138}], [], lo};
unicode_table(64129) ->
    {0, [{0, 23336}], [], lo};
unicode_table(64130) ->
    {0, [{0, 24274}], [], lo};
unicode_table(64131) ->
    {0, [{0, 24281}], [], lo};
unicode_table(64132) ->
    {0, [{0, 24425}], [], lo};
unicode_table(64133) ->
    {0, [{0, 24493}], [], lo};
unicode_table(64134) ->
    {0, [{0, 24792}], [], lo};
unicode_table(64135) ->
    {0, [{0, 24910}], [], lo};
unicode_table(64136) ->
    {0, [{0, 24840}], [], lo};
unicode_table(64137) ->
    {0, [{0, 24974}], [], lo};
unicode_table(64138) ->
    {0, [{0, 24928}], [], lo};
unicode_table(64139) ->
    {0, [{0, 25074}], [], lo};
unicode_table(64140) ->
    {0, [{0, 25140}], [], lo};
unicode_table(64141) ->
    {0, [{0, 25540}], [], lo};
unicode_table(64142) ->
    {0, [{0, 25628}], [], lo};
unicode_table(64143) ->
    {0, [{0, 25682}], [], lo};
unicode_table(64144) ->
    {0, [{0, 25942}], [], lo};
unicode_table(64145) ->
    {0, [{0, 26228}], [], lo};
unicode_table(64146) ->
    {0, [{0, 26391}], [], lo};
unicode_table(64147) ->
    {0, [{0, 26395}], [], lo};
unicode_table(64148) ->
    {0, [{0, 26454}], [], lo};
unicode_table(64149) ->
    {0, [{0, 27513}], [], lo};
unicode_table(64150) ->
    {0, [{0, 27578}], [], lo};
unicode_table(64151) ->
    {0, [{0, 27969}], [], lo};
unicode_table(64152) ->
    {0, [{0, 28379}], [], lo};
unicode_table(64153) ->
    {0, [{0, 28363}], [], lo};
unicode_table(64154) ->
    {0, [{0, 28450}], [], lo};
unicode_table(64155) ->
    {0, [{0, 28702}], [], lo};
unicode_table(64156) ->
    {0, [{0, 29038}], [], lo};
unicode_table(64157) ->
    {0, [{0, 30631}], [], lo};
unicode_table(64158) ->
    {0, [{0, 29237}], [], lo};
unicode_table(64159) ->
    {0, [{0, 29359}], [], lo};
unicode_table(64160) ->
    {0, [{0, 29482}], [], lo};
unicode_table(64161) ->
    {0, [{0, 29809}], [], lo};
unicode_table(64162) ->
    {0, [{0, 29958}], [], lo};
unicode_table(64163) ->
    {0, [{0, 30011}], [], lo};
unicode_table(64164) ->
    {0, [{0, 30237}], [], lo};
unicode_table(64165) ->
    {0, [{0, 30239}], [], lo};
unicode_table(64166) ->
    {0, [{0, 30410}], [], lo};
unicode_table(64167) ->
    {0, [{0, 30427}], [], lo};
unicode_table(64168) ->
    {0, [{0, 30452}], [], lo};
unicode_table(64169) ->
    {0, [{0, 30538}], [], lo};
unicode_table(64170) ->
    {0, [{0, 30528}], [], lo};
unicode_table(64171) ->
    {0, [{0, 30924}], [], lo};
unicode_table(64172) ->
    {0, [{0, 31409}], [], lo};
unicode_table(64173) ->
    {0, [{0, 31680}], [], lo};
unicode_table(64174) ->
    {0, [{0, 31867}], [], lo};
unicode_table(64175) ->
    {0, [{0, 32091}], [], lo};
unicode_table(64176) ->
    {0, [{0, 32244}], [], lo};
unicode_table(64177) ->
    {0, [{0, 32574}], [], lo};
unicode_table(64178) ->
    {0, [{0, 32773}], [], lo};
unicode_table(64179) ->
    {0, [{0, 33618}], [], lo};
unicode_table(64180) ->
    {0, [{0, 33775}], [], lo};
unicode_table(64181) ->
    {0, [{0, 34681}], [], lo};
unicode_table(64182) ->
    {0, [{0, 35137}], [], lo};
unicode_table(64183) ->
    {0, [{0, 35206}], [], lo};
unicode_table(64184) ->
    {0, [{0, 35222}], [], lo};
unicode_table(64185) ->
    {0, [{0, 35519}], [], lo};
unicode_table(64186) ->
    {0, [{0, 35576}], [], lo};
unicode_table(64187) ->
    {0, [{0, 35531}], [], lo};
unicode_table(64188) ->
    {0, [{0, 35585}], [], lo};
unicode_table(64189) ->
    {0, [{0, 35582}], [], lo};
unicode_table(64190) ->
    {0, [{0, 35565}], [], lo};
unicode_table(64191) ->
    {0, [{0, 35641}], [], lo};
unicode_table(64192) ->
    {0, [{0, 35722}], [], lo};
unicode_table(64193) ->
    {0, [{0, 36104}], [], lo};
unicode_table(64194) ->
    {0, [{0, 36664}], [], lo};
unicode_table(64195) ->
    {0, [{0, 36978}], [], lo};
unicode_table(64196) ->
    {0, [{0, 37273}], [], lo};
unicode_table(64197) ->
    {0, [{0, 37494}], [], lo};
unicode_table(64198) ->
    {0, [{0, 38524}], [], lo};
unicode_table(64199) ->
    {0, [{0, 38627}], [], lo};
unicode_table(64200) ->
    {0, [{0, 38742}], [], lo};
unicode_table(64201) ->
    {0, [{0, 38875}], [], lo};
unicode_table(64202) ->
    {0, [{0, 38911}], [], lo};
unicode_table(64203) ->
    {0, [{0, 38923}], [], lo};
unicode_table(64204) ->
    {0, [{0, 38971}], [], lo};
unicode_table(64205) ->
    {0, [{0, 39698}], [], lo};
unicode_table(64206) ->
    {0, [{0, 40860}], [], lo};
unicode_table(64207) ->
    {0, [{0, 141386}], [], lo};
unicode_table(64208) ->
    {0, [{0, 141380}], [], lo};
unicode_table(64209) ->
    {0, [{0, 144341}], [], lo};
unicode_table(64210) ->
    {0, [{0, 15261}], [], lo};
unicode_table(64211) ->
    {0, [{0, 16408}], [], lo};
unicode_table(64212) ->
    {0, [{0, 16441}], [], lo};
unicode_table(64213) ->
    {0, [{0, 152137}], [], lo};
unicode_table(64214) ->
    {0, [{0, 154832}], [], lo};
unicode_table(64215) ->
    {0, [{0, 163539}], [], lo};
unicode_table(64216) ->
    {0, [{0, 40771}], [], lo};
unicode_table(64217) ->
    {0, [{0, 40846}], [], lo};
unicode_table(64256) ->
    {0, [], {compat, [{0, 102}, {0, 102}]}, ll};
unicode_table(64257) ->
    {0, [], {compat, [{0, 102}, {0, 105}]}, ll};
unicode_table(64258) ->
    {0, [], {compat, [{0, 102}, {0, 108}]}, ll};
unicode_table(64259) ->
    {0, [], {compat, [{0, 102}, {0, 102}, {0, 105}]}, ll};
unicode_table(64260) ->
    {0, [], {compat, [{0, 102}, {0, 102}, {0, 108}]}, ll};
unicode_table(64261) ->
    {0, [], {compat, [{0, 115}, {0, 116}]}, ll};
unicode_table(64262) ->
    {0, [], {compat, [{0, 115}, {0, 116}]}, ll};
unicode_table(64275) ->
    {0, [], {compat, [{0, 1396}, {0, 1398}]}, ll};
unicode_table(64276) ->
    {0, [], {compat, [{0, 1396}, {0, 1381}]}, ll};
unicode_table(64277) ->
    {0, [], {compat, [{0, 1396}, {0, 1387}]}, ll};
unicode_table(64278) ->
    {0, [], {compat, [{0, 1406}, {0, 1398}]}, ll};
unicode_table(64279) ->
    {0, [], {compat, [{0, 1396}, {0, 1389}]}, ll};
unicode_table(64285) ->
    {0, [{0, 1497}, {14, 1460}], [], lo};
unicode_table(64286) ->
    {26, [], [], mn};
unicode_table(64287) ->
    {0, [{0, 1522}, {17, 1463}], [], lo};
unicode_table(64288) ->
    {0, [], {font, [{0, 1506}]}, lo};
unicode_table(64289) ->
    {0, [], {font, [{0, 1488}]}, lo};
unicode_table(64290) ->
    {0, [], {font, [{0, 1491}]}, lo};
unicode_table(64291) ->
    {0, [], {font, [{0, 1492}]}, lo};
unicode_table(64292) ->
    {0, [], {font, [{0, 1499}]}, lo};
unicode_table(64293) ->
    {0, [], {font, [{0, 1500}]}, lo};
unicode_table(64294) ->
    {0, [], {font, [{0, 1501}]}, lo};
unicode_table(64295) ->
    {0, [], {font, [{0, 1512}]}, lo};
unicode_table(64296) ->
    {0, [], {font, [{0, 1514}]}, lo};
unicode_table(64297) ->
    {0, [], {font, [{0, 43}]}, sm};
unicode_table(64298) ->
    {0, [{0, 1513}, {24, 1473}], [], lo};
unicode_table(64299) ->
    {0, [{0, 1513}, {25, 1474}], [], lo};
unicode_table(64300) ->
    {0, [{0, 1513}, {21, 1468}, {24, 1473}], [], lo};
unicode_table(64301) ->
    {0, [{0, 1513}, {21, 1468}, {25, 1474}], [], lo};
unicode_table(64302) ->
    {0, [{0, 1488}, {17, 1463}], [], lo};
unicode_table(64303) ->
    {0, [{0, 1488}, {18, 1464}], [], lo};
unicode_table(64304) ->
    {0, [{0, 1488}, {21, 1468}], [], lo};
unicode_table(64305) ->
    {0, [{0, 1489}, {21, 1468}], [], lo};
unicode_table(64306) ->
    {0, [{0, 1490}, {21, 1468}], [], lo};
unicode_table(64307) ->
    {0, [{0, 1491}, {21, 1468}], [], lo};
unicode_table(64308) ->
    {0, [{0, 1492}, {21, 1468}], [], lo};
unicode_table(64309) ->
    {0, [{0, 1493}, {21, 1468}], [], lo};
unicode_table(64310) ->
    {0, [{0, 1494}, {21, 1468}], [], lo};
unicode_table(64312) ->
    {0, [{0, 1496}, {21, 1468}], [], lo};
unicode_table(64313) ->
    {0, [{0, 1497}, {21, 1468}], [], lo};
unicode_table(64314) ->
    {0, [{0, 1498}, {21, 1468}], [], lo};
unicode_table(64315) ->
    {0, [{0, 1499}, {21, 1468}], [], lo};
unicode_table(64316) ->
    {0, [{0, 1500}, {21, 1468}], [], lo};
unicode_table(64318) ->
    {0, [{0, 1502}, {21, 1468}], [], lo};
unicode_table(64320) ->
    {0, [{0, 1504}, {21, 1468}], [], lo};
unicode_table(64321) ->
    {0, [{0, 1505}, {21, 1468}], [], lo};
unicode_table(64323) ->
    {0, [{0, 1507}, {21, 1468}], [], lo};
unicode_table(64324) ->
    {0, [{0, 1508}, {21, 1468}], [], lo};
unicode_table(64326) ->
    {0, [{0, 1510}, {21, 1468}], [], lo};
unicode_table(64327) ->
    {0, [{0, 1511}, {21, 1468}], [], lo};
unicode_table(64328) ->
    {0, [{0, 1512}, {21, 1468}], [], lo};
unicode_table(64329) ->
    {0, [{0, 1513}, {21, 1468}], [], lo};
unicode_table(64330) ->
    {0, [{0, 1514}, {21, 1468}], [], lo};
unicode_table(64331) ->
    {0, [{0, 1493}, {19, 1465}], [], lo};
unicode_table(64332) ->
    {0, [{0, 1489}, {23, 1471}], [], lo};
unicode_table(64333) ->
    {0, [{0, 1499}, {23, 1471}], [], lo};
unicode_table(64334) ->
    {0, [{0, 1508}, {23, 1471}], [], lo};
unicode_table(64335) ->
    {0, [], {compat, [{0, 1488}, {0, 1500}]}, lo};
unicode_table(64336) ->
    {0, [], {isolated, [{0, 1649}]}, lo};
unicode_table(64337) ->
    {0, [], {final, [{0, 1649}]}, lo};
unicode_table(64338) ->
    {0, [], {isolated, [{0, 1659}]}, lo};
unicode_table(64339) ->
    {0, [], {final, [{0, 1659}]}, lo};
unicode_table(64340) ->
    {0, [], {initial, [{0, 1659}]}, lo};
unicode_table(64341) ->
    {0, [], {medial, [{0, 1659}]}, lo};
unicode_table(64342) ->
    {0, [], {isolated, [{0, 1662}]}, lo};
unicode_table(64343) ->
    {0, [], {final, [{0, 1662}]}, lo};
unicode_table(64344) ->
    {0, [], {initial, [{0, 1662}]}, lo};
unicode_table(64345) ->
    {0, [], {medial, [{0, 1662}]}, lo};
unicode_table(64346) ->
    {0, [], {isolated, [{0, 1664}]}, lo};
unicode_table(64347) ->
    {0, [], {final, [{0, 1664}]}, lo};
unicode_table(64348) ->
    {0, [], {initial, [{0, 1664}]}, lo};
unicode_table(64349) ->
    {0, [], {medial, [{0, 1664}]}, lo};
unicode_table(64350) ->
    {0, [], {isolated, [{0, 1658}]}, lo};
unicode_table(64351) ->
    {0, [], {final, [{0, 1658}]}, lo};
unicode_table(64352) ->
    {0, [], {initial, [{0, 1658}]}, lo};
unicode_table(64353) ->
    {0, [], {medial, [{0, 1658}]}, lo};
unicode_table(64354) ->
    {0, [], {isolated, [{0, 1663}]}, lo};
unicode_table(64355) ->
    {0, [], {final, [{0, 1663}]}, lo};
unicode_table(64356) ->
    {0, [], {initial, [{0, 1663}]}, lo};
unicode_table(64357) ->
    {0, [], {medial, [{0, 1663}]}, lo};
unicode_table(64358) ->
    {0, [], {isolated, [{0, 1657}]}, lo};
unicode_table(64359) ->
    {0, [], {final, [{0, 1657}]}, lo};
unicode_table(64360) ->
    {0, [], {initial, [{0, 1657}]}, lo};
unicode_table(64361) ->
    {0, [], {medial, [{0, 1657}]}, lo};
unicode_table(64362) ->
    {0, [], {isolated, [{0, 1700}]}, lo};
unicode_table(64363) ->
    {0, [], {final, [{0, 1700}]}, lo};
unicode_table(64364) ->
    {0, [], {initial, [{0, 1700}]}, lo};
unicode_table(64365) ->
    {0, [], {medial, [{0, 1700}]}, lo};
unicode_table(64366) ->
    {0, [], {isolated, [{0, 1702}]}, lo};
unicode_table(64367) ->
    {0, [], {final, [{0, 1702}]}, lo};
unicode_table(64368) ->
    {0, [], {initial, [{0, 1702}]}, lo};
unicode_table(64369) ->
    {0, [], {medial, [{0, 1702}]}, lo};
unicode_table(64370) ->
    {0, [], {isolated, [{0, 1668}]}, lo};
unicode_table(64371) ->
    {0, [], {final, [{0, 1668}]}, lo};
unicode_table(64372) ->
    {0, [], {initial, [{0, 1668}]}, lo};
unicode_table(64373) ->
    {0, [], {medial, [{0, 1668}]}, lo};
unicode_table(64374) ->
    {0, [], {isolated, [{0, 1667}]}, lo};
unicode_table(64375) ->
    {0, [], {final, [{0, 1667}]}, lo};
unicode_table(64376) ->
    {0, [], {initial, [{0, 1667}]}, lo};
unicode_table(64377) ->
    {0, [], {medial, [{0, 1667}]}, lo};
unicode_table(64378) ->
    {0, [], {isolated, [{0, 1670}]}, lo};
unicode_table(64379) ->
    {0, [], {final, [{0, 1670}]}, lo};
unicode_table(64380) ->
    {0, [], {initial, [{0, 1670}]}, lo};
unicode_table(64381) ->
    {0, [], {medial, [{0, 1670}]}, lo};
unicode_table(64382) ->
    {0, [], {isolated, [{0, 1671}]}, lo};
unicode_table(64383) ->
    {0, [], {final, [{0, 1671}]}, lo};
unicode_table(64384) ->
    {0, [], {initial, [{0, 1671}]}, lo};
unicode_table(64385) ->
    {0, [], {medial, [{0, 1671}]}, lo};
unicode_table(64386) ->
    {0, [], {isolated, [{0, 1677}]}, lo};
unicode_table(64387) ->
    {0, [], {final, [{0, 1677}]}, lo};
unicode_table(64388) ->
    {0, [], {isolated, [{0, 1676}]}, lo};
unicode_table(64389) ->
    {0, [], {final, [{0, 1676}]}, lo};
unicode_table(64390) ->
    {0, [], {isolated, [{0, 1678}]}, lo};
unicode_table(64391) ->
    {0, [], {final, [{0, 1678}]}, lo};
unicode_table(64392) ->
    {0, [], {isolated, [{0, 1672}]}, lo};
unicode_table(64393) ->
    {0, [], {final, [{0, 1672}]}, lo};
unicode_table(64394) ->
    {0, [], {isolated, [{0, 1688}]}, lo};
unicode_table(64395) ->
    {0, [], {final, [{0, 1688}]}, lo};
unicode_table(64396) ->
    {0, [], {isolated, [{0, 1681}]}, lo};
unicode_table(64397) ->
    {0, [], {final, [{0, 1681}]}, lo};
unicode_table(64398) ->
    {0, [], {isolated, [{0, 1705}]}, lo};
unicode_table(64399) ->
    {0, [], {final, [{0, 1705}]}, lo};
unicode_table(64400) ->
    {0, [], {initial, [{0, 1705}]}, lo};
unicode_table(64401) ->
    {0, [], {medial, [{0, 1705}]}, lo};
unicode_table(64402) ->
    {0, [], {isolated, [{0, 1711}]}, lo};
unicode_table(64403) ->
    {0, [], {final, [{0, 1711}]}, lo};
unicode_table(64404) ->
    {0, [], {initial, [{0, 1711}]}, lo};
unicode_table(64405) ->
    {0, [], {medial, [{0, 1711}]}, lo};
unicode_table(64406) ->
    {0, [], {isolated, [{0, 1715}]}, lo};
unicode_table(64407) ->
    {0, [], {final, [{0, 1715}]}, lo};
unicode_table(64408) ->
    {0, [], {initial, [{0, 1715}]}, lo};
unicode_table(64409) ->
    {0, [], {medial, [{0, 1715}]}, lo};
unicode_table(64410) ->
    {0, [], {isolated, [{0, 1713}]}, lo};
unicode_table(64411) ->
    {0, [], {final, [{0, 1713}]}, lo};
unicode_table(64412) ->
    {0, [], {initial, [{0, 1713}]}, lo};
unicode_table(64413) ->
    {0, [], {medial, [{0, 1713}]}, lo};
unicode_table(64414) ->
    {0, [], {isolated, [{0, 1722}]}, lo};
unicode_table(64415) ->
    {0, [], {final, [{0, 1722}]}, lo};
unicode_table(64416) ->
    {0, [], {isolated, [{0, 1723}]}, lo};
unicode_table(64417) ->
    {0, [], {final, [{0, 1723}]}, lo};
unicode_table(64418) ->
    {0, [], {initial, [{0, 1723}]}, lo};
unicode_table(64419) ->
    {0, [], {medial, [{0, 1723}]}, lo};
unicode_table(64420) ->
    {0, [], {isolated, [{0, 1749}, {230, 1620}]}, lo};
unicode_table(64421) ->
    {0, [], {final, [{0, 1749}, {230, 1620}]}, lo};
unicode_table(64422) ->
    {0, [], {isolated, [{0, 1729}]}, lo};
unicode_table(64423) ->
    {0, [], {final, [{0, 1729}]}, lo};
unicode_table(64424) ->
    {0, [], {initial, [{0, 1729}]}, lo};
unicode_table(64425) ->
    {0, [], {medial, [{0, 1729}]}, lo};
unicode_table(64426) ->
    {0, [], {isolated, [{0, 1726}]}, lo};
unicode_table(64427) ->
    {0, [], {final, [{0, 1726}]}, lo};
unicode_table(64428) ->
    {0, [], {initial, [{0, 1726}]}, lo};
unicode_table(64429) ->
    {0, [], {medial, [{0, 1726}]}, lo};
unicode_table(64430) ->
    {0, [], {isolated, [{0, 1746}]}, lo};
unicode_table(64431) ->
    {0, [], {final, [{0, 1746}]}, lo};
unicode_table(64432) ->
    {0, [], {isolated, [{0, 1746}, {230, 1620}]}, lo};
unicode_table(64433) ->
    {0, [], {final, [{0, 1746}, {230, 1620}]}, lo};
unicode_table(64467) ->
    {0, [], {isolated, [{0, 1709}]}, lo};
unicode_table(64468) ->
    {0, [], {final, [{0, 1709}]}, lo};
unicode_table(64469) ->
    {0, [], {initial, [{0, 1709}]}, lo};
unicode_table(64470) ->
    {0, [], {medial, [{0, 1709}]}, lo};
unicode_table(64471) ->
    {0, [], {isolated, [{0, 1735}]}, lo};
unicode_table(64472) ->
    {0, [], {final, [{0, 1735}]}, lo};
unicode_table(64473) ->
    {0, [], {isolated, [{0, 1734}]}, lo};
unicode_table(64474) ->
    {0, [], {final, [{0, 1734}]}, lo};
unicode_table(64475) ->
    {0, [], {isolated, [{0, 1736}]}, lo};
unicode_table(64476) ->
    {0, [], {final, [{0, 1736}]}, lo};
unicode_table(64477) ->
    {0, [], {isolated, [{0, 1735}, {0, 1652}]}, lo};
unicode_table(64478) ->
    {0, [], {isolated, [{0, 1739}]}, lo};
unicode_table(64479) ->
    {0, [], {final, [{0, 1739}]}, lo};
unicode_table(64480) ->
    {0, [], {isolated, [{0, 1733}]}, lo};
unicode_table(64481) ->
    {0, [], {final, [{0, 1733}]}, lo};
unicode_table(64482) ->
    {0, [], {isolated, [{0, 1737}]}, lo};
unicode_table(64483) ->
    {0, [], {final, [{0, 1737}]}, lo};
unicode_table(64484) ->
    {0, [], {isolated, [{0, 1744}]}, lo};
unicode_table(64485) ->
    {0, [], {final, [{0, 1744}]}, lo};
unicode_table(64486) ->
    {0, [], {initial, [{0, 1744}]}, lo};
unicode_table(64487) ->
    {0, [], {medial, [{0, 1744}]}, lo};
unicode_table(64488) ->
    {0, [], {initial, [{0, 1609}]}, lo};
unicode_table(64489) ->
    {0, [], {medial, [{0, 1609}]}, lo};
unicode_table(64490) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1575}]}, lo};
unicode_table(64491) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1575}]}, lo};
unicode_table(64492) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1749}]}, lo};
unicode_table(64493) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1749}]}, lo};
unicode_table(64494) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1608}]}, lo};
unicode_table(64495) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1608}]}, lo};
unicode_table(64496) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1735}]}, lo};
unicode_table(64497) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1735}]}, lo};
unicode_table(64498) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1734}]}, lo};
unicode_table(64499) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1734}]}, lo};
unicode_table(64500) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1736}]}, lo};
unicode_table(64501) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1736}]}, lo};
unicode_table(64502) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1744}]}, lo};
unicode_table(64503) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1744}]}, lo};
unicode_table(64504) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}, {0, 1744}]}, lo};
unicode_table(64505) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1609}]}, lo};
unicode_table(64506) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1609}]}, lo};
unicode_table(64507) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}, {0, 1609}]}, lo};
unicode_table(64508) ->
    {0, [], {isolated, [{0, 1740}]}, lo};
unicode_table(64509) ->
    {0, [], {final, [{0, 1740}]}, lo};
unicode_table(64510) ->
    {0, [], {initial, [{0, 1740}]}, lo};
unicode_table(64511) ->
    {0, [], {medial, [{0, 1740}]}, lo};
unicode_table(64512) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1580}]}, lo};
unicode_table(64513) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1581}]}, lo};
unicode_table(64514) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1605}]}, lo};
unicode_table(64515) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1609}]}, lo};
unicode_table(64516) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}, {0, 1610}]}, lo};
unicode_table(64517) ->
    {0, [], {isolated, [{0, 1576}, {0, 1580}]}, lo};
unicode_table(64518) ->
    {0, [], {isolated, [{0, 1576}, {0, 1581}]}, lo};
unicode_table(64519) ->
    {0, [], {isolated, [{0, 1576}, {0, 1582}]}, lo};
unicode_table(64520) ->
    {0, [], {isolated, [{0, 1576}, {0, 1605}]}, lo};
unicode_table(64521) ->
    {0, [], {isolated, [{0, 1576}, {0, 1609}]}, lo};
unicode_table(64522) ->
    {0, [], {isolated, [{0, 1576}, {0, 1610}]}, lo};
unicode_table(64523) ->
    {0, [], {isolated, [{0, 1578}, {0, 1580}]}, lo};
unicode_table(64524) ->
    {0, [], {isolated, [{0, 1578}, {0, 1581}]}, lo};
unicode_table(64525) ->
    {0, [], {isolated, [{0, 1578}, {0, 1582}]}, lo};
unicode_table(64526) ->
    {0, [], {isolated, [{0, 1578}, {0, 1605}]}, lo};
unicode_table(64527) ->
    {0, [], {isolated, [{0, 1578}, {0, 1609}]}, lo};
unicode_table(64528) ->
    {0, [], {isolated, [{0, 1578}, {0, 1610}]}, lo};
unicode_table(64529) ->
    {0, [], {isolated, [{0, 1579}, {0, 1580}]}, lo};
unicode_table(64530) ->
    {0, [], {isolated, [{0, 1579}, {0, 1605}]}, lo};
unicode_table(64531) ->
    {0, [], {isolated, [{0, 1579}, {0, 1609}]}, lo};
unicode_table(64532) ->
    {0, [], {isolated, [{0, 1579}, {0, 1610}]}, lo};
unicode_table(64533) ->
    {0, [], {isolated, [{0, 1580}, {0, 1581}]}, lo};
unicode_table(64534) ->
    {0, [], {isolated, [{0, 1580}, {0, 1605}]}, lo};
unicode_table(64535) ->
    {0, [], {isolated, [{0, 1581}, {0, 1580}]}, lo};
unicode_table(64536) ->
    {0, [], {isolated, [{0, 1581}, {0, 1605}]}, lo};
unicode_table(64537) ->
    {0, [], {isolated, [{0, 1582}, {0, 1580}]}, lo};
unicode_table(64538) ->
    {0, [], {isolated, [{0, 1582}, {0, 1581}]}, lo};
unicode_table(64539) ->
    {0, [], {isolated, [{0, 1582}, {0, 1605}]}, lo};
unicode_table(64540) ->
    {0, [], {isolated, [{0, 1587}, {0, 1580}]}, lo};
unicode_table(64541) ->
    {0, [], {isolated, [{0, 1587}, {0, 1581}]}, lo};
unicode_table(64542) ->
    {0, [], {isolated, [{0, 1587}, {0, 1582}]}, lo};
unicode_table(64543) ->
    {0, [], {isolated, [{0, 1587}, {0, 1605}]}, lo};
unicode_table(64544) ->
    {0, [], {isolated, [{0, 1589}, {0, 1581}]}, lo};
unicode_table(64545) ->
    {0, [], {isolated, [{0, 1589}, {0, 1605}]}, lo};
unicode_table(64546) ->
    {0, [], {isolated, [{0, 1590}, {0, 1580}]}, lo};
unicode_table(64547) ->
    {0, [], {isolated, [{0, 1590}, {0, 1581}]}, lo};
unicode_table(64548) ->
    {0, [], {isolated, [{0, 1590}, {0, 1582}]}, lo};
unicode_table(64549) ->
    {0, [], {isolated, [{0, 1590}, {0, 1605}]}, lo};
unicode_table(64550) ->
    {0, [], {isolated, [{0, 1591}, {0, 1581}]}, lo};
unicode_table(64551) ->
    {0, [], {isolated, [{0, 1591}, {0, 1605}]}, lo};
unicode_table(64552) ->
    {0, [], {isolated, [{0, 1592}, {0, 1605}]}, lo};
unicode_table(64553) ->
    {0, [], {isolated, [{0, 1593}, {0, 1580}]}, lo};
unicode_table(64554) ->
    {0, [], {isolated, [{0, 1593}, {0, 1605}]}, lo};
unicode_table(64555) ->
    {0, [], {isolated, [{0, 1594}, {0, 1580}]}, lo};
unicode_table(64556) ->
    {0, [], {isolated, [{0, 1594}, {0, 1605}]}, lo};
unicode_table(64557) ->
    {0, [], {isolated, [{0, 1601}, {0, 1580}]}, lo};
unicode_table(64558) ->
    {0, [], {isolated, [{0, 1601}, {0, 1581}]}, lo};
unicode_table(64559) ->
    {0, [], {isolated, [{0, 1601}, {0, 1582}]}, lo};
unicode_table(64560) ->
    {0, [], {isolated, [{0, 1601}, {0, 1605}]}, lo};
unicode_table(64561) ->
    {0, [], {isolated, [{0, 1601}, {0, 1609}]}, lo};
unicode_table(64562) ->
    {0, [], {isolated, [{0, 1601}, {0, 1610}]}, lo};
unicode_table(64563) ->
    {0, [], {isolated, [{0, 1602}, {0, 1581}]}, lo};
unicode_table(64564) ->
    {0, [], {isolated, [{0, 1602}, {0, 1605}]}, lo};
unicode_table(64565) ->
    {0, [], {isolated, [{0, 1602}, {0, 1609}]}, lo};
unicode_table(64566) ->
    {0, [], {isolated, [{0, 1602}, {0, 1610}]}, lo};
unicode_table(64567) ->
    {0, [], {isolated, [{0, 1603}, {0, 1575}]}, lo};
unicode_table(64568) ->
    {0, [], {isolated, [{0, 1603}, {0, 1580}]}, lo};
unicode_table(64569) ->
    {0, [], {isolated, [{0, 1603}, {0, 1581}]}, lo};
unicode_table(64570) ->
    {0, [], {isolated, [{0, 1603}, {0, 1582}]}, lo};
unicode_table(64571) ->
    {0, [], {isolated, [{0, 1603}, {0, 1604}]}, lo};
unicode_table(64572) ->
    {0, [], {isolated, [{0, 1603}, {0, 1605}]}, lo};
unicode_table(64573) ->
    {0, [], {isolated, [{0, 1603}, {0, 1609}]}, lo};
unicode_table(64574) ->
    {0, [], {isolated, [{0, 1603}, {0, 1610}]}, lo};
unicode_table(64575) ->
    {0, [], {isolated, [{0, 1604}, {0, 1580}]}, lo};
unicode_table(64576) ->
    {0, [], {isolated, [{0, 1604}, {0, 1581}]}, lo};
unicode_table(64577) ->
    {0, [], {isolated, [{0, 1604}, {0, 1582}]}, lo};
unicode_table(64578) ->
    {0, [], {isolated, [{0, 1604}, {0, 1605}]}, lo};
unicode_table(64579) ->
    {0, [], {isolated, [{0, 1604}, {0, 1609}]}, lo};
unicode_table(64580) ->
    {0, [], {isolated, [{0, 1604}, {0, 1610}]}, lo};
unicode_table(64581) ->
    {0, [], {isolated, [{0, 1605}, {0, 1580}]}, lo};
unicode_table(64582) ->
    {0, [], {isolated, [{0, 1605}, {0, 1581}]}, lo};
unicode_table(64583) ->
    {0, [], {isolated, [{0, 1605}, {0, 1582}]}, lo};
unicode_table(64584) ->
    {0, [], {isolated, [{0, 1605}, {0, 1605}]}, lo};
unicode_table(64585) ->
    {0, [], {isolated, [{0, 1605}, {0, 1609}]}, lo};
unicode_table(64586) ->
    {0, [], {isolated, [{0, 1605}, {0, 1610}]}, lo};
unicode_table(64587) ->
    {0, [], {isolated, [{0, 1606}, {0, 1580}]}, lo};
unicode_table(64588) ->
    {0, [], {isolated, [{0, 1606}, {0, 1581}]}, lo};
unicode_table(64589) ->
    {0, [], {isolated, [{0, 1606}, {0, 1582}]}, lo};
unicode_table(64590) ->
    {0, [], {isolated, [{0, 1606}, {0, 1605}]}, lo};
unicode_table(64591) ->
    {0, [], {isolated, [{0, 1606}, {0, 1609}]}, lo};
unicode_table(64592) ->
    {0, [], {isolated, [{0, 1606}, {0, 1610}]}, lo};
unicode_table(64593) ->
    {0, [], {isolated, [{0, 1607}, {0, 1580}]}, lo};
unicode_table(64594) ->
    {0, [], {isolated, [{0, 1607}, {0, 1605}]}, lo};
unicode_table(64595) ->
    {0, [], {isolated, [{0, 1607}, {0, 1609}]}, lo};
unicode_table(64596) ->
    {0, [], {isolated, [{0, 1607}, {0, 1610}]}, lo};
unicode_table(64597) ->
    {0, [], {isolated, [{0, 1610}, {0, 1580}]}, lo};
unicode_table(64598) ->
    {0, [], {isolated, [{0, 1610}, {0, 1581}]}, lo};
unicode_table(64599) ->
    {0, [], {isolated, [{0, 1610}, {0, 1582}]}, lo};
unicode_table(64600) ->
    {0, [], {isolated, [{0, 1610}, {0, 1605}]}, lo};
unicode_table(64601) ->
    {0, [], {isolated, [{0, 1610}, {0, 1609}]}, lo};
unicode_table(64602) ->
    {0, [], {isolated, [{0, 1610}, {0, 1610}]}, lo};
unicode_table(64603) ->
    {0, [], {isolated, [{0, 1584}, {35, 1648}]}, lo};
unicode_table(64604) ->
    {0, [], {isolated, [{0, 1585}, {35, 1648}]}, lo};
unicode_table(64605) ->
    {0, [], {isolated, [{0, 1609}, {35, 1648}]}, lo};
unicode_table(64606) ->
    {0, [], {isolated, [{0, 32}, {28, 1612}, {33, 1617}]}, lo};
unicode_table(64607) ->
    {0, [], {isolated, [{0, 32}, {29, 1613}, {33, 1617}]}, lo};
unicode_table(64608) ->
    {0, [], {isolated, [{0, 32}, {30, 1614}, {33, 1617}]}, lo};
unicode_table(64609) ->
    {0, [], {isolated, [{0, 32}, {31, 1615}, {33, 1617}]}, lo};
unicode_table(64610) ->
    {0, [], {isolated, [{0, 32}, {32, 1616}, {33, 1617}]}, lo};
unicode_table(64611) ->
    {0, [], {isolated, [{0, 32}, {33, 1617}, {35, 1648}]}, lo};
unicode_table(64612) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1585}]}, lo};
unicode_table(64613) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1586}]}, lo};
unicode_table(64614) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1605}]}, lo};
unicode_table(64615) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1606}]}, lo};
unicode_table(64616) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1609}]}, lo};
unicode_table(64617) ->
    {0, [], {final, [{0, 1610}, {230, 1620}, {0, 1610}]}, lo};
unicode_table(64618) ->
    {0, [], {final, [{0, 1576}, {0, 1585}]}, lo};
unicode_table(64619) ->
    {0, [], {final, [{0, 1576}, {0, 1586}]}, lo};
unicode_table(64620) ->
    {0, [], {final, [{0, 1576}, {0, 1605}]}, lo};
unicode_table(64621) ->
    {0, [], {final, [{0, 1576}, {0, 1606}]}, lo};
unicode_table(64622) ->
    {0, [], {final, [{0, 1576}, {0, 1609}]}, lo};
unicode_table(64623) ->
    {0, [], {final, [{0, 1576}, {0, 1610}]}, lo};
unicode_table(64624) ->
    {0, [], {final, [{0, 1578}, {0, 1585}]}, lo};
unicode_table(64625) ->
    {0, [], {final, [{0, 1578}, {0, 1586}]}, lo};
unicode_table(64626) ->
    {0, [], {final, [{0, 1578}, {0, 1605}]}, lo};
unicode_table(64627) ->
    {0, [], {final, [{0, 1578}, {0, 1606}]}, lo};
unicode_table(64628) ->
    {0, [], {final, [{0, 1578}, {0, 1609}]}, lo};
unicode_table(64629) ->
    {0, [], {final, [{0, 1578}, {0, 1610}]}, lo};
unicode_table(64630) ->
    {0, [], {final, [{0, 1579}, {0, 1585}]}, lo};
unicode_table(64631) ->
    {0, [], {final, [{0, 1579}, {0, 1586}]}, lo};
unicode_table(64632) ->
    {0, [], {final, [{0, 1579}, {0, 1605}]}, lo};
unicode_table(64633) ->
    {0, [], {final, [{0, 1579}, {0, 1606}]}, lo};
unicode_table(64634) ->
    {0, [], {final, [{0, 1579}, {0, 1609}]}, lo};
unicode_table(64635) ->
    {0, [], {final, [{0, 1579}, {0, 1610}]}, lo};
unicode_table(64636) ->
    {0, [], {final, [{0, 1601}, {0, 1609}]}, lo};
unicode_table(64637) ->
    {0, [], {final, [{0, 1601}, {0, 1610}]}, lo};
unicode_table(64638) ->
    {0, [], {final, [{0, 1602}, {0, 1609}]}, lo};
unicode_table(64639) ->
    {0, [], {final, [{0, 1602}, {0, 1610}]}, lo};
unicode_table(64640) ->
    {0, [], {final, [{0, 1603}, {0, 1575}]}, lo};
unicode_table(64641) ->
    {0, [], {final, [{0, 1603}, {0, 1604}]}, lo};
unicode_table(64642) ->
    {0, [], {final, [{0, 1603}, {0, 1605}]}, lo};
unicode_table(64643) ->
    {0, [], {final, [{0, 1603}, {0, 1609}]}, lo};
unicode_table(64644) ->
    {0, [], {final, [{0, 1603}, {0, 1610}]}, lo};
unicode_table(64645) ->
    {0, [], {final, [{0, 1604}, {0, 1605}]}, lo};
unicode_table(64646) ->
    {0, [], {final, [{0, 1604}, {0, 1609}]}, lo};
unicode_table(64647) ->
    {0, [], {final, [{0, 1604}, {0, 1610}]}, lo};
unicode_table(64648) ->
    {0, [], {final, [{0, 1605}, {0, 1575}]}, lo};
unicode_table(64649) ->
    {0, [], {final, [{0, 1605}, {0, 1605}]}, lo};
unicode_table(64650) ->
    {0, [], {final, [{0, 1606}, {0, 1585}]}, lo};
unicode_table(64651) ->
    {0, [], {final, [{0, 1606}, {0, 1586}]}, lo};
unicode_table(64652) ->
    {0, [], {final, [{0, 1606}, {0, 1605}]}, lo};
unicode_table(64653) ->
    {0, [], {final, [{0, 1606}, {0, 1606}]}, lo};
unicode_table(64654) ->
    {0, [], {final, [{0, 1606}, {0, 1609}]}, lo};
unicode_table(64655) ->
    {0, [], {final, [{0, 1606}, {0, 1610}]}, lo};
unicode_table(64656) ->
    {0, [], {final, [{0, 1609}, {35, 1648}]}, lo};
unicode_table(64657) ->
    {0, [], {final, [{0, 1610}, {0, 1585}]}, lo};
unicode_table(64658) ->
    {0, [], {final, [{0, 1610}, {0, 1586}]}, lo};
unicode_table(64659) ->
    {0, [], {final, [{0, 1610}, {0, 1605}]}, lo};
unicode_table(64660) ->
    {0, [], {final, [{0, 1610}, {0, 1606}]}, lo};
unicode_table(64661) ->
    {0, [], {final, [{0, 1610}, {0, 1609}]}, lo};
unicode_table(64662) ->
    {0, [], {final, [{0, 1610}, {0, 1610}]}, lo};
unicode_table(64663) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}, {0, 1580}]}, lo};
unicode_table(64664) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}, {0, 1581}]}, lo};
unicode_table(64665) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}, {0, 1582}]}, lo};
unicode_table(64666) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}, {0, 1605}]}, lo};
unicode_table(64667) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}, {0, 1607}]}, lo};
unicode_table(64668) ->
    {0, [], {initial, [{0, 1576}, {0, 1580}]}, lo};
unicode_table(64669) ->
    {0, [], {initial, [{0, 1576}, {0, 1581}]}, lo};
unicode_table(64670) ->
    {0, [], {initial, [{0, 1576}, {0, 1582}]}, lo};
unicode_table(64671) ->
    {0, [], {initial, [{0, 1576}, {0, 1605}]}, lo};
unicode_table(64672) ->
    {0, [], {initial, [{0, 1576}, {0, 1607}]}, lo};
unicode_table(64673) ->
    {0, [], {initial, [{0, 1578}, {0, 1580}]}, lo};
unicode_table(64674) ->
    {0, [], {initial, [{0, 1578}, {0, 1581}]}, lo};
unicode_table(64675) ->
    {0, [], {initial, [{0, 1578}, {0, 1582}]}, lo};
unicode_table(64676) ->
    {0, [], {initial, [{0, 1578}, {0, 1605}]}, lo};
unicode_table(64677) ->
    {0, [], {initial, [{0, 1578}, {0, 1607}]}, lo};
unicode_table(64678) ->
    {0, [], {initial, [{0, 1579}, {0, 1605}]}, lo};
unicode_table(64679) ->
    {0, [], {initial, [{0, 1580}, {0, 1581}]}, lo};
unicode_table(64680) ->
    {0, [], {initial, [{0, 1580}, {0, 1605}]}, lo};
unicode_table(64681) ->
    {0, [], {initial, [{0, 1581}, {0, 1580}]}, lo};
unicode_table(64682) ->
    {0, [], {initial, [{0, 1581}, {0, 1605}]}, lo};
unicode_table(64683) ->
    {0, [], {initial, [{0, 1582}, {0, 1580}]}, lo};
unicode_table(64684) ->
    {0, [], {initial, [{0, 1582}, {0, 1605}]}, lo};
unicode_table(64685) ->
    {0, [], {initial, [{0, 1587}, {0, 1580}]}, lo};
unicode_table(64686) ->
    {0, [], {initial, [{0, 1587}, {0, 1581}]}, lo};
unicode_table(64687) ->
    {0, [], {initial, [{0, 1587}, {0, 1582}]}, lo};
unicode_table(64688) ->
    {0, [], {initial, [{0, 1587}, {0, 1605}]}, lo};
unicode_table(64689) ->
    {0, [], {initial, [{0, 1589}, {0, 1581}]}, lo};
unicode_table(64690) ->
    {0, [], {initial, [{0, 1589}, {0, 1582}]}, lo};
unicode_table(64691) ->
    {0, [], {initial, [{0, 1589}, {0, 1605}]}, lo};
unicode_table(64692) ->
    {0, [], {initial, [{0, 1590}, {0, 1580}]}, lo};
unicode_table(64693) ->
    {0, [], {initial, [{0, 1590}, {0, 1581}]}, lo};
unicode_table(64694) ->
    {0, [], {initial, [{0, 1590}, {0, 1582}]}, lo};
unicode_table(64695) ->
    {0, [], {initial, [{0, 1590}, {0, 1605}]}, lo};
unicode_table(64696) ->
    {0, [], {initial, [{0, 1591}, {0, 1581}]}, lo};
unicode_table(64697) ->
    {0, [], {initial, [{0, 1592}, {0, 1605}]}, lo};
unicode_table(64698) ->
    {0, [], {initial, [{0, 1593}, {0, 1580}]}, lo};
unicode_table(64699) ->
    {0, [], {initial, [{0, 1593}, {0, 1605}]}, lo};
unicode_table(64700) ->
    {0, [], {initial, [{0, 1594}, {0, 1580}]}, lo};
unicode_table(64701) ->
    {0, [], {initial, [{0, 1594}, {0, 1605}]}, lo};
unicode_table(64702) ->
    {0, [], {initial, [{0, 1601}, {0, 1580}]}, lo};
unicode_table(64703) ->
    {0, [], {initial, [{0, 1601}, {0, 1581}]}, lo};
unicode_table(64704) ->
    {0, [], {initial, [{0, 1601}, {0, 1582}]}, lo};
unicode_table(64705) ->
    {0, [], {initial, [{0, 1601}, {0, 1605}]}, lo};
unicode_table(64706) ->
    {0, [], {initial, [{0, 1602}, {0, 1581}]}, lo};
unicode_table(64707) ->
    {0, [], {initial, [{0, 1602}, {0, 1605}]}, lo};
unicode_table(64708) ->
    {0, [], {initial, [{0, 1603}, {0, 1580}]}, lo};
unicode_table(64709) ->
    {0, [], {initial, [{0, 1603}, {0, 1581}]}, lo};
unicode_table(64710) ->
    {0, [], {initial, [{0, 1603}, {0, 1582}]}, lo};
unicode_table(64711) ->
    {0, [], {initial, [{0, 1603}, {0, 1604}]}, lo};
unicode_table(64712) ->
    {0, [], {initial, [{0, 1603}, {0, 1605}]}, lo};
unicode_table(64713) ->
    {0, [], {initial, [{0, 1604}, {0, 1580}]}, lo};
unicode_table(64714) ->
    {0, [], {initial, [{0, 1604}, {0, 1581}]}, lo};
unicode_table(64715) ->
    {0, [], {initial, [{0, 1604}, {0, 1582}]}, lo};
unicode_table(64716) ->
    {0, [], {initial, [{0, 1604}, {0, 1605}]}, lo};
unicode_table(64717) ->
    {0, [], {initial, [{0, 1604}, {0, 1607}]}, lo};
unicode_table(64718) ->
    {0, [], {initial, [{0, 1605}, {0, 1580}]}, lo};
unicode_table(64719) ->
    {0, [], {initial, [{0, 1605}, {0, 1581}]}, lo};
unicode_table(64720) ->
    {0, [], {initial, [{0, 1605}, {0, 1582}]}, lo};
unicode_table(64721) ->
    {0, [], {initial, [{0, 1605}, {0, 1605}]}, lo};
unicode_table(64722) ->
    {0, [], {initial, [{0, 1606}, {0, 1580}]}, lo};
unicode_table(64723) ->
    {0, [], {initial, [{0, 1606}, {0, 1581}]}, lo};
unicode_table(64724) ->
    {0, [], {initial, [{0, 1606}, {0, 1582}]}, lo};
unicode_table(64725) ->
    {0, [], {initial, [{0, 1606}, {0, 1605}]}, lo};
unicode_table(64726) ->
    {0, [], {initial, [{0, 1606}, {0, 1607}]}, lo};
unicode_table(64727) ->
    {0, [], {initial, [{0, 1607}, {0, 1580}]}, lo};
unicode_table(64728) ->
    {0, [], {initial, [{0, 1607}, {0, 1605}]}, lo};
unicode_table(64729) ->
    {0, [], {initial, [{0, 1607}, {35, 1648}]}, lo};
unicode_table(64730) ->
    {0, [], {initial, [{0, 1610}, {0, 1580}]}, lo};
unicode_table(64731) ->
    {0, [], {initial, [{0, 1610}, {0, 1581}]}, lo};
unicode_table(64732) ->
    {0, [], {initial, [{0, 1610}, {0, 1582}]}, lo};
unicode_table(64733) ->
    {0, [], {initial, [{0, 1610}, {0, 1605}]}, lo};
unicode_table(64734) ->
    {0, [], {initial, [{0, 1610}, {0, 1607}]}, lo};
unicode_table(64735) ->
    {0, [], {medial, [{0, 1610}, {230, 1620}, {0, 1605}]}, lo};
unicode_table(64736) ->
    {0, [], {medial, [{0, 1610}, {230, 1620}, {0, 1607}]}, lo};
unicode_table(64737) ->
    {0, [], {medial, [{0, 1576}, {0, 1605}]}, lo};
unicode_table(64738) ->
    {0, [], {medial, [{0, 1576}, {0, 1607}]}, lo};
unicode_table(64739) ->
    {0, [], {medial, [{0, 1578}, {0, 1605}]}, lo};
unicode_table(64740) ->
    {0, [], {medial, [{0, 1578}, {0, 1607}]}, lo};
unicode_table(64741) ->
    {0, [], {medial, [{0, 1579}, {0, 1605}]}, lo};
unicode_table(64742) ->
    {0, [], {medial, [{0, 1579}, {0, 1607}]}, lo};
unicode_table(64743) ->
    {0, [], {medial, [{0, 1587}, {0, 1605}]}, lo};
unicode_table(64744) ->
    {0, [], {medial, [{0, 1587}, {0, 1607}]}, lo};
unicode_table(64745) ->
    {0, [], {medial, [{0, 1588}, {0, 1605}]}, lo};
unicode_table(64746) ->
    {0, [], {medial, [{0, 1588}, {0, 1607}]}, lo};
unicode_table(64747) ->
    {0, [], {medial, [{0, 1603}, {0, 1604}]}, lo};
unicode_table(64748) ->
    {0, [], {medial, [{0, 1603}, {0, 1605}]}, lo};
unicode_table(64749) ->
    {0, [], {medial, [{0, 1604}, {0, 1605}]}, lo};
unicode_table(64750) ->
    {0, [], {medial, [{0, 1606}, {0, 1605}]}, lo};
unicode_table(64751) ->
    {0, [], {medial, [{0, 1606}, {0, 1607}]}, lo};
unicode_table(64752) ->
    {0, [], {medial, [{0, 1610}, {0, 1605}]}, lo};
unicode_table(64753) ->
    {0, [], {medial, [{0, 1610}, {0, 1607}]}, lo};
unicode_table(64754) ->
    {0, [], {medial, [{0, 1600}, {30, 1614}, {33, 1617}]}, lo};
unicode_table(64755) ->
    {0, [], {medial, [{0, 1600}, {31, 1615}, {33, 1617}]}, lo};
unicode_table(64756) ->
    {0, [], {medial, [{0, 1600}, {32, 1616}, {33, 1617}]}, lo};
unicode_table(64757) ->
    {0, [], {isolated, [{0, 1591}, {0, 1609}]}, lo};
unicode_table(64758) ->
    {0, [], {isolated, [{0, 1591}, {0, 1610}]}, lo};
unicode_table(64759) ->
    {0, [], {isolated, [{0, 1593}, {0, 1609}]}, lo};
unicode_table(64760) ->
    {0, [], {isolated, [{0, 1593}, {0, 1610}]}, lo};
unicode_table(64761) ->
    {0, [], {isolated, [{0, 1594}, {0, 1609}]}, lo};
unicode_table(64762) ->
    {0, [], {isolated, [{0, 1594}, {0, 1610}]}, lo};
unicode_table(64763) ->
    {0, [], {isolated, [{0, 1587}, {0, 1609}]}, lo};
unicode_table(64764) ->
    {0, [], {isolated, [{0, 1587}, {0, 1610}]}, lo};
unicode_table(64765) ->
    {0, [], {isolated, [{0, 1588}, {0, 1609}]}, lo};
unicode_table(64766) ->
    {0, [], {isolated, [{0, 1588}, {0, 1610}]}, lo};
unicode_table(64767) ->
    {0, [], {isolated, [{0, 1581}, {0, 1609}]}, lo};
unicode_table(64768) ->
    {0, [], {isolated, [{0, 1581}, {0, 1610}]}, lo};
unicode_table(64769) ->
    {0, [], {isolated, [{0, 1580}, {0, 1609}]}, lo};
unicode_table(64770) ->
    {0, [], {isolated, [{0, 1580}, {0, 1610}]}, lo};
unicode_table(64771) ->
    {0, [], {isolated, [{0, 1582}, {0, 1609}]}, lo};
unicode_table(64772) ->
    {0, [], {isolated, [{0, 1582}, {0, 1610}]}, lo};
unicode_table(64773) ->
    {0, [], {isolated, [{0, 1589}, {0, 1609}]}, lo};
unicode_table(64774) ->
    {0, [], {isolated, [{0, 1589}, {0, 1610}]}, lo};
unicode_table(64775) ->
    {0, [], {isolated, [{0, 1590}, {0, 1609}]}, lo};
unicode_table(64776) ->
    {0, [], {isolated, [{0, 1590}, {0, 1610}]}, lo};
unicode_table(64777) ->
    {0, [], {isolated, [{0, 1588}, {0, 1580}]}, lo};
unicode_table(64778) ->
    {0, [], {isolated, [{0, 1588}, {0, 1581}]}, lo};
unicode_table(64779) ->
    {0, [], {isolated, [{0, 1588}, {0, 1582}]}, lo};
unicode_table(64780) ->
    {0, [], {isolated, [{0, 1588}, {0, 1605}]}, lo};
unicode_table(64781) ->
    {0, [], {isolated, [{0, 1588}, {0, 1585}]}, lo};
unicode_table(64782) ->
    {0, [], {isolated, [{0, 1587}, {0, 1585}]}, lo};
unicode_table(64783) ->
    {0, [], {isolated, [{0, 1589}, {0, 1585}]}, lo};
unicode_table(64784) ->
    {0, [], {isolated, [{0, 1590}, {0, 1585}]}, lo};
unicode_table(64785) ->
    {0, [], {final, [{0, 1591}, {0, 1609}]}, lo};
unicode_table(64786) ->
    {0, [], {final, [{0, 1591}, {0, 1610}]}, lo};
unicode_table(64787) ->
    {0, [], {final, [{0, 1593}, {0, 1609}]}, lo};
unicode_table(64788) ->
    {0, [], {final, [{0, 1593}, {0, 1610}]}, lo};
unicode_table(64789) ->
    {0, [], {final, [{0, 1594}, {0, 1609}]}, lo};
unicode_table(64790) ->
    {0, [], {final, [{0, 1594}, {0, 1610}]}, lo};
unicode_table(64791) ->
    {0, [], {final, [{0, 1587}, {0, 1609}]}, lo};
unicode_table(64792) ->
    {0, [], {final, [{0, 1587}, {0, 1610}]}, lo};
unicode_table(64793) ->
    {0, [], {final, [{0, 1588}, {0, 1609}]}, lo};
unicode_table(64794) ->
    {0, [], {final, [{0, 1588}, {0, 1610}]}, lo};
unicode_table(64795) ->
    {0, [], {final, [{0, 1581}, {0, 1609}]}, lo};
unicode_table(64796) ->
    {0, [], {final, [{0, 1581}, {0, 1610}]}, lo};
unicode_table(64797) ->
    {0, [], {final, [{0, 1580}, {0, 1609}]}, lo};
unicode_table(64798) ->
    {0, [], {final, [{0, 1580}, {0, 1610}]}, lo};
unicode_table(64799) ->
    {0, [], {final, [{0, 1582}, {0, 1609}]}, lo};
unicode_table(64800) ->
    {0, [], {final, [{0, 1582}, {0, 1610}]}, lo};
unicode_table(64801) ->
    {0, [], {final, [{0, 1589}, {0, 1609}]}, lo};
unicode_table(64802) ->
    {0, [], {final, [{0, 1589}, {0, 1610}]}, lo};
unicode_table(64803) ->
    {0, [], {final, [{0, 1590}, {0, 1609}]}, lo};
unicode_table(64804) ->
    {0, [], {final, [{0, 1590}, {0, 1610}]}, lo};
unicode_table(64805) ->
    {0, [], {final, [{0, 1588}, {0, 1580}]}, lo};
unicode_table(64806) ->
    {0, [], {final, [{0, 1588}, {0, 1581}]}, lo};
unicode_table(64807) ->
    {0, [], {final, [{0, 1588}, {0, 1582}]}, lo};
unicode_table(64808) ->
    {0, [], {final, [{0, 1588}, {0, 1605}]}, lo};
unicode_table(64809) ->
    {0, [], {final, [{0, 1588}, {0, 1585}]}, lo};
unicode_table(64810) ->
    {0, [], {final, [{0, 1587}, {0, 1585}]}, lo};
unicode_table(64811) ->
    {0, [], {final, [{0, 1589}, {0, 1585}]}, lo};
unicode_table(64812) ->
    {0, [], {final, [{0, 1590}, {0, 1585}]}, lo};
unicode_table(64813) ->
    {0, [], {initial, [{0, 1588}, {0, 1580}]}, lo};
unicode_table(64814) ->
    {0, [], {initial, [{0, 1588}, {0, 1581}]}, lo};
unicode_table(64815) ->
    {0, [], {initial, [{0, 1588}, {0, 1582}]}, lo};
unicode_table(64816) ->
    {0, [], {initial, [{0, 1588}, {0, 1605}]}, lo};
unicode_table(64817) ->
    {0, [], {initial, [{0, 1587}, {0, 1607}]}, lo};
unicode_table(64818) ->
    {0, [], {initial, [{0, 1588}, {0, 1607}]}, lo};
unicode_table(64819) ->
    {0, [], {initial, [{0, 1591}, {0, 1605}]}, lo};
unicode_table(64820) ->
    {0, [], {medial, [{0, 1587}, {0, 1580}]}, lo};
unicode_table(64821) ->
    {0, [], {medial, [{0, 1587}, {0, 1581}]}, lo};
unicode_table(64822) ->
    {0, [], {medial, [{0, 1587}, {0, 1582}]}, lo};
unicode_table(64823) ->
    {0, [], {medial, [{0, 1588}, {0, 1580}]}, lo};
unicode_table(64824) ->
    {0, [], {medial, [{0, 1588}, {0, 1581}]}, lo};
unicode_table(64825) ->
    {0, [], {medial, [{0, 1588}, {0, 1582}]}, lo};
unicode_table(64826) ->
    {0, [], {medial, [{0, 1591}, {0, 1605}]}, lo};
unicode_table(64827) ->
    {0, [], {medial, [{0, 1592}, {0, 1605}]}, lo};
unicode_table(64828) ->
    {0, [], {final, [{0, 1575}, {27, 1611}]}, lo};
unicode_table(64829) ->
    {0, [], {isolated, [{0, 1575}, {27, 1611}]}, lo};
unicode_table(64848) ->
    {0, [], {initial, [{0, 1578}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64849) ->
    {0, [], {final, [{0, 1578}, {0, 1581}, {0, 1580}]}, lo};
unicode_table(64850) ->
    {0, [], {initial, [{0, 1578}, {0, 1581}, {0, 1580}]}, lo};
unicode_table(64851) ->
    {0, [], {initial, [{0, 1578}, {0, 1581}, {0, 1605}]}, lo};
unicode_table(64852) ->
    {0, [], {initial, [{0, 1578}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64853) ->
    {0, [], {initial, [{0, 1578}, {0, 1605}, {0, 1580}]}, lo};
unicode_table(64854) ->
    {0, [], {initial, [{0, 1578}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64855) ->
    {0, [], {initial, [{0, 1578}, {0, 1605}, {0, 1582}]}, lo};
unicode_table(64856) ->
    {0, [], {final, [{0, 1580}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64857) ->
    {0, [], {initial, [{0, 1580}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64858) ->
    {0, [], {final, [{0, 1581}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64859) ->
    {0, [], {final, [{0, 1581}, {0, 1605}, {0, 1609}]}, lo};
unicode_table(64860) ->
    {0, [], {initial, [{0, 1587}, {0, 1581}, {0, 1580}]}, lo};
unicode_table(64861) ->
    {0, [], {initial, [{0, 1587}, {0, 1580}, {0, 1581}]}, lo};
unicode_table(64862) ->
    {0, [], {final, [{0, 1587}, {0, 1580}, {0, 1609}]}, lo};
unicode_table(64863) ->
    {0, [], {final, [{0, 1587}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64864) ->
    {0, [], {initial, [{0, 1587}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64865) ->
    {0, [], {initial, [{0, 1587}, {0, 1605}, {0, 1580}]}, lo};
unicode_table(64866) ->
    {0, [], {final, [{0, 1587}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64867) ->
    {0, [], {initial, [{0, 1587}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64868) ->
    {0, [], {final, [{0, 1589}, {0, 1581}, {0, 1581}]}, lo};
unicode_table(64869) ->
    {0, [], {initial, [{0, 1589}, {0, 1581}, {0, 1581}]}, lo};
unicode_table(64870) ->
    {0, [], {final, [{0, 1589}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64871) ->
    {0, [], {final, [{0, 1588}, {0, 1581}, {0, 1605}]}, lo};
unicode_table(64872) ->
    {0, [], {initial, [{0, 1588}, {0, 1581}, {0, 1605}]}, lo};
unicode_table(64873) ->
    {0, [], {final, [{0, 1588}, {0, 1580}, {0, 1610}]}, lo};
unicode_table(64874) ->
    {0, [], {final, [{0, 1588}, {0, 1605}, {0, 1582}]}, lo};
unicode_table(64875) ->
    {0, [], {initial, [{0, 1588}, {0, 1605}, {0, 1582}]}, lo};
unicode_table(64876) ->
    {0, [], {final, [{0, 1588}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64877) ->
    {0, [], {initial, [{0, 1588}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64878) ->
    {0, [], {final, [{0, 1590}, {0, 1581}, {0, 1609}]}, lo};
unicode_table(64879) ->
    {0, [], {final, [{0, 1590}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64880) ->
    {0, [], {initial, [{0, 1590}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64881) ->
    {0, [], {final, [{0, 1591}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64882) ->
    {0, [], {initial, [{0, 1591}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64883) ->
    {0, [], {initial, [{0, 1591}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64884) ->
    {0, [], {final, [{0, 1591}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64885) ->
    {0, [], {final, [{0, 1593}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64886) ->
    {0, [], {final, [{0, 1593}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64887) ->
    {0, [], {initial, [{0, 1593}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64888) ->
    {0, [], {final, [{0, 1593}, {0, 1605}, {0, 1609}]}, lo};
unicode_table(64889) ->
    {0, [], {final, [{0, 1594}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64890) ->
    {0, [], {final, [{0, 1594}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64891) ->
    {0, [], {final, [{0, 1594}, {0, 1605}, {0, 1609}]}, lo};
unicode_table(64892) ->
    {0, [], {final, [{0, 1601}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64893) ->
    {0, [], {initial, [{0, 1601}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64894) ->
    {0, [], {final, [{0, 1602}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64895) ->
    {0, [], {final, [{0, 1602}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64896) ->
    {0, [], {final, [{0, 1604}, {0, 1581}, {0, 1605}]}, lo};
unicode_table(64897) ->
    {0, [], {final, [{0, 1604}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64898) ->
    {0, [], {final, [{0, 1604}, {0, 1581}, {0, 1609}]}, lo};
unicode_table(64899) ->
    {0, [], {initial, [{0, 1604}, {0, 1580}, {0, 1580}]}, lo};
unicode_table(64900) ->
    {0, [], {final, [{0, 1604}, {0, 1580}, {0, 1580}]}, lo};
unicode_table(64901) ->
    {0, [], {final, [{0, 1604}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64902) ->
    {0, [], {initial, [{0, 1604}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64903) ->
    {0, [], {final, [{0, 1604}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64904) ->
    {0, [], {initial, [{0, 1604}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64905) ->
    {0, [], {initial, [{0, 1605}, {0, 1581}, {0, 1580}]}, lo};
unicode_table(64906) ->
    {0, [], {initial, [{0, 1605}, {0, 1581}, {0, 1605}]}, lo};
unicode_table(64907) ->
    {0, [], {final, [{0, 1605}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64908) ->
    {0, [], {initial, [{0, 1605}, {0, 1580}, {0, 1581}]}, lo};
unicode_table(64909) ->
    {0, [], {initial, [{0, 1605}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64910) ->
    {0, [], {initial, [{0, 1605}, {0, 1582}, {0, 1580}]}, lo};
unicode_table(64911) ->
    {0, [], {initial, [{0, 1605}, {0, 1582}, {0, 1605}]}, lo};
unicode_table(64914) ->
    {0, [], {initial, [{0, 1605}, {0, 1580}, {0, 1582}]}, lo};
unicode_table(64915) ->
    {0, [], {initial, [{0, 1607}, {0, 1605}, {0, 1580}]}, lo};
unicode_table(64916) ->
    {0, [], {initial, [{0, 1607}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64917) ->
    {0, [], {initial, [{0, 1606}, {0, 1581}, {0, 1605}]}, lo};
unicode_table(64918) ->
    {0, [], {final, [{0, 1606}, {0, 1581}, {0, 1609}]}, lo};
unicode_table(64919) ->
    {0, [], {final, [{0, 1606}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64920) ->
    {0, [], {initial, [{0, 1606}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64921) ->
    {0, [], {final, [{0, 1606}, {0, 1580}, {0, 1609}]}, lo};
unicode_table(64922) ->
    {0, [], {final, [{0, 1606}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64923) ->
    {0, [], {final, [{0, 1606}, {0, 1605}, {0, 1609}]}, lo};
unicode_table(64924) ->
    {0, [], {final, [{0, 1610}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64925) ->
    {0, [], {initial, [{0, 1610}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64926) ->
    {0, [], {final, [{0, 1576}, {0, 1582}, {0, 1610}]}, lo};
unicode_table(64927) ->
    {0, [], {final, [{0, 1578}, {0, 1580}, {0, 1610}]}, lo};
unicode_table(64928) ->
    {0, [], {final, [{0, 1578}, {0, 1580}, {0, 1609}]}, lo};
unicode_table(64929) ->
    {0, [], {final, [{0, 1578}, {0, 1582}, {0, 1610}]}, lo};
unicode_table(64930) ->
    {0, [], {final, [{0, 1578}, {0, 1582}, {0, 1609}]}, lo};
unicode_table(64931) ->
    {0, [], {final, [{0, 1578}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64932) ->
    {0, [], {final, [{0, 1578}, {0, 1605}, {0, 1609}]}, lo};
unicode_table(64933) ->
    {0, [], {final, [{0, 1580}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64934) ->
    {0, [], {final, [{0, 1580}, {0, 1581}, {0, 1609}]}, lo};
unicode_table(64935) ->
    {0, [], {final, [{0, 1580}, {0, 1605}, {0, 1609}]}, lo};
unicode_table(64936) ->
    {0, [], {final, [{0, 1587}, {0, 1582}, {0, 1609}]}, lo};
unicode_table(64937) ->
    {0, [], {final, [{0, 1589}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64938) ->
    {0, [], {final, [{0, 1588}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64939) ->
    {0, [], {final, [{0, 1590}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64940) ->
    {0, [], {final, [{0, 1604}, {0, 1580}, {0, 1610}]}, lo};
unicode_table(64941) ->
    {0, [], {final, [{0, 1604}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64942) ->
    {0, [], {final, [{0, 1610}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64943) ->
    {0, [], {final, [{0, 1610}, {0, 1580}, {0, 1610}]}, lo};
unicode_table(64944) ->
    {0, [], {final, [{0, 1610}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64945) ->
    {0, [], {final, [{0, 1605}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64946) ->
    {0, [], {final, [{0, 1602}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64947) ->
    {0, [], {final, [{0, 1606}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64948) ->
    {0, [], {initial, [{0, 1602}, {0, 1605}, {0, 1581}]}, lo};
unicode_table(64949) ->
    {0, [], {initial, [{0, 1604}, {0, 1581}, {0, 1605}]}, lo};
unicode_table(64950) ->
    {0, [], {final, [{0, 1593}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64951) ->
    {0, [], {final, [{0, 1603}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64952) ->
    {0, [], {initial, [{0, 1606}, {0, 1580}, {0, 1581}]}, lo};
unicode_table(64953) ->
    {0, [], {final, [{0, 1605}, {0, 1582}, {0, 1610}]}, lo};
unicode_table(64954) ->
    {0, [], {initial, [{0, 1604}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64955) ->
    {0, [], {final, [{0, 1603}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64956) ->
    {0, [], {final, [{0, 1604}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64957) ->
    {0, [], {final, [{0, 1606}, {0, 1580}, {0, 1581}]}, lo};
unicode_table(64958) ->
    {0, [], {final, [{0, 1580}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64959) ->
    {0, [], {final, [{0, 1581}, {0, 1580}, {0, 1610}]}, lo};
unicode_table(64960) ->
    {0, [], {final, [{0, 1605}, {0, 1580}, {0, 1610}]}, lo};
unicode_table(64961) ->
    {0, [], {final, [{0, 1601}, {0, 1605}, {0, 1610}]}, lo};
unicode_table(64962) ->
    {0, [], {final, [{0, 1576}, {0, 1581}, {0, 1610}]}, lo};
unicode_table(64963) ->
    {0, [], {initial, [{0, 1603}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64964) ->
    {0, [], {initial, [{0, 1593}, {0, 1580}, {0, 1605}]}, lo};
unicode_table(64965) ->
    {0, [], {initial, [{0, 1589}, {0, 1605}, {0, 1605}]}, lo};
unicode_table(64966) ->
    {0, [], {final, [{0, 1587}, {0, 1582}, {0, 1610}]}, lo};
unicode_table(64967) ->
    {0, [], {final, [{0, 1606}, {0, 1580}, {0, 1610}]}, lo};
unicode_table(65008) ->
    {0, [], {isolated, [{0, 1589}, {0, 1604}, {0, 1746}]}, lo};
unicode_table(65009) ->
    {0, [], {isolated, [{0, 1602}, {0, 1604}, {0, 1746}]}, lo};
unicode_table(65010) ->
    {0, [], {isolated, [{0, 1575}, {0, 1604}, {0, 1604}, {0, 1607}]}, lo};
unicode_table(65011) ->
    {0, [], {isolated, [{0, 1575}, {0, 1603}, {0, 1576}, {0, 1585}]}, lo};
unicode_table(65012) ->
    {0, [], {isolated, [{0, 1605}, {0, 1581}, {0, 1605}, {0, 1583}]}, lo};
unicode_table(65013) ->
    {0, [], {isolated, [{0, 1589}, {0, 1604}, {0, 1593}, {0, 1605}]}, lo};
unicode_table(65014) ->
    {0, [], {isolated, [{0, 1585}, {0, 1587}, {0, 1608}, {0, 1604}]}, lo};
unicode_table(65015) ->
    {0, [], {isolated, [{0, 1593}, {0, 1604}, {0, 1610}, {0, 1607}]}, lo};
unicode_table(65016) ->
    {0, [], {isolated, [{0, 1608}, {0, 1587}, {0, 1604}, {0, 1605}]}, lo};
unicode_table(65017) ->
    {0, [], {isolated, [{0, 1589}, {0, 1604}, {0, 1609}]}, lo};
unicode_table(65018) ->
    {0, [],
        {isolated, [
            {0, 1589},
            {0, 1604},
            {0, 1609},
            {0, 32},
            {0, 1575},
            {0, 1604},
            {0, 1604},
            {0, 1607},
            {0, 32},
            {0, 1593},
            {0, 1604},
            {0, 1610},
            {0, 1607},
            {0, 32},
            {0, 1608},
            {0, 1587},
            {0, 1604},
            {0, 1605}
        ]},
        lo};
unicode_table(65019) ->
    {0, [],
        {isolated, [
            {0, 1580}, {0, 1604}, {0, 32}, {0, 1580}, {0, 1604}, {0, 1575}, {0, 1604}, {0, 1607}
        ]},
        lo};
unicode_table(65020) ->
    {0, [], {isolated, [{0, 1585}, {0, 1740}, {0, 1575}, {0, 1604}]}, sc};
unicode_table(65040) ->
    {0, [], {vertical, [{0, 44}]}, po};
unicode_table(65041) ->
    {0, [], {vertical, [{0, 12289}]}, po};
unicode_table(65042) ->
    {0, [], {vertical, [{0, 12290}]}, po};
unicode_table(65043) ->
    {0, [], {vertical, [{0, 58}]}, po};
unicode_table(65044) ->
    {0, [], {vertical, [{0, 59}]}, po};
unicode_table(65045) ->
    {0, [], {vertical, [{0, 33}]}, po};
unicode_table(65046) ->
    {0, [], {vertical, [{0, 63}]}, po};
unicode_table(65047) ->
    {0, [], {vertical, [{0, 12310}]}, ps};
unicode_table(65048) ->
    {0, [], {vertical, [{0, 12311}]}, pe};
unicode_table(65049) ->
    {0, [], {vertical, [{0, 46}, {0, 46}, {0, 46}]}, po};
unicode_table(65056) ->
    {230, [], [], mn};
unicode_table(65057) ->
    {230, [], [], mn};
unicode_table(65058) ->
    {230, [], [], mn};
unicode_table(65059) ->
    {230, [], [], mn};
unicode_table(65060) ->
    {230, [], [], mn};
unicode_table(65061) ->
    {230, [], [], mn};
unicode_table(65062) ->
    {230, [], [], mn};
unicode_table(65063) ->
    {220, [], [], mn};
unicode_table(65064) ->
    {220, [], [], mn};
unicode_table(65065) ->
    {220, [], [], mn};
unicode_table(65066) ->
    {220, [], [], mn};
unicode_table(65067) ->
    {220, [], [], mn};
unicode_table(65068) ->
    {220, [], [], mn};
unicode_table(65069) ->
    {220, [], [], mn};
unicode_table(65070) ->
    {230, [], [], mn};
unicode_table(65071) ->
    {230, [], [], mn};
unicode_table(65072) ->
    {0, [], {vertical, [{0, 46}, {0, 46}]}, po};
unicode_table(65073) ->
    {0, [], {vertical, [{0, 8212}]}, pd};
unicode_table(65074) ->
    {0, [], {vertical, [{0, 8211}]}, pd};
unicode_table(65075) ->
    {0, [], {vertical, [{0, 95}]}, pc};
unicode_table(65076) ->
    {0, [], {vertical, [{0, 95}]}, pc};
unicode_table(65077) ->
    {0, [], {vertical, [{0, 40}]}, ps};
unicode_table(65078) ->
    {0, [], {vertical, [{0, 41}]}, pe};
unicode_table(65079) ->
    {0, [], {vertical, [{0, 123}]}, ps};
unicode_table(65080) ->
    {0, [], {vertical, [{0, 125}]}, pe};
unicode_table(65081) ->
    {0, [], {vertical, [{0, 12308}]}, ps};
unicode_table(65082) ->
    {0, [], {vertical, [{0, 12309}]}, pe};
unicode_table(65083) ->
    {0, [], {vertical, [{0, 12304}]}, ps};
unicode_table(65084) ->
    {0, [], {vertical, [{0, 12305}]}, pe};
unicode_table(65085) ->
    {0, [], {vertical, [{0, 12298}]}, ps};
unicode_table(65086) ->
    {0, [], {vertical, [{0, 12299}]}, pe};
unicode_table(65087) ->
    {0, [], {vertical, [{0, 12296}]}, ps};
unicode_table(65088) ->
    {0, [], {vertical, [{0, 12297}]}, pe};
unicode_table(65089) ->
    {0, [], {vertical, [{0, 12300}]}, ps};
unicode_table(65090) ->
    {0, [], {vertical, [{0, 12301}]}, pe};
unicode_table(65091) ->
    {0, [], {vertical, [{0, 12302}]}, ps};
unicode_table(65092) ->
    {0, [], {vertical, [{0, 12303}]}, pe};
unicode_table(65095) ->
    {0, [], {vertical, [{0, 91}]}, ps};
unicode_table(65096) ->
    {0, [], {vertical, [{0, 93}]}, pe};
unicode_table(65097) ->
    {0, [], {compat, [{0, 32}, {230, 773}]}, po};
unicode_table(65098) ->
    {0, [], {compat, [{0, 32}, {230, 773}]}, po};
unicode_table(65099) ->
    {0, [], {compat, [{0, 32}, {230, 773}]}, po};
unicode_table(65100) ->
    {0, [], {compat, [{0, 32}, {230, 773}]}, po};
unicode_table(65101) ->
    {0, [], {compat, [{0, 95}]}, pc};
unicode_table(65102) ->
    {0, [], {compat, [{0, 95}]}, pc};
unicode_table(65103) ->
    {0, [], {compat, [{0, 95}]}, pc};
unicode_table(65104) ->
    {0, [], {small, [{0, 44}]}, po};
unicode_table(65105) ->
    {0, [], {small, [{0, 12289}]}, po};
unicode_table(65106) ->
    {0, [], {small, [{0, 46}]}, po};
unicode_table(65108) ->
    {0, [], {small, [{0, 59}]}, po};
unicode_table(65109) ->
    {0, [], {small, [{0, 58}]}, po};
unicode_table(65110) ->
    {0, [], {small, [{0, 63}]}, po};
unicode_table(65111) ->
    {0, [], {small, [{0, 33}]}, po};
unicode_table(65112) ->
    {0, [], {small, [{0, 8212}]}, pd};
unicode_table(65113) ->
    {0, [], {small, [{0, 40}]}, ps};
unicode_table(65114) ->
    {0, [], {small, [{0, 41}]}, pe};
unicode_table(65115) ->
    {0, [], {small, [{0, 123}]}, ps};
unicode_table(65116) ->
    {0, [], {small, [{0, 125}]}, pe};
unicode_table(65117) ->
    {0, [], {small, [{0, 12308}]}, ps};
unicode_table(65118) ->
    {0, [], {small, [{0, 12309}]}, pe};
unicode_table(65119) ->
    {0, [], {small, [{0, 35}]}, po};
unicode_table(65120) ->
    {0, [], {small, [{0, 38}]}, po};
unicode_table(65121) ->
    {0, [], {small, [{0, 42}]}, po};
unicode_table(65122) ->
    {0, [], {small, [{0, 43}]}, sm};
unicode_table(65123) ->
    {0, [], {small, [{0, 45}]}, pd};
unicode_table(65124) ->
    {0, [], {small, [{0, 60}]}, sm};
unicode_table(65125) ->
    {0, [], {small, [{0, 62}]}, sm};
unicode_table(65126) ->
    {0, [], {small, [{0, 61}]}, sm};
unicode_table(65128) ->
    {0, [], {small, [{0, 92}]}, po};
unicode_table(65129) ->
    {0, [], {small, [{0, 36}]}, sc};
unicode_table(65130) ->
    {0, [], {small, [{0, 37}]}, po};
unicode_table(65131) ->
    {0, [], {small, [{0, 64}]}, po};
unicode_table(65136) ->
    {0, [], {isolated, [{0, 32}, {27, 1611}]}, lo};
unicode_table(65137) ->
    {0, [], {medial, [{0, 1600}, {27, 1611}]}, lo};
unicode_table(65138) ->
    {0, [], {isolated, [{0, 32}, {28, 1612}]}, lo};
unicode_table(65140) ->
    {0, [], {isolated, [{0, 32}, {29, 1613}]}, lo};
unicode_table(65142) ->
    {0, [], {isolated, [{0, 32}, {30, 1614}]}, lo};
unicode_table(65143) ->
    {0, [], {medial, [{0, 1600}, {30, 1614}]}, lo};
unicode_table(65144) ->
    {0, [], {isolated, [{0, 32}, {31, 1615}]}, lo};
unicode_table(65145) ->
    {0, [], {medial, [{0, 1600}, {31, 1615}]}, lo};
unicode_table(65146) ->
    {0, [], {isolated, [{0, 32}, {32, 1616}]}, lo};
unicode_table(65147) ->
    {0, [], {medial, [{0, 1600}, {32, 1616}]}, lo};
unicode_table(65148) ->
    {0, [], {isolated, [{0, 32}, {33, 1617}]}, lo};
unicode_table(65149) ->
    {0, [], {medial, [{0, 1600}, {33, 1617}]}, lo};
unicode_table(65150) ->
    {0, [], {isolated, [{0, 32}, {34, 1618}]}, lo};
unicode_table(65151) ->
    {0, [], {medial, [{0, 1600}, {34, 1618}]}, lo};
unicode_table(65152) ->
    {0, [], {isolated, [{0, 1569}]}, lo};
unicode_table(65153) ->
    {0, [], {isolated, [{0, 1575}, {230, 1619}]}, lo};
unicode_table(65154) ->
    {0, [], {final, [{0, 1575}, {230, 1619}]}, lo};
unicode_table(65155) ->
    {0, [], {isolated, [{0, 1575}, {230, 1620}]}, lo};
unicode_table(65156) ->
    {0, [], {final, [{0, 1575}, {230, 1620}]}, lo};
unicode_table(65157) ->
    {0, [], {isolated, [{0, 1608}, {230, 1620}]}, lo};
unicode_table(65158) ->
    {0, [], {final, [{0, 1608}, {230, 1620}]}, lo};
unicode_table(65159) ->
    {0, [], {isolated, [{0, 1575}, {220, 1621}]}, lo};
unicode_table(65160) ->
    {0, [], {final, [{0, 1575}, {220, 1621}]}, lo};
unicode_table(65161) ->
    {0, [], {isolated, [{0, 1610}, {230, 1620}]}, lo};
unicode_table(65162) ->
    {0, [], {final, [{0, 1610}, {230, 1620}]}, lo};
unicode_table(65163) ->
    {0, [], {initial, [{0, 1610}, {230, 1620}]}, lo};
unicode_table(65164) ->
    {0, [], {medial, [{0, 1610}, {230, 1620}]}, lo};
unicode_table(65165) ->
    {0, [], {isolated, [{0, 1575}]}, lo};
unicode_table(65166) ->
    {0, [], {final, [{0, 1575}]}, lo};
unicode_table(65167) ->
    {0, [], {isolated, [{0, 1576}]}, lo};
unicode_table(65168) ->
    {0, [], {final, [{0, 1576}]}, lo};
unicode_table(65169) ->
    {0, [], {initial, [{0, 1576}]}, lo};
unicode_table(65170) ->
    {0, [], {medial, [{0, 1576}]}, lo};
unicode_table(65171) ->
    {0, [], {isolated, [{0, 1577}]}, lo};
unicode_table(65172) ->
    {0, [], {final, [{0, 1577}]}, lo};
unicode_table(65173) ->
    {0, [], {isolated, [{0, 1578}]}, lo};
unicode_table(65174) ->
    {0, [], {final, [{0, 1578}]}, lo};
unicode_table(65175) ->
    {0, [], {initial, [{0, 1578}]}, lo};
unicode_table(65176) ->
    {0, [], {medial, [{0, 1578}]}, lo};
unicode_table(65177) ->
    {0, [], {isolated, [{0, 1579}]}, lo};
unicode_table(65178) ->
    {0, [], {final, [{0, 1579}]}, lo};
unicode_table(65179) ->
    {0, [], {initial, [{0, 1579}]}, lo};
unicode_table(65180) ->
    {0, [], {medial, [{0, 1579}]}, lo};
unicode_table(65181) ->
    {0, [], {isolated, [{0, 1580}]}, lo};
unicode_table(65182) ->
    {0, [], {final, [{0, 1580}]}, lo};
unicode_table(65183) ->
    {0, [], {initial, [{0, 1580}]}, lo};
unicode_table(65184) ->
    {0, [], {medial, [{0, 1580}]}, lo};
unicode_table(65185) ->
    {0, [], {isolated, [{0, 1581}]}, lo};
unicode_table(65186) ->
    {0, [], {final, [{0, 1581}]}, lo};
unicode_table(65187) ->
    {0, [], {initial, [{0, 1581}]}, lo};
unicode_table(65188) ->
    {0, [], {medial, [{0, 1581}]}, lo};
unicode_table(65189) ->
    {0, [], {isolated, [{0, 1582}]}, lo};
unicode_table(65190) ->
    {0, [], {final, [{0, 1582}]}, lo};
unicode_table(65191) ->
    {0, [], {initial, [{0, 1582}]}, lo};
unicode_table(65192) ->
    {0, [], {medial, [{0, 1582}]}, lo};
unicode_table(65193) ->
    {0, [], {isolated, [{0, 1583}]}, lo};
unicode_table(65194) ->
    {0, [], {final, [{0, 1583}]}, lo};
unicode_table(65195) ->
    {0, [], {isolated, [{0, 1584}]}, lo};
unicode_table(65196) ->
    {0, [], {final, [{0, 1584}]}, lo};
unicode_table(65197) ->
    {0, [], {isolated, [{0, 1585}]}, lo};
unicode_table(65198) ->
    {0, [], {final, [{0, 1585}]}, lo};
unicode_table(65199) ->
    {0, [], {isolated, [{0, 1586}]}, lo};
unicode_table(65200) ->
    {0, [], {final, [{0, 1586}]}, lo};
unicode_table(65201) ->
    {0, [], {isolated, [{0, 1587}]}, lo};
unicode_table(65202) ->
    {0, [], {final, [{0, 1587}]}, lo};
unicode_table(65203) ->
    {0, [], {initial, [{0, 1587}]}, lo};
unicode_table(65204) ->
    {0, [], {medial, [{0, 1587}]}, lo};
unicode_table(65205) ->
    {0, [], {isolated, [{0, 1588}]}, lo};
unicode_table(65206) ->
    {0, [], {final, [{0, 1588}]}, lo};
unicode_table(65207) ->
    {0, [], {initial, [{0, 1588}]}, lo};
unicode_table(65208) ->
    {0, [], {medial, [{0, 1588}]}, lo};
unicode_table(65209) ->
    {0, [], {isolated, [{0, 1589}]}, lo};
unicode_table(65210) ->
    {0, [], {final, [{0, 1589}]}, lo};
unicode_table(65211) ->
    {0, [], {initial, [{0, 1589}]}, lo};
unicode_table(65212) ->
    {0, [], {medial, [{0, 1589}]}, lo};
unicode_table(65213) ->
    {0, [], {isolated, [{0, 1590}]}, lo};
unicode_table(65214) ->
    {0, [], {final, [{0, 1590}]}, lo};
unicode_table(65215) ->
    {0, [], {initial, [{0, 1590}]}, lo};
unicode_table(65216) ->
    {0, [], {medial, [{0, 1590}]}, lo};
unicode_table(65217) ->
    {0, [], {isolated, [{0, 1591}]}, lo};
unicode_table(65218) ->
    {0, [], {final, [{0, 1591}]}, lo};
unicode_table(65219) ->
    {0, [], {initial, [{0, 1591}]}, lo};
unicode_table(65220) ->
    {0, [], {medial, [{0, 1591}]}, lo};
unicode_table(65221) ->
    {0, [], {isolated, [{0, 1592}]}, lo};
unicode_table(65222) ->
    {0, [], {final, [{0, 1592}]}, lo};
unicode_table(65223) ->
    {0, [], {initial, [{0, 1592}]}, lo};
unicode_table(65224) ->
    {0, [], {medial, [{0, 1592}]}, lo};
unicode_table(65225) ->
    {0, [], {isolated, [{0, 1593}]}, lo};
unicode_table(65226) ->
    {0, [], {final, [{0, 1593}]}, lo};
unicode_table(65227) ->
    {0, [], {initial, [{0, 1593}]}, lo};
unicode_table(65228) ->
    {0, [], {medial, [{0, 1593}]}, lo};
unicode_table(65229) ->
    {0, [], {isolated, [{0, 1594}]}, lo};
unicode_table(65230) ->
    {0, [], {final, [{0, 1594}]}, lo};
unicode_table(65231) ->
    {0, [], {initial, [{0, 1594}]}, lo};
unicode_table(65232) ->
    {0, [], {medial, [{0, 1594}]}, lo};
unicode_table(65233) ->
    {0, [], {isolated, [{0, 1601}]}, lo};
unicode_table(65234) ->
    {0, [], {final, [{0, 1601}]}, lo};
unicode_table(65235) ->
    {0, [], {initial, [{0, 1601}]}, lo};
unicode_table(65236) ->
    {0, [], {medial, [{0, 1601}]}, lo};
unicode_table(65237) ->
    {0, [], {isolated, [{0, 1602}]}, lo};
unicode_table(65238) ->
    {0, [], {final, [{0, 1602}]}, lo};
unicode_table(65239) ->
    {0, [], {initial, [{0, 1602}]}, lo};
unicode_table(65240) ->
    {0, [], {medial, [{0, 1602}]}, lo};
unicode_table(65241) ->
    {0, [], {isolated, [{0, 1603}]}, lo};
unicode_table(65242) ->
    {0, [], {final, [{0, 1603}]}, lo};
unicode_table(65243) ->
    {0, [], {initial, [{0, 1603}]}, lo};
unicode_table(65244) ->
    {0, [], {medial, [{0, 1603}]}, lo};
unicode_table(65245) ->
    {0, [], {isolated, [{0, 1604}]}, lo};
unicode_table(65246) ->
    {0, [], {final, [{0, 1604}]}, lo};
unicode_table(65247) ->
    {0, [], {initial, [{0, 1604}]}, lo};
unicode_table(65248) ->
    {0, [], {medial, [{0, 1604}]}, lo};
unicode_table(65249) ->
    {0, [], {isolated, [{0, 1605}]}, lo};
unicode_table(65250) ->
    {0, [], {final, [{0, 1605}]}, lo};
unicode_table(65251) ->
    {0, [], {initial, [{0, 1605}]}, lo};
unicode_table(65252) ->
    {0, [], {medial, [{0, 1605}]}, lo};
unicode_table(65253) ->
    {0, [], {isolated, [{0, 1606}]}, lo};
unicode_table(65254) ->
    {0, [], {final, [{0, 1606}]}, lo};
unicode_table(65255) ->
    {0, [], {initial, [{0, 1606}]}, lo};
unicode_table(65256) ->
    {0, [], {medial, [{0, 1606}]}, lo};
unicode_table(65257) ->
    {0, [], {isolated, [{0, 1607}]}, lo};
unicode_table(65258) ->
    {0, [], {final, [{0, 1607}]}, lo};
unicode_table(65259) ->
    {0, [], {initial, [{0, 1607}]}, lo};
unicode_table(65260) ->
    {0, [], {medial, [{0, 1607}]}, lo};
unicode_table(65261) ->
    {0, [], {isolated, [{0, 1608}]}, lo};
unicode_table(65262) ->
    {0, [], {final, [{0, 1608}]}, lo};
unicode_table(65263) ->
    {0, [], {isolated, [{0, 1609}]}, lo};
unicode_table(65264) ->
    {0, [], {final, [{0, 1609}]}, lo};
unicode_table(65265) ->
    {0, [], {isolated, [{0, 1610}]}, lo};
unicode_table(65266) ->
    {0, [], {final, [{0, 1610}]}, lo};
unicode_table(65267) ->
    {0, [], {initial, [{0, 1610}]}, lo};
unicode_table(65268) ->
    {0, [], {medial, [{0, 1610}]}, lo};
unicode_table(65269) ->
    {0, [], {isolated, [{0, 1604}, {0, 1575}, {230, 1619}]}, lo};
unicode_table(65270) ->
    {0, [], {final, [{0, 1604}, {0, 1575}, {230, 1619}]}, lo};
unicode_table(65271) ->
    {0, [], {isolated, [{0, 1604}, {0, 1575}, {230, 1620}]}, lo};
unicode_table(65272) ->
    {0, [], {final, [{0, 1604}, {0, 1575}, {230, 1620}]}, lo};
unicode_table(65273) ->
    {0, [], {isolated, [{0, 1604}, {0, 1575}, {220, 1621}]}, lo};
unicode_table(65274) ->
    {0, [], {final, [{0, 1604}, {0, 1575}, {220, 1621}]}, lo};
unicode_table(65275) ->
    {0, [], {isolated, [{0, 1604}, {0, 1575}]}, lo};
unicode_table(65276) ->
    {0, [], {final, [{0, 1604}, {0, 1575}]}, lo};
unicode_table(65281) ->
    {0, [], {wide, [{0, 33}]}, po};
unicode_table(65282) ->
    {0, [], {wide, [{0, 34}]}, po};
unicode_table(65283) ->
    {0, [], {wide, [{0, 35}]}, po};
unicode_table(65284) ->
    {0, [], {wide, [{0, 36}]}, sc};
unicode_table(65285) ->
    {0, [], {wide, [{0, 37}]}, po};
unicode_table(65286) ->
    {0, [], {wide, [{0, 38}]}, po};
unicode_table(65287) ->
    {0, [], {wide, [{0, 39}]}, po};
unicode_table(65288) ->
    {0, [], {wide, [{0, 40}]}, ps};
unicode_table(65289) ->
    {0, [], {wide, [{0, 41}]}, pe};
unicode_table(65290) ->
    {0, [], {wide, [{0, 42}]}, po};
unicode_table(65291) ->
    {0, [], {wide, [{0, 43}]}, sm};
unicode_table(65292) ->
    {0, [], {wide, [{0, 44}]}, po};
unicode_table(65293) ->
    {0, [], {wide, [{0, 45}]}, pd};
unicode_table(65294) ->
    {0, [], {wide, [{0, 46}]}, po};
unicode_table(65295) ->
    {0, [], {wide, [{0, 47}]}, po};
unicode_table(65296) ->
    {0, [], {wide, [{0, 48}]}, nd};
unicode_table(65297) ->
    {0, [], {wide, [{0, 49}]}, nd};
unicode_table(65298) ->
    {0, [], {wide, [{0, 50}]}, nd};
unicode_table(65299) ->
    {0, [], {wide, [{0, 51}]}, nd};
unicode_table(65300) ->
    {0, [], {wide, [{0, 52}]}, nd};
unicode_table(65301) ->
    {0, [], {wide, [{0, 53}]}, nd};
unicode_table(65302) ->
    {0, [], {wide, [{0, 54}]}, nd};
unicode_table(65303) ->
    {0, [], {wide, [{0, 55}]}, nd};
unicode_table(65304) ->
    {0, [], {wide, [{0, 56}]}, nd};
unicode_table(65305) ->
    {0, [], {wide, [{0, 57}]}, nd};
unicode_table(65306) ->
    {0, [], {wide, [{0, 58}]}, po};
unicode_table(65307) ->
    {0, [], {wide, [{0, 59}]}, po};
unicode_table(65308) ->
    {0, [], {wide, [{0, 60}]}, sm};
unicode_table(65309) ->
    {0, [], {wide, [{0, 61}]}, sm};
unicode_table(65310) ->
    {0, [], {wide, [{0, 62}]}, sm};
unicode_table(65311) ->
    {0, [], {wide, [{0, 63}]}, po};
unicode_table(65312) ->
    {0, [], {wide, [{0, 64}]}, po};
unicode_table(65313) ->
    {0, [], {wide, [{0, 65}]}, lu};
unicode_table(65314) ->
    {0, [], {wide, [{0, 66}]}, lu};
unicode_table(65315) ->
    {0, [], {wide, [{0, 67}]}, lu};
unicode_table(65316) ->
    {0, [], {wide, [{0, 68}]}, lu};
unicode_table(65317) ->
    {0, [], {wide, [{0, 69}]}, lu};
unicode_table(65318) ->
    {0, [], {wide, [{0, 70}]}, lu};
unicode_table(65319) ->
    {0, [], {wide, [{0, 71}]}, lu};
unicode_table(65320) ->
    {0, [], {wide, [{0, 72}]}, lu};
unicode_table(65321) ->
    {0, [], {wide, [{0, 73}]}, lu};
unicode_table(65322) ->
    {0, [], {wide, [{0, 74}]}, lu};
unicode_table(65323) ->
    {0, [], {wide, [{0, 75}]}, lu};
unicode_table(65324) ->
    {0, [], {wide, [{0, 76}]}, lu};
unicode_table(65325) ->
    {0, [], {wide, [{0, 77}]}, lu};
unicode_table(65326) ->
    {0, [], {wide, [{0, 78}]}, lu};
unicode_table(65327) ->
    {0, [], {wide, [{0, 79}]}, lu};
unicode_table(65328) ->
    {0, [], {wide, [{0, 80}]}, lu};
unicode_table(65329) ->
    {0, [], {wide, [{0, 81}]}, lu};
unicode_table(65330) ->
    {0, [], {wide, [{0, 82}]}, lu};
unicode_table(65331) ->
    {0, [], {wide, [{0, 83}]}, lu};
unicode_table(65332) ->
    {0, [], {wide, [{0, 84}]}, lu};
unicode_table(65333) ->
    {0, [], {wide, [{0, 85}]}, lu};
unicode_table(65334) ->
    {0, [], {wide, [{0, 86}]}, lu};
unicode_table(65335) ->
    {0, [], {wide, [{0, 87}]}, lu};
unicode_table(65336) ->
    {0, [], {wide, [{0, 88}]}, lu};
unicode_table(65337) ->
    {0, [], {wide, [{0, 89}]}, lu};
unicode_table(65338) ->
    {0, [], {wide, [{0, 90}]}, lu};
unicode_table(65339) ->
    {0, [], {wide, [{0, 91}]}, ps};
unicode_table(65340) ->
    {0, [], {wide, [{0, 92}]}, po};
unicode_table(65341) ->
    {0, [], {wide, [{0, 93}]}, pe};
unicode_table(65342) ->
    {0, [], {wide, [{0, 94}]}, sk};
unicode_table(65343) ->
    {0, [], {wide, [{0, 95}]}, pc};
unicode_table(65344) ->
    {0, [], {wide, [{0, 96}]}, sk};
unicode_table(65345) ->
    {0, [], {wide, [{0, 97}]}, ll};
unicode_table(65346) ->
    {0, [], {wide, [{0, 98}]}, ll};
unicode_table(65347) ->
    {0, [], {wide, [{0, 99}]}, ll};
unicode_table(65348) ->
    {0, [], {wide, [{0, 100}]}, ll};
unicode_table(65349) ->
    {0, [], {wide, [{0, 101}]}, ll};
unicode_table(65350) ->
    {0, [], {wide, [{0, 102}]}, ll};
unicode_table(65351) ->
    {0, [], {wide, [{0, 103}]}, ll};
unicode_table(65352) ->
    {0, [], {wide, [{0, 104}]}, ll};
unicode_table(65353) ->
    {0, [], {wide, [{0, 105}]}, ll};
unicode_table(65354) ->
    {0, [], {wide, [{0, 106}]}, ll};
unicode_table(65355) ->
    {0, [], {wide, [{0, 107}]}, ll};
unicode_table(65356) ->
    {0, [], {wide, [{0, 108}]}, ll};
unicode_table(65357) ->
    {0, [], {wide, [{0, 109}]}, ll};
unicode_table(65358) ->
    {0, [], {wide, [{0, 110}]}, ll};
unicode_table(65359) ->
    {0, [], {wide, [{0, 111}]}, ll};
unicode_table(65360) ->
    {0, [], {wide, [{0, 112}]}, ll};
unicode_table(65361) ->
    {0, [], {wide, [{0, 113}]}, ll};
unicode_table(65362) ->
    {0, [], {wide, [{0, 114}]}, ll};
unicode_table(65363) ->
    {0, [], {wide, [{0, 115}]}, ll};
unicode_table(65364) ->
    {0, [], {wide, [{0, 116}]}, ll};
unicode_table(65365) ->
    {0, [], {wide, [{0, 117}]}, ll};
unicode_table(65366) ->
    {0, [], {wide, [{0, 118}]}, ll};
unicode_table(65367) ->
    {0, [], {wide, [{0, 119}]}, ll};
unicode_table(65368) ->
    {0, [], {wide, [{0, 120}]}, ll};
unicode_table(65369) ->
    {0, [], {wide, [{0, 121}]}, ll};
unicode_table(65370) ->
    {0, [], {wide, [{0, 122}]}, ll};
unicode_table(65371) ->
    {0, [], {wide, [{0, 123}]}, ps};
unicode_table(65372) ->
    {0, [], {wide, [{0, 124}]}, sm};
unicode_table(65373) ->
    {0, [], {wide, [{0, 125}]}, pe};
unicode_table(65374) ->
    {0, [], {wide, [{0, 126}]}, sm};
unicode_table(65375) ->
    {0, [], {wide, [{0, 10629}]}, ps};
unicode_table(65376) ->
    {0, [], {wide, [{0, 10630}]}, pe};
unicode_table(65377) ->
    {0, [], {narrow, [{0, 12290}]}, po};
unicode_table(65378) ->
    {0, [], {narrow, [{0, 12300}]}, ps};
unicode_table(65379) ->
    {0, [], {narrow, [{0, 12301}]}, pe};
unicode_table(65380) ->
    {0, [], {narrow, [{0, 12289}]}, po};
unicode_table(65381) ->
    {0, [], {narrow, [{0, 12539}]}, po};
unicode_table(65382) ->
    {0, [], {narrow, [{0, 12530}]}, lo};
unicode_table(65383) ->
    {0, [], {narrow, [{0, 12449}]}, lo};
unicode_table(65384) ->
    {0, [], {narrow, [{0, 12451}]}, lo};
unicode_table(65385) ->
    {0, [], {narrow, [{0, 12453}]}, lo};
unicode_table(65386) ->
    {0, [], {narrow, [{0, 12455}]}, lo};
unicode_table(65387) ->
    {0, [], {narrow, [{0, 12457}]}, lo};
unicode_table(65388) ->
    {0, [], {narrow, [{0, 12515}]}, lo};
unicode_table(65389) ->
    {0, [], {narrow, [{0, 12517}]}, lo};
unicode_table(65390) ->
    {0, [], {narrow, [{0, 12519}]}, lo};
unicode_table(65391) ->
    {0, [], {narrow, [{0, 12483}]}, lo};
unicode_table(65392) ->
    {0, [], {narrow, [{0, 12540}]}, lm};
unicode_table(65393) ->
    {0, [], {narrow, [{0, 12450}]}, lo};
unicode_table(65394) ->
    {0, [], {narrow, [{0, 12452}]}, lo};
unicode_table(65395) ->
    {0, [], {narrow, [{0, 12454}]}, lo};
unicode_table(65396) ->
    {0, [], {narrow, [{0, 12456}]}, lo};
unicode_table(65397) ->
    {0, [], {narrow, [{0, 12458}]}, lo};
unicode_table(65398) ->
    {0, [], {narrow, [{0, 12459}]}, lo};
unicode_table(65399) ->
    {0, [], {narrow, [{0, 12461}]}, lo};
unicode_table(65400) ->
    {0, [], {narrow, [{0, 12463}]}, lo};
unicode_table(65401) ->
    {0, [], {narrow, [{0, 12465}]}, lo};
unicode_table(65402) ->
    {0, [], {narrow, [{0, 12467}]}, lo};
unicode_table(65403) ->
    {0, [], {narrow, [{0, 12469}]}, lo};
unicode_table(65404) ->
    {0, [], {narrow, [{0, 12471}]}, lo};
unicode_table(65405) ->
    {0, [], {narrow, [{0, 12473}]}, lo};
unicode_table(65406) ->
    {0, [], {narrow, [{0, 12475}]}, lo};
unicode_table(65407) ->
    {0, [], {narrow, [{0, 12477}]}, lo};
unicode_table(65408) ->
    {0, [], {narrow, [{0, 12479}]}, lo};
unicode_table(65409) ->
    {0, [], {narrow, [{0, 12481}]}, lo};
unicode_table(65410) ->
    {0, [], {narrow, [{0, 12484}]}, lo};
unicode_table(65411) ->
    {0, [], {narrow, [{0, 12486}]}, lo};
unicode_table(65412) ->
    {0, [], {narrow, [{0, 12488}]}, lo};
unicode_table(65413) ->
    {0, [], {narrow, [{0, 12490}]}, lo};
unicode_table(65414) ->
    {0, [], {narrow, [{0, 12491}]}, lo};
unicode_table(65415) ->
    {0, [], {narrow, [{0, 12492}]}, lo};
unicode_table(65416) ->
    {0, [], {narrow, [{0, 12493}]}, lo};
unicode_table(65417) ->
    {0, [], {narrow, [{0, 12494}]}, lo};
unicode_table(65418) ->
    {0, [], {narrow, [{0, 12495}]}, lo};
unicode_table(65419) ->
    {0, [], {narrow, [{0, 12498}]}, lo};
unicode_table(65420) ->
    {0, [], {narrow, [{0, 12501}]}, lo};
unicode_table(65421) ->
    {0, [], {narrow, [{0, 12504}]}, lo};
unicode_table(65422) ->
    {0, [], {narrow, [{0, 12507}]}, lo};
unicode_table(65423) ->
    {0, [], {narrow, [{0, 12510}]}, lo};
unicode_table(65424) ->
    {0, [], {narrow, [{0, 12511}]}, lo};
unicode_table(65425) ->
    {0, [], {narrow, [{0, 12512}]}, lo};
unicode_table(65426) ->
    {0, [], {narrow, [{0, 12513}]}, lo};
unicode_table(65427) ->
    {0, [], {narrow, [{0, 12514}]}, lo};
unicode_table(65428) ->
    {0, [], {narrow, [{0, 12516}]}, lo};
unicode_table(65429) ->
    {0, [], {narrow, [{0, 12518}]}, lo};
unicode_table(65430) ->
    {0, [], {narrow, [{0, 12520}]}, lo};
unicode_table(65431) ->
    {0, [], {narrow, [{0, 12521}]}, lo};
unicode_table(65432) ->
    {0, [], {narrow, [{0, 12522}]}, lo};
unicode_table(65433) ->
    {0, [], {narrow, [{0, 12523}]}, lo};
unicode_table(65434) ->
    {0, [], {narrow, [{0, 12524}]}, lo};
unicode_table(65435) ->
    {0, [], {narrow, [{0, 12525}]}, lo};
unicode_table(65436) ->
    {0, [], {narrow, [{0, 12527}]}, lo};
unicode_table(65437) ->
    {0, [], {narrow, [{0, 12531}]}, lo};
unicode_table(65438) ->
    {0, [], {narrow, [{8, 12441}]}, lm};
unicode_table(65439) ->
    {0, [], {narrow, [{8, 12442}]}, lm};
unicode_table(65440) ->
    {0, [], {narrow, [{0, 4448}]}, lo};
unicode_table(65441) ->
    {0, [], {narrow, [{0, 4352}]}, lo};
unicode_table(65442) ->
    {0, [], {narrow, [{0, 4353}]}, lo};
unicode_table(65443) ->
    {0, [], {narrow, [{0, 4522}]}, lo};
unicode_table(65444) ->
    {0, [], {narrow, [{0, 4354}]}, lo};
unicode_table(65445) ->
    {0, [], {narrow, [{0, 4524}]}, lo};
unicode_table(65446) ->
    {0, [], {narrow, [{0, 4525}]}, lo};
unicode_table(65447) ->
    {0, [], {narrow, [{0, 4355}]}, lo};
unicode_table(65448) ->
    {0, [], {narrow, [{0, 4356}]}, lo};
unicode_table(65449) ->
    {0, [], {narrow, [{0, 4357}]}, lo};
unicode_table(65450) ->
    {0, [], {narrow, [{0, 4528}]}, lo};
unicode_table(65451) ->
    {0, [], {narrow, [{0, 4529}]}, lo};
unicode_table(65452) ->
    {0, [], {narrow, [{0, 4530}]}, lo};
unicode_table(65453) ->
    {0, [], {narrow, [{0, 4531}]}, lo};
unicode_table(65454) ->
    {0, [], {narrow, [{0, 4532}]}, lo};
unicode_table(65455) ->
    {0, [], {narrow, [{0, 4533}]}, lo};
unicode_table(65456) ->
    {0, [], {narrow, [{0, 4378}]}, lo};
unicode_table(65457) ->
    {0, [], {narrow, [{0, 4358}]}, lo};
unicode_table(65458) ->
    {0, [], {narrow, [{0, 4359}]}, lo};
unicode_table(65459) ->
    {0, [], {narrow, [{0, 4360}]}, lo};
unicode_table(65460) ->
    {0, [], {narrow, [{0, 4385}]}, lo};
unicode_table(65461) ->
    {0, [], {narrow, [{0, 4361}]}, lo};
unicode_table(65462) ->
    {0, [], {narrow, [{0, 4362}]}, lo};
unicode_table(65463) ->
    {0, [], {narrow, [{0, 4363}]}, lo};
unicode_table(65464) ->
    {0, [], {narrow, [{0, 4364}]}, lo};
unicode_table(65465) ->
    {0, [], {narrow, [{0, 4365}]}, lo};
unicode_table(65466) ->
    {0, [], {narrow, [{0, 4366}]}, lo};
unicode_table(65467) ->
    {0, [], {narrow, [{0, 4367}]}, lo};
unicode_table(65468) ->
    {0, [], {narrow, [{0, 4368}]}, lo};
unicode_table(65469) ->
    {0, [], {narrow, [{0, 4369}]}, lo};
unicode_table(65470) ->
    {0, [], {narrow, [{0, 4370}]}, lo};
unicode_table(65474) ->
    {0, [], {narrow, [{0, 4449}]}, lo};
unicode_table(65475) ->
    {0, [], {narrow, [{0, 4450}]}, lo};
unicode_table(65476) ->
    {0, [], {narrow, [{0, 4451}]}, lo};
unicode_table(65477) ->
    {0, [], {narrow, [{0, 4452}]}, lo};
unicode_table(65478) ->
    {0, [], {narrow, [{0, 4453}]}, lo};
unicode_table(65479) ->
    {0, [], {narrow, [{0, 4454}]}, lo};
unicode_table(65482) ->
    {0, [], {narrow, [{0, 4455}]}, lo};
unicode_table(65483) ->
    {0, [], {narrow, [{0, 4456}]}, lo};
unicode_table(65484) ->
    {0, [], {narrow, [{0, 4457}]}, lo};
unicode_table(65485) ->
    {0, [], {narrow, [{0, 4458}]}, lo};
unicode_table(65486) ->
    {0, [], {narrow, [{0, 4459}]}, lo};
unicode_table(65487) ->
    {0, [], {narrow, [{0, 4460}]}, lo};
unicode_table(65490) ->
    {0, [], {narrow, [{0, 4461}]}, lo};
unicode_table(65491) ->
    {0, [], {narrow, [{0, 4462}]}, lo};
unicode_table(65492) ->
    {0, [], {narrow, [{0, 4463}]}, lo};
unicode_table(65493) ->
    {0, [], {narrow, [{0, 4464}]}, lo};
unicode_table(65494) ->
    {0, [], {narrow, [{0, 4465}]}, lo};
unicode_table(65495) ->
    {0, [], {narrow, [{0, 4466}]}, lo};
unicode_table(65498) ->
    {0, [], {narrow, [{0, 4467}]}, lo};
unicode_table(65499) ->
    {0, [], {narrow, [{0, 4468}]}, lo};
unicode_table(65500) ->
    {0, [], {narrow, [{0, 4469}]}, lo};
unicode_table(65504) ->
    {0, [], {wide, [{0, 162}]}, sc};
unicode_table(65505) ->
    {0, [], {wide, [{0, 163}]}, sc};
unicode_table(65506) ->
    {0, [], {wide, [{0, 172}]}, sm};
unicode_table(65507) ->
    {0, [], {wide, [{0, 32}, {230, 772}]}, sk};
unicode_table(65508) ->
    {0, [], {wide, [{0, 166}]}, so};
unicode_table(65509) ->
    {0, [], {wide, [{0, 165}]}, sc};
unicode_table(65510) ->
    {0, [], {wide, [{0, 8361}]}, sc};
unicode_table(65512) ->
    {0, [], {narrow, [{0, 9474}]}, so};
unicode_table(65513) ->
    {0, [], {narrow, [{0, 8592}]}, sm};
unicode_table(65514) ->
    {0, [], {narrow, [{0, 8593}]}, sm};
unicode_table(65515) ->
    {0, [], {narrow, [{0, 8594}]}, sm};
unicode_table(65516) ->
    {0, [], {narrow, [{0, 8595}]}, sm};
unicode_table(65517) ->
    {0, [], {narrow, [{0, 9632}]}, so};
unicode_table(65518) ->
    {0, [], {narrow, [{0, 9675}]}, so};
unicode_table(66045) ->
    {220, [], [], mn};
unicode_table(66272) ->
    {220, [], [], mn};
unicode_table(66422) ->
    {230, [], [], mn};
unicode_table(66423) ->
    {230, [], [], mn};
unicode_table(66424) ->
    {230, [], [], mn};
unicode_table(66425) ->
    {230, [], [], mn};
unicode_table(66426) ->
    {230, [], [], mn};
unicode_table(67457) ->
    {0, [], {super, [{0, 720}]}, lm};
unicode_table(67458) ->
    {0, [], {super, [{0, 721}]}, lm};
unicode_table(67459) ->
    {0, [], {super, [{0, 230}]}, lm};
unicode_table(67460) ->
    {0, [], {super, [{0, 665}]}, lm};
unicode_table(67461) ->
    {0, [], {super, [{0, 595}]}, lm};
unicode_table(67463) ->
    {0, [], {super, [{0, 675}]}, lm};
unicode_table(67464) ->
    {0, [], {super, [{0, 43878}]}, lm};
unicode_table(67465) ->
    {0, [], {super, [{0, 677}]}, lm};
unicode_table(67466) ->
    {0, [], {super, [{0, 676}]}, lm};
unicode_table(67467) ->
    {0, [], {super, [{0, 598}]}, lm};
unicode_table(67468) ->
    {0, [], {super, [{0, 599}]}, lm};
unicode_table(67469) ->
    {0, [], {super, [{0, 7569}]}, lm};
unicode_table(67470) ->
    {0, [], {super, [{0, 600}]}, lm};
unicode_table(67471) ->
    {0, [], {super, [{0, 606}]}, lm};
unicode_table(67472) ->
    {0, [], {super, [{0, 681}]}, lm};
unicode_table(67473) ->
    {0, [], {super, [{0, 612}]}, lm};
unicode_table(67474) ->
    {0, [], {super, [{0, 610}]}, lm};
unicode_table(67475) ->
    {0, [], {super, [{0, 608}]}, lm};
unicode_table(67476) ->
    {0, [], {super, [{0, 667}]}, lm};
unicode_table(67477) ->
    {0, [], {super, [{0, 295}]}, lm};
unicode_table(67478) ->
    {0, [], {super, [{0, 668}]}, lm};
unicode_table(67479) ->
    {0, [], {super, [{0, 615}]}, lm};
unicode_table(67480) ->
    {0, [], {super, [{0, 644}]}, lm};
unicode_table(67481) ->
    {0, [], {super, [{0, 682}]}, lm};
unicode_table(67482) ->
    {0, [], {super, [{0, 683}]}, lm};
unicode_table(67483) ->
    {0, [], {super, [{0, 620}]}, lm};
unicode_table(67484) ->
    {0, [], {super, [{0, 122628}]}, lm};
unicode_table(67485) ->
    {0, [], {super, [{0, 42894}]}, lm};
unicode_table(67486) ->
    {0, [], {super, [{0, 622}]}, lm};
unicode_table(67487) ->
    {0, [], {super, [{0, 122629}]}, lm};
unicode_table(67488) ->
    {0, [], {super, [{0, 654}]}, lm};
unicode_table(67489) ->
    {0, [], {super, [{0, 122630}]}, lm};
unicode_table(67490) ->
    {0, [], {super, [{0, 248}]}, lm};
unicode_table(67491) ->
    {0, [], {super, [{0, 630}]}, lm};
unicode_table(67492) ->
    {0, [], {super, [{0, 631}]}, lm};
unicode_table(67493) ->
    {0, [], {super, [{0, 113}]}, lm};
unicode_table(67494) ->
    {0, [], {super, [{0, 634}]}, lm};
unicode_table(67495) ->
    {0, [], {super, [{0, 122632}]}, lm};
unicode_table(67496) ->
    {0, [], {super, [{0, 637}]}, lm};
unicode_table(67497) ->
    {0, [], {super, [{0, 638}]}, lm};
unicode_table(67498) ->
    {0, [], {super, [{0, 640}]}, lm};
unicode_table(67499) ->
    {0, [], {super, [{0, 680}]}, lm};
unicode_table(67500) ->
    {0, [], {super, [{0, 678}]}, lm};
unicode_table(67501) ->
    {0, [], {super, [{0, 43879}]}, lm};
unicode_table(67502) ->
    {0, [], {super, [{0, 679}]}, lm};
unicode_table(67503) ->
    {0, [], {super, [{0, 648}]}, lm};
unicode_table(67504) ->
    {0, [], {super, [{0, 11377}]}, lm};
unicode_table(67506) ->
    {0, [], {super, [{0, 655}]}, lm};
unicode_table(67507) ->
    {0, [], {super, [{0, 673}]}, lm};
unicode_table(67508) ->
    {0, [], {super, [{0, 674}]}, lm};
unicode_table(67509) ->
    {0, [], {super, [{0, 664}]}, lm};
unicode_table(67510) ->
    {0, [], {super, [{0, 448}]}, lm};
unicode_table(67511) ->
    {0, [], {super, [{0, 449}]}, lm};
unicode_table(67512) ->
    {0, [], {super, [{0, 450}]}, lm};
unicode_table(67513) ->
    {0, [], {super, [{0, 122634}]}, lm};
unicode_table(67514) ->
    {0, [], {super, [{0, 122654}]}, lm};
unicode_table(68109) ->
    {220, [], [], mn};
unicode_table(68111) ->
    {230, [], [], mn};
unicode_table(68152) ->
    {230, [], [], mn};
unicode_table(68153) ->
    {1, [], [], mn};
unicode_table(68154) ->
    {220, [], [], mn};
unicode_table(68159) ->
    {9, [], [], mn};
unicode_table(68325) ->
    {230, [], [], mn};
unicode_table(68326) ->
    {220, [], [], mn};
unicode_table(68900) ->
    {230, [], [], mn};
unicode_table(68901) ->
    {230, [], [], mn};
unicode_table(68902) ->
    {230, [], [], mn};
unicode_table(68903) ->
    {230, [], [], mn};
unicode_table(69291) ->
    {230, [], [], mn};
unicode_table(69292) ->
    {230, [], [], mn};
unicode_table(69373) ->
    {220, [], [], mn};
unicode_table(69374) ->
    {220, [], [], mn};
unicode_table(69375) ->
    {220, [], [], mn};
unicode_table(69446) ->
    {220, [], [], mn};
unicode_table(69447) ->
    {220, [], [], mn};
unicode_table(69448) ->
    {230, [], [], mn};
unicode_table(69449) ->
    {230, [], [], mn};
unicode_table(69450) ->
    {230, [], [], mn};
unicode_table(69451) ->
    {220, [], [], mn};
unicode_table(69452) ->
    {230, [], [], mn};
unicode_table(69453) ->
    {220, [], [], mn};
unicode_table(69454) ->
    {220, [], [], mn};
unicode_table(69455) ->
    {220, [], [], mn};
unicode_table(69456) ->
    {220, [], [], mn};
unicode_table(69506) ->
    {230, [], [], mn};
unicode_table(69507) ->
    {220, [], [], mn};
unicode_table(69508) ->
    {230, [], [], mn};
unicode_table(69509) ->
    {220, [], [], mn};
unicode_table(69702) ->
    {9, [], [], mn};
unicode_table(69744) ->
    {9, [], [], mn};
unicode_table(69759) ->
    {9, [], [], mn};
unicode_table(69786) ->
    {0, [{0, 69785}, {7, 69818}], [], lo};
unicode_table(69788) ->
    {0, [{0, 69787}, {7, 69818}], [], lo};
unicode_table(69803) ->
    {0, [{0, 69797}, {7, 69818}], [], lo};
unicode_table(69817) ->
    {9, [], [], mn};
unicode_table(69818) ->
    {7, [], [], mn};
unicode_table(69888) ->
    {230, [], [], mn};
unicode_table(69889) ->
    {230, [], [], mn};
unicode_table(69890) ->
    {230, [], [], mn};
unicode_table(69934) ->
    {0, [{0, 69937}, {0, 69927}], [], mn};
unicode_table(69935) ->
    {0, [{0, 69938}, {0, 69927}], [], mn};
unicode_table(69939) ->
    {9, [], [], mn};
unicode_table(69940) ->
    {9, [], [], mn};
unicode_table(70003) ->
    {7, [], [], mn};
unicode_table(70080) ->
    {9, [], [], mc};
unicode_table(70090) ->
    {7, [], [], mn};
unicode_table(70197) ->
    {9, [], [], mc};
unicode_table(70198) ->
    {7, [], [], mn};
unicode_table(70377) ->
    {7, [], [], mn};
unicode_table(70378) ->
    {9, [], [], mn};
unicode_table(70459) ->
    {7, [], [], mn};
unicode_table(70460) ->
    {7, [], [], mn};
unicode_table(70475) ->
    {0, [{0, 70471}, {0, 70462}], [], mc};
unicode_table(70476) ->
    {0, [{0, 70471}, {0, 70487}], [], mc};
unicode_table(70477) ->
    {9, [], [], mc};
unicode_table(70502) ->
    {230, [], [], mn};
unicode_table(70503) ->
    {230, [], [], mn};
unicode_table(70504) ->
    {230, [], [], mn};
unicode_table(70505) ->
    {230, [], [], mn};
unicode_table(70506) ->
    {230, [], [], mn};
unicode_table(70507) ->
    {230, [], [], mn};
unicode_table(70508) ->
    {230, [], [], mn};
unicode_table(70512) ->
    {230, [], [], mn};
unicode_table(70513) ->
    {230, [], [], mn};
unicode_table(70514) ->
    {230, [], [], mn};
unicode_table(70515) ->
    {230, [], [], mn};
unicode_table(70516) ->
    {230, [], [], mn};
unicode_table(70722) ->
    {9, [], [], mn};
unicode_table(70726) ->
    {7, [], [], mn};
unicode_table(70750) ->
    {230, [], [], mn};
unicode_table(70843) ->
    {0, [{0, 70841}, {0, 70842}], [], mc};
unicode_table(70844) ->
    {0, [{0, 70841}, {0, 70832}], [], mc};
unicode_table(70846) ->
    {0, [{0, 70841}, {0, 70845}], [], mc};
unicode_table(70850) ->
    {9, [], [], mn};
unicode_table(70851) ->
    {7, [], [], mn};
unicode_table(71098) ->
    {0, [{0, 71096}, {0, 71087}], [], mc};
unicode_table(71099) ->
    {0, [{0, 71097}, {0, 71087}], [], mc};
unicode_table(71103) ->
    {9, [], [], mn};
unicode_table(71104) ->
    {7, [], [], mn};
unicode_table(71231) ->
    {9, [], [], mn};
unicode_table(71350) ->
    {9, [], [], mc};
unicode_table(71351) ->
    {7, [], [], mn};
unicode_table(71467) ->
    {9, [], [], mn};
unicode_table(71737) ->
    {9, [], [], mn};
unicode_table(71738) ->
    {7, [], [], mn};
unicode_table(71992) ->
    {0, [{0, 71989}, {0, 71984}], [], mc};
unicode_table(71997) ->
    {9, [], [], mc};
unicode_table(71998) ->
    {9, [], [], mn};
unicode_table(72003) ->
    {7, [], [], mn};
unicode_table(72160) ->
    {9, [], [], mn};
unicode_table(72244) ->
    {9, [], [], mn};
unicode_table(72263) ->
    {9, [], [], mn};
unicode_table(72345) ->
    {9, [], [], mn};
unicode_table(72767) ->
    {9, [], [], mn};
unicode_table(73026) ->
    {7, [], [], mn};
unicode_table(73028) ->
    {9, [], [], mn};
unicode_table(73029) ->
    {9, [], [], mn};
unicode_table(73111) ->
    {9, [], [], mn};
unicode_table(73537) ->
    {9, [], [], mc};
unicode_table(73538) ->
    {9, [], [], mn};
unicode_table(92912) ->
    {1, [], [], mn};
unicode_table(92913) ->
    {1, [], [], mn};
unicode_table(92914) ->
    {1, [], [], mn};
unicode_table(92915) ->
    {1, [], [], mn};
unicode_table(92916) ->
    {1, [], [], mn};
unicode_table(92976) ->
    {230, [], [], mn};
unicode_table(92977) ->
    {230, [], [], mn};
unicode_table(92978) ->
    {230, [], [], mn};
unicode_table(92979) ->
    {230, [], [], mn};
unicode_table(92980) ->
    {230, [], [], mn};
unicode_table(92981) ->
    {230, [], [], mn};
unicode_table(92982) ->
    {230, [], [], mn};
unicode_table(94192) ->
    {6, [], [], mc};
unicode_table(94193) ->
    {6, [], [], mc};
unicode_table(113822) ->
    {1, [], [], mn};
unicode_table(119134) ->
    {0, [{0, 119127}, {216, 119141}], [], so};
unicode_table(119135) ->
    {0, [{0, 119128}, {216, 119141}], [], so};
unicode_table(119136) ->
    {0, [{0, 119128}, {216, 119141}, {216, 119150}], [], so};
unicode_table(119137) ->
    {0, [{0, 119128}, {216, 119141}, {216, 119151}], [], so};
unicode_table(119138) ->
    {0, [{0, 119128}, {216, 119141}, {216, 119152}], [], so};
unicode_table(119139) ->
    {0, [{0, 119128}, {216, 119141}, {216, 119153}], [], so};
unicode_table(119140) ->
    {0, [{0, 119128}, {216, 119141}, {216, 119154}], [], so};
unicode_table(119141) ->
    {216, [], [], mc};
unicode_table(119142) ->
    {216, [], [], mc};
unicode_table(119143) ->
    {1, [], [], mn};
unicode_table(119144) ->
    {1, [], [], mn};
unicode_table(119145) ->
    {1, [], [], mn};
unicode_table(119149) ->
    {226, [], [], mc};
unicode_table(119150) ->
    {216, [], [], mc};
unicode_table(119151) ->
    {216, [], [], mc};
unicode_table(119152) ->
    {216, [], [], mc};
unicode_table(119153) ->
    {216, [], [], mc};
unicode_table(119154) ->
    {216, [], [], mc};
unicode_table(119163) ->
    {220, [], [], mn};
unicode_table(119164) ->
    {220, [], [], mn};
unicode_table(119165) ->
    {220, [], [], mn};
unicode_table(119166) ->
    {220, [], [], mn};
unicode_table(119167) ->
    {220, [], [], mn};
unicode_table(119168) ->
    {220, [], [], mn};
unicode_table(119169) ->
    {220, [], [], mn};
unicode_table(119170) ->
    {220, [], [], mn};
unicode_table(119173) ->
    {230, [], [], mn};
unicode_table(119174) ->
    {230, [], [], mn};
unicode_table(119175) ->
    {230, [], [], mn};
unicode_table(119176) ->
    {230, [], [], mn};
unicode_table(119177) ->
    {230, [], [], mn};
unicode_table(119178) ->
    {220, [], [], mn};
unicode_table(119179) ->
    {220, [], [], mn};
unicode_table(119210) ->
    {230, [], [], mn};
unicode_table(119211) ->
    {230, [], [], mn};
unicode_table(119212) ->
    {230, [], [], mn};
unicode_table(119213) ->
    {230, [], [], mn};
unicode_table(119227) ->
    {0, [{0, 119225}, {216, 119141}], [], so};
unicode_table(119228) ->
    {0, [{0, 119226}, {216, 119141}], [], so};
unicode_table(119229) ->
    {0, [{0, 119225}, {216, 119141}, {216, 119150}], [], so};
unicode_table(119230) ->
    {0, [{0, 119226}, {216, 119141}, {216, 119150}], [], so};
unicode_table(119231) ->
    {0, [{0, 119225}, {216, 119141}, {216, 119151}], [], so};
unicode_table(119232) ->
    {0, [{0, 119226}, {216, 119141}, {216, 119151}], [], so};
unicode_table(119362) ->
    {230, [], [], mn};
unicode_table(119363) ->
    {230, [], [], mn};
unicode_table(119364) ->
    {230, [], [], mn};
unicode_table(119808) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(119809) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(119810) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(119811) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(119812) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(119813) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(119814) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(119815) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(119816) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(119817) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(119818) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(119819) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(119820) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(119821) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(119822) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(119823) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(119824) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(119825) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(119826) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(119827) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(119828) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(119829) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(119830) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(119831) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(119832) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(119833) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(119834) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(119835) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(119836) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(119837) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(119838) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(119839) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(119840) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(119841) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(119842) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(119843) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(119844) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(119845) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(119846) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(119847) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(119848) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(119849) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(119850) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(119851) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(119852) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(119853) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(119854) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(119855) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(119856) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(119857) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(119858) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(119859) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(119860) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(119861) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(119862) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(119863) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(119864) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(119865) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(119866) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(119867) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(119868) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(119869) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(119870) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(119871) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(119872) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(119873) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(119874) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(119875) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(119876) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(119877) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(119878) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(119879) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(119880) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(119881) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(119882) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(119883) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(119884) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(119885) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(119886) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(119887) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(119888) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(119889) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(119890) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(119891) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(119892) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(119894) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(119895) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(119896) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(119897) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(119898) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(119899) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(119900) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(119901) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(119902) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(119903) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(119904) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(119905) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(119906) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(119907) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(119908) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(119909) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(119910) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(119911) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(119912) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(119913) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(119914) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(119915) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(119916) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(119917) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(119918) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(119919) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(119920) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(119921) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(119922) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(119923) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(119924) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(119925) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(119926) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(119927) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(119928) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(119929) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(119930) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(119931) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(119932) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(119933) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(119934) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(119935) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(119936) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(119937) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(119938) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(119939) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(119940) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(119941) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(119942) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(119943) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(119944) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(119945) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(119946) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(119947) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(119948) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(119949) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(119950) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(119951) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(119952) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(119953) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(119954) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(119955) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(119956) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(119957) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(119958) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(119959) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(119960) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(119961) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(119962) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(119963) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(119964) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(119966) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(119967) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(119970) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(119973) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(119974) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(119977) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(119978) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(119979) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(119980) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(119982) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(119983) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(119984) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(119985) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(119986) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(119987) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(119988) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(119989) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(119990) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(119991) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(119992) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(119993) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(119995) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(119997) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(119998) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(119999) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120000) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120001) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120002) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120003) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120005) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120006) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120007) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120008) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120009) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120010) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120011) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120012) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120013) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120014) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120015) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120016) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120017) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120018) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(120019) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120020) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120021) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120022) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120023) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(120024) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120025) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120026) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120027) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120028) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120029) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120030) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120031) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120032) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120033) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(120034) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120035) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120036) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120037) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120038) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120039) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120040) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120041) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(120042) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120043) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120044) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120045) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120046) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120047) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120048) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120049) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120050) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120051) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120052) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120053) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120054) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120055) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120056) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120057) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120058) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120059) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120060) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120061) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120062) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120063) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120064) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120065) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120066) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120067) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120068) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120069) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120071) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120072) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120073) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120074) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120077) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120078) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120079) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120080) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120081) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120082) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120083) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120084) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120086) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120087) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120088) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120089) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120090) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120091) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120092) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120094) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120095) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120096) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120097) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120098) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120099) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120100) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120101) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120102) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120103) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120104) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120105) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120106) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120107) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120108) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120109) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120110) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120111) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120112) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120113) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120114) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120115) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120116) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120117) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120118) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120119) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120120) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120121) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120123) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120124) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120125) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120126) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120128) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120129) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120130) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120131) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120132) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120134) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120138) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120139) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120140) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120141) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120142) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120143) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120144) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120146) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120147) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120148) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120149) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120150) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120151) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120152) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120153) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120154) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120155) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120156) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120157) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120158) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120159) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120160) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120161) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120162) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120163) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120164) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120165) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120166) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120167) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120168) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120169) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120170) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120171) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120172) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120173) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120174) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(120175) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120176) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120177) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120178) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120179) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(120180) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120181) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120182) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120183) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120184) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120185) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120186) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120187) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120188) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120189) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(120190) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120191) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120192) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120193) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120194) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120195) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120196) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120197) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(120198) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120199) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120200) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120201) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120202) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120203) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120204) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120205) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120206) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120207) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120208) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120209) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120210) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120211) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120212) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120213) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120214) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120215) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120216) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120217) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120218) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120219) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120220) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120221) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120222) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120223) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120224) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120225) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120226) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(120227) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120228) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120229) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120230) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120231) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(120232) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120233) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120234) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120235) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120236) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120237) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120238) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120239) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120240) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120241) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(120242) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120243) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120244) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120245) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120246) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120247) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120248) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120249) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(120250) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120251) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120252) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120253) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120254) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120255) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120256) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120257) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120258) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120259) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120260) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120261) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120262) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120263) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120264) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120265) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120266) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120267) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120268) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120269) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120270) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120271) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120272) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120273) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120274) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120275) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120276) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120277) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120278) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(120279) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120280) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120281) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120282) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120283) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(120284) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120285) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120286) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120287) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120288) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120289) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120290) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120291) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120292) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120293) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(120294) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120295) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120296) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120297) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120298) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120299) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120300) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120301) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(120302) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120303) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120304) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120305) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120306) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120307) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120308) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120309) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120310) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120311) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120312) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120313) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120314) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120315) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120316) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120317) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120318) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120319) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120320) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120321) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120322) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120323) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120324) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120325) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120326) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120327) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120328) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120329) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120330) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(120331) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120332) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120333) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120334) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120335) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(120336) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120337) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120338) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120339) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120340) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120341) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120342) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120343) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120344) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120345) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(120346) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120347) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120348) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120349) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120350) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120351) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120352) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120353) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(120354) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120355) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120356) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120357) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120358) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120359) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120360) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120361) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120362) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120363) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120364) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120365) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120366) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120367) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120368) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120369) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120370) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120371) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120372) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120373) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120374) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120375) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120376) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120377) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120378) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120379) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120380) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120381) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120382) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(120383) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120384) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120385) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120386) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120387) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(120388) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120389) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120390) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120391) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120392) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120393) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120394) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120395) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120396) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120397) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(120398) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120399) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120400) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120401) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120402) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120403) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120404) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120405) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(120406) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120407) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120408) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120409) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120410) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120411) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120412) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120413) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120414) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120415) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120416) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120417) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120418) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120419) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120420) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120421) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120422) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120423) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120424) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120425) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120426) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120427) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120428) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120429) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120430) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120431) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120432) ->
    {0, [], {font, [{0, 65}]}, lu};
unicode_table(120433) ->
    {0, [], {font, [{0, 66}]}, lu};
unicode_table(120434) ->
    {0, [], {font, [{0, 67}]}, lu};
unicode_table(120435) ->
    {0, [], {font, [{0, 68}]}, lu};
unicode_table(120436) ->
    {0, [], {font, [{0, 69}]}, lu};
unicode_table(120437) ->
    {0, [], {font, [{0, 70}]}, lu};
unicode_table(120438) ->
    {0, [], {font, [{0, 71}]}, lu};
unicode_table(120439) ->
    {0, [], {font, [{0, 72}]}, lu};
unicode_table(120440) ->
    {0, [], {font, [{0, 73}]}, lu};
unicode_table(120441) ->
    {0, [], {font, [{0, 74}]}, lu};
unicode_table(120442) ->
    {0, [], {font, [{0, 75}]}, lu};
unicode_table(120443) ->
    {0, [], {font, [{0, 76}]}, lu};
unicode_table(120444) ->
    {0, [], {font, [{0, 77}]}, lu};
unicode_table(120445) ->
    {0, [], {font, [{0, 78}]}, lu};
unicode_table(120446) ->
    {0, [], {font, [{0, 79}]}, lu};
unicode_table(120447) ->
    {0, [], {font, [{0, 80}]}, lu};
unicode_table(120448) ->
    {0, [], {font, [{0, 81}]}, lu};
unicode_table(120449) ->
    {0, [], {font, [{0, 82}]}, lu};
unicode_table(120450) ->
    {0, [], {font, [{0, 83}]}, lu};
unicode_table(120451) ->
    {0, [], {font, [{0, 84}]}, lu};
unicode_table(120452) ->
    {0, [], {font, [{0, 85}]}, lu};
unicode_table(120453) ->
    {0, [], {font, [{0, 86}]}, lu};
unicode_table(120454) ->
    {0, [], {font, [{0, 87}]}, lu};
unicode_table(120455) ->
    {0, [], {font, [{0, 88}]}, lu};
unicode_table(120456) ->
    {0, [], {font, [{0, 89}]}, lu};
unicode_table(120457) ->
    {0, [], {font, [{0, 90}]}, lu};
unicode_table(120458) ->
    {0, [], {font, [{0, 97}]}, ll};
unicode_table(120459) ->
    {0, [], {font, [{0, 98}]}, ll};
unicode_table(120460) ->
    {0, [], {font, [{0, 99}]}, ll};
unicode_table(120461) ->
    {0, [], {font, [{0, 100}]}, ll};
unicode_table(120462) ->
    {0, [], {font, [{0, 101}]}, ll};
unicode_table(120463) ->
    {0, [], {font, [{0, 102}]}, ll};
unicode_table(120464) ->
    {0, [], {font, [{0, 103}]}, ll};
unicode_table(120465) ->
    {0, [], {font, [{0, 104}]}, ll};
unicode_table(120466) ->
    {0, [], {font, [{0, 105}]}, ll};
unicode_table(120467) ->
    {0, [], {font, [{0, 106}]}, ll};
unicode_table(120468) ->
    {0, [], {font, [{0, 107}]}, ll};
unicode_table(120469) ->
    {0, [], {font, [{0, 108}]}, ll};
unicode_table(120470) ->
    {0, [], {font, [{0, 109}]}, ll};
unicode_table(120471) ->
    {0, [], {font, [{0, 110}]}, ll};
unicode_table(120472) ->
    {0, [], {font, [{0, 111}]}, ll};
unicode_table(120473) ->
    {0, [], {font, [{0, 112}]}, ll};
unicode_table(120474) ->
    {0, [], {font, [{0, 113}]}, ll};
unicode_table(120475) ->
    {0, [], {font, [{0, 114}]}, ll};
unicode_table(120476) ->
    {0, [], {font, [{0, 115}]}, ll};
unicode_table(120477) ->
    {0, [], {font, [{0, 116}]}, ll};
unicode_table(120478) ->
    {0, [], {font, [{0, 117}]}, ll};
unicode_table(120479) ->
    {0, [], {font, [{0, 118}]}, ll};
unicode_table(120480) ->
    {0, [], {font, [{0, 119}]}, ll};
unicode_table(120481) ->
    {0, [], {font, [{0, 120}]}, ll};
unicode_table(120482) ->
    {0, [], {font, [{0, 121}]}, ll};
unicode_table(120483) ->
    {0, [], {font, [{0, 122}]}, ll};
unicode_table(120484) ->
    {0, [], {font, [{0, 305}]}, ll};
unicode_table(120485) ->
    {0, [], {font, [{0, 567}]}, ll};
unicode_table(120488) ->
    {0, [], {font, [{0, 913}]}, lu};
unicode_table(120489) ->
    {0, [], {font, [{0, 914}]}, lu};
unicode_table(120490) ->
    {0, [], {font, [{0, 915}]}, lu};
unicode_table(120491) ->
    {0, [], {font, [{0, 916}]}, lu};
unicode_table(120492) ->
    {0, [], {font, [{0, 917}]}, lu};
unicode_table(120493) ->
    {0, [], {font, [{0, 918}]}, lu};
unicode_table(120494) ->
    {0, [], {font, [{0, 919}]}, lu};
unicode_table(120495) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120496) ->
    {0, [], {font, [{0, 921}]}, lu};
unicode_table(120497) ->
    {0, [], {font, [{0, 922}]}, lu};
unicode_table(120498) ->
    {0, [], {font, [{0, 923}]}, lu};
unicode_table(120499) ->
    {0, [], {font, [{0, 924}]}, lu};
unicode_table(120500) ->
    {0, [], {font, [{0, 925}]}, lu};
unicode_table(120501) ->
    {0, [], {font, [{0, 926}]}, lu};
unicode_table(120502) ->
    {0, [], {font, [{0, 927}]}, lu};
unicode_table(120503) ->
    {0, [], {font, [{0, 928}]}, lu};
unicode_table(120504) ->
    {0, [], {font, [{0, 929}]}, lu};
unicode_table(120505) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120506) ->
    {0, [], {font, [{0, 931}]}, lu};
unicode_table(120507) ->
    {0, [], {font, [{0, 932}]}, lu};
unicode_table(120508) ->
    {0, [], {font, [{0, 933}]}, lu};
unicode_table(120509) ->
    {0, [], {font, [{0, 934}]}, lu};
unicode_table(120510) ->
    {0, [], {font, [{0, 935}]}, lu};
unicode_table(120511) ->
    {0, [], {font, [{0, 936}]}, lu};
unicode_table(120512) ->
    {0, [], {font, [{0, 937}]}, lu};
unicode_table(120513) ->
    {0, [], {font, [{0, 8711}]}, sm};
unicode_table(120514) ->
    {0, [], {font, [{0, 945}]}, ll};
unicode_table(120515) ->
    {0, [], {font, [{0, 946}]}, ll};
unicode_table(120516) ->
    {0, [], {font, [{0, 947}]}, ll};
unicode_table(120517) ->
    {0, [], {font, [{0, 948}]}, ll};
unicode_table(120518) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120519) ->
    {0, [], {font, [{0, 950}]}, ll};
unicode_table(120520) ->
    {0, [], {font, [{0, 951}]}, ll};
unicode_table(120521) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120522) ->
    {0, [], {font, [{0, 953}]}, ll};
unicode_table(120523) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120524) ->
    {0, [], {font, [{0, 955}]}, ll};
unicode_table(120525) ->
    {0, [], {font, [{0, 956}]}, ll};
unicode_table(120526) ->
    {0, [], {font, [{0, 957}]}, ll};
unicode_table(120527) ->
    {0, [], {font, [{0, 958}]}, ll};
unicode_table(120528) ->
    {0, [], {font, [{0, 959}]}, ll};
unicode_table(120529) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120530) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120531) ->
    {0, [], {font, [{0, 962}]}, ll};
unicode_table(120532) ->
    {0, [], {font, [{0, 963}]}, ll};
unicode_table(120533) ->
    {0, [], {font, [{0, 964}]}, ll};
unicode_table(120534) ->
    {0, [], {font, [{0, 965}]}, ll};
unicode_table(120535) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120536) ->
    {0, [], {font, [{0, 967}]}, ll};
unicode_table(120537) ->
    {0, [], {font, [{0, 968}]}, ll};
unicode_table(120538) ->
    {0, [], {font, [{0, 969}]}, ll};
unicode_table(120539) ->
    {0, [], {font, [{0, 8706}]}, sm};
unicode_table(120540) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120541) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120542) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120543) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120544) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120545) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120546) ->
    {0, [], {font, [{0, 913}]}, lu};
unicode_table(120547) ->
    {0, [], {font, [{0, 914}]}, lu};
unicode_table(120548) ->
    {0, [], {font, [{0, 915}]}, lu};
unicode_table(120549) ->
    {0, [], {font, [{0, 916}]}, lu};
unicode_table(120550) ->
    {0, [], {font, [{0, 917}]}, lu};
unicode_table(120551) ->
    {0, [], {font, [{0, 918}]}, lu};
unicode_table(120552) ->
    {0, [], {font, [{0, 919}]}, lu};
unicode_table(120553) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120554) ->
    {0, [], {font, [{0, 921}]}, lu};
unicode_table(120555) ->
    {0, [], {font, [{0, 922}]}, lu};
unicode_table(120556) ->
    {0, [], {font, [{0, 923}]}, lu};
unicode_table(120557) ->
    {0, [], {font, [{0, 924}]}, lu};
unicode_table(120558) ->
    {0, [], {font, [{0, 925}]}, lu};
unicode_table(120559) ->
    {0, [], {font, [{0, 926}]}, lu};
unicode_table(120560) ->
    {0, [], {font, [{0, 927}]}, lu};
unicode_table(120561) ->
    {0, [], {font, [{0, 928}]}, lu};
unicode_table(120562) ->
    {0, [], {font, [{0, 929}]}, lu};
unicode_table(120563) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120564) ->
    {0, [], {font, [{0, 931}]}, lu};
unicode_table(120565) ->
    {0, [], {font, [{0, 932}]}, lu};
unicode_table(120566) ->
    {0, [], {font, [{0, 933}]}, lu};
unicode_table(120567) ->
    {0, [], {font, [{0, 934}]}, lu};
unicode_table(120568) ->
    {0, [], {font, [{0, 935}]}, lu};
unicode_table(120569) ->
    {0, [], {font, [{0, 936}]}, lu};
unicode_table(120570) ->
    {0, [], {font, [{0, 937}]}, lu};
unicode_table(120571) ->
    {0, [], {font, [{0, 8711}]}, sm};
unicode_table(120572) ->
    {0, [], {font, [{0, 945}]}, ll};
unicode_table(120573) ->
    {0, [], {font, [{0, 946}]}, ll};
unicode_table(120574) ->
    {0, [], {font, [{0, 947}]}, ll};
unicode_table(120575) ->
    {0, [], {font, [{0, 948}]}, ll};
unicode_table(120576) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120577) ->
    {0, [], {font, [{0, 950}]}, ll};
unicode_table(120578) ->
    {0, [], {font, [{0, 951}]}, ll};
unicode_table(120579) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120580) ->
    {0, [], {font, [{0, 953}]}, ll};
unicode_table(120581) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120582) ->
    {0, [], {font, [{0, 955}]}, ll};
unicode_table(120583) ->
    {0, [], {font, [{0, 956}]}, ll};
unicode_table(120584) ->
    {0, [], {font, [{0, 957}]}, ll};
unicode_table(120585) ->
    {0, [], {font, [{0, 958}]}, ll};
unicode_table(120586) ->
    {0, [], {font, [{0, 959}]}, ll};
unicode_table(120587) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120588) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120589) ->
    {0, [], {font, [{0, 962}]}, ll};
unicode_table(120590) ->
    {0, [], {font, [{0, 963}]}, ll};
unicode_table(120591) ->
    {0, [], {font, [{0, 964}]}, ll};
unicode_table(120592) ->
    {0, [], {font, [{0, 965}]}, ll};
unicode_table(120593) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120594) ->
    {0, [], {font, [{0, 967}]}, ll};
unicode_table(120595) ->
    {0, [], {font, [{0, 968}]}, ll};
unicode_table(120596) ->
    {0, [], {font, [{0, 969}]}, ll};
unicode_table(120597) ->
    {0, [], {font, [{0, 8706}]}, sm};
unicode_table(120598) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120599) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120600) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120601) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120602) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120603) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120604) ->
    {0, [], {font, [{0, 913}]}, lu};
unicode_table(120605) ->
    {0, [], {font, [{0, 914}]}, lu};
unicode_table(120606) ->
    {0, [], {font, [{0, 915}]}, lu};
unicode_table(120607) ->
    {0, [], {font, [{0, 916}]}, lu};
unicode_table(120608) ->
    {0, [], {font, [{0, 917}]}, lu};
unicode_table(120609) ->
    {0, [], {font, [{0, 918}]}, lu};
unicode_table(120610) ->
    {0, [], {font, [{0, 919}]}, lu};
unicode_table(120611) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120612) ->
    {0, [], {font, [{0, 921}]}, lu};
unicode_table(120613) ->
    {0, [], {font, [{0, 922}]}, lu};
unicode_table(120614) ->
    {0, [], {font, [{0, 923}]}, lu};
unicode_table(120615) ->
    {0, [], {font, [{0, 924}]}, lu};
unicode_table(120616) ->
    {0, [], {font, [{0, 925}]}, lu};
unicode_table(120617) ->
    {0, [], {font, [{0, 926}]}, lu};
unicode_table(120618) ->
    {0, [], {font, [{0, 927}]}, lu};
unicode_table(120619) ->
    {0, [], {font, [{0, 928}]}, lu};
unicode_table(120620) ->
    {0, [], {font, [{0, 929}]}, lu};
unicode_table(120621) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120622) ->
    {0, [], {font, [{0, 931}]}, lu};
unicode_table(120623) ->
    {0, [], {font, [{0, 932}]}, lu};
unicode_table(120624) ->
    {0, [], {font, [{0, 933}]}, lu};
unicode_table(120625) ->
    {0, [], {font, [{0, 934}]}, lu};
unicode_table(120626) ->
    {0, [], {font, [{0, 935}]}, lu};
unicode_table(120627) ->
    {0, [], {font, [{0, 936}]}, lu};
unicode_table(120628) ->
    {0, [], {font, [{0, 937}]}, lu};
unicode_table(120629) ->
    {0, [], {font, [{0, 8711}]}, sm};
unicode_table(120630) ->
    {0, [], {font, [{0, 945}]}, ll};
unicode_table(120631) ->
    {0, [], {font, [{0, 946}]}, ll};
unicode_table(120632) ->
    {0, [], {font, [{0, 947}]}, ll};
unicode_table(120633) ->
    {0, [], {font, [{0, 948}]}, ll};
unicode_table(120634) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120635) ->
    {0, [], {font, [{0, 950}]}, ll};
unicode_table(120636) ->
    {0, [], {font, [{0, 951}]}, ll};
unicode_table(120637) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120638) ->
    {0, [], {font, [{0, 953}]}, ll};
unicode_table(120639) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120640) ->
    {0, [], {font, [{0, 955}]}, ll};
unicode_table(120641) ->
    {0, [], {font, [{0, 956}]}, ll};
unicode_table(120642) ->
    {0, [], {font, [{0, 957}]}, ll};
unicode_table(120643) ->
    {0, [], {font, [{0, 958}]}, ll};
unicode_table(120644) ->
    {0, [], {font, [{0, 959}]}, ll};
unicode_table(120645) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120646) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120647) ->
    {0, [], {font, [{0, 962}]}, ll};
unicode_table(120648) ->
    {0, [], {font, [{0, 963}]}, ll};
unicode_table(120649) ->
    {0, [], {font, [{0, 964}]}, ll};
unicode_table(120650) ->
    {0, [], {font, [{0, 965}]}, ll};
unicode_table(120651) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120652) ->
    {0, [], {font, [{0, 967}]}, ll};
unicode_table(120653) ->
    {0, [], {font, [{0, 968}]}, ll};
unicode_table(120654) ->
    {0, [], {font, [{0, 969}]}, ll};
unicode_table(120655) ->
    {0, [], {font, [{0, 8706}]}, sm};
unicode_table(120656) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120657) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120658) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120659) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120660) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120661) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120662) ->
    {0, [], {font, [{0, 913}]}, lu};
unicode_table(120663) ->
    {0, [], {font, [{0, 914}]}, lu};
unicode_table(120664) ->
    {0, [], {font, [{0, 915}]}, lu};
unicode_table(120665) ->
    {0, [], {font, [{0, 916}]}, lu};
unicode_table(120666) ->
    {0, [], {font, [{0, 917}]}, lu};
unicode_table(120667) ->
    {0, [], {font, [{0, 918}]}, lu};
unicode_table(120668) ->
    {0, [], {font, [{0, 919}]}, lu};
unicode_table(120669) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120670) ->
    {0, [], {font, [{0, 921}]}, lu};
unicode_table(120671) ->
    {0, [], {font, [{0, 922}]}, lu};
unicode_table(120672) ->
    {0, [], {font, [{0, 923}]}, lu};
unicode_table(120673) ->
    {0, [], {font, [{0, 924}]}, lu};
unicode_table(120674) ->
    {0, [], {font, [{0, 925}]}, lu};
unicode_table(120675) ->
    {0, [], {font, [{0, 926}]}, lu};
unicode_table(120676) ->
    {0, [], {font, [{0, 927}]}, lu};
unicode_table(120677) ->
    {0, [], {font, [{0, 928}]}, lu};
unicode_table(120678) ->
    {0, [], {font, [{0, 929}]}, lu};
unicode_table(120679) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120680) ->
    {0, [], {font, [{0, 931}]}, lu};
unicode_table(120681) ->
    {0, [], {font, [{0, 932}]}, lu};
unicode_table(120682) ->
    {0, [], {font, [{0, 933}]}, lu};
unicode_table(120683) ->
    {0, [], {font, [{0, 934}]}, lu};
unicode_table(120684) ->
    {0, [], {font, [{0, 935}]}, lu};
unicode_table(120685) ->
    {0, [], {font, [{0, 936}]}, lu};
unicode_table(120686) ->
    {0, [], {font, [{0, 937}]}, lu};
unicode_table(120687) ->
    {0, [], {font, [{0, 8711}]}, sm};
unicode_table(120688) ->
    {0, [], {font, [{0, 945}]}, ll};
unicode_table(120689) ->
    {0, [], {font, [{0, 946}]}, ll};
unicode_table(120690) ->
    {0, [], {font, [{0, 947}]}, ll};
unicode_table(120691) ->
    {0, [], {font, [{0, 948}]}, ll};
unicode_table(120692) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120693) ->
    {0, [], {font, [{0, 950}]}, ll};
unicode_table(120694) ->
    {0, [], {font, [{0, 951}]}, ll};
unicode_table(120695) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120696) ->
    {0, [], {font, [{0, 953}]}, ll};
unicode_table(120697) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120698) ->
    {0, [], {font, [{0, 955}]}, ll};
unicode_table(120699) ->
    {0, [], {font, [{0, 956}]}, ll};
unicode_table(120700) ->
    {0, [], {font, [{0, 957}]}, ll};
unicode_table(120701) ->
    {0, [], {font, [{0, 958}]}, ll};
unicode_table(120702) ->
    {0, [], {font, [{0, 959}]}, ll};
unicode_table(120703) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120704) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120705) ->
    {0, [], {font, [{0, 962}]}, ll};
unicode_table(120706) ->
    {0, [], {font, [{0, 963}]}, ll};
unicode_table(120707) ->
    {0, [], {font, [{0, 964}]}, ll};
unicode_table(120708) ->
    {0, [], {font, [{0, 965}]}, ll};
unicode_table(120709) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120710) ->
    {0, [], {font, [{0, 967}]}, ll};
unicode_table(120711) ->
    {0, [], {font, [{0, 968}]}, ll};
unicode_table(120712) ->
    {0, [], {font, [{0, 969}]}, ll};
unicode_table(120713) ->
    {0, [], {font, [{0, 8706}]}, sm};
unicode_table(120714) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120715) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120716) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120717) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120718) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120719) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120720) ->
    {0, [], {font, [{0, 913}]}, lu};
unicode_table(120721) ->
    {0, [], {font, [{0, 914}]}, lu};
unicode_table(120722) ->
    {0, [], {font, [{0, 915}]}, lu};
unicode_table(120723) ->
    {0, [], {font, [{0, 916}]}, lu};
unicode_table(120724) ->
    {0, [], {font, [{0, 917}]}, lu};
unicode_table(120725) ->
    {0, [], {font, [{0, 918}]}, lu};
unicode_table(120726) ->
    {0, [], {font, [{0, 919}]}, lu};
unicode_table(120727) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120728) ->
    {0, [], {font, [{0, 921}]}, lu};
unicode_table(120729) ->
    {0, [], {font, [{0, 922}]}, lu};
unicode_table(120730) ->
    {0, [], {font, [{0, 923}]}, lu};
unicode_table(120731) ->
    {0, [], {font, [{0, 924}]}, lu};
unicode_table(120732) ->
    {0, [], {font, [{0, 925}]}, lu};
unicode_table(120733) ->
    {0, [], {font, [{0, 926}]}, lu};
unicode_table(120734) ->
    {0, [], {font, [{0, 927}]}, lu};
unicode_table(120735) ->
    {0, [], {font, [{0, 928}]}, lu};
unicode_table(120736) ->
    {0, [], {font, [{0, 929}]}, lu};
unicode_table(120737) ->
    {0, [], {font, [{0, 920}]}, lu};
unicode_table(120738) ->
    {0, [], {font, [{0, 931}]}, lu};
unicode_table(120739) ->
    {0, [], {font, [{0, 932}]}, lu};
unicode_table(120740) ->
    {0, [], {font, [{0, 933}]}, lu};
unicode_table(120741) ->
    {0, [], {font, [{0, 934}]}, lu};
unicode_table(120742) ->
    {0, [], {font, [{0, 935}]}, lu};
unicode_table(120743) ->
    {0, [], {font, [{0, 936}]}, lu};
unicode_table(120744) ->
    {0, [], {font, [{0, 937}]}, lu};
unicode_table(120745) ->
    {0, [], {font, [{0, 8711}]}, sm};
unicode_table(120746) ->
    {0, [], {font, [{0, 945}]}, ll};
unicode_table(120747) ->
    {0, [], {font, [{0, 946}]}, ll};
unicode_table(120748) ->
    {0, [], {font, [{0, 947}]}, ll};
unicode_table(120749) ->
    {0, [], {font, [{0, 948}]}, ll};
unicode_table(120750) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120751) ->
    {0, [], {font, [{0, 950}]}, ll};
unicode_table(120752) ->
    {0, [], {font, [{0, 951}]}, ll};
unicode_table(120753) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120754) ->
    {0, [], {font, [{0, 953}]}, ll};
unicode_table(120755) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120756) ->
    {0, [], {font, [{0, 955}]}, ll};
unicode_table(120757) ->
    {0, [], {font, [{0, 956}]}, ll};
unicode_table(120758) ->
    {0, [], {font, [{0, 957}]}, ll};
unicode_table(120759) ->
    {0, [], {font, [{0, 958}]}, ll};
unicode_table(120760) ->
    {0, [], {font, [{0, 959}]}, ll};
unicode_table(120761) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120762) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120763) ->
    {0, [], {font, [{0, 962}]}, ll};
unicode_table(120764) ->
    {0, [], {font, [{0, 963}]}, ll};
unicode_table(120765) ->
    {0, [], {font, [{0, 964}]}, ll};
unicode_table(120766) ->
    {0, [], {font, [{0, 965}]}, ll};
unicode_table(120767) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120768) ->
    {0, [], {font, [{0, 967}]}, ll};
unicode_table(120769) ->
    {0, [], {font, [{0, 968}]}, ll};
unicode_table(120770) ->
    {0, [], {font, [{0, 969}]}, ll};
unicode_table(120771) ->
    {0, [], {font, [{0, 8706}]}, sm};
unicode_table(120772) ->
    {0, [], {font, [{0, 949}]}, ll};
unicode_table(120773) ->
    {0, [], {font, [{0, 952}]}, ll};
unicode_table(120774) ->
    {0, [], {font, [{0, 954}]}, ll};
unicode_table(120775) ->
    {0, [], {font, [{0, 966}]}, ll};
unicode_table(120776) ->
    {0, [], {font, [{0, 961}]}, ll};
unicode_table(120777) ->
    {0, [], {font, [{0, 960}]}, ll};
unicode_table(120778) ->
    {0, [], {font, [{0, 988}]}, lu};
unicode_table(120779) ->
    {0, [], {font, [{0, 989}]}, ll};
unicode_table(120782) ->
    {0, [], {font, [{0, 48}]}, nd};
unicode_table(120783) ->
    {0, [], {font, [{0, 49}]}, nd};
unicode_table(120784) ->
    {0, [], {font, [{0, 50}]}, nd};
unicode_table(120785) ->
    {0, [], {font, [{0, 51}]}, nd};
unicode_table(120786) ->
    {0, [], {font, [{0, 52}]}, nd};
unicode_table(120787) ->
    {0, [], {font, [{0, 53}]}, nd};
unicode_table(120788) ->
    {0, [], {font, [{0, 54}]}, nd};
unicode_table(120789) ->
    {0, [], {font, [{0, 55}]}, nd};
unicode_table(120790) ->
    {0, [], {font, [{0, 56}]}, nd};
unicode_table(120791) ->
    {0, [], {font, [{0, 57}]}, nd};
unicode_table(120792) ->
    {0, [], {font, [{0, 48}]}, nd};
unicode_table(120793) ->
    {0, [], {font, [{0, 49}]}, nd};
unicode_table(120794) ->
    {0, [], {font, [{0, 50}]}, nd};
unicode_table(120795) ->
    {0, [], {font, [{0, 51}]}, nd};
unicode_table(120796) ->
    {0, [], {font, [{0, 52}]}, nd};
unicode_table(120797) ->
    {0, [], {font, [{0, 53}]}, nd};
unicode_table(120798) ->
    {0, [], {font, [{0, 54}]}, nd};
unicode_table(120799) ->
    {0, [], {font, [{0, 55}]}, nd};
unicode_table(120800) ->
    {0, [], {font, [{0, 56}]}, nd};
unicode_table(120801) ->
    {0, [], {font, [{0, 57}]}, nd};
unicode_table(120802) ->
    {0, [], {font, [{0, 48}]}, nd};
unicode_table(120803) ->
    {0, [], {font, [{0, 49}]}, nd};
unicode_table(120804) ->
    {0, [], {font, [{0, 50}]}, nd};
unicode_table(120805) ->
    {0, [], {font, [{0, 51}]}, nd};
unicode_table(120806) ->
    {0, [], {font, [{0, 52}]}, nd};
unicode_table(120807) ->
    {0, [], {font, [{0, 53}]}, nd};
unicode_table(120808) ->
    {0, [], {font, [{0, 54}]}, nd};
unicode_table(120809) ->
    {0, [], {font, [{0, 55}]}, nd};
unicode_table(120810) ->
    {0, [], {font, [{0, 56}]}, nd};
unicode_table(120811) ->
    {0, [], {font, [{0, 57}]}, nd};
unicode_table(120812) ->
    {0, [], {font, [{0, 48}]}, nd};
unicode_table(120813) ->
    {0, [], {font, [{0, 49}]}, nd};
unicode_table(120814) ->
    {0, [], {font, [{0, 50}]}, nd};
unicode_table(120815) ->
    {0, [], {font, [{0, 51}]}, nd};
unicode_table(120816) ->
    {0, [], {font, [{0, 52}]}, nd};
unicode_table(120817) ->
    {0, [], {font, [{0, 53}]}, nd};
unicode_table(120818) ->
    {0, [], {font, [{0, 54}]}, nd};
unicode_table(120819) ->
    {0, [], {font, [{0, 55}]}, nd};
unicode_table(120820) ->
    {0, [], {font, [{0, 56}]}, nd};
unicode_table(120821) ->
    {0, [], {font, [{0, 57}]}, nd};
unicode_table(120822) ->
    {0, [], {font, [{0, 48}]}, nd};
unicode_table(120823) ->
    {0, [], {font, [{0, 49}]}, nd};
unicode_table(120824) ->
    {0, [], {font, [{0, 50}]}, nd};
unicode_table(120825) ->
    {0, [], {font, [{0, 51}]}, nd};
unicode_table(120826) ->
    {0, [], {font, [{0, 52}]}, nd};
unicode_table(120827) ->
    {0, [], {font, [{0, 53}]}, nd};
unicode_table(120828) ->
    {0, [], {font, [{0, 54}]}, nd};
unicode_table(120829) ->
    {0, [], {font, [{0, 55}]}, nd};
unicode_table(120830) ->
    {0, [], {font, [{0, 56}]}, nd};
unicode_table(120831) ->
    {0, [], {font, [{0, 57}]}, nd};
unicode_table(122880) ->
    {230, [], [], mn};
unicode_table(122881) ->
    {230, [], [], mn};
unicode_table(122882) ->
    {230, [], [], mn};
unicode_table(122883) ->
    {230, [], [], mn};
unicode_table(122884) ->
    {230, [], [], mn};
unicode_table(122885) ->
    {230, [], [], mn};
unicode_table(122886) ->
    {230, [], [], mn};
unicode_table(122888) ->
    {230, [], [], mn};
unicode_table(122889) ->
    {230, [], [], mn};
unicode_table(122890) ->
    {230, [], [], mn};
unicode_table(122891) ->
    {230, [], [], mn};
unicode_table(122892) ->
    {230, [], [], mn};
unicode_table(122893) ->
    {230, [], [], mn};
unicode_table(122894) ->
    {230, [], [], mn};
unicode_table(122895) ->
    {230, [], [], mn};
unicode_table(122896) ->
    {230, [], [], mn};
unicode_table(122897) ->
    {230, [], [], mn};
unicode_table(122898) ->
    {230, [], [], mn};
unicode_table(122899) ->
    {230, [], [], mn};
unicode_table(122900) ->
    {230, [], [], mn};
unicode_table(122901) ->
    {230, [], [], mn};
unicode_table(122902) ->
    {230, [], [], mn};
unicode_table(122903) ->
    {230, [], [], mn};
unicode_table(122904) ->
    {230, [], [], mn};
unicode_table(122907) ->
    {230, [], [], mn};
unicode_table(122908) ->
    {230, [], [], mn};
unicode_table(122909) ->
    {230, [], [], mn};
unicode_table(122910) ->
    {230, [], [], mn};
unicode_table(122911) ->
    {230, [], [], mn};
unicode_table(122912) ->
    {230, [], [], mn};
unicode_table(122913) ->
    {230, [], [], mn};
unicode_table(122915) ->
    {230, [], [], mn};
unicode_table(122916) ->
    {230, [], [], mn};
unicode_table(122918) ->
    {230, [], [], mn};
unicode_table(122919) ->
    {230, [], [], mn};
unicode_table(122920) ->
    {230, [], [], mn};
unicode_table(122921) ->
    {230, [], [], mn};
unicode_table(122922) ->
    {230, [], [], mn};
unicode_table(122928) ->
    {0, [], {super, [{0, 1072}]}, lm};
unicode_table(122929) ->
    {0, [], {super, [{0, 1073}]}, lm};
unicode_table(122930) ->
    {0, [], {super, [{0, 1074}]}, lm};
unicode_table(122931) ->
    {0, [], {super, [{0, 1075}]}, lm};
unicode_table(122932) ->
    {0, [], {super, [{0, 1076}]}, lm};
unicode_table(122933) ->
    {0, [], {super, [{0, 1077}]}, lm};
unicode_table(122934) ->
    {0, [], {super, [{0, 1078}]}, lm};
unicode_table(122935) ->
    {0, [], {super, [{0, 1079}]}, lm};
unicode_table(122936) ->
    {0, [], {super, [{0, 1080}]}, lm};
unicode_table(122937) ->
    {0, [], {super, [{0, 1082}]}, lm};
unicode_table(122938) ->
    {0, [], {super, [{0, 1083}]}, lm};
unicode_table(122939) ->
    {0, [], {super, [{0, 1084}]}, lm};
unicode_table(122940) ->
    {0, [], {super, [{0, 1086}]}, lm};
unicode_table(122941) ->
    {0, [], {super, [{0, 1087}]}, lm};
unicode_table(122942) ->
    {0, [], {super, [{0, 1088}]}, lm};
unicode_table(122943) ->
    {0, [], {super, [{0, 1089}]}, lm};
unicode_table(122944) ->
    {0, [], {super, [{0, 1090}]}, lm};
unicode_table(122945) ->
    {0, [], {super, [{0, 1091}]}, lm};
unicode_table(122946) ->
    {0, [], {super, [{0, 1092}]}, lm};
unicode_table(122947) ->
    {0, [], {super, [{0, 1093}]}, lm};
unicode_table(122948) ->
    {0, [], {super, [{0, 1094}]}, lm};
unicode_table(122949) ->
    {0, [], {super, [{0, 1095}]}, lm};
unicode_table(122950) ->
    {0, [], {super, [{0, 1096}]}, lm};
unicode_table(122951) ->
    {0, [], {super, [{0, 1099}]}, lm};
unicode_table(122952) ->
    {0, [], {super, [{0, 1101}]}, lm};
unicode_table(122953) ->
    {0, [], {super, [{0, 1102}]}, lm};
unicode_table(122954) ->
    {0, [], {super, [{0, 42633}]}, lm};
unicode_table(122955) ->
    {0, [], {super, [{0, 1241}]}, lm};
unicode_table(122956) ->
    {0, [], {super, [{0, 1110}]}, lm};
unicode_table(122957) ->
    {0, [], {super, [{0, 1112}]}, lm};
unicode_table(122958) ->
    {0, [], {super, [{0, 1257}]}, lm};
unicode_table(122959) ->
    {0, [], {super, [{0, 1199}]}, lm};
unicode_table(122960) ->
    {0, [], {super, [{0, 1231}]}, lm};
unicode_table(122961) ->
    {0, [], {sub, [{0, 1072}]}, lm};
unicode_table(122962) ->
    {0, [], {sub, [{0, 1073}]}, lm};
unicode_table(122963) ->
    {0, [], {sub, [{0, 1074}]}, lm};
unicode_table(122964) ->
    {0, [], {sub, [{0, 1075}]}, lm};
unicode_table(122965) ->
    {0, [], {sub, [{0, 1076}]}, lm};
unicode_table(122966) ->
    {0, [], {sub, [{0, 1077}]}, lm};
unicode_table(122967) ->
    {0, [], {sub, [{0, 1078}]}, lm};
unicode_table(122968) ->
    {0, [], {sub, [{0, 1079}]}, lm};
unicode_table(122969) ->
    {0, [], {sub, [{0, 1080}]}, lm};
unicode_table(122970) ->
    {0, [], {sub, [{0, 1082}]}, lm};
unicode_table(122971) ->
    {0, [], {sub, [{0, 1083}]}, lm};
unicode_table(122972) ->
    {0, [], {sub, [{0, 1086}]}, lm};
unicode_table(122973) ->
    {0, [], {sub, [{0, 1087}]}, lm};
unicode_table(122974) ->
    {0, [], {sub, [{0, 1089}]}, lm};
unicode_table(122975) ->
    {0, [], {sub, [{0, 1091}]}, lm};
unicode_table(122976) ->
    {0, [], {sub, [{0, 1092}]}, lm};
unicode_table(122977) ->
    {0, [], {sub, [{0, 1093}]}, lm};
unicode_table(122978) ->
    {0, [], {sub, [{0, 1094}]}, lm};
unicode_table(122979) ->
    {0, [], {sub, [{0, 1095}]}, lm};
unicode_table(122980) ->
    {0, [], {sub, [{0, 1096}]}, lm};
unicode_table(122981) ->
    {0, [], {sub, [{0, 1098}]}, lm};
unicode_table(122982) ->
    {0, [], {sub, [{0, 1099}]}, lm};
unicode_table(122983) ->
    {0, [], {sub, [{0, 1169}]}, lm};
unicode_table(122984) ->
    {0, [], {sub, [{0, 1110}]}, lm};
unicode_table(122985) ->
    {0, [], {sub, [{0, 1109}]}, lm};
unicode_table(122986) ->
    {0, [], {sub, [{0, 1119}]}, lm};
unicode_table(122987) ->
    {0, [], {super, [{0, 1195}]}, lm};
unicode_table(122988) ->
    {0, [], {super, [{0, 42577}]}, lm};
unicode_table(122989) ->
    {0, [], {super, [{0, 1201}]}, lm};
unicode_table(123023) ->
    {230, [], [], mn};
unicode_table(123184) ->
    {230, [], [], mn};
unicode_table(123185) ->
    {230, [], [], mn};
unicode_table(123186) ->
    {230, [], [], mn};
unicode_table(123187) ->
    {230, [], [], mn};
unicode_table(123188) ->
    {230, [], [], mn};
unicode_table(123189) ->
    {230, [], [], mn};
unicode_table(123190) ->
    {230, [], [], mn};
unicode_table(123566) ->
    {230, [], [], mn};
unicode_table(123628) ->
    {230, [], [], mn};
unicode_table(123629) ->
    {230, [], [], mn};
unicode_table(123630) ->
    {230, [], [], mn};
unicode_table(123631) ->
    {230, [], [], mn};
unicode_table(124140) ->
    {232, [], [], mn};
unicode_table(124141) ->
    {232, [], [], mn};
unicode_table(124142) ->
    {220, [], [], mn};
unicode_table(124143) ->
    {230, [], [], mn};
unicode_table(125136) ->
    {220, [], [], mn};
unicode_table(125137) ->
    {220, [], [], mn};
unicode_table(125138) ->
    {220, [], [], mn};
unicode_table(125139) ->
    {220, [], [], mn};
unicode_table(125140) ->
    {220, [], [], mn};
unicode_table(125141) ->
    {220, [], [], mn};
unicode_table(125142) ->
    {220, [], [], mn};
unicode_table(125252) ->
    {230, [], [], mn};
unicode_table(125253) ->
    {230, [], [], mn};
unicode_table(125254) ->
    {230, [], [], mn};
unicode_table(125255) ->
    {230, [], [], mn};
unicode_table(125256) ->
    {230, [], [], mn};
unicode_table(125257) ->
    {230, [], [], mn};
unicode_table(125258) ->
    {7, [], [], mn};
unicode_table(126464) ->
    {0, [], {font, [{0, 1575}]}, lo};
unicode_table(126465) ->
    {0, [], {font, [{0, 1576}]}, lo};
unicode_table(126466) ->
    {0, [], {font, [{0, 1580}]}, lo};
unicode_table(126467) ->
    {0, [], {font, [{0, 1583}]}, lo};
unicode_table(126469) ->
    {0, [], {font, [{0, 1608}]}, lo};
unicode_table(126470) ->
    {0, [], {font, [{0, 1586}]}, lo};
unicode_table(126471) ->
    {0, [], {font, [{0, 1581}]}, lo};
unicode_table(126472) ->
    {0, [], {font, [{0, 1591}]}, lo};
unicode_table(126473) ->
    {0, [], {font, [{0, 1610}]}, lo};
unicode_table(126474) ->
    {0, [], {font, [{0, 1603}]}, lo};
unicode_table(126475) ->
    {0, [], {font, [{0, 1604}]}, lo};
unicode_table(126476) ->
    {0, [], {font, [{0, 1605}]}, lo};
unicode_table(126477) ->
    {0, [], {font, [{0, 1606}]}, lo};
unicode_table(126478) ->
    {0, [], {font, [{0, 1587}]}, lo};
unicode_table(126479) ->
    {0, [], {font, [{0, 1593}]}, lo};
unicode_table(126480) ->
    {0, [], {font, [{0, 1601}]}, lo};
unicode_table(126481) ->
    {0, [], {font, [{0, 1589}]}, lo};
unicode_table(126482) ->
    {0, [], {font, [{0, 1602}]}, lo};
unicode_table(126483) ->
    {0, [], {font, [{0, 1585}]}, lo};
unicode_table(126484) ->
    {0, [], {font, [{0, 1588}]}, lo};
unicode_table(126485) ->
    {0, [], {font, [{0, 1578}]}, lo};
unicode_table(126486) ->
    {0, [], {font, [{0, 1579}]}, lo};
unicode_table(126487) ->
    {0, [], {font, [{0, 1582}]}, lo};
unicode_table(126488) ->
    {0, [], {font, [{0, 1584}]}, lo};
unicode_table(126489) ->
    {0, [], {font, [{0, 1590}]}, lo};
unicode_table(126490) ->
    {0, [], {font, [{0, 1592}]}, lo};
unicode_table(126491) ->
    {0, [], {font, [{0, 1594}]}, lo};
unicode_table(126492) ->
    {0, [], {font, [{0, 1646}]}, lo};
unicode_table(126493) ->
    {0, [], {font, [{0, 1722}]}, lo};
unicode_table(126494) ->
    {0, [], {font, [{0, 1697}]}, lo};
unicode_table(126495) ->
    {0, [], {font, [{0, 1647}]}, lo};
unicode_table(126497) ->
    {0, [], {font, [{0, 1576}]}, lo};
unicode_table(126498) ->
    {0, [], {font, [{0, 1580}]}, lo};
unicode_table(126500) ->
    {0, [], {font, [{0, 1607}]}, lo};
unicode_table(126503) ->
    {0, [], {font, [{0, 1581}]}, lo};
unicode_table(126505) ->
    {0, [], {font, [{0, 1610}]}, lo};
unicode_table(126506) ->
    {0, [], {font, [{0, 1603}]}, lo};
unicode_table(126507) ->
    {0, [], {font, [{0, 1604}]}, lo};
unicode_table(126508) ->
    {0, [], {font, [{0, 1605}]}, lo};
unicode_table(126509) ->
    {0, [], {font, [{0, 1606}]}, lo};
unicode_table(126510) ->
    {0, [], {font, [{0, 1587}]}, lo};
unicode_table(126511) ->
    {0, [], {font, [{0, 1593}]}, lo};
unicode_table(126512) ->
    {0, [], {font, [{0, 1601}]}, lo};
unicode_table(126513) ->
    {0, [], {font, [{0, 1589}]}, lo};
unicode_table(126514) ->
    {0, [], {font, [{0, 1602}]}, lo};
unicode_table(126516) ->
    {0, [], {font, [{0, 1588}]}, lo};
unicode_table(126517) ->
    {0, [], {font, [{0, 1578}]}, lo};
unicode_table(126518) ->
    {0, [], {font, [{0, 1579}]}, lo};
unicode_table(126519) ->
    {0, [], {font, [{0, 1582}]}, lo};
unicode_table(126521) ->
    {0, [], {font, [{0, 1590}]}, lo};
unicode_table(126523) ->
    {0, [], {font, [{0, 1594}]}, lo};
unicode_table(126530) ->
    {0, [], {font, [{0, 1580}]}, lo};
unicode_table(126535) ->
    {0, [], {font, [{0, 1581}]}, lo};
unicode_table(126537) ->
    {0, [], {font, [{0, 1610}]}, lo};
unicode_table(126539) ->
    {0, [], {font, [{0, 1604}]}, lo};
unicode_table(126541) ->
    {0, [], {font, [{0, 1606}]}, lo};
unicode_table(126542) ->
    {0, [], {font, [{0, 1587}]}, lo};
unicode_table(126543) ->
    {0, [], {font, [{0, 1593}]}, lo};
unicode_table(126545) ->
    {0, [], {font, [{0, 1589}]}, lo};
unicode_table(126546) ->
    {0, [], {font, [{0, 1602}]}, lo};
unicode_table(126548) ->
    {0, [], {font, [{0, 1588}]}, lo};
unicode_table(126551) ->
    {0, [], {font, [{0, 1582}]}, lo};
unicode_table(126553) ->
    {0, [], {font, [{0, 1590}]}, lo};
unicode_table(126555) ->
    {0, [], {font, [{0, 1594}]}, lo};
unicode_table(126557) ->
    {0, [], {font, [{0, 1722}]}, lo};
unicode_table(126559) ->
    {0, [], {font, [{0, 1647}]}, lo};
unicode_table(126561) ->
    {0, [], {font, [{0, 1576}]}, lo};
unicode_table(126562) ->
    {0, [], {font, [{0, 1580}]}, lo};
unicode_table(126564) ->
    {0, [], {font, [{0, 1607}]}, lo};
unicode_table(126567) ->
    {0, [], {font, [{0, 1581}]}, lo};
unicode_table(126568) ->
    {0, [], {font, [{0, 1591}]}, lo};
unicode_table(126569) ->
    {0, [], {font, [{0, 1610}]}, lo};
unicode_table(126570) ->
    {0, [], {font, [{0, 1603}]}, lo};
unicode_table(126572) ->
    {0, [], {font, [{0, 1605}]}, lo};
unicode_table(126573) ->
    {0, [], {font, [{0, 1606}]}, lo};
unicode_table(126574) ->
    {0, [], {font, [{0, 1587}]}, lo};
unicode_table(126575) ->
    {0, [], {font, [{0, 1593}]}, lo};
unicode_table(126576) ->
    {0, [], {font, [{0, 1601}]}, lo};
unicode_table(126577) ->
    {0, [], {font, [{0, 1589}]}, lo};
unicode_table(126578) ->
    {0, [], {font, [{0, 1602}]}, lo};
unicode_table(126580) ->
    {0, [], {font, [{0, 1588}]}, lo};
unicode_table(126581) ->
    {0, [], {font, [{0, 1578}]}, lo};
unicode_table(126582) ->
    {0, [], {font, [{0, 1579}]}, lo};
unicode_table(126583) ->
    {0, [], {font, [{0, 1582}]}, lo};
unicode_table(126585) ->
    {0, [], {font, [{0, 1590}]}, lo};
unicode_table(126586) ->
    {0, [], {font, [{0, 1592}]}, lo};
unicode_table(126587) ->
    {0, [], {font, [{0, 1594}]}, lo};
unicode_table(126588) ->
    {0, [], {font, [{0, 1646}]}, lo};
unicode_table(126590) ->
    {0, [], {font, [{0, 1697}]}, lo};
unicode_table(126592) ->
    {0, [], {font, [{0, 1575}]}, lo};
unicode_table(126593) ->
    {0, [], {font, [{0, 1576}]}, lo};
unicode_table(126594) ->
    {0, [], {font, [{0, 1580}]}, lo};
unicode_table(126595) ->
    {0, [], {font, [{0, 1583}]}, lo};
unicode_table(126596) ->
    {0, [], {font, [{0, 1607}]}, lo};
unicode_table(126597) ->
    {0, [], {font, [{0, 1608}]}, lo};
unicode_table(126598) ->
    {0, [], {font, [{0, 1586}]}, lo};
unicode_table(126599) ->
    {0, [], {font, [{0, 1581}]}, lo};
unicode_table(126600) ->
    {0, [], {font, [{0, 1591}]}, lo};
unicode_table(126601) ->
    {0, [], {font, [{0, 1610}]}, lo};
unicode_table(126603) ->
    {0, [], {font, [{0, 1604}]}, lo};
unicode_table(126604) ->
    {0, [], {font, [{0, 1605}]}, lo};
unicode_table(126605) ->
    {0, [], {font, [{0, 1606}]}, lo};
unicode_table(126606) ->
    {0, [], {font, [{0, 1587}]}, lo};
unicode_table(126607) ->
    {0, [], {font, [{0, 1593}]}, lo};
unicode_table(126608) ->
    {0, [], {font, [{0, 1601}]}, lo};
unicode_table(126609) ->
    {0, [], {font, [{0, 1589}]}, lo};
unicode_table(126610) ->
    {0, [], {font, [{0, 1602}]}, lo};
unicode_table(126611) ->
    {0, [], {font, [{0, 1585}]}, lo};
unicode_table(126612) ->
    {0, [], {font, [{0, 1588}]}, lo};
unicode_table(126613) ->
    {0, [], {font, [{0, 1578}]}, lo};
unicode_table(126614) ->
    {0, [], {font, [{0, 1579}]}, lo};
unicode_table(126615) ->
    {0, [], {font, [{0, 1582}]}, lo};
unicode_table(126616) ->
    {0, [], {font, [{0, 1584}]}, lo};
unicode_table(126617) ->
    {0, [], {font, [{0, 1590}]}, lo};
unicode_table(126618) ->
    {0, [], {font, [{0, 1592}]}, lo};
unicode_table(126619) ->
    {0, [], {font, [{0, 1594}]}, lo};
unicode_table(126625) ->
    {0, [], {font, [{0, 1576}]}, lo};
unicode_table(126626) ->
    {0, [], {font, [{0, 1580}]}, lo};
unicode_table(126627) ->
    {0, [], {font, [{0, 1583}]}, lo};
unicode_table(126629) ->
    {0, [], {font, [{0, 1608}]}, lo};
unicode_table(126630) ->
    {0, [], {font, [{0, 1586}]}, lo};
unicode_table(126631) ->
    {0, [], {font, [{0, 1581}]}, lo};
unicode_table(126632) ->
    {0, [], {font, [{0, 1591}]}, lo};
unicode_table(126633) ->
    {0, [], {font, [{0, 1610}]}, lo};
unicode_table(126635) ->
    {0, [], {font, [{0, 1604}]}, lo};
unicode_table(126636) ->
    {0, [], {font, [{0, 1605}]}, lo};
unicode_table(126637) ->
    {0, [], {font, [{0, 1606}]}, lo};
unicode_table(126638) ->
    {0, [], {font, [{0, 1587}]}, lo};
unicode_table(126639) ->
    {0, [], {font, [{0, 1593}]}, lo};
unicode_table(126640) ->
    {0, [], {font, [{0, 1601}]}, lo};
unicode_table(126641) ->
    {0, [], {font, [{0, 1589}]}, lo};
unicode_table(126642) ->
    {0, [], {font, [{0, 1602}]}, lo};
unicode_table(126643) ->
    {0, [], {font, [{0, 1585}]}, lo};
unicode_table(126644) ->
    {0, [], {font, [{0, 1588}]}, lo};
unicode_table(126645) ->
    {0, [], {font, [{0, 1578}]}, lo};
unicode_table(126646) ->
    {0, [], {font, [{0, 1579}]}, lo};
unicode_table(126647) ->
    {0, [], {font, [{0, 1582}]}, lo};
unicode_table(126648) ->
    {0, [], {font, [{0, 1584}]}, lo};
unicode_table(126649) ->
    {0, [], {font, [{0, 1590}]}, lo};
unicode_table(126650) ->
    {0, [], {font, [{0, 1592}]}, lo};
unicode_table(126651) ->
    {0, [], {font, [{0, 1594}]}, lo};
unicode_table(127232) ->
    {0, [], {compat, [{0, 48}, {0, 46}]}, no};
unicode_table(127233) ->
    {0, [], {compat, [{0, 48}, {0, 44}]}, no};
unicode_table(127234) ->
    {0, [], {compat, [{0, 49}, {0, 44}]}, no};
unicode_table(127235) ->
    {0, [], {compat, [{0, 50}, {0, 44}]}, no};
unicode_table(127236) ->
    {0, [], {compat, [{0, 51}, {0, 44}]}, no};
unicode_table(127237) ->
    {0, [], {compat, [{0, 52}, {0, 44}]}, no};
unicode_table(127238) ->
    {0, [], {compat, [{0, 53}, {0, 44}]}, no};
unicode_table(127239) ->
    {0, [], {compat, [{0, 54}, {0, 44}]}, no};
unicode_table(127240) ->
    {0, [], {compat, [{0, 55}, {0, 44}]}, no};
unicode_table(127241) ->
    {0, [], {compat, [{0, 56}, {0, 44}]}, no};
unicode_table(127242) ->
    {0, [], {compat, [{0, 57}, {0, 44}]}, no};
unicode_table(127248) ->
    {0, [], {compat, [{0, 40}, {0, 65}, {0, 41}]}, so};
unicode_table(127249) ->
    {0, [], {compat, [{0, 40}, {0, 66}, {0, 41}]}, so};
unicode_table(127250) ->
    {0, [], {compat, [{0, 40}, {0, 67}, {0, 41}]}, so};
unicode_table(127251) ->
    {0, [], {compat, [{0, 40}, {0, 68}, {0, 41}]}, so};
unicode_table(127252) ->
    {0, [], {compat, [{0, 40}, {0, 69}, {0, 41}]}, so};
unicode_table(127253) ->
    {0, [], {compat, [{0, 40}, {0, 70}, {0, 41}]}, so};
unicode_table(127254) ->
    {0, [], {compat, [{0, 40}, {0, 71}, {0, 41}]}, so};
unicode_table(127255) ->
    {0, [], {compat, [{0, 40}, {0, 72}, {0, 41}]}, so};
unicode_table(127256) ->
    {0, [], {compat, [{0, 40}, {0, 73}, {0, 41}]}, so};
unicode_table(127257) ->
    {0, [], {compat, [{0, 40}, {0, 74}, {0, 41}]}, so};
unicode_table(127258) ->
    {0, [], {compat, [{0, 40}, {0, 75}, {0, 41}]}, so};
unicode_table(127259) ->
    {0, [], {compat, [{0, 40}, {0, 76}, {0, 41}]}, so};
unicode_table(127260) ->
    {0, [], {compat, [{0, 40}, {0, 77}, {0, 41}]}, so};
unicode_table(127261) ->
    {0, [], {compat, [{0, 40}, {0, 78}, {0, 41}]}, so};
unicode_table(127262) ->
    {0, [], {compat, [{0, 40}, {0, 79}, {0, 41}]}, so};
unicode_table(127263) ->
    {0, [], {compat, [{0, 40}, {0, 80}, {0, 41}]}, so};
unicode_table(127264) ->
    {0, [], {compat, [{0, 40}, {0, 81}, {0, 41}]}, so};
unicode_table(127265) ->
    {0, [], {compat, [{0, 40}, {0, 82}, {0, 41}]}, so};
unicode_table(127266) ->
    {0, [], {compat, [{0, 40}, {0, 83}, {0, 41}]}, so};
unicode_table(127267) ->
    {0, [], {compat, [{0, 40}, {0, 84}, {0, 41}]}, so};
unicode_table(127268) ->
    {0, [], {compat, [{0, 40}, {0, 85}, {0, 41}]}, so};
unicode_table(127269) ->
    {0, [], {compat, [{0, 40}, {0, 86}, {0, 41}]}, so};
unicode_table(127270) ->
    {0, [], {compat, [{0, 40}, {0, 87}, {0, 41}]}, so};
unicode_table(127271) ->
    {0, [], {compat, [{0, 40}, {0, 88}, {0, 41}]}, so};
unicode_table(127272) ->
    {0, [], {compat, [{0, 40}, {0, 89}, {0, 41}]}, so};
unicode_table(127273) ->
    {0, [], {compat, [{0, 40}, {0, 90}, {0, 41}]}, so};
unicode_table(127274) ->
    {0, [], {compat, [{0, 12308}, {0, 83}, {0, 12309}]}, so};
unicode_table(127275) ->
    {0, [], {circle, [{0, 67}]}, so};
unicode_table(127276) ->
    {0, [], {circle, [{0, 82}]}, so};
unicode_table(127277) ->
    {0, [], {circle, [{0, 67}, {0, 68}]}, so};
unicode_table(127278) ->
    {0, [], {circle, [{0, 87}, {0, 90}]}, so};
unicode_table(127280) ->
    {0, [], {square, [{0, 65}]}, so};
unicode_table(127281) ->
    {0, [], {square, [{0, 66}]}, so};
unicode_table(127282) ->
    {0, [], {square, [{0, 67}]}, so};
unicode_table(127283) ->
    {0, [], {square, [{0, 68}]}, so};
unicode_table(127284) ->
    {0, [], {square, [{0, 69}]}, so};
unicode_table(127285) ->
    {0, [], {square, [{0, 70}]}, so};
unicode_table(127286) ->
    {0, [], {square, [{0, 71}]}, so};
unicode_table(127287) ->
    {0, [], {square, [{0, 72}]}, so};
unicode_table(127288) ->
    {0, [], {square, [{0, 73}]}, so};
unicode_table(127289) ->
    {0, [], {square, [{0, 74}]}, so};
unicode_table(127290) ->
    {0, [], {square, [{0, 75}]}, so};
unicode_table(127291) ->
    {0, [], {square, [{0, 76}]}, so};
unicode_table(127292) ->
    {0, [], {square, [{0, 77}]}, so};
unicode_table(127293) ->
    {0, [], {square, [{0, 78}]}, so};
unicode_table(127294) ->
    {0, [], {square, [{0, 79}]}, so};
unicode_table(127295) ->
    {0, [], {square, [{0, 80}]}, so};
unicode_table(127296) ->
    {0, [], {square, [{0, 81}]}, so};
unicode_table(127297) ->
    {0, [], {square, [{0, 82}]}, so};
unicode_table(127298) ->
    {0, [], {square, [{0, 83}]}, so};
unicode_table(127299) ->
    {0, [], {square, [{0, 84}]}, so};
unicode_table(127300) ->
    {0, [], {square, [{0, 85}]}, so};
unicode_table(127301) ->
    {0, [], {square, [{0, 86}]}, so};
unicode_table(127302) ->
    {0, [], {square, [{0, 87}]}, so};
unicode_table(127303) ->
    {0, [], {square, [{0, 88}]}, so};
unicode_table(127304) ->
    {0, [], {square, [{0, 89}]}, so};
unicode_table(127305) ->
    {0, [], {square, [{0, 90}]}, so};
unicode_table(127306) ->
    {0, [], {square, [{0, 72}, {0, 86}]}, so};
unicode_table(127307) ->
    {0, [], {square, [{0, 77}, {0, 86}]}, so};
unicode_table(127308) ->
    {0, [], {square, [{0, 83}, {0, 68}]}, so};
unicode_table(127309) ->
    {0, [], {square, [{0, 83}, {0, 83}]}, so};
unicode_table(127310) ->
    {0, [], {square, [{0, 80}, {0, 80}, {0, 86}]}, so};
unicode_table(127311) ->
    {0, [], {square, [{0, 87}, {0, 67}]}, so};
unicode_table(127338) ->
    {0, [], {super, [{0, 77}, {0, 67}]}, so};
unicode_table(127339) ->
    {0, [], {super, [{0, 77}, {0, 68}]}, so};
unicode_table(127340) ->
    {0, [], {super, [{0, 77}, {0, 82}]}, so};
unicode_table(127376) ->
    {0, [], {square, [{0, 68}, {0, 74}]}, so};
unicode_table(127488) ->
    {0, [], {square, [{0, 12411}, {0, 12363}]}, so};
unicode_table(127489) ->
    {0, [], {square, [{0, 12467}, {0, 12467}]}, so};
unicode_table(127490) ->
    {0, [], {square, [{0, 12469}]}, so};
unicode_table(127504) ->
    {0, [], {square, [{0, 25163}]}, so};
unicode_table(127505) ->
    {0, [], {square, [{0, 23383}]}, so};
unicode_table(127506) ->
    {0, [], {square, [{0, 21452}]}, so};
unicode_table(127507) ->
    {0, [], {square, [{0, 12486}, {8, 12441}]}, so};
unicode_table(127508) ->
    {0, [], {square, [{0, 20108}]}, so};
unicode_table(127509) ->
    {0, [], {square, [{0, 22810}]}, so};
unicode_table(127510) ->
    {0, [], {square, [{0, 35299}]}, so};
unicode_table(127511) ->
    {0, [], {square, [{0, 22825}]}, so};
unicode_table(127512) ->
    {0, [], {square, [{0, 20132}]}, so};
unicode_table(127513) ->
    {0, [], {square, [{0, 26144}]}, so};
unicode_table(127514) ->
    {0, [], {square, [{0, 28961}]}, so};
unicode_table(127515) ->
    {0, [], {square, [{0, 26009}]}, so};
unicode_table(127516) ->
    {0, [], {square, [{0, 21069}]}, so};
unicode_table(127517) ->
    {0, [], {square, [{0, 24460}]}, so};
unicode_table(127518) ->
    {0, [], {square, [{0, 20877}]}, so};
unicode_table(127519) ->
    {0, [], {square, [{0, 26032}]}, so};
unicode_table(127520) ->
    {0, [], {square, [{0, 21021}]}, so};
unicode_table(127521) ->
    {0, [], {square, [{0, 32066}]}, so};
unicode_table(127522) ->
    {0, [], {square, [{0, 29983}]}, so};
unicode_table(127523) ->
    {0, [], {square, [{0, 36009}]}, so};
unicode_table(127524) ->
    {0, [], {square, [{0, 22768}]}, so};
unicode_table(127525) ->
    {0, [], {square, [{0, 21561}]}, so};
unicode_table(127526) ->
    {0, [], {square, [{0, 28436}]}, so};
unicode_table(127527) ->
    {0, [], {square, [{0, 25237}]}, so};
unicode_table(127528) ->
    {0, [], {square, [{0, 25429}]}, so};
unicode_table(127529) ->
    {0, [], {square, [{0, 19968}]}, so};
unicode_table(127530) ->
    {0, [], {square, [{0, 19977}]}, so};
unicode_table(127531) ->
    {0, [], {square, [{0, 36938}]}, so};
unicode_table(127532) ->
    {0, [], {square, [{0, 24038}]}, so};
unicode_table(127533) ->
    {0, [], {square, [{0, 20013}]}, so};
unicode_table(127534) ->
    {0, [], {square, [{0, 21491}]}, so};
unicode_table(127535) ->
    {0, [], {square, [{0, 25351}]}, so};
unicode_table(127536) ->
    {0, [], {square, [{0, 36208}]}, so};
unicode_table(127537) ->
    {0, [], {square, [{0, 25171}]}, so};
unicode_table(127538) ->
    {0, [], {square, [{0, 31105}]}, so};
unicode_table(127539) ->
    {0, [], {square, [{0, 31354}]}, so};
unicode_table(127540) ->
    {0, [], {square, [{0, 21512}]}, so};
unicode_table(127541) ->
    {0, [], {square, [{0, 28288}]}, so};
unicode_table(127542) ->
    {0, [], {square, [{0, 26377}]}, so};
unicode_table(127543) ->
    {0, [], {square, [{0, 26376}]}, so};
unicode_table(127544) ->
    {0, [], {square, [{0, 30003}]}, so};
unicode_table(127545) ->
    {0, [], {square, [{0, 21106}]}, so};
unicode_table(127546) ->
    {0, [], {square, [{0, 21942}]}, so};
unicode_table(127547) ->
    {0, [], {square, [{0, 37197}]}, so};
unicode_table(127552) ->
    {0, [], {compat, [{0, 12308}, {0, 26412}, {0, 12309}]}, so};
unicode_table(127553) ->
    {0, [], {compat, [{0, 12308}, {0, 19977}, {0, 12309}]}, so};
unicode_table(127554) ->
    {0, [], {compat, [{0, 12308}, {0, 20108}, {0, 12309}]}, so};
unicode_table(127555) ->
    {0, [], {compat, [{0, 12308}, {0, 23433}, {0, 12309}]}, so};
unicode_table(127556) ->
    {0, [], {compat, [{0, 12308}, {0, 28857}, {0, 12309}]}, so};
unicode_table(127557) ->
    {0, [], {compat, [{0, 12308}, {0, 25171}, {0, 12309}]}, so};
unicode_table(127558) ->
    {0, [], {compat, [{0, 12308}, {0, 30423}, {0, 12309}]}, so};
unicode_table(127559) ->
    {0, [], {compat, [{0, 12308}, {0, 21213}, {0, 12309}]}, so};
unicode_table(127560) ->
    {0, [], {compat, [{0, 12308}, {0, 25943}, {0, 12309}]}, so};
unicode_table(127568) ->
    {0, [], {circle, [{0, 24471}]}, so};
unicode_table(127569) ->
    {0, [], {circle, [{0, 21487}]}, so};
unicode_table(130032) ->
    {0, [], {font, [{0, 48}]}, nd};
unicode_table(130033) ->
    {0, [], {font, [{0, 49}]}, nd};
unicode_table(130034) ->
    {0, [], {font, [{0, 50}]}, nd};
unicode_table(130035) ->
    {0, [], {font, [{0, 51}]}, nd};
unicode_table(130036) ->
    {0, [], {font, [{0, 52}]}, nd};
unicode_table(130037) ->
    {0, [], {font, [{0, 53}]}, nd};
unicode_table(130038) ->
    {0, [], {font, [{0, 54}]}, nd};
unicode_table(130039) ->
    {0, [], {font, [{0, 55}]}, nd};
unicode_table(130040) ->
    {0, [], {font, [{0, 56}]}, nd};
unicode_table(130041) ->
    {0, [], {font, [{0, 57}]}, nd};
unicode_table(194560) ->
    {0, [{0, 20029}], [], lo};
unicode_table(194561) ->
    {0, [{0, 20024}], [], lo};
unicode_table(194562) ->
    {0, [{0, 20033}], [], lo};
unicode_table(194563) ->
    {0, [{0, 131362}], [], lo};
unicode_table(194564) ->
    {0, [{0, 20320}], [], lo};
unicode_table(194565) ->
    {0, [{0, 20398}], [], lo};
unicode_table(194566) ->
    {0, [{0, 20411}], [], lo};
unicode_table(194567) ->
    {0, [{0, 20482}], [], lo};
unicode_table(194568) ->
    {0, [{0, 20602}], [], lo};
unicode_table(194569) ->
    {0, [{0, 20633}], [], lo};
unicode_table(194570) ->
    {0, [{0, 20711}], [], lo};
unicode_table(194571) ->
    {0, [{0, 20687}], [], lo};
unicode_table(194572) ->
    {0, [{0, 13470}], [], lo};
unicode_table(194573) ->
    {0, [{0, 132666}], [], lo};
unicode_table(194574) ->
    {0, [{0, 20813}], [], lo};
unicode_table(194575) ->
    {0, [{0, 20820}], [], lo};
unicode_table(194576) ->
    {0, [{0, 20836}], [], lo};
unicode_table(194577) ->
    {0, [{0, 20855}], [], lo};
unicode_table(194578) ->
    {0, [{0, 132380}], [], lo};
unicode_table(194579) ->
    {0, [{0, 13497}], [], lo};
unicode_table(194580) ->
    {0, [{0, 20839}], [], lo};
unicode_table(194581) ->
    {0, [{0, 20877}], [], lo};
unicode_table(194582) ->
    {0, [{0, 132427}], [], lo};
unicode_table(194583) ->
    {0, [{0, 20887}], [], lo};
unicode_table(194584) ->
    {0, [{0, 20900}], [], lo};
unicode_table(194585) ->
    {0, [{0, 20172}], [], lo};
unicode_table(194586) ->
    {0, [{0, 20908}], [], lo};
unicode_table(194587) ->
    {0, [{0, 20917}], [], lo};
unicode_table(194588) ->
    {0, [{0, 168415}], [], lo};
unicode_table(194589) ->
    {0, [{0, 20981}], [], lo};
unicode_table(194590) ->
    {0, [{0, 20995}], [], lo};
unicode_table(194591) ->
    {0, [{0, 13535}], [], lo};
unicode_table(194592) ->
    {0, [{0, 21051}], [], lo};
unicode_table(194593) ->
    {0, [{0, 21062}], [], lo};
unicode_table(194594) ->
    {0, [{0, 21106}], [], lo};
unicode_table(194595) ->
    {0, [{0, 21111}], [], lo};
unicode_table(194596) ->
    {0, [{0, 13589}], [], lo};
unicode_table(194597) ->
    {0, [{0, 21191}], [], lo};
unicode_table(194598) ->
    {0, [{0, 21193}], [], lo};
unicode_table(194599) ->
    {0, [{0, 21220}], [], lo};
unicode_table(194600) ->
    {0, [{0, 21242}], [], lo};
unicode_table(194601) ->
    {0, [{0, 21253}], [], lo};
unicode_table(194602) ->
    {0, [{0, 21254}], [], lo};
unicode_table(194603) ->
    {0, [{0, 21271}], [], lo};
unicode_table(194604) ->
    {0, [{0, 21321}], [], lo};
unicode_table(194605) ->
    {0, [{0, 21329}], [], lo};
unicode_table(194606) ->
    {0, [{0, 21338}], [], lo};
unicode_table(194607) ->
    {0, [{0, 21363}], [], lo};
unicode_table(194608) ->
    {0, [{0, 21373}], [], lo};
unicode_table(194609) ->
    {0, [{0, 21375}], [], lo};
unicode_table(194610) ->
    {0, [{0, 21375}], [], lo};
unicode_table(194611) ->
    {0, [{0, 21375}], [], lo};
unicode_table(194612) ->
    {0, [{0, 133676}], [], lo};
unicode_table(194613) ->
    {0, [{0, 28784}], [], lo};
unicode_table(194614) ->
    {0, [{0, 21450}], [], lo};
unicode_table(194615) ->
    {0, [{0, 21471}], [], lo};
unicode_table(194616) ->
    {0, [{0, 133987}], [], lo};
unicode_table(194617) ->
    {0, [{0, 21483}], [], lo};
unicode_table(194618) ->
    {0, [{0, 21489}], [], lo};
unicode_table(194619) ->
    {0, [{0, 21510}], [], lo};
unicode_table(194620) ->
    {0, [{0, 21662}], [], lo};
unicode_table(194621) ->
    {0, [{0, 21560}], [], lo};
unicode_table(194622) ->
    {0, [{0, 21576}], [], lo};
unicode_table(194623) ->
    {0, [{0, 21608}], [], lo};
unicode_table(194624) ->
    {0, [{0, 21666}], [], lo};
unicode_table(194625) ->
    {0, [{0, 21750}], [], lo};
unicode_table(194626) ->
    {0, [{0, 21776}], [], lo};
unicode_table(194627) ->
    {0, [{0, 21843}], [], lo};
unicode_table(194628) ->
    {0, [{0, 21859}], [], lo};
unicode_table(194629) ->
    {0, [{0, 21892}], [], lo};
unicode_table(194630) ->
    {0, [{0, 21892}], [], lo};
unicode_table(194631) ->
    {0, [{0, 21913}], [], lo};
unicode_table(194632) ->
    {0, [{0, 21931}], [], lo};
unicode_table(194633) ->
    {0, [{0, 21939}], [], lo};
unicode_table(194634) ->
    {0, [{0, 21954}], [], lo};
unicode_table(194635) ->
    {0, [{0, 22294}], [], lo};
unicode_table(194636) ->
    {0, [{0, 22022}], [], lo};
unicode_table(194637) ->
    {0, [{0, 22295}], [], lo};
unicode_table(194638) ->
    {0, [{0, 22097}], [], lo};
unicode_table(194639) ->
    {0, [{0, 22132}], [], lo};
unicode_table(194640) ->
    {0, [{0, 20999}], [], lo};
unicode_table(194641) ->
    {0, [{0, 22766}], [], lo};
unicode_table(194642) ->
    {0, [{0, 22478}], [], lo};
unicode_table(194643) ->
    {0, [{0, 22516}], [], lo};
unicode_table(194644) ->
    {0, [{0, 22541}], [], lo};
unicode_table(194645) ->
    {0, [{0, 22411}], [], lo};
unicode_table(194646) ->
    {0, [{0, 22578}], [], lo};
unicode_table(194647) ->
    {0, [{0, 22577}], [], lo};
unicode_table(194648) ->
    {0, [{0, 22700}], [], lo};
unicode_table(194649) ->
    {0, [{0, 136420}], [], lo};
unicode_table(194650) ->
    {0, [{0, 22770}], [], lo};
unicode_table(194651) ->
    {0, [{0, 22775}], [], lo};
unicode_table(194652) ->
    {0, [{0, 22790}], [], lo};
unicode_table(194653) ->
    {0, [{0, 22810}], [], lo};
unicode_table(194654) ->
    {0, [{0, 22818}], [], lo};
unicode_table(194655) ->
    {0, [{0, 22882}], [], lo};
unicode_table(194656) ->
    {0, [{0, 136872}], [], lo};
unicode_table(194657) ->
    {0, [{0, 136938}], [], lo};
unicode_table(194658) ->
    {0, [{0, 23020}], [], lo};
unicode_table(194659) ->
    {0, [{0, 23067}], [], lo};
unicode_table(194660) ->
    {0, [{0, 23079}], [], lo};
unicode_table(194661) ->
    {0, [{0, 23000}], [], lo};
unicode_table(194662) ->
    {0, [{0, 23142}], [], lo};
unicode_table(194663) ->
    {0, [{0, 14062}], [], lo};
unicode_table(194664) ->
    {0, [{0, 14076}], [], lo};
unicode_table(194665) ->
    {0, [{0, 23304}], [], lo};
unicode_table(194666) ->
    {0, [{0, 23358}], [], lo};
unicode_table(194667) ->
    {0, [{0, 23358}], [], lo};
unicode_table(194668) ->
    {0, [{0, 137672}], [], lo};
unicode_table(194669) ->
    {0, [{0, 23491}], [], lo};
unicode_table(194670) ->
    {0, [{0, 23512}], [], lo};
unicode_table(194671) ->
    {0, [{0, 23527}], [], lo};
unicode_table(194672) ->
    {0, [{0, 23539}], [], lo};
unicode_table(194673) ->
    {0, [{0, 138008}], [], lo};
unicode_table(194674) ->
    {0, [{0, 23551}], [], lo};
unicode_table(194675) ->
    {0, [{0, 23558}], [], lo};
unicode_table(194676) ->
    {0, [{0, 24403}], [], lo};
unicode_table(194677) ->
    {0, [{0, 23586}], [], lo};
unicode_table(194678) ->
    {0, [{0, 14209}], [], lo};
unicode_table(194679) ->
    {0, [{0, 23648}], [], lo};
unicode_table(194680) ->
    {0, [{0, 23662}], [], lo};
unicode_table(194681) ->
    {0, [{0, 23744}], [], lo};
unicode_table(194682) ->
    {0, [{0, 23693}], [], lo};
unicode_table(194683) ->
    {0, [{0, 138724}], [], lo};
unicode_table(194684) ->
    {0, [{0, 23875}], [], lo};
unicode_table(194685) ->
    {0, [{0, 138726}], [], lo};
unicode_table(194686) ->
    {0, [{0, 23918}], [], lo};
unicode_table(194687) ->
    {0, [{0, 23915}], [], lo};
unicode_table(194688) ->
    {0, [{0, 23932}], [], lo};
unicode_table(194689) ->
    {0, [{0, 24033}], [], lo};
unicode_table(194690) ->
    {0, [{0, 24034}], [], lo};
unicode_table(194691) ->
    {0, [{0, 14383}], [], lo};
unicode_table(194692) ->
    {0, [{0, 24061}], [], lo};
unicode_table(194693) ->
    {0, [{0, 24104}], [], lo};
unicode_table(194694) ->
    {0, [{0, 24125}], [], lo};
unicode_table(194695) ->
    {0, [{0, 24169}], [], lo};
unicode_table(194696) ->
    {0, [{0, 14434}], [], lo};
unicode_table(194697) ->
    {0, [{0, 139651}], [], lo};
unicode_table(194698) ->
    {0, [{0, 14460}], [], lo};
unicode_table(194699) ->
    {0, [{0, 24240}], [], lo};
unicode_table(194700) ->
    {0, [{0, 24243}], [], lo};
unicode_table(194701) ->
    {0, [{0, 24246}], [], lo};
unicode_table(194702) ->
    {0, [{0, 24266}], [], lo};
unicode_table(194703) ->
    {0, [{0, 172946}], [], lo};
unicode_table(194704) ->
    {0, [{0, 24318}], [], lo};
unicode_table(194705) ->
    {0, [{0, 140081}], [], lo};
unicode_table(194706) ->
    {0, [{0, 140081}], [], lo};
unicode_table(194707) ->
    {0, [{0, 33281}], [], lo};
unicode_table(194708) ->
    {0, [{0, 24354}], [], lo};
unicode_table(194709) ->
    {0, [{0, 24354}], [], lo};
unicode_table(194710) ->
    {0, [{0, 14535}], [], lo};
unicode_table(194711) ->
    {0, [{0, 144056}], [], lo};
unicode_table(194712) ->
    {0, [{0, 156122}], [], lo};
unicode_table(194713) ->
    {0, [{0, 24418}], [], lo};
unicode_table(194714) ->
    {0, [{0, 24427}], [], lo};
unicode_table(194715) ->
    {0, [{0, 14563}], [], lo};
unicode_table(194716) ->
    {0, [{0, 24474}], [], lo};
unicode_table(194717) ->
    {0, [{0, 24525}], [], lo};
unicode_table(194718) ->
    {0, [{0, 24535}], [], lo};
unicode_table(194719) ->
    {0, [{0, 24569}], [], lo};
unicode_table(194720) ->
    {0, [{0, 24705}], [], lo};
unicode_table(194721) ->
    {0, [{0, 14650}], [], lo};
unicode_table(194722) ->
    {0, [{0, 14620}], [], lo};
unicode_table(194723) ->
    {0, [{0, 24724}], [], lo};
unicode_table(194724) ->
    {0, [{0, 141012}], [], lo};
unicode_table(194725) ->
    {0, [{0, 24775}], [], lo};
unicode_table(194726) ->
    {0, [{0, 24904}], [], lo};
unicode_table(194727) ->
    {0, [{0, 24908}], [], lo};
unicode_table(194728) ->
    {0, [{0, 24910}], [], lo};
unicode_table(194729) ->
    {0, [{0, 24908}], [], lo};
unicode_table(194730) ->
    {0, [{0, 24954}], [], lo};
unicode_table(194731) ->
    {0, [{0, 24974}], [], lo};
unicode_table(194732) ->
    {0, [{0, 25010}], [], lo};
unicode_table(194733) ->
    {0, [{0, 24996}], [], lo};
unicode_table(194734) ->
    {0, [{0, 25007}], [], lo};
unicode_table(194735) ->
    {0, [{0, 25054}], [], lo};
unicode_table(194736) ->
    {0, [{0, 25074}], [], lo};
unicode_table(194737) ->
    {0, [{0, 25078}], [], lo};
unicode_table(194738) ->
    {0, [{0, 25104}], [], lo};
unicode_table(194739) ->
    {0, [{0, 25115}], [], lo};
unicode_table(194740) ->
    {0, [{0, 25181}], [], lo};
unicode_table(194741) ->
    {0, [{0, 25265}], [], lo};
unicode_table(194742) ->
    {0, [{0, 25300}], [], lo};
unicode_table(194743) ->
    {0, [{0, 25424}], [], lo};
unicode_table(194744) ->
    {0, [{0, 142092}], [], lo};
unicode_table(194745) ->
    {0, [{0, 25405}], [], lo};
unicode_table(194746) ->
    {0, [{0, 25340}], [], lo};
unicode_table(194747) ->
    {0, [{0, 25448}], [], lo};
unicode_table(194748) ->
    {0, [{0, 25475}], [], lo};
unicode_table(194749) ->
    {0, [{0, 25572}], [], lo};
unicode_table(194750) ->
    {0, [{0, 142321}], [], lo};
unicode_table(194751) ->
    {0, [{0, 25634}], [], lo};
unicode_table(194752) ->
    {0, [{0, 25541}], [], lo};
unicode_table(194753) ->
    {0, [{0, 25513}], [], lo};
unicode_table(194754) ->
    {0, [{0, 14894}], [], lo};
unicode_table(194755) ->
    {0, [{0, 25705}], [], lo};
unicode_table(194756) ->
    {0, [{0, 25726}], [], lo};
unicode_table(194757) ->
    {0, [{0, 25757}], [], lo};
unicode_table(194758) ->
    {0, [{0, 25719}], [], lo};
unicode_table(194759) ->
    {0, [{0, 14956}], [], lo};
unicode_table(194760) ->
    {0, [{0, 25935}], [], lo};
unicode_table(194761) ->
    {0, [{0, 25964}], [], lo};
unicode_table(194762) ->
    {0, [{0, 143370}], [], lo};
unicode_table(194763) ->
    {0, [{0, 26083}], [], lo};
unicode_table(194764) ->
    {0, [{0, 26360}], [], lo};
unicode_table(194765) ->
    {0, [{0, 26185}], [], lo};
unicode_table(194766) ->
    {0, [{0, 15129}], [], lo};
unicode_table(194767) ->
    {0, [{0, 26257}], [], lo};
unicode_table(194768) ->
    {0, [{0, 15112}], [], lo};
unicode_table(194769) ->
    {0, [{0, 15076}], [], lo};
unicode_table(194770) ->
    {0, [{0, 20882}], [], lo};
unicode_table(194771) ->
    {0, [{0, 20885}], [], lo};
unicode_table(194772) ->
    {0, [{0, 26368}], [], lo};
unicode_table(194773) ->
    {0, [{0, 26268}], [], lo};
unicode_table(194774) ->
    {0, [{0, 32941}], [], lo};
unicode_table(194775) ->
    {0, [{0, 17369}], [], lo};
unicode_table(194776) ->
    {0, [{0, 26391}], [], lo};
unicode_table(194777) ->
    {0, [{0, 26395}], [], lo};
unicode_table(194778) ->
    {0, [{0, 26401}], [], lo};
unicode_table(194779) ->
    {0, [{0, 26462}], [], lo};
unicode_table(194780) ->
    {0, [{0, 26451}], [], lo};
unicode_table(194781) ->
    {0, [{0, 144323}], [], lo};
unicode_table(194782) ->
    {0, [{0, 15177}], [], lo};
unicode_table(194783) ->
    {0, [{0, 26618}], [], lo};
unicode_table(194784) ->
    {0, [{0, 26501}], [], lo};
unicode_table(194785) ->
    {0, [{0, 26706}], [], lo};
unicode_table(194786) ->
    {0, [{0, 26757}], [], lo};
unicode_table(194787) ->
    {0, [{0, 144493}], [], lo};
unicode_table(194788) ->
    {0, [{0, 26766}], [], lo};
unicode_table(194789) ->
    {0, [{0, 26655}], [], lo};
unicode_table(194790) ->
    {0, [{0, 26900}], [], lo};
unicode_table(194791) ->
    {0, [{0, 15261}], [], lo};
unicode_table(194792) ->
    {0, [{0, 26946}], [], lo};
unicode_table(194793) ->
    {0, [{0, 27043}], [], lo};
unicode_table(194794) ->
    {0, [{0, 27114}], [], lo};
unicode_table(194795) ->
    {0, [{0, 27304}], [], lo};
unicode_table(194796) ->
    {0, [{0, 145059}], [], lo};
unicode_table(194797) ->
    {0, [{0, 27355}], [], lo};
unicode_table(194798) ->
    {0, [{0, 15384}], [], lo};
unicode_table(194799) ->
    {0, [{0, 27425}], [], lo};
unicode_table(194800) ->
    {0, [{0, 145575}], [], lo};
unicode_table(194801) ->
    {0, [{0, 27476}], [], lo};
unicode_table(194802) ->
    {0, [{0, 15438}], [], lo};
unicode_table(194803) ->
    {0, [{0, 27506}], [], lo};
unicode_table(194804) ->
    {0, [{0, 27551}], [], lo};
unicode_table(194805) ->
    {0, [{0, 27578}], [], lo};
unicode_table(194806) ->
    {0, [{0, 27579}], [], lo};
unicode_table(194807) ->
    {0, [{0, 146061}], [], lo};
unicode_table(194808) ->
    {0, [{0, 138507}], [], lo};
unicode_table(194809) ->
    {0, [{0, 146170}], [], lo};
unicode_table(194810) ->
    {0, [{0, 27726}], [], lo};
unicode_table(194811) ->
    {0, [{0, 146620}], [], lo};
unicode_table(194812) ->
    {0, [{0, 27839}], [], lo};
unicode_table(194813) ->
    {0, [{0, 27853}], [], lo};
unicode_table(194814) ->
    {0, [{0, 27751}], [], lo};
unicode_table(194815) ->
    {0, [{0, 27926}], [], lo};
unicode_table(194816) ->
    {0, [{0, 27966}], [], lo};
unicode_table(194817) ->
    {0, [{0, 28023}], [], lo};
unicode_table(194818) ->
    {0, [{0, 27969}], [], lo};
unicode_table(194819) ->
    {0, [{0, 28009}], [], lo};
unicode_table(194820) ->
    {0, [{0, 28024}], [], lo};
unicode_table(194821) ->
    {0, [{0, 28037}], [], lo};
unicode_table(194822) ->
    {0, [{0, 146718}], [], lo};
unicode_table(194823) ->
    {0, [{0, 27956}], [], lo};
unicode_table(194824) ->
    {0, [{0, 28207}], [], lo};
unicode_table(194825) ->
    {0, [{0, 28270}], [], lo};
unicode_table(194826) ->
    {0, [{0, 15667}], [], lo};
unicode_table(194827) ->
    {0, [{0, 28363}], [], lo};
unicode_table(194828) ->
    {0, [{0, 28359}], [], lo};
unicode_table(194829) ->
    {0, [{0, 147153}], [], lo};
unicode_table(194830) ->
    {0, [{0, 28153}], [], lo};
unicode_table(194831) ->
    {0, [{0, 28526}], [], lo};
unicode_table(194832) ->
    {0, [{0, 147294}], [], lo};
unicode_table(194833) ->
    {0, [{0, 147342}], [], lo};
unicode_table(194834) ->
    {0, [{0, 28614}], [], lo};
unicode_table(194835) ->
    {0, [{0, 28729}], [], lo};
unicode_table(194836) ->
    {0, [{0, 28702}], [], lo};
unicode_table(194837) ->
    {0, [{0, 28699}], [], lo};
unicode_table(194838) ->
    {0, [{0, 15766}], [], lo};
unicode_table(194839) ->
    {0, [{0, 28746}], [], lo};
unicode_table(194840) ->
    {0, [{0, 28797}], [], lo};
unicode_table(194841) ->
    {0, [{0, 28791}], [], lo};
unicode_table(194842) ->
    {0, [{0, 28845}], [], lo};
unicode_table(194843) ->
    {0, [{0, 132389}], [], lo};
unicode_table(194844) ->
    {0, [{0, 28997}], [], lo};
unicode_table(194845) ->
    {0, [{0, 148067}], [], lo};
unicode_table(194846) ->
    {0, [{0, 29084}], [], lo};
unicode_table(194847) ->
    {0, [{0, 148395}], [], lo};
unicode_table(194848) ->
    {0, [{0, 29224}], [], lo};
unicode_table(194849) ->
    {0, [{0, 29237}], [], lo};
unicode_table(194850) ->
    {0, [{0, 29264}], [], lo};
unicode_table(194851) ->
    {0, [{0, 149000}], [], lo};
unicode_table(194852) ->
    {0, [{0, 29312}], [], lo};
unicode_table(194853) ->
    {0, [{0, 29333}], [], lo};
unicode_table(194854) ->
    {0, [{0, 149301}], [], lo};
unicode_table(194855) ->
    {0, [{0, 149524}], [], lo};
unicode_table(194856) ->
    {0, [{0, 29562}], [], lo};
unicode_table(194857) ->
    {0, [{0, 29579}], [], lo};
unicode_table(194858) ->
    {0, [{0, 16044}], [], lo};
unicode_table(194859) ->
    {0, [{0, 29605}], [], lo};
unicode_table(194860) ->
    {0, [{0, 16056}], [], lo};
unicode_table(194861) ->
    {0, [{0, 16056}], [], lo};
unicode_table(194862) ->
    {0, [{0, 29767}], [], lo};
unicode_table(194863) ->
    {0, [{0, 29788}], [], lo};
unicode_table(194864) ->
    {0, [{0, 29809}], [], lo};
unicode_table(194865) ->
    {0, [{0, 29829}], [], lo};
unicode_table(194866) ->
    {0, [{0, 29898}], [], lo};
unicode_table(194867) ->
    {0, [{0, 16155}], [], lo};
unicode_table(194868) ->
    {0, [{0, 29988}], [], lo};
unicode_table(194869) ->
    {0, [{0, 150582}], [], lo};
unicode_table(194870) ->
    {0, [{0, 30014}], [], lo};
unicode_table(194871) ->
    {0, [{0, 150674}], [], lo};
unicode_table(194872) ->
    {0, [{0, 30064}], [], lo};
unicode_table(194873) ->
    {0, [{0, 139679}], [], lo};
unicode_table(194874) ->
    {0, [{0, 30224}], [], lo};
unicode_table(194875) ->
    {0, [{0, 151457}], [], lo};
unicode_table(194876) ->
    {0, [{0, 151480}], [], lo};
unicode_table(194877) ->
    {0, [{0, 151620}], [], lo};
unicode_table(194878) ->
    {0, [{0, 16380}], [], lo};
unicode_table(194879) ->
    {0, [{0, 16392}], [], lo};
unicode_table(194880) ->
    {0, [{0, 30452}], [], lo};
unicode_table(194881) ->
    {0, [{0, 151795}], [], lo};
unicode_table(194882) ->
    {0, [{0, 151794}], [], lo};
unicode_table(194883) ->
    {0, [{0, 151833}], [], lo};
unicode_table(194884) ->
    {0, [{0, 151859}], [], lo};
unicode_table(194885) ->
    {0, [{0, 30494}], [], lo};
unicode_table(194886) ->
    {0, [{0, 30495}], [], lo};
unicode_table(194887) ->
    {0, [{0, 30495}], [], lo};
unicode_table(194888) ->
    {0, [{0, 30538}], [], lo};
unicode_table(194889) ->
    {0, [{0, 16441}], [], lo};
unicode_table(194890) ->
    {0, [{0, 30603}], [], lo};
unicode_table(194891) ->
    {0, [{0, 16454}], [], lo};
unicode_table(194892) ->
    {0, [{0, 16534}], [], lo};
unicode_table(194893) ->
    {0, [{0, 152605}], [], lo};
unicode_table(194894) ->
    {0, [{0, 30798}], [], lo};
unicode_table(194895) ->
    {0, [{0, 30860}], [], lo};
unicode_table(194896) ->
    {0, [{0, 30924}], [], lo};
unicode_table(194897) ->
    {0, [{0, 16611}], [], lo};
unicode_table(194898) ->
    {0, [{0, 153126}], [], lo};
unicode_table(194899) ->
    {0, [{0, 31062}], [], lo};
unicode_table(194900) ->
    {0, [{0, 153242}], [], lo};
unicode_table(194901) ->
    {0, [{0, 153285}], [], lo};
unicode_table(194902) ->
    {0, [{0, 31119}], [], lo};
unicode_table(194903) ->
    {0, [{0, 31211}], [], lo};
unicode_table(194904) ->
    {0, [{0, 16687}], [], lo};
unicode_table(194905) ->
    {0, [{0, 31296}], [], lo};
unicode_table(194906) ->
    {0, [{0, 31306}], [], lo};
unicode_table(194907) ->
    {0, [{0, 31311}], [], lo};
unicode_table(194908) ->
    {0, [{0, 153980}], [], lo};
unicode_table(194909) ->
    {0, [{0, 154279}], [], lo};
unicode_table(194910) ->
    {0, [{0, 154279}], [], lo};
unicode_table(194911) ->
    {0, [{0, 31470}], [], lo};
unicode_table(194912) ->
    {0, [{0, 16898}], [], lo};
unicode_table(194913) ->
    {0, [{0, 154539}], [], lo};
unicode_table(194914) ->
    {0, [{0, 31686}], [], lo};
unicode_table(194915) ->
    {0, [{0, 31689}], [], lo};
unicode_table(194916) ->
    {0, [{0, 16935}], [], lo};
unicode_table(194917) ->
    {0, [{0, 154752}], [], lo};
unicode_table(194918) ->
    {0, [{0, 31954}], [], lo};
unicode_table(194919) ->
    {0, [{0, 17056}], [], lo};
unicode_table(194920) ->
    {0, [{0, 31976}], [], lo};
unicode_table(194921) ->
    {0, [{0, 31971}], [], lo};
unicode_table(194922) ->
    {0, [{0, 32000}], [], lo};
unicode_table(194923) ->
    {0, [{0, 155526}], [], lo};
unicode_table(194924) ->
    {0, [{0, 32099}], [], lo};
unicode_table(194925) ->
    {0, [{0, 17153}], [], lo};
unicode_table(194926) ->
    {0, [{0, 32199}], [], lo};
unicode_table(194927) ->
    {0, [{0, 32258}], [], lo};
unicode_table(194928) ->
    {0, [{0, 32325}], [], lo};
unicode_table(194929) ->
    {0, [{0, 17204}], [], lo};
unicode_table(194930) ->
    {0, [{0, 156200}], [], lo};
unicode_table(194931) ->
    {0, [{0, 156231}], [], lo};
unicode_table(194932) ->
    {0, [{0, 17241}], [], lo};
unicode_table(194933) ->
    {0, [{0, 156377}], [], lo};
unicode_table(194934) ->
    {0, [{0, 32634}], [], lo};
unicode_table(194935) ->
    {0, [{0, 156478}], [], lo};
unicode_table(194936) ->
    {0, [{0, 32661}], [], lo};
unicode_table(194937) ->
    {0, [{0, 32762}], [], lo};
unicode_table(194938) ->
    {0, [{0, 32773}], [], lo};
unicode_table(194939) ->
    {0, [{0, 156890}], [], lo};
unicode_table(194940) ->
    {0, [{0, 156963}], [], lo};
unicode_table(194941) ->
    {0, [{0, 32864}], [], lo};
unicode_table(194942) ->
    {0, [{0, 157096}], [], lo};
unicode_table(194943) ->
    {0, [{0, 32880}], [], lo};
unicode_table(194944) ->
    {0, [{0, 144223}], [], lo};
unicode_table(194945) ->
    {0, [{0, 17365}], [], lo};
unicode_table(194946) ->
    {0, [{0, 32946}], [], lo};
unicode_table(194947) ->
    {0, [{0, 33027}], [], lo};
unicode_table(194948) ->
    {0, [{0, 17419}], [], lo};
unicode_table(194949) ->
    {0, [{0, 33086}], [], lo};
unicode_table(194950) ->
    {0, [{0, 23221}], [], lo};
unicode_table(194951) ->
    {0, [{0, 157607}], [], lo};
unicode_table(194952) ->
    {0, [{0, 157621}], [], lo};
unicode_table(194953) ->
    {0, [{0, 144275}], [], lo};
unicode_table(194954) ->
    {0, [{0, 144284}], [], lo};
unicode_table(194955) ->
    {0, [{0, 33281}], [], lo};
unicode_table(194956) ->
    {0, [{0, 33284}], [], lo};
unicode_table(194957) ->
    {0, [{0, 36766}], [], lo};
unicode_table(194958) ->
    {0, [{0, 17515}], [], lo};
unicode_table(194959) ->
    {0, [{0, 33425}], [], lo};
unicode_table(194960) ->
    {0, [{0, 33419}], [], lo};
unicode_table(194961) ->
    {0, [{0, 33437}], [], lo};
unicode_table(194962) ->
    {0, [{0, 21171}], [], lo};
unicode_table(194963) ->
    {0, [{0, 33457}], [], lo};
unicode_table(194964) ->
    {0, [{0, 33459}], [], lo};
unicode_table(194965) ->
    {0, [{0, 33469}], [], lo};
unicode_table(194966) ->
    {0, [{0, 33510}], [], lo};
unicode_table(194967) ->
    {0, [{0, 158524}], [], lo};
unicode_table(194968) ->
    {0, [{0, 33509}], [], lo};
unicode_table(194969) ->
    {0, [{0, 33565}], [], lo};
unicode_table(194970) ->
    {0, [{0, 33635}], [], lo};
unicode_table(194971) ->
    {0, [{0, 33709}], [], lo};
unicode_table(194972) ->
    {0, [{0, 33571}], [], lo};
unicode_table(194973) ->
    {0, [{0, 33725}], [], lo};
unicode_table(194974) ->
    {0, [{0, 33767}], [], lo};
unicode_table(194975) ->
    {0, [{0, 33879}], [], lo};
unicode_table(194976) ->
    {0, [{0, 33619}], [], lo};
unicode_table(194977) ->
    {0, [{0, 33738}], [], lo};
unicode_table(194978) ->
    {0, [{0, 33740}], [], lo};
unicode_table(194979) ->
    {0, [{0, 33756}], [], lo};
unicode_table(194980) ->
    {0, [{0, 158774}], [], lo};
unicode_table(194981) ->
    {0, [{0, 159083}], [], lo};
unicode_table(194982) ->
    {0, [{0, 158933}], [], lo};
unicode_table(194983) ->
    {0, [{0, 17707}], [], lo};
unicode_table(194984) ->
    {0, [{0, 34033}], [], lo};
unicode_table(194985) ->
    {0, [{0, 34035}], [], lo};
unicode_table(194986) ->
    {0, [{0, 34070}], [], lo};
unicode_table(194987) ->
    {0, [{0, 160714}], [], lo};
unicode_table(194988) ->
    {0, [{0, 34148}], [], lo};
unicode_table(194989) ->
    {0, [{0, 159532}], [], lo};
unicode_table(194990) ->
    {0, [{0, 17757}], [], lo};
unicode_table(194991) ->
    {0, [{0, 17761}], [], lo};
unicode_table(194992) ->
    {0, [{0, 159665}], [], lo};
unicode_table(194993) ->
    {0, [{0, 159954}], [], lo};
unicode_table(194994) ->
    {0, [{0, 17771}], [], lo};
unicode_table(194995) ->
    {0, [{0, 34384}], [], lo};
unicode_table(194996) ->
    {0, [{0, 34396}], [], lo};
unicode_table(194997) ->
    {0, [{0, 34407}], [], lo};
unicode_table(194998) ->
    {0, [{0, 34409}], [], lo};
unicode_table(194999) ->
    {0, [{0, 34473}], [], lo};
unicode_table(195000) ->
    {0, [{0, 34440}], [], lo};
unicode_table(195001) ->
    {0, [{0, 34574}], [], lo};
unicode_table(195002) ->
    {0, [{0, 34530}], [], lo};
unicode_table(195003) ->
    {0, [{0, 34681}], [], lo};
unicode_table(195004) ->
    {0, [{0, 34600}], [], lo};
unicode_table(195005) ->
    {0, [{0, 34667}], [], lo};
unicode_table(195006) ->
    {0, [{0, 34694}], [], lo};
unicode_table(195007) ->
    {0, [{0, 17879}], [], lo};
unicode_table(195008) ->
    {0, [{0, 34785}], [], lo};
unicode_table(195009) ->
    {0, [{0, 34817}], [], lo};
unicode_table(195010) ->
    {0, [{0, 17913}], [], lo};
unicode_table(195011) ->
    {0, [{0, 34912}], [], lo};
unicode_table(195012) ->
    {0, [{0, 34915}], [], lo};
unicode_table(195013) ->
    {0, [{0, 161383}], [], lo};
unicode_table(195014) ->
    {0, [{0, 35031}], [], lo};
unicode_table(195015) ->
    {0, [{0, 35038}], [], lo};
unicode_table(195016) ->
    {0, [{0, 17973}], [], lo};
unicode_table(195017) ->
    {0, [{0, 35066}], [], lo};
unicode_table(195018) ->
    {0, [{0, 13499}], [], lo};
unicode_table(195019) ->
    {0, [{0, 161966}], [], lo};
unicode_table(195020) ->
    {0, [{0, 162150}], [], lo};
unicode_table(195021) ->
    {0, [{0, 18110}], [], lo};
unicode_table(195022) ->
    {0, [{0, 18119}], [], lo};
unicode_table(195023) ->
    {0, [{0, 35488}], [], lo};
unicode_table(195024) ->
    {0, [{0, 35565}], [], lo};
unicode_table(195025) ->
    {0, [{0, 35722}], [], lo};
unicode_table(195026) ->
    {0, [{0, 35925}], [], lo};
unicode_table(195027) ->
    {0, [{0, 162984}], [], lo};
unicode_table(195028) ->
    {0, [{0, 36011}], [], lo};
unicode_table(195029) ->
    {0, [{0, 36033}], [], lo};
unicode_table(195030) ->
    {0, [{0, 36123}], [], lo};
unicode_table(195031) ->
    {0, [{0, 36215}], [], lo};
unicode_table(195032) ->
    {0, [{0, 163631}], [], lo};
unicode_table(195033) ->
    {0, [{0, 133124}], [], lo};
unicode_table(195034) ->
    {0, [{0, 36299}], [], lo};
unicode_table(195035) ->
    {0, [{0, 36284}], [], lo};
unicode_table(195036) ->
    {0, [{0, 36336}], [], lo};
unicode_table(195037) ->
    {0, [{0, 133342}], [], lo};
unicode_table(195038) ->
    {0, [{0, 36564}], [], lo};
unicode_table(195039) ->
    {0, [{0, 36664}], [], lo};
unicode_table(195040) ->
    {0, [{0, 165330}], [], lo};
unicode_table(195041) ->
    {0, [{0, 165357}], [], lo};
unicode_table(195042) ->
    {0, [{0, 37012}], [], lo};
unicode_table(195043) ->
    {0, [{0, 37105}], [], lo};
unicode_table(195044) ->
    {0, [{0, 37137}], [], lo};
unicode_table(195045) ->
    {0, [{0, 165678}], [], lo};
unicode_table(195046) ->
    {0, [{0, 37147}], [], lo};
unicode_table(195047) ->
    {0, [{0, 37432}], [], lo};
unicode_table(195048) ->
    {0, [{0, 37591}], [], lo};
unicode_table(195049) ->
    {0, [{0, 37592}], [], lo};
unicode_table(195050) ->
    {0, [{0, 37500}], [], lo};
unicode_table(195051) ->
    {0, [{0, 37881}], [], lo};
unicode_table(195052) ->
    {0, [{0, 37909}], [], lo};
unicode_table(195053) ->
    {0, [{0, 166906}], [], lo};
unicode_table(195054) ->
    {0, [{0, 38283}], [], lo};
unicode_table(195055) ->
    {0, [{0, 18837}], [], lo};
unicode_table(195056) ->
    {0, [{0, 38327}], [], lo};
unicode_table(195057) ->
    {0, [{0, 167287}], [], lo};
unicode_table(195058) ->
    {0, [{0, 18918}], [], lo};
unicode_table(195059) ->
    {0, [{0, 38595}], [], lo};
unicode_table(195060) ->
    {0, [{0, 23986}], [], lo};
unicode_table(195061) ->
    {0, [{0, 38691}], [], lo};
unicode_table(195062) ->
    {0, [{0, 168261}], [], lo};
unicode_table(195063) ->
    {0, [{0, 168474}], [], lo};
unicode_table(195064) ->
    {0, [{0, 19054}], [], lo};
unicode_table(195065) ->
    {0, [{0, 19062}], [], lo};
unicode_table(195066) ->
    {0, [{0, 38880}], [], lo};
unicode_table(195067) ->
    {0, [{0, 168970}], [], lo};
unicode_table(195068) ->
    {0, [{0, 19122}], [], lo};
unicode_table(195069) ->
    {0, [{0, 169110}], [], lo};
unicode_table(195070) ->
    {0, [{0, 38923}], [], lo};
unicode_table(195071) ->
    {0, [{0, 38923}], [], lo};
unicode_table(195072) ->
    {0, [{0, 38953}], [], lo};
unicode_table(195073) ->
    {0, [{0, 169398}], [], lo};
unicode_table(195074) ->
    {0, [{0, 39138}], [], lo};
unicode_table(195075) ->
    {0, [{0, 19251}], [], lo};
unicode_table(195076) ->
    {0, [{0, 39209}], [], lo};
unicode_table(195077) ->
    {0, [{0, 39335}], [], lo};
unicode_table(195078) ->
    {0, [{0, 39362}], [], lo};
unicode_table(195079) ->
    {0, [{0, 39422}], [], lo};
unicode_table(195080) ->
    {0, [{0, 19406}], [], lo};
unicode_table(195081) ->
    {0, [{0, 170800}], [], lo};
unicode_table(195082) ->
    {0, [{0, 39698}], [], lo};
unicode_table(195083) ->
    {0, [{0, 40000}], [], lo};
unicode_table(195084) ->
    {0, [{0, 40189}], [], lo};
unicode_table(195085) ->
    {0, [{0, 19662}], [], lo};
unicode_table(195086) ->
    {0, [{0, 19693}], [], lo};
unicode_table(195087) ->
    {0, [{0, 40295}], [], lo};
unicode_table(195088) ->
    {0, [{0, 172238}], [], lo};
unicode_table(195089) ->
    {0, [{0, 19704}], [], lo};
unicode_table(195090) ->
    {0, [{0, 172293}], [], lo};
unicode_table(195091) ->
    {0, [{0, 172558}], [], lo};
unicode_table(195092) ->
    {0, [{0, 172689}], [], lo};
unicode_table(195093) ->
    {0, [{0, 40635}], [], lo};
unicode_table(195094) ->
    {0, [{0, 19798}], [], lo};
unicode_table(195095) ->
    {0, [{0, 40697}], [], lo};
unicode_table(195096) ->
    {0, [{0, 40702}], [], lo};
unicode_table(195097) ->
    {0, [{0, 40709}], [], lo};
unicode_table(195098) ->
    {0, [{0, 40719}], [], lo};
unicode_table(195099) ->
    {0, [{0, 40726}], [], lo};
unicode_table(195100) ->
    {0, [{0, 40763}], [], lo};
unicode_table(195101) ->
    {0, [{0, 173568}], [], lo};
unicode_table(_) ->
    {0, [], [], lookup_category}.
