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

-module(huge).

-export([start/0, test_func/1]).

start() ->
    (test_func(501) - 41) +
        (last_char(test_func(1), 0) - 48) * 2 +
        (last_char(test_func(500), 0) - 47) * 4 +
        (last_char(test_func(263), 0) - 50) * 8 +
        (last_char(test_func(12), 0) - 49) * 16.

last_char([], T) ->
    T;
last_char([H | T], _) ->
    last_char(T, H).

test_func(1) ->
    "Test1";
test_func(2) ->
    "Test2";
test_func(3) ->
    "Test3";
test_func(4) ->
    "Test4";
test_func(5) ->
    "Test5";
test_func(6) ->
    "Test6";
test_func(7) ->
    "Test7";
test_func(8) ->
    "Test8";
test_func(9) ->
    "Test9";
test_func(10) ->
    "Test10";
test_func(11) ->
    "Test11";
test_func(12) ->
    "Test12";
test_func(13) ->
    "Test13";
test_func(14) ->
    "Test14";
test_func(15) ->
    "Test15";
test_func(16) ->
    "Test16";
test_func(17) ->
    "Test17";
test_func(18) ->
    "Test18";
test_func(19) ->
    "Test19";
test_func(20) ->
    "Test20";
test_func(21) ->
    "Test21";
test_func(22) ->
    "Test22";
test_func(23) ->
    "Test23";
test_func(24) ->
    "Test24";
test_func(25) ->
    "Test25";
test_func(26) ->
    "Test26";
test_func(27) ->
    "Test27";
test_func(28) ->
    "Test28";
test_func(29) ->
    "Test29";
test_func(30) ->
    "Test30";
test_func(31) ->
    "Test31";
test_func(32) ->
    "Test32";
test_func(33) ->
    "Test33";
test_func(34) ->
    "Test34";
test_func(35) ->
    "Test35";
test_func(36) ->
    "Test36";
test_func(37) ->
    "Test37";
test_func(38) ->
    "Test38";
test_func(39) ->
    "Test39";
test_func(40) ->
    "Test40";
test_func(41) ->
    "Test41";
test_func(42) ->
    "Test42";
test_func(43) ->
    "Test43";
test_func(44) ->
    "Test44";
test_func(45) ->
    "Test45";
test_func(46) ->
    "Test46";
test_func(47) ->
    "Test47";
test_func(48) ->
    "Test48";
test_func(49) ->
    "Test49";
test_func(50) ->
    "Test50";
test_func(51) ->
    "Test51";
test_func(52) ->
    "Test52";
test_func(53) ->
    "Test53";
test_func(54) ->
    "Test54";
test_func(55) ->
    "Test55";
test_func(56) ->
    "Test56";
test_func(57) ->
    "Test57";
test_func(58) ->
    "Test58";
test_func(59) ->
    "Test59";
test_func(60) ->
    "Test60";
test_func(61) ->
    "Test61";
test_func(62) ->
    "Test62";
test_func(63) ->
    "Test63";
test_func(64) ->
    "Test64";
test_func(65) ->
    "Test65";
test_func(66) ->
    "Test66";
test_func(67) ->
    "Test67";
test_func(68) ->
    "Test68";
test_func(69) ->
    "Test69";
test_func(70) ->
    "Test70";
test_func(71) ->
    "Test71";
test_func(72) ->
    "Test72";
test_func(73) ->
    "Test73";
test_func(74) ->
    "Test74";
test_func(75) ->
    "Test75";
test_func(76) ->
    "Test76";
test_func(77) ->
    "Test77";
test_func(78) ->
    "Test78";
test_func(79) ->
    "Test79";
test_func(80) ->
    "Test80";
test_func(81) ->
    "Test81";
test_func(82) ->
    "Test82";
test_func(83) ->
    "Test83";
test_func(84) ->
    "Test84";
test_func(85) ->
    "Test85";
test_func(86) ->
    "Test86";
test_func(87) ->
    "Test87";
test_func(88) ->
    "Test88";
test_func(89) ->
    "Test89";
test_func(90) ->
    "Test90";
test_func(91) ->
    "Test91";
test_func(92) ->
    "Test92";
test_func(93) ->
    "Test93";
test_func(94) ->
    "Test94";
test_func(95) ->
    "Test95";
test_func(96) ->
    "Test96";
test_func(97) ->
    "Test97";
test_func(98) ->
    "Test98";
test_func(99) ->
    "Test99";
test_func(100) ->
    "Test100";
test_func(101) ->
    "Test101";
test_func(102) ->
    "Test102";
test_func(103) ->
    "Test103";
test_func(104) ->
    "Test104";
test_func(105) ->
    "Test105";
test_func(106) ->
    "Test106";
test_func(107) ->
    "Test107";
test_func(108) ->
    "Test108";
test_func(109) ->
    "Test109";
test_func(110) ->
    "Test110";
test_func(111) ->
    "Test111";
test_func(112) ->
    "Test112";
test_func(113) ->
    "Test113";
test_func(114) ->
    "Test114";
test_func(115) ->
    "Test115";
test_func(116) ->
    "Test116";
test_func(117) ->
    "Test117";
test_func(118) ->
    "Test118";
test_func(119) ->
    "Test119";
test_func(120) ->
    "Test120";
test_func(121) ->
    "Test121";
test_func(122) ->
    "Test122";
test_func(123) ->
    "Test123";
test_func(124) ->
    "Test124";
test_func(125) ->
    "Test125";
test_func(126) ->
    "Test126";
test_func(127) ->
    "Test127";
test_func(128) ->
    "Test128";
test_func(129) ->
    "Test129";
test_func(130) ->
    "Test130";
test_func(131) ->
    "Test131";
test_func(132) ->
    "Test132";
test_func(133) ->
    "Test133";
test_func(134) ->
    "Test134";
test_func(135) ->
    "Test135";
test_func(136) ->
    "Test136";
test_func(137) ->
    "Test137";
test_func(138) ->
    "Test138";
test_func(139) ->
    "Test139";
test_func(140) ->
    "Test140";
test_func(141) ->
    "Test141";
test_func(142) ->
    "Test142";
test_func(143) ->
    "Test143";
test_func(144) ->
    "Test144";
test_func(145) ->
    "Test145";
test_func(146) ->
    "Test146";
test_func(147) ->
    "Test147";
test_func(148) ->
    "Test148";
test_func(149) ->
    "Test149";
test_func(150) ->
    "Test150";
test_func(151) ->
    "Test151";
test_func(152) ->
    "Test152";
test_func(153) ->
    "Test153";
test_func(154) ->
    "Test154";
test_func(155) ->
    "Test155";
test_func(156) ->
    "Test156";
test_func(157) ->
    "Test157";
test_func(158) ->
    "Test158";
test_func(159) ->
    "Test159";
test_func(160) ->
    "Test160";
test_func(161) ->
    "Test161";
test_func(162) ->
    "Test162";
test_func(163) ->
    "Test163";
test_func(164) ->
    "Test164";
test_func(165) ->
    "Test165";
test_func(166) ->
    "Test166";
test_func(167) ->
    "Test167";
test_func(168) ->
    "Test168";
test_func(169) ->
    "Test169";
test_func(170) ->
    "Test170";
test_func(171) ->
    "Test171";
test_func(172) ->
    "Test172";
test_func(173) ->
    "Test173";
test_func(174) ->
    "Test174";
test_func(175) ->
    "Test175";
test_func(176) ->
    "Test176";
test_func(177) ->
    "Test177";
test_func(178) ->
    "Test178";
test_func(179) ->
    "Test179";
test_func(180) ->
    "Test180";
test_func(181) ->
    "Test181";
test_func(182) ->
    "Test182";
test_func(183) ->
    "Test183";
test_func(184) ->
    "Test184";
test_func(185) ->
    "Test185";
test_func(186) ->
    "Test186";
test_func(187) ->
    "Test187";
test_func(188) ->
    "Test188";
test_func(189) ->
    "Test189";
test_func(190) ->
    "Test190";
test_func(191) ->
    "Test191";
test_func(192) ->
    "Test192";
test_func(193) ->
    "Test193";
test_func(194) ->
    "Test194";
test_func(195) ->
    "Test195";
test_func(196) ->
    "Test196";
test_func(197) ->
    "Test197";
test_func(198) ->
    "Test198";
test_func(199) ->
    "Test199";
test_func(200) ->
    "Test200";
test_func(201) ->
    "Test201";
test_func(202) ->
    "Test202";
test_func(203) ->
    "Test203";
test_func(204) ->
    "Test204";
test_func(205) ->
    "Test205";
test_func(206) ->
    "Test206";
test_func(207) ->
    "Test207";
test_func(208) ->
    "Test208";
test_func(209) ->
    "Test209";
test_func(210) ->
    "Test210";
test_func(211) ->
    "Test211";
test_func(212) ->
    "Test212";
test_func(213) ->
    "Test213";
test_func(214) ->
    "Test214";
test_func(215) ->
    "Test215";
test_func(216) ->
    "Test216";
test_func(217) ->
    "Test217";
test_func(218) ->
    "Test218";
test_func(219) ->
    "Test219";
test_func(220) ->
    "Test220";
test_func(221) ->
    "Test221";
test_func(222) ->
    "Test222";
test_func(223) ->
    "Test223";
test_func(224) ->
    "Test224";
test_func(225) ->
    "Test225";
test_func(226) ->
    "Test226";
test_func(227) ->
    "Test227";
test_func(228) ->
    "Test228";
test_func(229) ->
    "Test229";
test_func(230) ->
    "Test230";
test_func(231) ->
    "Test231";
test_func(232) ->
    "Test232";
test_func(233) ->
    "Test233";
test_func(234) ->
    "Test234";
test_func(235) ->
    "Test235";
test_func(236) ->
    "Test236";
test_func(237) ->
    "Test237";
test_func(238) ->
    "Test238";
test_func(239) ->
    "Test239";
test_func(240) ->
    "Test240";
test_func(241) ->
    "Test241";
test_func(242) ->
    "Test242";
test_func(243) ->
    "Test243";
test_func(244) ->
    "Test244";
test_func(245) ->
    "Test245";
test_func(246) ->
    "Test246";
test_func(247) ->
    "Test247";
test_func(248) ->
    "Test248";
test_func(249) ->
    "Test249";
test_func(250) ->
    "Test250";
test_func(251) ->
    "Test251";
test_func(252) ->
    "Test252";
test_func(253) ->
    "Test253";
test_func(254) ->
    "Test254";
test_func(255) ->
    "Test255";
test_func(256) ->
    "Test256";
test_func(257) ->
    "Test257";
test_func(258) ->
    "Test258";
test_func(259) ->
    "Test259";
test_func(260) ->
    "Test260";
test_func(261) ->
    "Test261";
test_func(262) ->
    "Test262";
test_func(263) ->
    "Test263";
test_func(264) ->
    "Test264";
test_func(265) ->
    "Test265";
test_func(266) ->
    "Test266";
test_func(267) ->
    "Test267";
test_func(268) ->
    "Test268";
test_func(269) ->
    "Test269";
test_func(270) ->
    "Test270";
test_func(271) ->
    "Test271";
test_func(272) ->
    "Test272";
test_func(273) ->
    "Test273";
test_func(274) ->
    "Test274";
test_func(275) ->
    "Test275";
test_func(276) ->
    "Test276";
test_func(277) ->
    "Test277";
test_func(278) ->
    "Test278";
test_func(279) ->
    "Test279";
test_func(280) ->
    "Test280";
test_func(281) ->
    "Test281";
test_func(282) ->
    "Test282";
test_func(283) ->
    "Test283";
test_func(284) ->
    "Test284";
test_func(285) ->
    "Test285";
test_func(286) ->
    "Test286";
test_func(287) ->
    "Test287";
test_func(288) ->
    "Test288";
test_func(289) ->
    "Test289";
test_func(290) ->
    "Test290";
test_func(291) ->
    "Test291";
test_func(292) ->
    "Test292";
test_func(293) ->
    "Test293";
test_func(294) ->
    "Test294";
test_func(295) ->
    "Test295";
test_func(296) ->
    "Test296";
test_func(297) ->
    "Test297";
test_func(298) ->
    "Test298";
test_func(299) ->
    "Test299";
test_func(300) ->
    "Test300";
test_func(301) ->
    "Test301";
test_func(302) ->
    "Test302";
test_func(303) ->
    "Test303";
test_func(304) ->
    "Test304";
test_func(305) ->
    "Test305";
test_func(306) ->
    "Test306";
test_func(307) ->
    "Test307";
test_func(308) ->
    "Test308";
test_func(309) ->
    "Test309";
test_func(310) ->
    "Test310";
test_func(311) ->
    "Test311";
test_func(312) ->
    "Test312";
test_func(313) ->
    "Test313";
test_func(314) ->
    "Test314";
test_func(315) ->
    "Test315";
test_func(316) ->
    "Test316";
test_func(317) ->
    "Test317";
test_func(318) ->
    "Test318";
test_func(319) ->
    "Test319";
test_func(320) ->
    "Test320";
test_func(321) ->
    "Test321";
test_func(322) ->
    "Test322";
test_func(323) ->
    "Test323";
test_func(324) ->
    "Test324";
test_func(325) ->
    "Test325";
test_func(326) ->
    "Test326";
test_func(327) ->
    "Test327";
test_func(328) ->
    "Test328";
test_func(329) ->
    "Test329";
test_func(330) ->
    "Test330";
test_func(331) ->
    "Test331";
test_func(332) ->
    "Test332";
test_func(333) ->
    "Test333";
test_func(334) ->
    "Test334";
test_func(335) ->
    "Test335";
test_func(336) ->
    "Test336";
test_func(337) ->
    "Test337";
test_func(338) ->
    "Test338";
test_func(339) ->
    "Test339";
test_func(340) ->
    "Test340";
test_func(341) ->
    "Test341";
test_func(342) ->
    "Test342";
test_func(343) ->
    "Test343";
test_func(344) ->
    "Test344";
test_func(345) ->
    "Test345";
test_func(346) ->
    "Test346";
test_func(347) ->
    "Test347";
test_func(348) ->
    "Test348";
test_func(349) ->
    "Test349";
test_func(350) ->
    "Test350";
test_func(351) ->
    "Test351";
test_func(352) ->
    "Test352";
test_func(353) ->
    "Test353";
test_func(354) ->
    "Test354";
test_func(355) ->
    "Test355";
test_func(356) ->
    "Test356";
test_func(357) ->
    "Test357";
test_func(358) ->
    "Test358";
test_func(359) ->
    "Test359";
test_func(360) ->
    "Test360";
test_func(361) ->
    "Test361";
test_func(362) ->
    "Test362";
test_func(363) ->
    "Test363";
test_func(364) ->
    "Test364";
test_func(365) ->
    "Test365";
test_func(366) ->
    "Test366";
test_func(367) ->
    "Test367";
test_func(368) ->
    "Test368";
test_func(369) ->
    "Test369";
test_func(370) ->
    "Test370";
test_func(371) ->
    "Test371";
test_func(372) ->
    "Test372";
test_func(373) ->
    "Test373";
test_func(374) ->
    "Test374";
test_func(375) ->
    "Test375";
test_func(376) ->
    "Test376";
test_func(377) ->
    "Test377";
test_func(378) ->
    "Test378";
test_func(379) ->
    "Test379";
test_func(380) ->
    "Test380";
test_func(381) ->
    "Test381";
test_func(382) ->
    "Test382";
test_func(383) ->
    "Test383";
test_func(384) ->
    "Test384";
test_func(385) ->
    "Test385";
test_func(386) ->
    "Test386";
test_func(387) ->
    "Test387";
test_func(388) ->
    "Test388";
test_func(389) ->
    "Test389";
test_func(390) ->
    "Test390";
test_func(391) ->
    "Test391";
test_func(392) ->
    "Test392";
test_func(393) ->
    "Test393";
test_func(394) ->
    "Test394";
test_func(395) ->
    "Test395";
test_func(396) ->
    "Test396";
test_func(397) ->
    "Test397";
test_func(398) ->
    "Test398";
test_func(399) ->
    "Test399";
test_func(400) ->
    "Test400";
test_func(401) ->
    "Test401";
test_func(402) ->
    "Test402";
test_func(403) ->
    "Test403";
test_func(404) ->
    "Test404";
test_func(405) ->
    "Test405";
test_func(406) ->
    "Test406";
test_func(407) ->
    "Test407";
test_func(408) ->
    "Test408";
test_func(409) ->
    "Test409";
test_func(410) ->
    "Test410";
test_func(411) ->
    "Test411";
test_func(412) ->
    "Test412";
test_func(413) ->
    "Test413";
test_func(414) ->
    "Test414";
test_func(415) ->
    "Test415";
test_func(416) ->
    "Test416";
test_func(417) ->
    "Test417";
test_func(418) ->
    "Test418";
test_func(419) ->
    "Test419";
test_func(420) ->
    "Test420";
test_func(421) ->
    "Test421";
test_func(422) ->
    "Test422";
test_func(423) ->
    "Test423";
test_func(424) ->
    "Test424";
test_func(425) ->
    "Test425";
test_func(426) ->
    "Test426";
test_func(427) ->
    "Test427";
test_func(428) ->
    "Test428";
test_func(429) ->
    "Test429";
test_func(430) ->
    "Test430";
test_func(431) ->
    "Test431";
test_func(432) ->
    "Test432";
test_func(433) ->
    "Test433";
test_func(434) ->
    "Test434";
test_func(435) ->
    "Test435";
test_func(436) ->
    "Test436";
test_func(437) ->
    "Test437";
test_func(438) ->
    "Test438";
test_func(439) ->
    "Test439";
test_func(440) ->
    "Test440";
test_func(441) ->
    "Test441";
test_func(442) ->
    "Test442";
test_func(443) ->
    "Test443";
test_func(444) ->
    "Test444";
test_func(445) ->
    "Test445";
test_func(446) ->
    "Test446";
test_func(447) ->
    "Test447";
test_func(448) ->
    "Test448";
test_func(449) ->
    "Test449";
test_func(450) ->
    "Test450";
test_func(451) ->
    "Test451";
test_func(452) ->
    "Test452";
test_func(453) ->
    "Test453";
test_func(454) ->
    "Test454";
test_func(455) ->
    "Test455";
test_func(456) ->
    "Test456";
test_func(457) ->
    "Test457";
test_func(458) ->
    "Test458";
test_func(459) ->
    "Test459";
test_func(460) ->
    "Test460";
test_func(461) ->
    "Test461";
test_func(462) ->
    "Test462";
test_func(463) ->
    "Test463";
test_func(464) ->
    "Test464";
test_func(465) ->
    "Test465";
test_func(466) ->
    "Test466";
test_func(467) ->
    "Test467";
test_func(468) ->
    "Test468";
test_func(469) ->
    "Test469";
test_func(470) ->
    "Test470";
test_func(471) ->
    "Test471";
test_func(472) ->
    "Test472";
test_func(473) ->
    "Test473";
test_func(474) ->
    "Test474";
test_func(475) ->
    "Test475";
test_func(476) ->
    "Test476";
test_func(477) ->
    "Test477";
test_func(478) ->
    "Test478";
test_func(479) ->
    "Test479";
test_func(480) ->
    "Test480";
test_func(481) ->
    "Test481";
test_func(482) ->
    "Test482";
test_func(483) ->
    "Test483";
test_func(484) ->
    "Test484";
test_func(485) ->
    "Test485";
test_func(486) ->
    "Test486";
test_func(487) ->
    "Test487";
test_func(488) ->
    "Test488";
test_func(489) ->
    "Test489";
test_func(490) ->
    "Test490";
test_func(491) ->
    "Test491";
test_func(492) ->
    "Test492";
test_func(493) ->
    "Test493";
test_func(494) ->
    "Test494";
test_func(495) ->
    "Test495";
test_func(496) ->
    "Test496";
test_func(497) ->
    "Test497";
test_func(498) ->
    "Test498";
test_func(499) ->
    "Test499";
test_func(500) ->
    "Test500";
test_func(501) ->
    42.
