#
# This file is part of AtomVM.
#
# Copyright 2022 Davide Bettio <davide@uninstall.it>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

from pytest_embedded import Dut
import pytest
import os

@pytest.fixture(autouse=True)
def create_sd_image():
    path = 'sd_image.bin'
    with open(path, 'wb') as f:
        f.truncate(1 * 1024 * 1024)
    yield path
    os.unlink(path)

def pytest_generate_tests(metafunc):
    option_value = metafunc.config.option.target
    if option_value is None or option_value == 'esp32':
        metafunc.parametrize(
            'qemu_extra_args',
            [
                '-nic user,model=open_eth -drive file=sd_image.bin,if=sd,format=raw'
            ],indirect=True
        )
    if option_value == 'esp32c3':
        metafunc.parametrize(
            'qemu_extra_args',
            [
                '-nic user,model=open_eth -drive file=qemu_esp32c3_efuse.bin,if=none,format=raw,id=efuse -global driver=nvram.esp32c3.efuse,property=drive,value=efuse'
            ],indirect=True
        )

def test_atomvm(dut, redirect):
     dut.expect_unity_test_output(timeout=180)
     assert len(dut.testsuite.testcases) > 0
     assert dut.testsuite.attrs['failures'] == 0
     assert dut.testsuite.attrs['errors'] == 0
