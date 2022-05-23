#
# This file is part of AtomVM.
#
# Copyright 2017-2020 Davide Bettio <davide@uninstall.it>
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

DEFAULT_COMPONENT_NIFS := "gpio nvs ledc"
DEFAULT_COMPONENT_PORTS := "gpio network socket uart spi i2c"

COMPONENT_NIF_FILE := $(COMPONENT_PATH)/component_nifs.txt
COMPONENT_EXTRA_CLEAN := component_nifs.h

COMPONENT_PORT_FILE := $(COMPONENT_PATH)/component_ports.txt
COMPONENT_EXTRA_CLEAN := component_ports.h


platform_nifs.o main.o : component_nifs.h

component_nifs.h: $(COMPONENT_PATH)/component_nifs.h.in $(if $(wildcard $(COMPONENT_NIF_FILE)),$(COMPONENT_NIF_FILE),default_component_nifs)
	@echo "Generating component_nifs.h ..."
	@python $(COMPONENT_PATH)/component_nifs.py $(COMPONENT_PATH)/component_nifs.h.in $(COMPONENT_NIF_FILE) > $@

.PHONY: default_component_nifs
default_component_nifs:
	@echo "# GENERATED FILE -- EDIT TO ADD OR REMOVE COMPONENT NIFS" > $(COMPONENT_PATH)/component_nifs.txt
	@for i in $$(echo $(DEFAULT_COMPONENT_NIFS)); do echo "$${i}_nif" >> $(COMPONENT_PATH)/component_nifs.txt; done

sys.o main.o : component_ports.h

component_ports.h: $(COMPONENT_PATH)/component_ports.h.in $(if $(wildcard $(COMPONENT_PORT_FILE)),$(COMPONENT_PORT_FILE),default_component_ports)
	@echo "Generating component_ports.h ..."
	@python $(COMPONENT_PATH)/component_ports.py $(COMPONENT_PATH)/component_ports.h.in $(COMPONENT_PORT_FILE) > $@

.PHONY: default_component_ports
default_component_ports:
	@echo "# GENERATED FILE -- EDIT TO ADD OR REMOVE COMPONENT DRIVERS" > $(COMPONENT_PATH)/component_ports.txt
	for i in $$(echo $(DEFAULT_COMPONENT_PORTS)); do echo "$${i}_driver" >> $(COMPONENT_PATH)/component_ports.txt; done

## Add the libatomvm build component to the include path for generated headers (e.g., version.h)
COMPONENT_INCLUDES += $(COMPONENT_PATH)/../build/libatomvm
