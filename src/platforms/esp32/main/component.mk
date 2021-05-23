
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
