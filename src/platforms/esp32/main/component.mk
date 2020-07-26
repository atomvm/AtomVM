
COMPONENT_NIF_FILE := $(COMPONENT_PATH)/component_nifs.txt
COMPONENT_EXTRA_CLEAN := component_nifs.h

platform_nifs.o : component_nifs.h

component_nifs.h: $(COMPONENT_PATH)/component_nifs.h.in $(if $(wildcard $(COMPONENT_NIF_FILE)),$(COMPONENT_NIF_FILE),)
	@echo "Generating component_nifs.h ..."
	@python $(COMPONENT_PATH)/component_nifs.py $(COMPONENT_PATH)/component_nifs.h.in $(COMPONENT_NIF_FILE) > $@
