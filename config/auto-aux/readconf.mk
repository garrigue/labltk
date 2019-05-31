where = $(shell ocamlc -where)
include $(where)/Makefile.config

includes:
	@echo "$(X11_INCLUDES)"

libs:
	@echo "$(X11_LINK)"
