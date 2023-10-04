# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

# better redefine this variable with your preferred fonts; not really sure about licensing
FONTS := ./hplhs-oldstyle.ttf $(shell find /usr/share/fonts -name Ubuntu-R.ttf -o -name DejaVuSans.ttf)

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	NATIVE_CC = cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	NATIVE_CC = cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	NATIVE_CC = gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
#	CFLAGS ?= -g -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR)

FREETYPE_CFLAGS ?= $(shell pkg-config --cflags --libs freetype2)

#LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

C_FILES := $(shell find $(C_SRC_DIR) -type f -name "*.c")
SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*_nif.c" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT): fonts.h $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

mk_font: mk_font.c
	$(c_verbose) $(NATIVE_CC) $< $(FREETYPE_CFLAGS) -lm $(OUTPUT_OPTION)

format:
	clang-format --style=LLVM -i $(C_FILES)

fonts.h: mk_font $(FONTS)
	./mk_font $(FONTS) > $@

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS) mk_font
