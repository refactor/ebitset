PROJECT = ebitset
PROJECT_DESCRIPTION = fixed-size bitset for tile
PROJECT_VERSION = 0.2.0

EUNIT_OPTS = verbose
TEST_DEPS = proper

SHELL=/bin/bash
SHELL_DEPS = kjell recon
SHELL_ERL = $(DEPS_DIR)/kjell/bin/kjell

dep_recon_commit = 2.5.0

CFLAGS += -DNIF_DEBUG

include erlang.mk
