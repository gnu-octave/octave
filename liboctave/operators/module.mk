include liboctave/operators/vx-op-inc.mk
include liboctave/operators/mx-op-inc.mk
include liboctave/operators/smx-op-inc.mk

include liboctave/operators/vx-op-src.mk
include liboctave/operators/mx-op-src.mk
include liboctave/operators/smx-op-src.mk

BUILT_LIBOCTAVE_OPERATORS_SOURCES = \
  $(MX_OP_SRC) \
  $(VX_OP_SRC) \
  $(SMX_OP_SRC)

BUILT_LIBOCTAVE_OPERATORS_INC = \
  liboctave/operators/mx-ops.h \
  liboctave/operators/smx-ops.h \
  liboctave/operators/vx-ops.h \
  $(MX_OP_INC) \
  $(VX_OP_INC) \
  $(SMX_OP_INC)

BUILT_LIBOCTAVE_OPERATORS_FILES = \
  $(BUILT_LIBOCTAVE_OPERATORS_SOURCES) \
  $(BUILT_LIBOCTAVE_OPERATORS_INC)

BUILT_FULL_MATRIX_OPERATORS_FILES = \
  liboctave/operators/mx-ops.h \
  $(MX_OP_INC) \
  $(MX_OP_SRC)

BUILT_SPARSE_MATRIX_OPERATORS_FILES = \
  liboctave/operators/smx-ops.h \
  $(SMX_OP_INC) \
  $(SMX_OP_SRC)

BUILT_VECTOR_OPERATORS_FILES = \
  liboctave/operators/vx-ops.h \
  $(VX_OP_INC) \
  $(VX_OP_SRC)

LIBOCTAVE_OPERATORS_INC = \
  liboctave/operators/mx-base.h \
  liboctave/operators/mx-defs.h \
  liboctave/operators/mx-ext.h \
  liboctave/operators/mx-op-decl.h \
  liboctave/operators/mx-op-defs.h \
  liboctave/operators/Sparse-diag-op-defs.h \
  liboctave/operators/Sparse-op-decls.h \
  liboctave/operators/Sparse-op-defs.h \
  liboctave/operators/Sparse-perm-op-defs.h

## There are no distributed source files in this directory
LIBOCTAVE_OPERATORS_SRC =

LIBOCTAVE_TEMPLATE_SRC += \
  liboctave/operators/mx-inlines.cc

## Special rules for sources which must be built before rest of compilation.

OP_SRCDIR = $(srcdir)/liboctave/operators

define run-mk-ops
  rm -f $@-t $@ && \
  $(AWK) -f $(OP_SRCDIR)/mk-ops.awk -v build_file=$(notdir $@) $< > $@-t && \
  mv $@-t $@
endef

$(BUILT_FULL_MATRIX_OPERATORS_FILES): liboctave/operators/mx-ops liboctave/operators/mk-ops.awk
	$(AM_V_GEN)$(run-mk-ops)

$(BUILT_SPARSE_MATRIX_OPERATORS_FILES): liboctave/operators/smx-ops liboctave/operators/mk-ops.awk
	$(AM_V_GEN)$(run-mk-ops)

$(BUILT_VECTOR_OPERATORS_FILES): liboctave/operators/vx-ops liboctave/operators/mk-ops.awk
	$(AM_V_GEN)$(run-mk-ops)

noinst_LTLIBRARIES += liboctave/operators/liboperators.la

liboctave_operators_liboperators_la_SOURCES = $(LIBOCTAVE_OPERATORS_SRC)

nodist_liboctave_operators_liboperators_la_SOURCES = $(BUILT_LIBOCTAVE_OPERATORS_SOURCES)

liboctave_operators_liboperators_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

liboctave_operators_liboperators_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_operators_liboperators_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/operators/liboperators.la

liboctave_EXTRA_DIST += \
  liboctave/operators/config-ops.sh \
  liboctave/operators/mk-ops.awk \
  liboctave/operators/mx-ops \
  liboctave/operators/smx-ops \
  liboctave/operators/vx-ops

liboctave_DISTCLEANFILES += $(BUILT_LIBOCTAVE_OPERATORS_SOURCES)
