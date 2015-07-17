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

OP_SRCDIR = $(abs_top_srcdir)/liboctave/operators

define run-mx-ops
  ( cd liboctave/operators; \
    $(AWK) -f $(OP_SRCDIR)/$(2)mk-ops.awk prefix=$(1) $(OP_SRCDIR)/$(1)-ops \
  )
endef

## Special rules for sources which must be built before rest of compilation.
$(VX_OP_INC) $(VX_OP_SRC) : liboctave/operators/mk-ops.awk liboctave/operators/vx-ops
	$(AM_V_GEN)$(call run-mx-ops,vx)

$(MX_OP_INC) $(MX_OP_SRC) : liboctave/operators/mk-ops.awk liboctave/operators/mx-ops
	$(AM_V_GEN)$(call run-mx-ops,mx)

$(SMX_OP_INC) $(SMX_OP_SRC) : liboctave/operators/sparse-mk-ops.awk liboctave/operators/smx-ops
	$(AM_V_GEN)$(call run-mx-ops,smx,sparse-)

liboctave/operators/mx-ops.h : liboctave/operators/mk-ops.awk liboctave/operators/mx-ops
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(OP_SRCDIR)/mk-ops.awk prefix=mx make_inclusive_header=mx-ops.h $(OP_SRCDIR)/mx-ops > $@-t && \
	mv $@-t $@

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
  liboctave/operators/sparse-mk-ops.awk \
  liboctave/operators/smx-ops \
  liboctave/operators/vx-ops

liboctave_DISTCLEANFILES += $(BUILT_LIBOCTAVE_OPERATORS_SOURCES)
