include %reldir%/vx-op-inc.mk
include %reldir%/mx-op-inc.mk
include %reldir%/smx-op-inc.mk

include %reldir%/vx-op-src.mk
include %reldir%/mx-op-src.mk
include %reldir%/smx-op-src.mk

OP_MK_FILES := \
  $(srcdir)/%reldir%/vx-op-inc.mk \
  $(srcdir)/%reldir%/mx-op-inc.mk \
  $(srcdir)/%reldir%/smx-op-inc.mk \
  $(srcdir)/%reldir%/vx-op-src.mk \
  $(srcdir)/%reldir%/mx-op-src.mk \
  $(srcdir)/%reldir%/smx-op-src.mk

$(OP_MK_FILES) : %.mk : $(srcdir)/%reldir%/config-ops.sh $(srcdir)/%reldir%/mk-ops.awk
	$(AM_V_GEN)$(SHELL) $(srcdir)/%reldir%/config-ops.sh $(top_srcdir) `echo $(*F) | $(SED) 's/-op-.*//'` `echo $(*F) | $(SED) 's/.*-op-//'`


BUILT_LIBOCTAVE_OPERATORS_SOURCES = \
  $(MX_OP_SRC) \
  $(VX_OP_SRC) \
  $(SMX_OP_SRC)

BUILT_LIBOCTAVE_OPERATORS_INC = \
  %reldir%/mx-ops.h \
  %reldir%/smx-ops.h \
  %reldir%/vx-ops.h \
  $(MX_OP_INC) \
  $(VX_OP_INC) \
  $(SMX_OP_INC)

BUILT_LIBOCTAVE_OPERATORS_FILES = \
  $(BUILT_LIBOCTAVE_OPERATORS_SOURCES) \
  $(BUILT_LIBOCTAVE_OPERATORS_INC)

BUILT_FULL_MATRIX_OPERATORS_FILES = \
  %reldir%/mx-ops.h \
  $(MX_OP_INC) \
  $(MX_OP_SRC)

BUILT_SPARSE_MATRIX_OPERATORS_FILES = \
  %reldir%/smx-ops.h \
  $(SMX_OP_INC) \
  $(SMX_OP_SRC)

BUILT_VECTOR_OPERATORS_FILES = \
  %reldir%/vx-ops.h \
  $(VX_OP_INC) \
  $(VX_OP_SRC)

LIBOCTAVE_OPERATORS_INC = \
  %reldir%/mx-base.h \
  %reldir%/mx-defs.h \
  %reldir%/mx-ext.h \
  %reldir%/mx-op-decl.h \
  %reldir%/mx-op-defs.h \
  %reldir%/Sparse-diag-op-defs.h \
  %reldir%/Sparse-op-decls.h \
  %reldir%/Sparse-op-defs.h \
  %reldir%/Sparse-perm-op-defs.h

## There are no distributed source files in this directory
LIBOCTAVE_OPERATORS_SRC =

LIBOCTAVE_TEMPLATE_SRC += \
  %reldir%/mx-inlines.cc

## Special rules for sources which must be built before rest of compilation.

OP_SRCDIR = $(srcdir)/%reldir%

define run-mk-ops
  rm -f $@-t $@ && \
  $(AWK) -f $(OP_SRCDIR)/mk-ops.awk -v build_file=$(notdir $@) $< > $@-t && \
  mv $@-t $@
endef

$(BUILT_FULL_MATRIX_OPERATORS_FILES): %reldir%/mx-ops %reldir%/mk-ops.awk
	$(AM_V_GEN)$(run-mk-ops)

$(BUILT_SPARSE_MATRIX_OPERATORS_FILES): %reldir%/smx-ops %reldir%/mk-ops.awk
	$(AM_V_GEN)$(run-mk-ops)

$(BUILT_VECTOR_OPERATORS_FILES): %reldir%/vx-ops %reldir%/mk-ops.awk
	$(AM_V_GEN)$(run-mk-ops)

noinst_LTLIBRARIES += %reldir%/liboperators.la

%canon_reldir%_liboperators_la_SOURCES = $(LIBOCTAVE_OPERATORS_SRC)

nodist_%canon_reldir%_liboperators_la_SOURCES = $(BUILT_LIBOCTAVE_OPERATORS_SOURCES)

%canon_reldir%_liboperators_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

liboctave_liboctave_la_LIBADD += %reldir%/liboperators.la

liboctave_EXTRA_DIST += \
  %reldir%/config-ops.sh \
  %reldir%/mk-ops.awk \
  %reldir%/mx-ops \
  %reldir%/smx-ops \
  %reldir%/vx-ops

liboctave_CLEANFILES += \
  $(BUILT_LIBOCTAVE_OPERATORS_FILES)
