EXTRA_DIST += \
  interpfcn/module.mk

INTERPFCN_INCLUDES = \
  interpfcn/data.h \
  interpfcn/debug.h \
  interpfcn/defun.h \
  interpfcn/dirfns.h \
  interpfcn/error.h \
  interpfcn/file-io.h \
  interpfcn/help.h \
  interpfcn/input.h \
  interpfcn/load-path.h \
  interpfcn/load-save.h \
  interpfcn/ls-oct-ascii.h \
  interpfcn/oct-hist.h \
  interpfcn/pager.h \
  interpfcn/pr-output.h \
  interpfcn/profiler.h \
  interpfcn/sighandlers.h \
  interpfcn/symtab.h \
  interpfcn/sysdep.h \
  interpfcn/toplev.h \
  interpfcn/utils.h \
  interpfcn/variables.h

INTERPFCN_SRC = \
  interpfcn/data.cc \
  interpfcn/debug.cc \
  interpfcn/defaults.cc \
  interpfcn/defun.cc \
  interpfcn/dirfns.cc \
  interpfcn/error.cc \
  interpfcn/file-io.cc \
  interpfcn/graphics.cc \
  interpfcn/help.cc \
  interpfcn/input.cc \
  interpfcn/load-path.cc \
  interpfcn/load-save.cc \
  interpfcn/ls-oct-ascii.cc \
  interpfcn/oct-hist.cc \
  interpfcn/pager.cc \
  interpfcn/pr-output.cc \
  interpfcn/profiler.cc \
  interpfcn/sighandlers.cc \
  interpfcn/symtab.cc \
  interpfcn/sysdep.cc \
  interpfcn/toplev.cc \
  interpfcn/utils.cc \
  interpfcn/variables.cc

## defaults.h and graphics.h must depend on Makefile.  Calling configure
## may change default/config values.  However, calling configure will also
## regenerate the Makefiles from Makefile.am and trigger the rules below.
interpfcn/defaults.h: interpfcn/defaults.in.h Makefile
	@$(do_subst_default_vals)

interpfcn/graphics.h: interpfcn/graphics.in.h genprops.awk Makefile
	$(AWK) -f $(srcdir)/genprops.awk $< > $@-t
	mv $@-t $@

interpfcn/graphics-props.cc: interpfcn/graphics.in.h genprops.awk Makefile
	$(AWK) -v emit_graphics_props=1 -f $(srcdir)/genprops.awk $< > $@-t
	mv $@-t $@

