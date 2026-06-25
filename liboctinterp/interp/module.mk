INTERP_INC = \
  %reldir%/Cell.h \
  %reldir%/auto-shlib.h \
  %reldir%/call-stack.h \
  %reldir%/cdisplay.h \
  %reldir%/cmd-edit.h \
  %reldir%/cmd-hist.h \
  %reldir%/defun-dld.h \
  %reldir%/defun-int.h \
  %reldir%/defun.h \
  %reldir%/display.h \
  %reldir%/dynamic-ld.h \
  %reldir%/error.h \
  %reldir%/errwarn.h \
  %reldir%/event-manager.h \
  %reldir%/event-queue.h \
  %reldir%/fcn-info.h \
  %reldir%/hook-fcn.h \
  %reldir%/input.h \
  %reldir%/interpreter.h \
  %reldir%/load-path.h \
  %reldir%/mx-type-traits.h \
  %reldir%/mxarray.h \
  %reldir%/oct-errno.h \
  %reldir%/oct-hist.h \
  %reldir%/oct-map.h \
  %reldir%/oct-process.h \
  %reldir%/oct-rl-edit.h \
  %reldir%/oct-rl-hist.h \
  %reldir%/pager.h \
  %reldir%/panic.h \
  %reldir%/pr-flt-fmt.h \
  %reldir%/pr-output.h \
  %reldir%/sparse-xdiv.h \
  %reldir%/sparse-xpow.h \
  %reldir%/stack-frame.h \
  %reldir%/syminfo.h \
  %reldir%/symrec.h \
  %reldir%/symscope.h \
  %reldir%/symtab.h \
  %reldir%/url-handle-manager.h \
  %reldir%/utils.h \
  %reldir%/variables.h \
  %reldir%/xdiv.h \
  %reldir%/xnorm.h \
  %reldir%/xpow.h

NOINSTALL_INTERP_INC = \
  %reldir%/interpreter-private.h \
  %reldir%/mex-private.h

INTERP_C_SRC = \
  %reldir%/oct-rl-edit.c \
  %reldir%/oct-rl-hist.c

INTERP_SRC = \
  %reldir%/Cell.cc \
  %reldir%/auto-shlib.cc \
  %reldir%/call-stack.cc \
  %reldir%/cdisplay.c \
  %reldir%/cmd-edit.cc \
  %reldir%/cmd-hist.cc \
  %reldir%/debug.cc \
  %reldir%/defun.cc \
  %reldir%/display.cc \
  %reldir%/dynamic-ld.cc \
  %reldir%/error.cc \
  %reldir%/errwarn.cc \
  %reldir%/event-manager.cc \
  %reldir%/event-queue.cc \
  %reldir%/fcn-info.cc \
  %reldir%/hook-fcn.cc \
  %reldir%/input.cc \
  %reldir%/interpreter-private.cc \
  %reldir%/interpreter.cc \
  %reldir%/load-path.cc \
  %reldir%/mxarray.cc \
  %reldir%/oct-hist.cc \
  %reldir%/oct-map.cc \
  %reldir%/oct-process.cc \
  %reldir%/pager.cc \
  %reldir%/panic.cc \
  %reldir%/pr-flt-fmt.cc \
  %reldir%/pr-output.cc \
  %reldir%/sparse-xdiv.cc \
  %reldir%/sparse-xpow.cc \
  %reldir%/stack-frame.cc \
  %reldir%/syminfo.cc \
  %reldir%/symrec.cc \
  %reldir%/symscope.cc \
  %reldir%/symtab.cc \
  %reldir%/url-handle-manager.cc \
  %reldir%/utils.cc \
  %reldir%/variables.cc \
  %reldir%/xdiv.cc \
  %reldir%/xnorm.cc \
  %reldir%/xpow.cc \
	$(INTERP_C_SRC) \
  $(NOINSTALL_INTERP_INC)

## Start library specification
noinst_LTLIBRARIES += %reldir%/libinterp.la

%canon_reldir%_libinterp_la_SOURCES := $(INTERP_SRC)

nodist_%canon_reldir%_libinterp_la_SOURCES = \
  %reldir%/mxtypes.h \
  %reldir%/oct-errno.cc

%canon_reldir%_libinterp_la_CPPFLAGS = \
  $(liboctinterp_liboctinterp_la_CPPFLAGS) \
  -DOCTAVE_MEX_SOVERSION="$(OCTAVE_LIBOCTMEX_SOVERSION_MAJOR)"

liboctinterp_liboctinterp_la_LIBADD += %reldir%/libinterp.la

## Special rules for sources which must be built before rest of compilation.

%reldir%/oct-errno.cc: %reldir%/oct-errno.in.cc %reldir%/mk-errno-list.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	if [ -n "$(PERL)" ]; then \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --perl "$(PERL)" < $< > $@-t; \
	elif [ -n "$(PYTHON)" ]; then \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SHELL) $(srcdir)/%reldir%/mk-errno-list.sh --sed "$(SED)" < $< > $@-t; \
	fi && \
	mv $@-t $@

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

liboctinterp_EXTRA_DIST += \
  %reldir%/mk-errno-list.sh \
  %reldir%/mxtypes.in.h \
  %reldir%/oct-errno.in.cc

liboctinterp_DISTCLEANFILES += \
  %reldir%/mxtypes.h \
  %reldir%/oct-errno.cc
