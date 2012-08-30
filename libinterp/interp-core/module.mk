EXTRA_DIST += \
  interp-core/module.mk \
  interp-core/gl2ps.c \
  interp-core/mxarray.in.h \
  interp-core/oct-errno.in.cc

JIT_INCLUDES = \
  interp-core/jit-util.h \
  interp-core/jit-typeinfo.h \
  interp-core/jit-ir.h \
  interp-core/pt-jit.h

INTERP_CORE_INCLUDES = \
  interp-core/Cell.h \
  interp-core/c-file-ptr-stream.h \
  interp-core/comment-list.h \
  interp-core/cutils.h \
  interp-core/defun-dld.h \
  interp-core/defun-int.h \
  interp-core/display.h \
  interp-core/dynamic-ld.h \
  interp-core/gl-render.h \
  interp-core/gl2ps-renderer.h \
  interp-core/gl2ps.h \
  interp-core/gripes.h \
  interp-core/ls-ascii-helper.h \
  interp-core/ls-hdf5.h \
  interp-core/ls-mat-ascii.h \
  interp-core/ls-mat4.h \
  interp-core/ls-mat5.h \
  interp-core/ls-oct-binary.h \
  interp-core/ls-utils.h \
  interp-core/mex.h \
  interp-core/mexproto.h \
  interp-core/mxarray.in.h \
  interp-core/oct-errno.h \
  interp-core/oct-fstrm.h \
  interp-core/oct-hdf5.h \
  interp-core/oct-iostrm.h \
  interp-core/oct-lvalue.h \
  interp-core/oct-map.h \
  interp-core/oct-obj.h \
  interp-core/oct-prcstrm.h \
  interp-core/oct-procbuf.h \
  interp-core/oct-stdstrm.h \
  interp-core/oct-stream.h \
  interp-core/oct-strstrm.h \
  interp-core/oct.h \
  interp-core/procstream.h \
  interp-core/siglist.h \
  interp-core/sparse-xdiv.h \
  interp-core/sparse-xpow.h \
  interp-core/txt-eng-ft.h \
  interp-core/txt-eng.h \
  interp-core/unwind-prot.h \
  interp-core/xdiv.h \
  interp-core/xnorm.h \
  interp-core/xpow.h \
  interp-core/zfstream.h \
  $(JIT_INCLUDES)

JIT_SRC = \
  interp-core/jit-util.cc \
  interp-core/jit-typeinfo.cc \
  interp-core/jit-ir.cc \
  interp-core/pt-jit.cc

C_INTERP_CORE_SRC = \
  interp-core/cutils.c \
  interp-core/matherr.c \
  interp-core/siglist.c \
  interp-core/xgl2ps.c

INTERP_CORE_SRC = \
  interp-core/Cell.cc \
  interp-core/c-file-ptr-stream.cc \
  interp-core/comment-list.cc \
  interp-core/display.cc \
  interp-core/dynamic-ld.cc \
  interp-core/gl-render.cc \
  interp-core/gl2ps-renderer.cc \
  interp-core/gripes.cc \
  interp-core/ls-ascii-helper.cc \
  interp-core/ls-hdf5.cc \
  interp-core/ls-mat-ascii.cc \
  interp-core/ls-mat4.cc \
  interp-core/ls-mat5.cc \
  interp-core/ls-oct-binary.cc \
  interp-core/ls-utils.cc \
  interp-core/mex.cc \
  interp-core/oct-fstrm.cc \
  interp-core/oct-iostrm.cc \
  interp-core/oct-lvalue.cc \
  interp-core/oct-map.cc \
  interp-core/oct-obj.cc \
  interp-core/oct-prcstrm.cc \
  interp-core/oct-procbuf.cc \
  interp-core/oct-stream.cc \
  interp-core/oct-strstrm.cc \
  interp-core/procstream.cc \
  interp-core/sparse-xdiv.cc \
  interp-core/sparse-xpow.cc \
  interp-core/txt-eng-ft.cc \
  interp-core/unwind-prot.cc \
  interp-core/xdiv.cc \
  interp-core/xnorm.cc \
  interp-core/xpow.cc \
  interp-core/zfstream.cc \
  $(JIT_SRC) \
  $(C_INTERP_CORE_SRC)

## FIXME: I don't believe this rule actually fires
display.df display.lo: CPPFLAGS += $(X11_FLAGS)

## Special rules for sources which must be built before rest of compilation.
interp-core/oct-errno.cc: interp-core/oct-errno.in.cc Makefile
	if test -n "$(PERL)"; then \
	  $(srcdir)/mk-errno-list --perl "$(PERL)" < $< > $@-t; \
	elif test -n "$(PYTHON)"; then \
	  $(srcdir)/mk-errno-list --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SED) '/@SYSDEP_ERRNO_LIST@/D' $< > $@-t; \
	fi
	mv $@-t $@

interp-core/mxarray.h: interp-core/mxarray.in.h Makefile
	$(SED) < $< \
	  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically from $(<F) by Make.|" \
	  -e "s|%OCTAVE_IDX_TYPE%|${OCTAVE_IDX_TYPE}|" > $@-t
	mv $@-t $@

noinst_LTLIBRARIES += interp-core/libinterp-core.la

interp_core_libinterp_core_la_SOURCES = $(INTERP_CORE_SRC)
interp_core_libinterp_core_la_CPPFLAGS = $(liboctinterp_la_CPPFLAGS)
