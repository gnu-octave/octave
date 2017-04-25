nodist_liboctave_external_libexternal_la_SOURCES =

liboctave_external_libexternal_la_FFLAGS = $(F77_INTEGER_8_FLAG)

liboctave_external_libexternal_la_DEPENDENCIES = liboctave/external/external.def

EXTERNAL_INC =

EXTERNAL_SOURCES =

include liboctave/external/amos/module.mk
include liboctave/external/blas-xtra/module.mk
include liboctave/external/daspk/module.mk
include liboctave/external/dasrt/module.mk
include liboctave/external/dassl/module.mk
include liboctave/external/Faddeeva/module.mk
include liboctave/external/fftpack/module.mk
include liboctave/external/lapack-xtra/module.mk
include liboctave/external/odepack/module.mk
include liboctave/external/ordered-qz/module.mk
include liboctave/external/quadpack/module.mk
include liboctave/external/ranlib/module.mk
include liboctave/external/slatec-err/module.mk
include liboctave/external/slatec-fn/module.mk

liboctave/external/external.def: $(liboctave_external_libexternal_la_SOURCES) build-aux/mk-f77-def.sh
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) build-aux/mk-f77-def.sh $(srcdir) $(liboctave_external_libexternal_la_SOURCES) > $@-t && \
	mv $@-t $@

liboctave_CLEANFILES += \
  liboctave/external/external.def \
  liboctave/external/ranlib/ranlib.def \
  $(nodist_liboctave_external_libexternal_la_SOURCES)

noinst_LTLIBRARIES += liboctave/external/libexternal.la

liboctave_external_libexternal_la_SOURCES = $(EXTERNAL_SOURCES)

liboctave_external_libexternal_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

liboctave_external_libexternal_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_external_libexternal_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/external/libexternal.la
