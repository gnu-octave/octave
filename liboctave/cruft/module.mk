nodist_liboctave_cruft_libcruft_la_SOURCES =

liboctave_cruft_libcruft_la_FFLAGS = $(F77_INTEGER_8_FLAG)

liboctave_cruft_libcruft_la_DEPENDENCIES = liboctave/cruft/cruft.def

CRUFT_INC =

CRUFT_SOURCES =

include liboctave/cruft/amos/module.mk
include liboctave/cruft/blas-xtra/module.mk
include liboctave/cruft/daspk/module.mk
include liboctave/cruft/dasrt/module.mk
include liboctave/cruft/dassl/module.mk
include liboctave/cruft/Faddeeva/module.mk
include liboctave/cruft/fftpack/module.mk
include liboctave/cruft/lapack-xtra/module.mk
include liboctave/cruft/misc/module.mk
include liboctave/cruft/odepack/module.mk
include liboctave/cruft/ordered-qz/module.mk
include liboctave/cruft/quadpack/module.mk
include liboctave/cruft/ranlib/module.mk
include liboctave/cruft/slatec-err/module.mk
include liboctave/cruft/slatec-fn/module.mk

define gen-cruft-def
  rm -f $@-t $@ && \
  $(SHELL) liboctave/cruft/mkf77def $(top_srcdir) $(liboctave_cruft_libcruft_la_SOURCES) > $@-t && \
  mv $@-t $@
endef

## Special rules for files which must be built before compilation
liboctave/cruft/cruft.def: $(liboctave_cruft_libcruft_la_SOURCES) liboctave/cruft/mkf77def
	$(AM_V_GEN)$(gen-cruft-def)

liboctave_DISTCLEANFILES += \
  liboctave/cruft/cruft.def \
  liboctave/cruft/mkf77def \
  liboctave/cruft/ranlib/ranlib.def \
  $(nodist_liboctave_cruft_libcruft_la_SOURCES)

noinst_LTLIBRARIES += liboctave/cruft/libcruft.la

liboctave_cruft_libcruft_la_SOURCES = $(CRUFT_SOURCES)

liboctave_cruft_libcruft_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

liboctave_cruft_libcruft_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_cruft_libcruft_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/cruft/libcruft.la

liboctave_EXTRA_DIST += liboctave/cruft/mkf77def.in
