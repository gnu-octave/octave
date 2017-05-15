nodist_%canon_reldir%_libexternal_la_SOURCES =

%canon_reldir%_libexternal_la_FFLAGS = $(F77_INTEGER_8_FLAG)

%canon_reldir%_libexternal_la_DEPENDENCIES = %reldir%/external.def

EXTERNAL_INC =

EXTERNAL_SOURCES =

include %reldir%/amos/module.mk
include %reldir%/blas-xtra/module.mk
include %reldir%/daspk/module.mk
include %reldir%/dasrt/module.mk
include %reldir%/dassl/module.mk
include %reldir%/Faddeeva/module.mk
include %reldir%/fftpack/module.mk
include %reldir%/lapack-xtra/module.mk
include %reldir%/odepack/module.mk
include %reldir%/ordered-qz/module.mk
include %reldir%/quadpack/module.mk
include %reldir%/ranlib/module.mk
include %reldir%/slatec-err/module.mk
include %reldir%/slatec-fn/module.mk

%reldir%/external.def: $(%canon_reldir%_libexternal_la_SOURCES) build-aux/mk-f77-def.sh
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) build-aux/mk-f77-def.sh $(srcdir) $(%canon_reldir%_libexternal_la_SOURCES) > $@-t && \
	mv $@-t $@

liboctave_CLEANFILES += \
  %reldir%/external.def \
  %reldir%/ranlib/ranlib.def \
  $(nodist_%canon_reldir%_libexternal_la_SOURCES)

noinst_LTLIBRARIES += %reldir%/libexternal.la

%canon_reldir%_libexternal_la_SOURCES = $(EXTERNAL_SOURCES)

%canon_reldir%_libexternal_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

%canon_reldir%_libexternal_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

%canon_reldir%_libexternal_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += %reldir%/libexternal.la
