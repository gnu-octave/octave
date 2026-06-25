EXTERNAL_INC =

EXTERNAL_SOURCES =

include %reldir%/amos/module.mk
include %reldir%/blas-xtra/module.mk
include %reldir%/daspk/module.mk
include %reldir%/dasrt/module.mk
include %reldir%/dassl/module.mk
include %reldir%/Faddeeva/module.mk
include %reldir%/lapack-xtra/module.mk
include %reldir%/odepack/module.mk
include %reldir%/quadpack/module.mk
include %reldir%/randlib/module.mk
include %reldir%/slatec-err/module.mk
include %reldir%/slatec-fn/module.mk

## Start library specification 
noinst_LTLIBRARIES += %reldir%/libexternal.la

%canon_reldir%_libexternal_la_SOURCES := $(EXTERNAL_SOURCES)

%canon_reldir%_libexternal_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

%canon_reldir%_libexternal_la_FFLAGS := $(F77_INTEGER_8_FLAG)

liboctave_liboctave_la_LIBADD += %reldir%/libexternal.la

## Special rules:
## Mostly for sources which must be built before rest of compilation.

%canon_reldir%_libexternal_la_DEPENDENCIES = %reldir%/external.def

%reldir%/external.def: $(EXTERNAL_SOURCES) %reldir%/mk-f77-def.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) %reldir%/mk-f77-def.sh $(srcdir) $(EXTERNAL_SOURCES) > $@-t && \
	mv $@-t $@

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

liboctave_EXTRA_DIST += %reldir%/mk-f77-def.in.sh

liboctave_DISTCLEANFILES += \
  %reldir%/external.def \
  %reldir%/mk-f77-def.sh
