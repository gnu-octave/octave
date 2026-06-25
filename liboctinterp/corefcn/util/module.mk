BUILT_COREFCN_UTIL_INC = \
  %reldir%/default-defs.h

BUILT_SOURCES += $(BUILT_COREFCN_UTIL_INC)

COREFCN_UTIL_INC = \
  %reldir%/defaults.h \
  %reldir%/environment.h \
  %reldir%/help.h \
  %reldir%/sighandlers.h \
  %reldir%/sysdep.h

COREFCN_UTIL_SRC = \
  %reldir%/__magick_read__.cc \
  %reldir%/bsxfun.cc \
  %reldir%/cellfun.cc \
  %reldir%/compile.cc \
  %reldir%/defaults.cc \
  %reldir%/environment.cc \
  %reldir%/find.cc \
  %reldir%/help.cc \
  %reldir%/jsondecode.cc \
  %reldir%/jsonencode.cc \
  %reldir%/lookup.cc \
  %reldir%/matrix_type.cc \
  %reldir%/regexp.cc \
  %reldir%/sighandlers.cc \
  %reldir%/strfind.cc \
  %reldir%/strfns.cc \
  %reldir%/sub2ind.cc \
  %reldir%/sysdep.cc \
  %reldir%/tril.cc \
  %reldir%/typecast.cc

noinst_LTLIBRARIES += %reldir%/libutil.la

%canon_reldir%_libutil_la_SOURCES := $(COREFCN_UTIL_SRC)

%canon_reldir%_libutil_la_CPPFLAGS = \
  $(liboctinterp_liboctinterp_la_CPPFLAGS) \
  $(MAGICK_CPPFLAGS)

liboctinterp_liboctinterp_la_LIBADD += %reldir%/libutil.la

## Special rules for sources which must be built before rest of compilation.

%reldir%/default-defs.h: %reldir%/default-defs.in.h build-aux/subst-config-vals.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-config-vals.sh)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

liboctinterp_EXTRA_DIST += \
  %reldir%/default-defs.in.h

liboctinterp_DISTCLEANFILES += \
  $(BUILT_COREFCN_UTIL_INC)
