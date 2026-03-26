DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

COREFCN_UTIL_SRC = \
  %reldir%/bsxfun.cc \
  %reldir%/cellfun.cc \
  %reldir%/find.cc \
  %reldir%/jsondecode.cc \
  %reldir%/jsonencode.cc \
  %reldir%/lookup.cc \
  %reldir%/matrix_type.cc \
  %reldir%/regexp.cc \
  %reldir%/strfind.cc \
  %reldir%/strfns.cc \
  %reldir%/sub2ind.cc \
  %reldir%/tril.cc \
  %reldir%/typecast.cc

noinst_LTLIBRARIES += \
  %reldir%/libutil.la

%canon_reldir%_libutil_la_SOURCES = $(COREFCN_UTIL_SRC)

%canon_reldir%_libutil_la_CPPFLAGS = \
  $(libinterp_liboctinterp_la_CPPFLAGS)

libinterp_liboctinterp_la_LIBADD += %reldir%/libutil.la
