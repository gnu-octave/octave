UTIL_INC = \
  %reldir%/action-container.h \
  %reldir%/base-list.h \
  %reldir%/byte-swap.h \
  %reldir%/caseless-str.h \
  %reldir%/cmd-edit.h \
  %reldir%/cmd-hist.h \
  %reldir%/data-conv.h \
  %reldir%/file-info.h \
  %reldir%/glob-match.h \
  %reldir%/lo-array-errwarn.h \
  %reldir%/lo-hash.h \
  %reldir%/lo-ieee.h \
  %reldir%/lo-regexp.h \
  %reldir%/lo-traits.h \
  %reldir%/lo-utils.h \
  %reldir%/f77-fcn.h \
  %reldir%/lo-error.h \
  %reldir%/octave-preserve-stream-state.h \
  %reldir%/quit.h \
  %reldir%/oct-atomic.h \
  %reldir%/oct-base64.h \
  %reldir%/oct-binmap.h \
  %reldir%/oct-cmplx.h \
  %reldir%/oct-glob.h \
  %reldir%/oct-inttypes-fwd.h \
  %reldir%/oct-inttypes.h \
  %reldir%/oct-locbuf.h \
  %reldir%/oct-mutex.h \
  %reldir%/oct-refcount.h \
  %reldir%/oct-rl-edit.h \
  %reldir%/oct-rl-hist.h \
  %reldir%/oct-shlib.h \
  %reldir%/oct-sort.h \
  %reldir%/oct-string.h \
  %reldir%/pathsearch.h \
  %reldir%/singleton-cleanup.h \
  %reldir%/sparse-util.h \
  %reldir%/str-vec.h \
  %reldir%/unwind-prot.h \
  %reldir%/url-transfer.h

NOINSTALL_UTIL_INC = \
  %reldir%/kpse.h \
  %reldir%/oct-sparse.h

UTIL_F77_SRC = \
  %reldir%/d1mach.f \
  %reldir%/i1mach.f \
  %reldir%/r1mach.f

UTIL_C_SRC = \
  %reldir%/blaswrap.c \
  %reldir%/f77-fcn.c \
  %reldir%/lo-error.c \
  %reldir%/oct-rl-edit.c \
  %reldir%/oct-rl-hist.c

UTIL_SRC = \
  %reldir%/action-container.cc \
  %reldir%/cmd-edit.cc \
  %reldir%/cmd-hist.cc \
  %reldir%/data-conv.cc \
  %reldir%/f77-dummy-main.cc \
  %reldir%/file-info.cc \
  %reldir%/glob-match.cc \
  %reldir%/kpse.cc \
  %reldir%/lo-array-errwarn.cc \
  %reldir%/lo-hash.cc \
  %reldir%/lo-ieee.cc \
  %reldir%/lo-regexp.cc \
  %reldir%/lo-utils.cc \
  %reldir%/quit.cc \
  %reldir%/oct-atomic.c \
  %reldir%/oct-base64.cc \
  %reldir%/oct-cmplx.cc \
  %reldir%/oct-glob.cc \
  %reldir%/oct-inttypes.cc \
  %reldir%/oct-mutex.cc \
  %reldir%/oct-shlib.cc \
  %reldir%/oct-sparse.cc \
  %reldir%/oct-string.cc \
  %reldir%/pathsearch.cc \
  %reldir%/singleton-cleanup.cc \
  %reldir%/sparse-util.cc \
  %reldir%/str-vec.cc \
  %reldir%/unwind-prot.cc \
  %reldir%/url-transfer.cc \
  $(UTIL_F77_SRC) \
  $(UTIL_C_SRC) \
  $(NOINSTALL_UTIL_INC)

liboctave_EXTRA_DIST += \
  %reldir%/d1mach-tst.for

LIBOCTAVE_TEMPLATE_SRC += \
  %reldir%/oct-sort.cc

noinst_LTLIBRARIES += %reldir%/libutil.la

%canon_reldir%_libutil_la_SOURCES = $(UTIL_SRC)

%canon_reldir%_libutil_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS) \
  $(CURL_CPPFLAGS) \
  $(PCRE_CPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

%canon_reldir%_libutil_la_FFLAGS = $(F77_INTEGER_8_FLAG)

liboctave_liboctave_la_LIBADD += %reldir%/libutil.la
