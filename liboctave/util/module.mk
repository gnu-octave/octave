UTIL_INC = \
  liboctave/util/action-container.h \
  liboctave/util/base-list.h \
  liboctave/util/byte-swap.h \
  liboctave/util/caseless-str.h \
  liboctave/util/cmd-edit.h \
  liboctave/util/cmd-hist.h \
  liboctave/util/data-conv.h \
  liboctave/util/functor.h \
  liboctave/util/glob-match.h \
  liboctave/util/lo-array-errwarn.h \
  liboctave/util/lo-array-gripes.h \
  liboctave/util/lo-cutils.h \
  liboctave/util/lo-ieee.h \
  liboctave/util/lo-macros.h \
  liboctave/util/lo-math.h \
  liboctave/util/lo-traits.h \
  liboctave/util/lo-utils.h \
  liboctave/util/oct-alloc.h \
  liboctave/util/oct-base64.h \
  liboctave/util/oct-binmap.h \
  liboctave/util/oct-cmplx.h \
  liboctave/util/oct-glob.h \
  liboctave/util/oct-inttypes.h \
  liboctave/util/oct-inttypes-fwd.h \
  liboctave/util/oct-locbuf.h \
  liboctave/util/oct-mutex.h \
  liboctave/util/oct-refcount.h \
  liboctave/util/oct-rl-edit.h \
  liboctave/util/oct-rl-hist.h \
  liboctave/util/oct-shlib.h \
  liboctave/util/oct-sort.h \
  liboctave/util/pathsearch.h \
  liboctave/util/lo-regexp.h \
  liboctave/util/singleton-cleanup.h \
  liboctave/util/sparse-sort.h \
  liboctave/util/sparse-util.h \
  liboctave/util/str-vec.h \
  liboctave/util/sun-utils.h \
  liboctave/util/unwind-prot.h \
  liboctave/util/url-transfer.h

NOINSTALL_UTIL_INC = \
  liboctave/util/oct-sparse.h \
  liboctave/util/statdefs.h

UTIL_C_SRC = \
  liboctave/util/f2c-main.c \
  liboctave/util/lo-cutils.c \
  liboctave/util/oct-rl-edit.c \
  liboctave/util/oct-rl-hist.c

UTIL_SRC = \
  liboctave/util/cmd-edit.cc \
  liboctave/util/cmd-hist.cc \
  liboctave/util/data-conv.cc \
  liboctave/util/glob-match.cc \
  liboctave/util/lo-array-errwarn.cc \
  liboctave/util/lo-array-gripes.cc \
  liboctave/util/lo-ieee.cc \
  liboctave/util/lo-utils.cc \
  liboctave/util/oct-base64.cc \
  liboctave/util/oct-glob.cc \
  liboctave/util/oct-inttypes.cc \
  liboctave/util/oct-locbuf.cc \
  liboctave/util/oct-mutex.cc \
  liboctave/util/oct-shlib.cc \
  liboctave/util/pathsearch.cc \
  liboctave/util/lo-regexp.cc \
  liboctave/util/singleton-cleanup.cc \
  liboctave/util/sparse-sort.cc \
  liboctave/util/sparse-util.cc \
  liboctave/util/str-vec.cc \
  liboctave/util/unwind-prot.cc \
  liboctave/util/url-transfer.cc \
  $(UTIL_C_SRC) \
  $(NOINSTALL_UTIL_INC)

LIBOCTAVE_TEMPLATE_SRC += \
  liboctave/util/oct-sort.cc

EXTRA_DIST += \
  liboctave/util/kpse.cc

noinst_LTLIBRARIES += liboctave/util/libutil.la

liboctave_util_libutil_la_SOURCES = $(UTIL_SRC)

liboctave_util_libutil_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS) \
  $(CURL_CPPFLAGS) \
  $(PCRE_CPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_util_libutil_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_util_libutil_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/util/libutil.la
