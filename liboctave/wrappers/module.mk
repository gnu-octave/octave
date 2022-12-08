NOINSTALL_WRAPPERS_INC = \
  %reldir%/areadlink-wrapper.h \
  %reldir%/async-system-wrapper.h \
  %reldir%/base64-wrappers.h \
  %reldir%/canonicalize-file-name-wrapper.h \
  %reldir%/dirent-wrappers.h \
  %reldir%/fcntl-wrappers.h \
  %reldir%/filepos-wrappers.h \
  %reldir%/fpucw-wrappers.h \
  %reldir%/gen-tempname-wrapper.h \
  %reldir%/getopt-wrapper.h \
  %reldir%/glob-wrappers.h \
  %reldir%/hash-wrappers.h \
  %reldir%/iconv-wrappers.h \
  %reldir%/intprops-wrappers.h \
  %reldir%/localcharset-wrapper.h \
  %reldir%/math-wrappers.h \
  %reldir%/mkostemp-wrapper.h \
  %reldir%/mkostemps-wrapper.h \
  %reldir%/nanosleep-wrapper.h \
  %reldir%/nproc-wrapper.h \
  %reldir%/octave-popen2.h \
  %reldir%/putenv-wrapper.h \
  %reldir%/set-program-name-wrapper.h \
  %reldir%/signal-wrappers.h \
  %reldir%/stat-wrappers.h \
  %reldir%/strcase-wrappers.h \
  %reldir%/strdup-wrapper.h \
  %reldir%/strftime-wrapper.h \
  %reldir%/strmode-wrapper.h \
  %reldir%/strptime-wrapper.h \
  %reldir%/time-wrappers.h \
  %reldir%/uname-wrapper.h \
  %reldir%/unicase-wrappers.h \
  %reldir%/uniconv-wrappers.h \
  %reldir%/unictype-wrappers.h \
  %reldir%/unistd-wrappers.h \
  %reldir%/unistr-wrappers.h \
  %reldir%/unsetenv-wrapper.h \
  %reldir%/vasprintf-wrapper.h \
  %reldir%/wait-for-input.h \
  %reldir%/wait-wrappers.h

WRAPPERS_SRC = \
  %reldir%/areadlink-wrapper.c \
  %reldir%/async-system-wrapper.c \
  %reldir%/base64-wrappers.c \
  %reldir%/canonicalize-file-name-wrapper.c \
  %reldir%/cxx-signal-helpers.cc \
  %reldir%/dirent-wrappers.c \
  %reldir%/fcntl-wrappers.c \
  %reldir%/filepos-wrappers.c \
  %reldir%/fpucw-wrappers.c \
  %reldir%/gen-tempname-wrapper.c \
  %reldir%/getopt-wrapper.c \
  %reldir%/glob-wrappers.c \
  %reldir%/hash-wrappers.c \
  %reldir%/iconv-wrappers.c \
  %reldir%/intprops-wrappers.c \
  %reldir%/localcharset-wrapper.c \
  %reldir%/math-wrappers.c \
  %reldir%/mkostemp-wrapper.c \
  %reldir%/mkostemps-wrapper.c \
  %reldir%/nanosleep-wrapper.c \
  %reldir%/nproc-wrapper.c \
  %reldir%/octave-popen2.c \
  %reldir%/putenv-wrapper.c \
  %reldir%/set-program-name-wrapper.c \
  %reldir%/signal-wrappers.c \
  %reldir%/stat-wrappers.c \
  %reldir%/strcase-wrappers.c \
  %reldir%/strdup-wrapper.c \
  %reldir%/strftime-wrapper.c \
  %reldir%/strmode-wrapper.c \
  %reldir%/strptime-wrapper.c \
  %reldir%/time-wrappers.c \
  %reldir%/uname-wrapper.c \
  %reldir%/unicase-wrappers.c \
  %reldir%/uniconv-wrappers.c \
  %reldir%/unictype-wrappers.c \
  %reldir%/unistd-wrappers.c \
  %reldir%/unistr-wrappers.c \
  %reldir%/unsetenv-wrapper.c \
  %reldir%/vasprintf-wrapper.c \
  %reldir%/wait-for-input.c \
  %reldir%/wait-wrappers.c \
  $(NOINSTALL_WRAPPERS_INC)

noinst_LTLIBRARIES += %reldir%/libwrappers.la

%canon_reldir%_libwrappers_la_SOURCES = $(WRAPPERS_SRC)

%canon_reldir%_libwrappers_la_CPPFLAGS = \
  @OCTAVE_DLL_DEFS@ \
  @EXTERNAL_DLL_DEFS@ \
  -Ilibgnu -I$(srcdir)/libgnu

liboctave_liboctave_la_LIBADD += %reldir%/libwrappers.la
