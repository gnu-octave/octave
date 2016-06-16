NOINSTALL_WRAPPERS_INC = \
  liboctave/wrappers/areadlink-wrapper.h \
  liboctave/wrappers/base64-wrappers.h \
  liboctave/wrappers/canonicalize-file-name-wrapper.h \
  liboctave/wrappers/dirent-wrappers.h \
  liboctave/wrappers/fcntl-wrappers.h \
  liboctave/wrappers/filepos-wrappers.h \
  liboctave/wrappers/fpucw-wrapper.h \
  liboctave/wrappers/gen-tempname-wrapper.h \
  liboctave/wrappers/glob-wrappers.h \
  liboctave/wrappers/hash-wrappers.h \
  liboctave/wrappers/mkostemp-wrapper.h \
  liboctave/wrappers/nanosleep-wrapper.h \
  liboctave/wrappers/nproc-wrapper.h \
  liboctave/wrappers/octave-popen2.h \
  liboctave/wrappers/putenv-wrapper.h \
  liboctave/wrappers/set-program-name-wrapper.h \
  liboctave/wrappers/stat-wrappers.h \
  liboctave/wrappers/strftime-wrapper.h \
  liboctave/wrappers/strmode-wrapper.h \
  liboctave/wrappers/strptime-wrapper.h \
  liboctave/wrappers/uname-wrapper.h \
  liboctave/wrappers/unistd-wrappers.h \
  liboctave/wrappers/unsetenv-wrapper.h \
  liboctave/wrappers/vasprintf-wrapper.h \
  liboctave/wrappers/wait-for-input.h \
  liboctave/wrappers/wait-wrappers.h

WRAPPERS_SRC = \
  liboctave/wrappers/areadlink-wrapper.c \
  liboctave/wrappers/base64-wrappers.c \
  liboctave/wrappers/canonicalize-file-name-wrapper.c \
  liboctave/wrappers/dirent-wrappers.c \
  liboctave/wrappers/fcntl-wrappers.c \
  liboctave/wrappers/filepos-wrappers.c \
  liboctave/wrappers/fpucw-wrapper.c \
  liboctave/wrappers/gen-tempname-wrapper.c \
  liboctave/wrappers/glob-wrappers.c \
  liboctave/wrappers/hash-wrappers.c \
  liboctave/wrappers/mkostemp-wrapper.c \
  liboctave/wrappers/nanosleep-wrapper.c \
  liboctave/wrappers/nproc-wrapper.c \
  liboctave/wrappers/octave-popen2.c \
  liboctave/wrappers/putenv-wrapper.c \
  liboctave/wrappers/set-program-name-wrapper.c \
  liboctave/wrappers/stat-wrappers.c \
  liboctave/wrappers/strftime-wrapper.c \
  liboctave/wrappers/strmode-wrapper.c \
  liboctave/wrappers/strptime-wrapper.c \
  liboctave/wrappers/uname-wrapper.c \
  liboctave/wrappers/unistd-wrappers.c \
  liboctave/wrappers/unsetenv-wrapper.c \
  liboctave/wrappers/vasprintf-wrapper.c \
  liboctave/wrappers/wait-for-input.c \
  liboctave/wrappers/wait-wrappers.c \
  $(NOINSTALL_WRAPPERS_INC)

noinst_LTLIBRARIES += liboctave/wrappers/libwrappers.la

liboctave_wrappers_libwrappers_la_SOURCES = $(WRAPPERS_SRC)

liboctave_wrappers_libwrappers_la_CPPFLAGS = \
  -Ilibgnu -I$(srcdir)/libgnu

liboctave_wrappers_libwrappers_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/wrappers/libwrappers.la
