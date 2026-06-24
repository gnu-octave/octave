COREFCN_SYSTEM_INC = \
  %reldir%/file-io.h

COREFCN_SYSTEM_SRC = \
  %reldir%/__ftp__.cc \
  %reldir%/dirfns.cc \
  %reldir%/dlmread.cc \
  %reldir%/file-io.cc \
  %reldir%/getgrent.cc \
  %reldir%/getpwent.cc \
  %reldir%/getrusage.cc \
  %reldir%/nproc.cc \
  %reldir%/syscalls.cc \
  %reldir%/time.cc \
  %reldir%/toplev.cc \
  %reldir%/urlwrite.cc

noinst_LTLIBRARIES += %reldir%/libsystem.la

%canon_reldir%_libsystem_la_SOURCES := $(COREFCN_SYSTEM_SRC)

%canon_reldir%_libsystem_la_CPPFLAGS = \
  $(liboctinterp_liboctinterp_la_CPPFLAGS)

liboctinterp_liboctinterp_la_LIBADD += %reldir%/libsystem.la
