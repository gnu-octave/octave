SYSTEM_INC = \
  %reldir%/child-list.h \
  %reldir%/dir-ops.h \
  %reldir%/file-ops.h \
  %reldir%/file-stat.h \
  %reldir%/lo-sysdep.h \
  %reldir%/lo-sysinfo.h \
  %reldir%/mach-info.h \
  %reldir%/oct-env.h \
  %reldir%/oct-group.h \
  %reldir%/oct-password.h \
  %reldir%/oct-syscalls.h \
  %reldir%/oct-time.h \
  %reldir%/oct-uname.h

SYSTEM_SRC = \
  %reldir%/child-list.cc \
  %reldir%/cmach-info.c \
  %reldir%/dir-ops.cc \
  %reldir%/file-ops.cc \
  %reldir%/file-stat.cc \
  %reldir%/lo-sysdep.cc \
  %reldir%/lo-sysinfo.cc \
  %reldir%/mach-info.cc \
  %reldir%/oct-env.cc \
  %reldir%/oct-group.cc \
  %reldir%/oct-password.cc \
  %reldir%/oct-syscalls.cc \
  %reldir%/oct-time.cc \
  %reldir%/oct-uname.cc

noinst_HEADERS += \
  %reldir%/cmach-info.h

noinst_LTLIBRARIES += %reldir%/libsystem.la

%canon_reldir%_libsystem_la_SOURCES = $(SYSTEM_SRC)

%canon_reldir%_libsystem_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

liboctave_liboctave_la_LIBADD += %reldir%/libsystem.la
