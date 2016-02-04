SYSTEM_INC = \
  liboctave/system/dir-ops.h \
  liboctave/system/file-ops.h \
  liboctave/system/file-stat.h \
  liboctave/system/lo-sysdep.h \
  liboctave/system/mach-info.h \
  liboctave/system/oct-env.h \
  liboctave/system/oct-group.h \
  liboctave/system/oct-openmp.h \
  liboctave/system/oct-passwd.h \
  liboctave/system/oct-syscalls.h \
  liboctave/system/oct-time.h \
  liboctave/system/oct-uname.h

NOINSTALL_SYSTEM_INC = \
  liboctave/system/pathlen.h \
  liboctave/system/sysdir.h \
  liboctave/system/syswait.h

SYSTEM_SRC = \
  liboctave/system/dir-ops.cc \
  liboctave/system/file-ops.cc \
  liboctave/system/file-stat.cc \
  liboctave/system/lo-sysdep.cc \
  liboctave/system/mach-info.cc \
  liboctave/system/oct-env.cc \
  liboctave/system/oct-group.cc \
  liboctave/system/oct-passwd.cc \
  liboctave/system/oct-syscalls.cc \
  liboctave/system/oct-time.cc \
  liboctave/system/oct-uname.cc \
  $(NOINSTALL_SYSTEM_INC)

noinst_LTLIBRARIES += liboctave/system/libsystem.la

liboctave_system_libsystem_la_SOURCES = $(SYSTEM_SRC)

liboctave_system_libsystem_la_CPPFLAGS = $(liboctave_liboctave_la_CPPFLAGS)

liboctave_system_libsystem_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_system_libsystem_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/system/libsystem.la
