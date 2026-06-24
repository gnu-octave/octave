LOAD_SAVE_INC = \
  %reldir%/gzfstream.h \
  %reldir%/load-save.h \
  %reldir%/ls-ascii-helper.h \
  %reldir%/ls-hdf5.h \
  %reldir%/ls-mat-ascii.h \
  %reldir%/ls-mat-subsys.h \
  %reldir%/ls-mat4.h \
  %reldir%/ls-mat5.h \
  %reldir%/ls-oct-binary.h \
  %reldir%/ls-oct-text.h \
  %reldir%/ls-utils.h \
  %reldir%/oct-hdf5-types.h

NOINSTALL_LOAD_SAVE_INC = \
  %reldir%/oct-hdf5.h

LOAD_SAVE_SRC = \
  %reldir%/coct-hdf5-types.c \
  %reldir%/gzfstream.cc \
  %reldir%/load-save.cc \
  %reldir%/ls-ascii-helper.cc \
  %reldir%/ls-hdf5.cc \
  %reldir%/ls-mat-ascii.cc \
  %reldir%/ls-mat-subsys.cc \
  %reldir%/ls-mat4.cc \
  %reldir%/ls-mat5.cc \
  %reldir%/ls-oct-binary.cc \
  %reldir%/ls-oct-text.cc \
  %reldir%/ls-utils.cc \
  %reldir%/oct-hdf5-types.cc \
  $(NOINSTALL_LOAD_SAVE_INC)

noinst_LTLIBRARIES += %reldir%/libload-save.la

%canon_reldir%_libload_save_la_SOURCES := $(LOAD_SAVE_SRC)

%canon_reldir%_libload_save_la_CPPFLAGS = \
  $(liboctinterp_liboctinterp_la_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  $(Z_CPPFLAGS)

liboctinterp_liboctinterp_la_LIBADD += %reldir%/libload-save.la
