STREAM_INC = \
  %reldir%/c-file-ptr-stream.h \
  %reldir%/oct-fstrm.h \
  %reldir%/oct-iostrm.h \
  %reldir%/oct-prcstrm.h \
  %reldir%/oct-procbuf.h \
  %reldir%/oct-stdstrm.h \
  %reldir%/oct-stream.h \
  %reldir%/oct-strstrm.h \
  %reldir%/procstream.h

STREAM_SRC = \
  %reldir%/c-file-ptr-stream.cc \
  %reldir%/oct-fstrm.cc \
  %reldir%/oct-iostrm.cc \
  %reldir%/oct-prcstrm.cc \
  %reldir%/oct-procbuf.cc \
  %reldir%/oct-stream.cc \
  %reldir%/oct-strstrm.cc \
  %reldir%/procstream.cc

noinst_LTLIBRARIES += %reldir%/libstream.la

%canon_reldir%_libstream_la_SOURCES := $(STREAM_SRC)

%canon_reldir%_libstream_la_CPPFLAGS = \
  $(liboctinterp_liboctinterp_la_CPPFLAGS) \
  $(Z_CPPFLAGS)

liboctinterp_liboctinterp_la_LIBADD += %reldir%/libstream.la
