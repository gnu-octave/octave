EXTERNAL_SOURCES += \
  %reldir%/cdotc3.f \
  %reldir%/cmatm3.f \
  %reldir%/ddot3.f \
  %reldir%/dmatm3.f \
  %reldir%/sdot3.f \
  %reldir%/smatm3.f \
  %reldir%/xcdotc.f \
  %reldir%/xcdotu.f \
  %reldir%/xddot.f \
  %reldir%/xdnrm2.f \
  %reldir%/xdznrm2.f \
  %reldir%/xscnrm2.f \
  %reldir%/xsdot.f \
  %reldir%/xsnrm2.f \
  %reldir%/xzdotc.f \
  %reldir%/xzdotu.f \
  %reldir%/zdotc3.f \
  %reldir%/zmatm3.f

XERBLA_SRC = %reldir%/xerbla.cc

%canon_reldir%_libxerbla_la_SOURCES := $(XERBLA_SRC)

%canon_reldir%_libxerbla_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS)

if AMCOND_BUILD_EXTERNAL_LIBXERBLA
  octlib_LTLIBRARIES += %reldir%/libxerbla.la

  %canon_reldir%_libxerbla_la_LDFLAGS := \
    $(WARN_LDFLAGS) \
    $(NO_UNDEFINED_LDFLAG) \
    -avoid-version \
    -bindir $(bindir)

else    # no external libxerbla
  noinst_LTLIBRARIES += %reldir%/libxerbla.la

  liboctave_liboctave_la_LIBADD += %reldir%/libxerbla.la
endif
