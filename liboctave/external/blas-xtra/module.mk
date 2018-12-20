EXTERNAL_SOURCES += \
  %reldir%/ddot3.f \
  %reldir%/zdotc3.f \
  %reldir%/sdot3.f \
  %reldir%/cdotc3.f \
  %reldir%/dmatm3.f \
  %reldir%/zmatm3.f \
  %reldir%/smatm3.f \
  %reldir%/cmatm3.f \
  %reldir%/xddot.f \
  %reldir%/xdnrm2.f \
  %reldir%/xdznrm2.f \
  %reldir%/xzdotc.f \
  %reldir%/xzdotu.f \
  %reldir%/xsdot.f \
  %reldir%/xsnrm2.f \
  %reldir%/xscnrm2.f \
  %reldir%/xcdotc.f \
  %reldir%/xcdotu.f \
  %reldir%/cconv2.f \
  %reldir%/csconv2.f \
  %reldir%/dconv2.f \
  %reldir%/sconv2.f \
  %reldir%/zconv2.f \
  %reldir%/zdconv2.f

XERBLA_SRC = \
  %reldir%/xerbla.cc

%canon_reldir%_libxerbla_la_SOURCES = $(XERBLA_SRC)

%canon_reldir%_libxerbla_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS)

if AMCOND_BUILD_EXTERNAL_LIBXERBLA
  octlib_LTLIBRARIES += %reldir%/libxerbla.la

  %canon_reldir%_libxerbla_la_LDFLAGS = \
    -avoid-version \
    $(NO_UNDEFINED_LDFLAG) \
    -bindir $(bindir) \
    $(WARN_LDFLAGS)
else
  noinst_LTLIBRARIES += %reldir%/libxerbla.la

  liboctave_liboctave_la_LIBADD += %reldir%/libxerbla.la
endif
