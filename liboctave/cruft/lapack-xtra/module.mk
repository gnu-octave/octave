EXTRA_DIST += lapack-xtra/module.mk

libcruft_la_SOURCES += \
  lapack-xtra/xclange.f \
  lapack-xtra/xdlamch.f \
  lapack-xtra/xdlange.f \
  lapack-xtra/xilaenv.f \
  lapack-xtra/xslamch.f \
  lapack-xtra/xslange.f \
  lapack-xtra/xzlange.f \
  lapack-xtra/zrsf2csf.f \
  lapack-xtra/crsf2csf.f
