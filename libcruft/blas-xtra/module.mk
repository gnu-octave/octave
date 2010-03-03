EXTRA_DIST += blas-xtra/module.mk

libcruft_la_SOURCES += \
  blas-xtra/ddot3.f \
  blas-xtra/zdotc3.f \
  blas-xtra/sdot3.f \
  blas-xtra/cdotc3.f \
  blas-xtra/dmatm3.f \
  blas-xtra/zmatm3.f \
  blas-xtra/smatm3.f \
  blas-xtra/cmatm3.f \
  blas-xtra/xddot.f \
  blas-xtra/xdnrm2.f \
  blas-xtra/xdznrm2.f \
  blas-xtra/xzdotc.f \
  blas-xtra/xzdotu.f \
  blas-xtra/xsdot.f \
  blas-xtra/xsnrm2.f \
  blas-xtra/xscnrm2.f \
  blas-xtra/xcdotc.f \
  blas-xtra/xcdotu.f \
  blas-xtra/xerbla.f \
  blas-xtra/cconv2.f \
  blas-xtra/csconv2.f \
  blas-xtra/dconv2.f \
  blas-xtra/sconv2.f \
  blas-xtra/zconv2.f \
  blas-xtra/zdconv2.f
