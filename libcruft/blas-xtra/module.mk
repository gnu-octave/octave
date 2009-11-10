EXTRA_DIST += blas-xtra/module.mk

libcruft_la_SOURCES += \
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
  blas-xtra/xerbla.f
