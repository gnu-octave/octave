## The following libraries may be needed to satisfy gnulib dependencies:
##
##   $(COPYSIGN_LIBM)
##   $(FLOOR_LIBM)
##   $(GETHOSTNAME_LIB)
##   $(LIBSOCKET)
##   $(LIB_NANOSLEEP)
##   $(LTLIBINTL)
##   $(ROUNDF_LIBM)
##   $(ROUND_LIBM)
##   $(TRUNCF_LIBM)
##   $(TRUNC_LIBM)

LIBCRUFT_LINK_DEPS = \
  $(COPYSIGN_LIBM) \
  $(FLOOR_LIBM) \
  $(GETHOSTNAME_LIB) \
  $(LIBSOCKET) \
  $(LIB_NANOSLEEP) \
  $(LTLIBINTL) \
  $(ROUNDF_LIBM) \
  $(ROUND_LIBM) \
  $(TRUNCF_LIBM) \
  $(TRUNC_LIBM) \
  $(LAPACK_LIBS) \
  $(BLAS_LIBS) \
  $(FLIBS) \
  $(LIBS)

LIBCRUFT_LINK_OPTS =
