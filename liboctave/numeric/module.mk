LIBOCTAVE_OPT_INC = \
  liboctave/numeric/DASPK-opts.h \
  liboctave/numeric/DASRT-opts.h \
  liboctave/numeric/DASSL-opts.h \
  liboctave/numeric/LSODE-opts.h \
  liboctave/numeric/Quad-opts.h

LIBOCTAVE_OPT_IN = $(LIBOCTAVE_OPT_INC:.h=.in)

NUMERIC_INC = \
  liboctave/numeric/CollocWt.h \
  liboctave/numeric/DAE.h \
  liboctave/numeric/DAEFunc.h \
  liboctave/numeric/DAERT.h \
  liboctave/numeric/DAERTFunc.h \
  liboctave/numeric/DASPK.h \
  liboctave/numeric/DASRT.h \
  liboctave/numeric/DASSL.h \
  liboctave/numeric/DET.h \
  liboctave/numeric/EIG.h \
  liboctave/numeric/gsvd.h \
  liboctave/numeric/LSODE.h \
  liboctave/numeric/ODE.h \
  liboctave/numeric/ODEFunc.h \
  liboctave/numeric/ODES.h \
  liboctave/numeric/ODESFunc.h \
  liboctave/numeric/Quad.h \
  liboctave/numeric/aepbalance.h \
  liboctave/numeric/base-dae.h \
  liboctave/numeric/base-de.h \
  liboctave/numeric/base-min.h \
  liboctave/numeric/bsxfun-decl.h \
  liboctave/numeric/bsxfun.h \
  liboctave/numeric/chol.h \
  liboctave/numeric/eigs-base.h \
  liboctave/numeric/fEIG.h \
  liboctave/numeric/gepbalance.h \
  liboctave/numeric/hess.h \
  liboctave/numeric/lo-amos-proto.h \
  liboctave/numeric/lo-arpack-proto.h \
  liboctave/numeric/lo-blas-proto.h \
  liboctave/numeric/lo-fftpack-proto.h \
  liboctave/numeric/lo-lapack-proto.h \
  liboctave/numeric/lo-mappers.h \
  liboctave/numeric/lo-qrupdate-proto.h \
  liboctave/numeric/lo-ranlib-proto.h \
  liboctave/numeric/lo-slatec-proto.h \
  liboctave/numeric/lo-specfun.h \
  liboctave/numeric/lu.h \
  liboctave/numeric/oct-convn.h \
  liboctave/numeric/oct-fftw.h \
  liboctave/numeric/oct-norm.h \
  liboctave/numeric/oct-rand.h \
  liboctave/numeric/oct-spparms.h \
  liboctave/numeric/qr.h \
  liboctave/numeric/qrp.h \
  liboctave/numeric/randgamma.h \
  liboctave/numeric/randmtzig.h \
  liboctave/numeric/randpoisson.h \
  liboctave/numeric/schur.h \
  liboctave/numeric/sparse-chol.h \
  liboctave/numeric/sparse-dmsolve.h \
  liboctave/numeric/sparse-lu.h \
  liboctave/numeric/sparse-qr.h \
  liboctave/numeric/svd.h

NUMERIC_SRC = \
  liboctave/numeric/CollocWt.cc \
  liboctave/numeric/DASPK.cc \
  liboctave/numeric/DASRT.cc \
  liboctave/numeric/DASSL.cc \
  liboctave/numeric/EIG.cc \
  liboctave/numeric/gsvd.cc \
  liboctave/numeric/LSODE.cc \
  liboctave/numeric/ODES.cc \
  liboctave/numeric/Quad.cc \
  liboctave/numeric/aepbalance.cc \
  liboctave/numeric/chol.cc \
  liboctave/numeric/eigs-base.cc \
  liboctave/numeric/fEIG.cc \
  liboctave/numeric/gepbalance.cc \
  liboctave/numeric/hess.cc \
  liboctave/numeric/lo-mappers.cc \
  liboctave/numeric/lo-specfun.cc \
  liboctave/numeric/lu.cc \
  liboctave/numeric/oct-convn.cc \
  liboctave/numeric/oct-fftw.cc \
  liboctave/numeric/oct-norm.cc \
  liboctave/numeric/oct-rand.cc \
  liboctave/numeric/oct-spparms.cc \
  liboctave/numeric/qr.cc \
  liboctave/numeric/qrp.cc \
  liboctave/numeric/randgamma.cc \
  liboctave/numeric/randmtzig.cc \
  liboctave/numeric/randpoisson.cc \
  liboctave/numeric/schur.cc \
  liboctave/numeric/sparse-chol.cc \
  liboctave/numeric/sparse-dmsolve.cc \
  liboctave/numeric/sparse-lu.cc \
  liboctave/numeric/sparse-qr.cc \
  liboctave/numeric/svd.cc

LIBOCTAVE_TEMPLATE_SRC += \
  liboctave/numeric/bsxfun-defs.cc

## Special rules for sources which must be built before rest of compilation.
$(LIBOCTAVE_OPT_INC) : %.h : %.in
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/build-aux/mk-opts.pl --opt-class-header $< > $@-t && \
	mv $@-t $@

$(LIBOCTAVE_OPT_INC) : $(srcdir)/build-aux/mk-opts.pl

noinst_LTLIBRARIES += liboctave/numeric/libnumeric.la

liboctave_numeric_libnumeric_la_SOURCES = $(NUMERIC_SRC)

liboctave_numeric_libnumeric_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS) \
  -I$(srcdir)/liboctave/cruft/Faddeeva \
  $(FFTW_XCPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_numeric_libnumeric_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_numeric_libnumeric_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/numeric/libnumeric.la

liboctave_EXTRA_DIST += $(LIBOCTAVE_OPT_IN)

liboctave_CLEANFILES += \
  $(LIBOCTAVE_OPT_INC)
