LIBOCTAVE_OPT_INC = \
  %reldir%/DASPK-opts.h \
  %reldir%/DASRT-opts.h \
  %reldir%/DASSL-opts.h \
  %reldir%/LSODE-opts.h \
  %reldir%/Quad-opts.h

LIBOCTAVE_OPT_IN = $(LIBOCTAVE_OPT_INC:.h=.in)

NUMERIC_INC = \
  %reldir%/CollocWt.h \
  %reldir%/DAE.h \
  %reldir%/DAEFunc.h \
  %reldir%/DAERT.h \
  %reldir%/DAERTFunc.h \
  %reldir%/DASPK.h \
  %reldir%/DASRT.h \
  %reldir%/DASSL.h \
  %reldir%/DET.h \
  %reldir%/EIG.h \
  %reldir%/gsvd.h \
  %reldir%/LSODE.h \
  %reldir%/ODE.h \
  %reldir%/ODEFunc.h \
  %reldir%/ODES.h \
  %reldir%/ODESFunc.h \
  %reldir%/Quad.h \
  %reldir%/aepbalance.h \
  %reldir%/base-dae.h \
  %reldir%/base-de.h \
  %reldir%/bsxfun-decl.h \
  %reldir%/bsxfun.h \
  %reldir%/chol.h \
  %reldir%/eigs-base.h \
  %reldir%/fEIG.h \
  %reldir%/gepbalance.h \
  %reldir%/hess.h \
  %reldir%/lo-amos-proto.h \
  %reldir%/lo-arpack-proto.h \
  %reldir%/lo-blas-proto.h \
  %reldir%/lo-lapack-proto.h \
  %reldir%/lo-mappers.h \
  %reldir%/lo-qrupdate-proto.h \
  %reldir%/lo-ranlib-proto.h \
  %reldir%/lo-slatec-proto.h \
  %reldir%/lo-specfun.h \
  %reldir%/lu.h \
  %reldir%/oct-convn.h \
  %reldir%/oct-fftw.h \
  %reldir%/oct-norm.h \
  %reldir%/oct-rand.h \
  %reldir%/oct-spparms.h \
  %reldir%/qr.h \
  %reldir%/qrp.h \
  %reldir%/randgamma.h \
  %reldir%/randmtzig.h \
  %reldir%/randpoisson.h \
  %reldir%/schur.h \
  %reldir%/sparse-chol.h \
  %reldir%/sparse-dmsolve.h \
  %reldir%/sparse-lu.h \
  %reldir%/sparse-qr.h \
  %reldir%/svd.h

NUMERIC_SRC = \
  %reldir%/CollocWt.cc \
  %reldir%/DASPK.cc \
  %reldir%/DASRT.cc \
  %reldir%/DASSL.cc \
  %reldir%/EIG.cc \
  %reldir%/gsvd.cc \
  %reldir%/LSODE.cc \
  %reldir%/ODES.cc \
  %reldir%/Quad.cc \
  %reldir%/aepbalance.cc \
  %reldir%/chol.cc \
  %reldir%/eigs-base.cc \
  %reldir%/fEIG.cc \
  %reldir%/gepbalance.cc \
  %reldir%/hess.cc \
  %reldir%/lo-mappers.cc \
  %reldir%/lo-specfun.cc \
  %reldir%/lu.cc \
  %reldir%/oct-convn.cc \
  %reldir%/oct-fftw.cc \
  %reldir%/oct-norm.cc \
  %reldir%/oct-rand.cc \
  %reldir%/oct-spparms.cc \
  %reldir%/qr.cc \
  %reldir%/qrp.cc \
  %reldir%/randgamma.cc \
  %reldir%/randmtzig.cc \
  %reldir%/randpoisson.cc \
  %reldir%/schur.cc \
  %reldir%/sparse-chol.cc \
  %reldir%/sparse-dmsolve.cc \
  %reldir%/sparse-lu.cc \
  %reldir%/sparse-qr.cc \
  %reldir%/svd.cc

LIBOCTAVE_TEMPLATE_SRC += \
  %reldir%/bsxfun-defs.cc

## Special rules for sources which must be built before rest of compilation.
$(LIBOCTAVE_OPT_INC) : %.h : %.in
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/build-aux/mk-opts.pl --opt-class-header $< > $@-t && \
	mv $@-t $@

$(LIBOCTAVE_OPT_INC) : $(srcdir)/build-aux/mk-opts.pl

noinst_LTLIBRARIES += %reldir%/libnumeric.la

%canon_reldir%_libnumeric_la_SOURCES = $(NUMERIC_SRC)

%canon_reldir%_libnumeric_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS) \
  -I$(srcdir)/liboctave/external/Faddeeva \
  $(FFTW_XCPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_liboctave_la_LIBADD += %reldir%/libnumeric.la

liboctave_EXTRA_DIST += $(LIBOCTAVE_OPT_IN)

liboctave_CLEANFILES += \
  $(LIBOCTAVE_OPT_INC)
