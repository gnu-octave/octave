LIBOCTAVE_OPT_INC = \
  liboctave/numeric/DASPK-opts.h \
  liboctave/numeric/DASRT-opts.h \
  liboctave/numeric/DASSL-opts.h \
  liboctave/numeric/LSODE-opts.h \
  liboctave/numeric/Quad-opts.h

LIBOCTAVE_OPT_IN = $(LIBOCTAVE_OPT_INC:.h=.in)

NUMERIC_INC = \
  liboctave/numeric/CmplxAEPBAL.h \
  liboctave/numeric/CmplxCHOL.h \
  liboctave/numeric/CmplxGEPBAL.h \
  liboctave/numeric/CmplxHESS.h \
  liboctave/numeric/CmplxLU.h \
  liboctave/numeric/CmplxQR.h \
  liboctave/numeric/CmplxQRP.h \
  liboctave/numeric/CmplxSCHUR.h \
  liboctave/numeric/CmplxSVD.h \
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
  liboctave/numeric/LSODE.h \
  liboctave/numeric/ODE.h \
  liboctave/numeric/ODEFunc.h \
  liboctave/numeric/ODES.h \
  liboctave/numeric/ODESFunc.h \
  liboctave/numeric/Quad.h \
  liboctave/numeric/base-aepbal.h \
  liboctave/numeric/base-dae.h \
  liboctave/numeric/base-de.h \
  liboctave/numeric/base-lu.h \
  liboctave/numeric/base-min.h \
  liboctave/numeric/base-qr.h \
  liboctave/numeric/bsxfun-decl.h \
  liboctave/numeric/bsxfun.h \
  liboctave/numeric/dbleAEPBAL.h \
  liboctave/numeric/dbleCHOL.h \
  liboctave/numeric/dbleGEPBAL.h \
  liboctave/numeric/dbleHESS.h \
  liboctave/numeric/dbleLU.h \
  liboctave/numeric/dbleQR.h \
  liboctave/numeric/dbleQRP.h \
  liboctave/numeric/dbleSCHUR.h \
  liboctave/numeric/dbleSVD.h \
  liboctave/numeric/eigs-base.h \
  liboctave/numeric/fCmplxAEPBAL.h \
  liboctave/numeric/fCmplxCHOL.h \
  liboctave/numeric/fCmplxGEPBAL.h \
  liboctave/numeric/fCmplxHESS.h \
  liboctave/numeric/fCmplxLU.h \
  liboctave/numeric/fCmplxQR.h \
  liboctave/numeric/fCmplxQRP.h \
  liboctave/numeric/fCmplxSCHUR.h \
  liboctave/numeric/fCmplxSVD.h \
  liboctave/numeric/fEIG.h \
  liboctave/numeric/floatAEPBAL.h \
  liboctave/numeric/floatCHOL.h \
  liboctave/numeric/floatGEPBAL.h \
  liboctave/numeric/floatHESS.h \
  liboctave/numeric/floatLU.h \
  liboctave/numeric/floatQR.h \
  liboctave/numeric/floatQRP.h \
  liboctave/numeric/floatSCHUR.h \
  liboctave/numeric/floatSVD.h \
  liboctave/numeric/lo-mappers.h \
  liboctave/numeric/lo-specfun.h \
  liboctave/numeric/oct-convn.h \
  liboctave/numeric/oct-fftw.h \
  liboctave/numeric/oct-norm.h \
  liboctave/numeric/oct-rand.h \
  liboctave/numeric/oct-spparms.h \
  liboctave/numeric/randgamma.h \
  liboctave/numeric/randmtzig.h \
  liboctave/numeric/randpoisson.h \
  liboctave/numeric/sparse-chol.h \
  liboctave/numeric/sparse-dmsolve.h \
  liboctave/numeric/sparse-lu.h \
  liboctave/numeric/sparse-qr.h

NUMERIC_C_SRC = \
  liboctave/numeric/randgamma.c \
  liboctave/numeric/randmtzig.c \
  liboctave/numeric/randpoisson.c

NUMERIC_SRC = \
  liboctave/numeric/CmplxAEPBAL.cc \
  liboctave/numeric/CmplxCHOL.cc \
  liboctave/numeric/CmplxGEPBAL.cc \
  liboctave/numeric/CmplxHESS.cc \
  liboctave/numeric/CmplxLU.cc \
  liboctave/numeric/CmplxQR.cc \
  liboctave/numeric/CmplxQRP.cc \
  liboctave/numeric/CmplxSCHUR.cc \
  liboctave/numeric/CmplxSVD.cc \
  liboctave/numeric/CollocWt.cc \
  liboctave/numeric/DASPK.cc \
  liboctave/numeric/DASRT.cc \
  liboctave/numeric/DASSL.cc \
  liboctave/numeric/EIG.cc \
  liboctave/numeric/LSODE.cc \
  liboctave/numeric/ODES.cc \
  liboctave/numeric/Quad.cc \
  liboctave/numeric/dbleAEPBAL.cc \
  liboctave/numeric/dbleCHOL.cc \
  liboctave/numeric/dbleGEPBAL.cc \
  liboctave/numeric/dbleHESS.cc \
  liboctave/numeric/dbleLU.cc \
  liboctave/numeric/dbleQR.cc \
  liboctave/numeric/dbleQRP.cc \
  liboctave/numeric/dbleSCHUR.cc \
  liboctave/numeric/dbleSVD.cc \
  liboctave/numeric/eigs-base.cc \
  liboctave/numeric/fCmplxAEPBAL.cc \
  liboctave/numeric/fCmplxCHOL.cc \
  liboctave/numeric/fCmplxGEPBAL.cc \
  liboctave/numeric/fCmplxHESS.cc \
  liboctave/numeric/fCmplxLU.cc \
  liboctave/numeric/fCmplxQR.cc \
  liboctave/numeric/fCmplxQRP.cc \
  liboctave/numeric/fCmplxSCHUR.cc \
  liboctave/numeric/fCmplxSVD.cc \
  liboctave/numeric/fEIG.cc \
  liboctave/numeric/floatAEPBAL.cc \
  liboctave/numeric/floatCHOL.cc \
  liboctave/numeric/floatGEPBAL.cc \
  liboctave/numeric/floatHESS.cc \
  liboctave/numeric/floatLU.cc \
  liboctave/numeric/floatQR.cc \
  liboctave/numeric/floatQRP.cc \
  liboctave/numeric/floatSCHUR.cc \
  liboctave/numeric/floatSVD.cc \
  liboctave/numeric/lo-mappers.cc \
  liboctave/numeric/lo-specfun.cc \
  liboctave/numeric/oct-convn.cc \
  liboctave/numeric/oct-fftw.cc \
  liboctave/numeric/oct-norm.cc \
  liboctave/numeric/oct-rand.cc \
  liboctave/numeric/oct-spparms.cc \
  liboctave/numeric/sparse-chol.cc \
  liboctave/numeric/sparse-dmsolve.cc \
  liboctave/numeric/sparse-lu.cc \
  liboctave/numeric/sparse-qr.cc \
  $(NUMERIC_C_SRC)

LIBOCTAVE_TEMPLATE_SRC += \
  liboctave/numeric/base-lu.cc \
  liboctave/numeric/base-qr.cc \
  liboctave/numeric/bsxfun-defs.cc

## Special rules for sources which must be built before rest of compilation.
$(LIBOCTAVE_OPT_INC) : %.h : %.in
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(top_srcdir)/build-aux/mk-opts.pl --opt-class-header $< > $@-t && \
	mv $@-t $@

$(LIBOCTAVE_OPT_INC) : $(top_srcdir)/build-aux/mk-opts.pl

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

