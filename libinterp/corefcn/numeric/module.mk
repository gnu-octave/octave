## Options functions for Fortran packages like LSODE, DASPK.
## These are generated automagically by configure and Perl.
OPT_HANDLERS = \
  %reldir%/DASPK-opts.cc \
  %reldir%/DASRT-opts.cc \
  %reldir%/DASSL-opts.cc \
  %reldir%/LSODE-opts.cc \
  %reldir%/Quad-opts.cc

$(OPT_HANDLERS): %reldir%/%.cc : liboctave/numeric/%.in | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/build-aux/mk-opts.pl --opt-handler-fcns $< > $@-t && \
	mv $@-t $@

$(OPT_HANDLERS): $(srcdir)/build-aux/mk-opts.pl

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

COREFCN_NUMERIC_SRC = \
  %reldir%/__betainc__.cc \
  %reldir%/__contourc__.cc \
  %reldir%/__dsearchn__.cc \
  %reldir%/__eigs__.cc \
  %reldir%/__expint__.cc \
  %reldir%/__gammainc__.cc \
  %reldir%/__ichol__.cc \
  %reldir%/__ilu__.cc \
  %reldir%/__isprimelarge__.cc \
  %reldir%/__lin_interpn__.cc \
  %reldir%/__pchip_deriv__.cc \
  %reldir%/__qp__.cc \
  %reldir%/amd.cc \
  %reldir%/balance.cc \
  %reldir%/besselj.cc \
  %reldir%/bitfcns.cc \
  %reldir%/ccolamd.cc \
  %reldir%/chol.cc \
  %reldir%/colamd.cc \
  %reldir%/colloc.cc \
  %reldir%/conv2.cc \
  %reldir%/daspk.cc \
  %reldir%/dasrt.cc \
  %reldir%/dassl.cc \
  %reldir%/det.cc \
  %reldir%/dmperm.cc \
  %reldir%/dot.cc \
  %reldir%/eig.cc \
  %reldir%/ellipj.cc \
  %reldir%/fft.cc \
  %reldir%/fft2.cc \
  %reldir%/fftn.cc \
  %reldir%/filter.cc \
  %reldir%/gcd.cc \
  %reldir%/givens.cc \
  %reldir%/gsvd.cc \
  %reldir%/hash.cc \
  %reldir%/hess.cc \
  %reldir%/hex2num.cc \
  %reldir%/inv.cc \
  %reldir%/kron.cc \
  %reldir%/lsode.cc \
  %reldir%/lu.cc \
  %reldir%/mappers.cc \
  %reldir%/max.cc \
  %reldir%/mgorth.cc \
  %reldir%/ordqz.cc \
  %reldir%/ordschur.cc \
  %reldir%/perms.cc \
  %reldir%/pinv.cc \
  %reldir%/pow2.cc \
  %reldir%/psi.cc \
  %reldir%/qr.cc \
  %reldir%/quad.cc \
  %reldir%/quadcc.cc \
  %reldir%/qz.cc \
  %reldir%/rand.cc \
  %reldir%/rcond.cc \
  %reldir%/schur.cc \
  %reldir%/sparse.cc \
  %reldir%/spparms.cc \
  %reldir%/sqrtm.cc \
  %reldir%/stream-euler.cc \
  %reldir%/svd.cc \
  %reldir%/sylvester.cc \
  %reldir%/symbfact.cc \
  %reldir%/symrcm.cc \
  %reldir%/trexc.cc \
  %reldir%/tsearch.cc

noinst_LTLIBRARIES += \
  %reldir%/libnumeric.la

%canon_reldir%_libnumeric_la_SOURCES = $(COREFCN_NUMERIC_SRC)

%canon_reldir%_libnumeric_la_CPPFLAGS = \
  $(libinterp_liboctinterp_la_CPPFLAGS) \
  -I%reldir% -I$(srcdir)/%reldir% \
  $(FFTW_XCPPFLAGS) \
  $(SPARSE_XCPPFLAGS)
