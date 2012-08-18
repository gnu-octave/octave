EXTRA_DIST += \
  corefcn/module.mk

## Options functions for Fortran packages like LSODE, DASPK.
## These are generated automagically by configure and Perl.
OPT_HANDLERS = \
  corefcn/DASPK-opts.cc \
  corefcn/DASRT-opts.cc \
  corefcn/DASSL-opts.cc \
  corefcn/LSODE-opts.cc \
  corefcn/Quad-opts.cc

OPT_INC = \
  $(top_builddir)/liboctave/DASPK-opts.h \
  $(top_builddir)/liboctave/DASRT-opts.h \
  $(top_builddir)/liboctave/DASSL-opts.h \
  $(top_builddir)/liboctave/LSODE-opts.h \
  $(top_builddir)/liboctave/Quad-opts.h

$(OPT_HANDLERS): corefcn/%.cc : $(top_builddir)/liboctave/%.in
	$(PERL) $(top_srcdir)/build-aux/mk-opts.pl --opt-handler-fcns $< > $@-t
	mv $@-t $@

$(OPT_INC) : %.h : %.in
	$(MAKE) -C $(top_builddir)/liboctave $(@F)

COREFCN_SRC = \
  corefcn/__contourc__.cc \
  corefcn/__dispatch__.cc \
  corefcn/__lin_interpn__.cc \
  corefcn/__pchip_deriv__.cc \
  corefcn/__qp__.cc \
  corefcn/balance.cc \
  corefcn/besselj.cc \
  corefcn/betainc.cc \
  corefcn/bitfcns.cc \
  corefcn/bsxfun.cc \
  corefcn/cellfun.cc \
  corefcn/colloc.cc \
  corefcn/conv2.cc \
  corefcn/daspk.cc \
  corefcn/dasrt.cc \
  corefcn/dassl.cc \
  corefcn/det.cc \
  corefcn/dlmread.cc \
  corefcn/dot.cc \
  corefcn/eig.cc \
  corefcn/fft.cc \
  corefcn/fft2.cc \
  corefcn/fftn.cc \
  corefcn/filter.cc \
  corefcn/find.cc \
  corefcn/gammainc.cc \
  corefcn/gcd.cc \
  corefcn/getgrent.cc \
  corefcn/getpwent.cc \
  corefcn/getrusage.cc \
  corefcn/givens.cc \
  corefcn/hess.cc \
  corefcn/hex2num.cc \
  corefcn/inv.cc \
  corefcn/kron.cc \
  corefcn/lookup.cc \
  corefcn/lsode.cc \
  corefcn/lu.cc \
  corefcn/luinc.cc \
  corefcn/mappers.cc \
  corefcn/matrix_type.cc \
  corefcn/max.cc \
  corefcn/md5sum.cc \
  corefcn/mgorth.cc \
  corefcn/nproc.cc \
  corefcn/pinv.cc \
  corefcn/quad.cc \
  corefcn/quadcc.cc \
  corefcn/qz.cc \
  corefcn/rand.cc \
  corefcn/rcond.cc \
  corefcn/regexp.cc \
  corefcn/schur.cc \
  corefcn/sparse.cc \
  corefcn/spparms.cc \
  corefcn/sqrtm.cc \
  corefcn/str2double.cc \
  corefcn/strfind.cc \
  corefcn/strfns.cc \
  corefcn/sub2ind.cc \
  corefcn/svd.cc \
  corefcn/syl.cc \
  corefcn/syscalls.cc \
  corefcn/time.cc \
  corefcn/tril.cc \
  corefcn/typecast.cc

noinst_LTLIBRARIES += corefcn/libcorefcn.la

corefcn_libcorefcn_la_SOURCES = $(COREFCN_SRC)

