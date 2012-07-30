EXTRA_DIST += \
  corefcn/module.mk

COREFCN_SRC = \
  corefcn/__contourc__.cc \
  corefcn/__dispatch__.cc \
  corefcn/__lin_interpn__.cc \
  corefcn/__pchip_deriv__.cc \
  corefcn/__qp__.cc \
  corefcn/balance.cc \
  corefcn/besselj.cc \
  corefcn/betainc.cc \
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
  corefcn/spparms.cc \
  corefcn/sqrtm.cc \
  corefcn/str2double.cc \
  corefcn/strfind.cc \
  corefcn/sub2ind.cc \
  corefcn/svd.cc \
  corefcn/syl.cc \
  corefcn/time.cc \
  corefcn/tril.cc \
  corefcn/typecast.cc

noinst_LTLIBRARIES += corefcn/libcorefcn.la

corefcn_libcorefcn_la_SOURCES = $(COREFCN_SRC)

