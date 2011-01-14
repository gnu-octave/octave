# Makefile for octave's liboctave directory
#
# Copyright (C) 1993-2011 John W. Eaton
#
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

include $(top_srcdir)/common.mk

AM_CPPFLAGS = \
  @CPPFLAGS@ -I../libgnu -I$(top_srcdir)/libgnu \
  -I$(top_srcdir)/libcruft/misc

EXTRA_DIST = \
  ChangeLog \
  config-ops.sh \
  mk-ops.awk \
  mx-op-inc.mk \
  mx-op-src.mk \
  mx-ops \
  smx-op-inc.mk \
  smx-op-src.mk \
  sparse-mk-ops.awk \
  sparse-mx-ops \
  vx-op-inc.mk \
  vx-op-src.mk \
  vx-ops \
  $(OPT_IN)

MATRIX_INC = \
  Array-util.h \
  Array.h \
  Array2.h \
  Array3.h \
  ArrayN.h \
  CColVector.h \
  CDiagMatrix.h \
  CMatrix.h \
  CNDArray.h \
  CRowVector.h \
  CSparse.h \
  CmplxAEPBAL.h \
  CmplxCHOL.h \
  CmplxGEPBAL.h \
  CmplxHESS.h \
  CmplxLU.h \
  CmplxQR.h \
  CmplxQRP.h \
  CmplxSCHUR.h \
  CmplxSVD.h \
  DET.h \
  DiagArray2.h \
  EIG.h \
  MArray-decl.h \
  MArray-defs.h \
  MArray.h \
  MArray2.h \
  MArrayN.h \
  MDiagArray2.h \
  MSparse-defs.h \
  MSparse.h \
  Matrix.h \
  MatrixType.h \
  PermMatrix.h \
  Sparse-diag-op-defs.h \
  Sparse-op-defs.h \
  Sparse-perm-op-defs.h \
  Sparse.h \
  SparseCmplxCHOL.h \
  SparseCmplxLU.h \
  SparseCmplxQR.h \
  SparseQR.h \
  SparsedbleCHOL.h \
  SparsedbleLU.h \
  base-aepbal.h \
  base-lu.h \
  base-qr.h \
  boolMatrix.h \
  boolNDArray.h \
  boolSparse.h \
  bsxfun-decl.h \
  chMatrix.h \
  chNDArray.h \
  dColVector.h \
  dDiagMatrix.h \
  dMatrix.h \
  dNDArray.h \
  dRowVector.h \
  dSparse.h \
  dbleAEPBAL.h \
  dbleCHOL.h \
  dbleGEPBAL.h \
  dbleHESS.h \
  dbleLU.h \
  dbleQR.h \
  dbleQRP.h \
  dbleSCHUR.h \
  dbleSVD.h \
  dim-vector.h \
  fCColVector.h \
  fCDiagMatrix.h \
  fCMatrix.h \
  fCNDArray.h \
  fCRowVector.h \
  fCmplxAEPBAL.h \
  fCmplxCHOL.h \
  fCmplxGEPBAL.h \
  fCmplxHESS.h \
  fCmplxLU.h \
  fCmplxQR.h \
  fCmplxQRP.h \
  fCmplxSCHUR.h \
  fCmplxSVD.h \
  fColVector.h \
  fDiagMatrix.h \
  fEIG.h \
  fMatrix.h \
  fNDArray.h \
  fRowVector.h \
  floatAEPBAL.h \
  floatCHOL.h \
  floatGEPBAL.h \
  floatHESS.h \
  floatLU.h \
  floatQR.h \
  floatQRP.h \
  floatSCHUR.h \
  floatSVD.h \
  int16NDArray.h \
  int32NDArray.h \
  int64NDArray.h \
  int8NDArray.h \
  intNDArray.h \
  mx-base.h \
  mx-defs.h \
  mx-ext.h \
  mx-op-decl.h \
  mx-op-defs.h \
  sparse-base-chol.h \
  sparse-base-lu.h \
  uint16NDArray.h \
  uint32NDArray.h \
  uint64NDArray.h \
  uint8NDArray.h

OPT_IN = \
  DASPK-opts.in \
  DASRT-opts.in \
  DASSL-opts.in \
  LSODE-opts.in \
  Quad-opts.in

OPT_INC = \
  DASPK-opts.h \
  DASRT-opts.h \
  DASSL-opts.h \
  LSODE-opts.h \
  Quad-opts.h

INCS = \
  CollocWt.h \
  DAE.h \
  DAEFunc.h \
  DAERT.h \
  DAERTFunc.h \
  DASPK.h \
  DASRT.h \
  DASSL.h \
  LSODE.h \
  ODE.h \
  ODEFunc.h \
  ODES.h \
  ODESFunc.h \
  Quad.h \
  Range.h \
  base-dae.h \
  base-de.h \
  base-min.h \
  byte-swap.h \
  caseless-str.h \
  cmd-edit.h \
  cmd-hist.h \
  data-conv.h \
  dir-ops.h \
  file-ops.h \
  file-stat.h \
  functor.h \
  glob-match.h \
  idx-vector.h \
  lo-array-gripes.h \
  lo-cutils.h \
  lo-ieee.h \
  lo-macros.h \
  lo-mappers.h \
  lo-math.h \
  lo-specfun.h \
  lo-sysdep.h \
  lo-traits.h \
  lo-utils.h \
  mach-info.h \
  oct-alloc.h \
  oct-binmap.h \
  oct-cmplx.h \
  oct-convn.h \
  oct-env.h \
  oct-fftw.h \
  oct-glob.h \
  oct-group.h \
  oct-inttypes.h \
  oct-locbuf.h \
  oct-md5.h \
  oct-mem.h \
  oct-mutex.h \
  oct-norm.h \
  oct-openmp.h \
  oct-passwd.h \
  oct-rand.h \
  oct-rl-edit.h \
  oct-rl-hist.h \
  oct-shlib.h \
  oct-sort.h \
  oct-sparse.h \
  oct-spparms.h \
  oct-syscalls.h \
  oct-time.h \
  oct-uname.h \
  pathlen.h \
  pathsearch.h \
  randgamma.h \
  randmtzig.h \
  randpoisson.h \
  regex-match.h \
  sparse-sort.h \
  sparse-util.h \
  statdefs.h \
  str-vec.h \
  sun-utils.h \
  sysdir.h \
  syswait.h \
  $(MATRIX_INC)

PRIVATE_INCS = \
  oct-glob.h

OTHER_INC = \
  intNDArray.cc \
  kpse.cc \
  mx-inlines.cc

include vx-op-inc.mk
include mx-op-inc.mk
include smx-op-inc.mk

$(srcdir)/vx-op-inc.mk: $(srcdir)/config-ops.sh $(srcdir)/mk-ops.awk $(srcdir)/vx-ops
	$(srcdir)/config-ops.sh $(top_srcdir) vx inc

$(srcdir)/mx-op-inc.mk: $(srcdir)/config-ops.sh $(srcdir)/mk-ops.awk $(srcdir)/mx-ops
	$(srcdir)/config-ops.sh $(top_srcdir) mx inc

$(srcdir)/smx-op-inc.mk: $(srcdir)/config-ops.sh $(srcdir)/sparse-mk-ops.awk $(srcdir)/sparse-mx-ops
	$(srcdir)/config-ops.sh $(top_srcdir) smx inc

BUILT_INCS = \
  mx-ops.h \
  $(OPT_INC) \
  $(MX_OP_INC) \
  $(VX_OP_INC) \
  $(SMX_OP_INC)

BUILT_SOURCES = $(BUILT_INCS)

TEMPLATE_SRC = \
  Array.cc \
  DiagArray2.cc \
  MArray.cc \
  MDiagArray2.cc \
  base-lu.cc \
  base-qr.cc \
  bsxfun-defs.cc \
  eigs-base.cc \
  oct-sort.cc \
  sparse-base-chol.cc \
  sparse-base-lu.cc \
  sparse-dmsolve.cc

TI_SRC = \
  Array-C.cc \
  Array-b.cc \
  Array-ch.cc \
  Array-d.cc \
  Array-f.cc \
  Array-fC.cc \
  Array-i.cc \
  Array-idx-vec.cc \
  Array-s.cc \
  Array-str.cc \
  Array-voidp.cc \
  MArray-C.cc \
  MArray-d.cc \
  MArray-f.cc \
  MArray-fC.cc \
  MArray-i.cc \
  MArray-s.cc \
  MSparse-C.cc \
  MSparse-d.cc \
  Sparse-C.cc \
  Sparse-b.cc \
  Sparse-d.cc \
  oct-inttypes.cc

MATRIX_SRC = \
  Array-util.cc \
  CColVector.cc \
  CDiagMatrix.cc \
  CMatrix.cc \
  CNDArray.cc \
  CRowVector.cc \
  CSparse.cc \
  CmplxAEPBAL.cc \
  CmplxCHOL.cc \
  CmplxGEPBAL.cc \
  CmplxHESS.cc \
  CmplxLU.cc \
  CmplxQR.cc \
  CmplxQRP.cc \
  CmplxSCHUR.cc \
  CmplxSVD.cc \
  EIG.cc \
  MSparse.cc \
  MatrixType.cc \
  PermMatrix.cc \
  Sparse.cc \
  SparseCmplxCHOL.cc \
  SparseCmplxLU.cc \
  SparseCmplxQR.cc \
  SparseQR.cc \
  SparsedbleCHOL.cc \
  SparsedbleLU.cc \
  boolMatrix.cc \
  boolNDArray.cc \
  boolSparse.cc \
  chMatrix.cc \
  chNDArray.cc \
  dim-vector.cc \
  dColVector.cc \
  dDiagMatrix.cc \
  dMatrix.cc \
  dNDArray.cc \
  dRowVector.cc \
  dSparse.cc \
  dbleAEPBAL.cc \
  dbleCHOL.cc \
  dbleGEPBAL.cc \
  dbleHESS.cc \
  dbleLU.cc \
  dbleQR.cc \
  dbleQRP.cc \
  dbleSCHUR.cc \
  dbleSVD.cc \
  fCColVector.cc \
  fCDiagMatrix.cc \
  fCMatrix.cc \
  fCNDArray.cc \
  fCRowVector.cc \
  fCmplxAEPBAL.cc \
  fCmplxCHOL.cc \
  fCmplxGEPBAL.cc \
  fCmplxHESS.cc \
  fCmplxLU.cc \
  fCmplxQR.cc \
  fCmplxQRP.cc \
  fCmplxSCHUR.cc \
  fCmplxSVD.cc \
  fColVector.cc \
  fDiagMatrix.cc \
  fEIG.cc \
  fMatrix.cc \
  fNDArray.cc \
  fRowVector.cc \
  floatAEPBAL.cc \
  floatCHOL.cc \
  floatGEPBAL.cc \
  floatHESS.cc \
  floatLU.cc \
  floatQR.cc \
  floatQRP.cc \
  floatSCHUR.cc \
  floatSVD.cc \
  int16NDArray.cc \
  int32NDArray.cc \
  int64NDArray.cc \
  int8NDArray.cc \
  uint16NDArray.cc \
  uint32NDArray.cc \
  uint64NDArray.cc \
  uint8NDArray.cc

octlib_LTLIBRARIES = liboctave.la

LIBOCTAVE_CXX_SOURCES = \
  CollocWt.cc \
  DASPK.cc \
  DASRT.cc \
  DASSL.cc \
  LSODE.cc \
  ODES.cc \
  Quad.cc \
  Range.cc \
  data-conv.cc \
  dir-ops.cc \
  file-ops.cc \
  file-stat.cc \
  glob-match.cc \
  idx-vector.cc \
  lo-array-gripes.cc \
  lo-ieee.cc \
  lo-mappers.cc \
  lo-specfun.cc \
  lo-sysdep.cc \
  lo-utils.cc \
  mach-info.cc \
  oct-alloc.cc \
  oct-convn.cc \
  oct-env.cc \
  oct-fftw.cc \
  oct-glob.cc \
  oct-group.cc \
  oct-locbuf.cc \
  oct-md5.cc \
  oct-mutex.cc \
  oct-norm.cc \
  oct-passwd.cc \
  oct-rand.cc \
  oct-shlib.cc \
  oct-spparms.cc \
  oct-syscalls.cc \
  oct-time.cc \
  oct-uname.cc \
  pathsearch.cc \
  regex-match.cc \
  sparse-sort.cc \
  sparse-util.cc \
  str-vec.cc \
  $(TI_SRC) \
  $(MATRIX_SRC)

include vx-op-src.mk
include mx-op-src.mk
include smx-op-src.mk

$(srcdir)/vx-op-src.mk: $(srcdir)/config-ops.sh $(srcdir)/mk-ops.awk $(srcdir)/vx-ops
	$(srcdir)/config-ops.sh $(top_srcdir) vx src

$(srcdir)/mx-op-src.mk: $(srcdir)/config-ops.sh $(srcdir)/mk-ops.awk $(srcdir)/mx-ops
	$(srcdir)/config-ops.sh $(top_srcdir) mx src

$(srcdir)/smx-op-src.mk: $(srcdir)/config-ops.sh $(srcdir)/sparse-mk-ops.awk $(srcdir)/sparse-mx-ops
	$(srcdir)/config-ops.sh $(top_srcdir) smx src

BUILT_LIBOCTAVE_CXX_SOURCES = \
  $(MX_OP_SRC) \
  $(VX_OP_SRC) \
  $(SMX_OP_SRC)

LIBOCTAVE_C_SOURCES = \
  f2c-main.c \
  lo-cieee.c \
  lo-cutils.c \
  randgamma.c \
  randmtzig.c \
  randpoisson.c \
  tempnam.c \
  tempname.c

LIBOCT_READLINE_CXX_SOURCES = cmd-edit.cc cmd-hist.cc

LIBOCT_READLINE_C_SOURCES = oct-rl-edit.c oct-rl-hist.c

LIBOCT_READLINE_SOURCES = $(LIBOCT_READLINE_CXX_SOURCES) $(LIBOCT_READLINE_C_SOURCES)

LINK_DEPS = \
  $(RLD_FLAG) \
  ../libcruft/libcruft.la \
  ../libcruft/libranlib.la \
  ../libgnu/libgnu.la \
  $(SPARSE_XLIBS) \
  $(ARPACK_LIBS) \
  $(QRUPDATE_LIBS) \
  $(FFTW_XLIBS) \
  $(LAPACK_LIBS) $(BLAS_LIBS) \
  $(READLINE_LIBS) $(TERM_LIBS) \
  $(LIBGLOB) $(REGEX_LIBS) $(DL_LIBS) \
  $(FLIBS) \
  $(PTHREAD_LIBS) $(LIBS)

liboctave_la_SOURCES = \
  $(LIBOCTAVE_CXX_SOURCES) \
  $(LIBOCTAVE_C_SOURCES) \
  $(LIBOCTAVE_SOURCES) \
  $(LIBOCT_READLINE_SOURCES) \
  $(LIBOCT_PATHSEARCH_SOURCES)

nodist_liboctave_la_SOURCES = \
  $(BUILT_LIBOCTAVE_CXX_SOURCES)

liboctave_la_LIBADD = $(LINK_DEPS)

liboctave_la_CPPFLAGS = \
  @OCTAVE_DLL_DEFS@ \
  $(SPARSE_XCPPFLAGS) \
  $(FFTW_XCPPFLAGS) \
  $(ARPACK_CPPFLAGS) \
  $(AM_CPPFLAGS)

liboctave_la_LDFLAGS = -release $(version) $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(SPARSE_XLDFLAGS) \
  $(ARPACK_LDFLAGS) \
  $(QRUPDATE_LDFLAGS) \
  $(FFTW_XLDFLAGS)

octinclude_HEADERS = \
  $(INCS) \
  $(OTHER_INC) \
  $(TEMPLATE_SRC) \
  $(EXTRA_HEADERS)

nodist_octinclude_HEADERS = \
  $(BUILT_INCS)

$(OPT_INC) : %.h : %.in $(top_srcdir)/mk-opts.pl
	@echo making $@ from $<
	@$(PERL) $(top_srcdir)/mk-opts.pl --opt-class-header $< > $@-t
	mv $@-t $@

$(VX_OP_INC) $(VX_OP_SRC) : $(srcdir)/mk-ops.awk vx-ops
	$(AWK) -f $(srcdir)/mk-ops.awk prefix=vx $(srcdir)/vx-ops

$(MX_OP_INC) $(MX_OP_SRC) : $(srcdir)/mk-ops.awk mx-ops
	$(AWK) -f $(srcdir)/mk-ops.awk prefix=mx $(srcdir)/mx-ops

$(SMX_OP_INC) $(SMX_OP_SRC) : $(srcdir)/sparse-mk-ops.awk sparse-mx-ops
	$(AWK) -f $(srcdir)/sparse-mk-ops.awk prefix=smx $(srcdir)/sparse-mx-ops

mx-ops.h : $(srcdir)/mk-ops.awk mx-ops
	$(AWK) -f $(srcdir)/mk-ops.awk prefix=mx make_inclusive_header=mx-ops.h $(srcdir)/mx-ops > $@-t
	mv $@-t $@

DISTCLEANFILES = $(BUILT_INCS) $(BUILT_LIBOCTAVE_CXX_SOURCES)

