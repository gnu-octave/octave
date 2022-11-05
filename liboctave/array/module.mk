ARRAY_INC = \
  %reldir%/Array-fwd.h \
  %reldir%/Array-util.h \
  %reldir%/Array.h \
  %reldir%/CColVector.h \
  %reldir%/CDiagMatrix.h \
  %reldir%/CMatrix.h \
  %reldir%/CNDArray.h \
  %reldir%/CRowVector.h \
  %reldir%/CSparse.h \
  %reldir%/DiagArray2.h \
  %reldir%/MArray-fwd.h \
  %reldir%/MArray.h \
  %reldir%/MDiagArray2.h \
  %reldir%/MSparse.h \
  %reldir%/Matrix.h \
  %reldir%/MatrixType.h \
  %reldir%/PermMatrix.h \
  %reldir%/Range.h \
  %reldir%/Sparse-fwd.h \
  %reldir%/Sparse.h \
  %reldir%/boolMatrix.h \
  %reldir%/boolNDArray.h \
  %reldir%/boolSparse.h \
  %reldir%/chMatrix.h \
  %reldir%/chNDArray.h \
  %reldir%/dColVector.h \
  %reldir%/dDiagMatrix.h \
  %reldir%/dMatrix.h \
  %reldir%/dNDArray.h \
  %reldir%/dRowVector.h \
  %reldir%/dSparse.h \
  %reldir%/dim-vector.h \
  %reldir%/fCColVector.h \
  %reldir%/fCDiagMatrix.h \
  %reldir%/fCMatrix.h \
  %reldir%/fCNDArray.h \
  %reldir%/fCRowVector.h \
  %reldir%/fColVector.h \
  %reldir%/fDiagMatrix.h \
  %reldir%/fMatrix.h \
  %reldir%/fNDArray.h \
  %reldir%/fRowVector.h \
  %reldir%/idx-vector.h \
  %reldir%/int16NDArray.h \
  %reldir%/int32NDArray.h \
  %reldir%/int64NDArray.h \
  %reldir%/int8NDArray.h \
  %reldir%/intNDArray-fwd.h \
  %reldir%/intNDArray.h \
  %reldir%/mx-fwd.h \
  %reldir%/range-fwd.h \
  %reldir%/uint16NDArray.h \
  %reldir%/uint32NDArray.h \
  %reldir%/uint64NDArray.h \
  %reldir%/uint8NDArray.h

ARRAY_SRC = \
  %reldir%/Array-C.cc \
  %reldir%/Array-b.cc \
  %reldir%/Array-ch.cc \
  %reldir%/Array-d.cc \
  %reldir%/Array-f.cc \
  %reldir%/Array-fC.cc \
  %reldir%/Array-i.cc \
  %reldir%/Array-idx-vec.cc \
  %reldir%/Array-s.cc \
  %reldir%/Array-str.cc \
  %reldir%/Array-util.cc \
  %reldir%/Array-voidp.cc \
  %reldir%/CColVector.cc \
  %reldir%/CDiagMatrix.cc \
  %reldir%/CMatrix.cc \
  %reldir%/CNDArray.cc \
  %reldir%/CRowVector.cc \
  %reldir%/CSparse.cc \
  %reldir%/MArray-C.cc \
  %reldir%/MArray-d.cc \
  %reldir%/MArray-f.cc \
  %reldir%/MArray-fC.cc \
  %reldir%/MArray-i.cc \
  %reldir%/MArray-s.cc \
  %reldir%/MSparse-C.cc \
  %reldir%/MSparse-d.cc \
  %reldir%/MatrixType.cc \
  %reldir%/PermMatrix.cc \
  %reldir%/Range.cc \
  %reldir%/Sparse-C.cc \
  %reldir%/Sparse-b.cc \
  %reldir%/Sparse-d.cc \
  %reldir%/boolMatrix.cc \
  %reldir%/boolNDArray.cc \
  %reldir%/boolSparse.cc \
  %reldir%/chMatrix.cc \
  %reldir%/chNDArray.cc \
  %reldir%/dColVector.cc \
  %reldir%/dDiagMatrix.cc \
  %reldir%/dMatrix.cc \
  %reldir%/dNDArray.cc \
  %reldir%/dRowVector.cc \
  %reldir%/dSparse.cc \
  %reldir%/dim-vector.cc \
  %reldir%/fCColVector.cc \
  %reldir%/fCDiagMatrix.cc \
  %reldir%/fCMatrix.cc \
  %reldir%/fCNDArray.cc \
  %reldir%/fCRowVector.cc \
  %reldir%/fColVector.cc \
  %reldir%/fDiagMatrix.cc \
  %reldir%/fMatrix.cc \
  %reldir%/fNDArray.cc \
  %reldir%/fRowVector.cc \
  %reldir%/idx-vector.cc \
  %reldir%/int16NDArray.cc \
  %reldir%/int32NDArray.cc \
  %reldir%/int64NDArray.cc \
  %reldir%/int8NDArray.cc \
  %reldir%/uint16NDArray.cc \
  %reldir%/uint32NDArray.cc \
  %reldir%/uint64NDArray.cc \
  %reldir%/uint8NDArray.cc

LIBOCTAVE_TEMPLATE_SRC += \
  %reldir%/Array-base.cc \
  %reldir%/Array-oct.cc \
  %reldir%/DiagArray2.cc \
  %reldir%/MArray.cc \
  %reldir%/MDiagArray2.cc \
  %reldir%/MSparse.cc \
  %reldir%/Sparse.cc \
  %reldir%/intNDArray.cc

noinst_LTLIBRARIES += %reldir%/libarray.la

%canon_reldir%_libarray_la_SOURCES = $(ARRAY_SRC)

%canon_reldir%_libarray_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS) \
  $(FFTW_XCPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_liboctave_la_LIBADD += %reldir%/libarray.la
