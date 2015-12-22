ARRAY_INC = \
  liboctave/array/Array.h \
  liboctave/array/Array-util.h \
  liboctave/array/boolMatrix.h \
  liboctave/array/boolNDArray.h \
  liboctave/array/boolSparse.h \
  liboctave/array/CColVector.h \
  liboctave/array/CDiagMatrix.h \
  liboctave/array/chMatrix.h \
  liboctave/array/chNDArray.h \
  liboctave/array/CMatrix.h \
  liboctave/array/CNDArray.h \
  liboctave/array/CRowVector.h \
  liboctave/array/CSparse.h \
  liboctave/array/dColVector.h \
  liboctave/array/dDiagMatrix.h \
  liboctave/array/DiagArray2.h \
  liboctave/array/dim-vector.h \
  liboctave/array/dMatrix.h \
  liboctave/array/dNDArray.h \
  liboctave/array/dRowVector.h \
  liboctave/array/dSparse.h \
  liboctave/array/fCColVector.h \
  liboctave/array/fCDiagMatrix.h \
  liboctave/array/fCMatrix.h \
  liboctave/array/fCNDArray.h \
  liboctave/array/fColVector.h \
  liboctave/array/fCRowVector.h \
  liboctave/array/fDiagMatrix.h \
  liboctave/array/fMatrix.h \
  liboctave/array/fNDArray.h \
  liboctave/array/fRowVector.h \
  liboctave/array/idx-vector.h \
  liboctave/array/int16NDArray.h \
  liboctave/array/int32NDArray.h \
  liboctave/array/int64NDArray.h \
  liboctave/array/int8NDArray.h \
  liboctave/array/intNDArray.h \
  liboctave/array/MArray.h \
  liboctave/array/Matrix.h \
  liboctave/array/MatrixType.h \
  liboctave/array/MDiagArray2.h \
  liboctave/array/MSparse.h \
  liboctave/array/PermMatrix.h \
  liboctave/array/Range.h \
  liboctave/array/Sparse.h \
  liboctave/array/uint16NDArray.h \
  liboctave/array/uint32NDArray.h \
  liboctave/array/uint64NDArray.h \
  liboctave/array/uint8NDArray.h

ARRAY_SRC = \
  liboctave/array/Array-b.cc \
  liboctave/array/Array-C.cc \
  liboctave/array/Array-ch.cc \
  liboctave/array/Array-d.cc \
  liboctave/array/Array-f.cc \
  liboctave/array/Array-fC.cc \
  liboctave/array/Array-i.cc \
  liboctave/array/Array-idx-vec.cc \
  liboctave/array/Array-s.cc \
  liboctave/array/Array-str.cc \
  liboctave/array/Array-util.cc \
  liboctave/array/Array-voidp.cc \
  liboctave/array/boolMatrix.cc \
  liboctave/array/boolNDArray.cc \
  liboctave/array/boolSparse.cc \
  liboctave/array/CColVector.cc \
  liboctave/array/CDiagMatrix.cc \
  liboctave/array/chMatrix.cc \
  liboctave/array/chNDArray.cc \
  liboctave/array/CMatrix.cc \
  liboctave/array/CNDArray.cc \
  liboctave/array/CRowVector.cc \
  liboctave/array/CSparse.cc \
  liboctave/array/dColVector.cc \
  liboctave/array/dDiagMatrix.cc \
  liboctave/array/dim-vector.cc \
  liboctave/array/dMatrix.cc \
  liboctave/array/dNDArray.cc \
  liboctave/array/dRowVector.cc \
  liboctave/array/dSparse.cc \
  liboctave/array/fCColVector.cc \
  liboctave/array/fCDiagMatrix.cc \
  liboctave/array/fCMatrix.cc \
  liboctave/array/fCNDArray.cc \
  liboctave/array/fColVector.cc \
  liboctave/array/fCRowVector.cc \
  liboctave/array/fDiagMatrix.cc \
  liboctave/array/fMatrix.cc \
  liboctave/array/fNDArray.cc \
  liboctave/array/fRowVector.cc \
  liboctave/array/idx-vector.cc \
  liboctave/array/int16NDArray.cc \
  liboctave/array/int32NDArray.cc \
  liboctave/array/int64NDArray.cc \
  liboctave/array/int8NDArray.cc \
  liboctave/array/MArray-C.cc \
  liboctave/array/MArray-d.cc \
  liboctave/array/MArray-f.cc \
  liboctave/array/MArray-fC.cc \
  liboctave/array/MArray-i.cc \
  liboctave/array/MArray-s.cc \
  liboctave/array/MatrixType.cc \
  liboctave/array/MSparse-C.cc \
  liboctave/array/MSparse-d.cc \
  liboctave/array/PermMatrix.cc \
  liboctave/array/Range.cc \
  liboctave/array/Sparse-b.cc \
  liboctave/array/Sparse-C.cc \
  liboctave/array/Sparse-d.cc \
  liboctave/array/uint16NDArray.cc \
  liboctave/array/uint32NDArray.cc \
  liboctave/array/uint64NDArray.cc \
  liboctave/array/uint8NDArray.cc

LIBOCTAVE_TEMPLATE_SRC += \
  liboctave/array/Array.cc \
  liboctave/array/DiagArray2.cc \
  liboctave/array/intNDArray.cc \
  liboctave/array/MArray.cc \
  liboctave/array/MDiagArray2.cc \
  liboctave/array/MSparse.cc \
  liboctave/array/Sparse.cc

noinst_LTLIBRARIES += liboctave/array/libarray.la

liboctave_array_libarray_la_SOURCES = $(ARRAY_SRC)

liboctave_array_libarray_la_CPPFLAGS = \
  $(liboctave_liboctave_la_CPPFLAGS) \
  $(FFTW_XCPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_array_libarray_la_CFLAGS = $(liboctave_liboctave_la_CFLAGS)

liboctave_array_libarray_la_CXXFLAGS = $(liboctave_liboctave_la_CXXFLAGS)

liboctave_liboctave_la_LIBADD += liboctave/array/libarray.la
