/*

Copyright (C) 1996, 1997, 1998, 2002, 2004, 2005, 2006, 2007, 2008
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_liboctave_specfun_h)
#define octave_liboctave_specfun_h 1

#include "oct-cmplx.h"
#include "oct-types.h"
#include "ArrayN.h"

template <class T> class Array2;
class Matrix;
class ComplexMatrix;
class NDArray;
class ComplexNDArray;
class RowVector;
class ComplexColumnVector;
class FloatMatrix;
class FloatComplexMatrix;
class FloatNDArray;
class FloatComplexNDArray;
class FloatRowVector;
class FloatComplexColumnVector;
class Range;

#if !defined (HAVE_ACOSH)
extern OCTAVE_API double acosh (double);
#endif

#if !defined (HAVE_ASINH)
extern OCTAVE_API double asinh (double);
#endif

#if !defined (HAVE_ATANH)
extern OCTAVE_API double atanh (double);
#endif

#if !defined (HAVE_ERF)
extern OCTAVE_API double erf (double);
#endif

#if !defined (HAVE_ERFC)
extern OCTAVE_API double erfc (double);
#endif

#if !defined (HAVE_ACOSHF)
extern OCTAVE_API float acoshf (float);
#endif

#if !defined (HAVE_ASINHF)
extern OCTAVE_API float asinhf (float);
#endif

#if !defined (HAVE_ATANHF)
extern OCTAVE_API float atanhf (float);
#endif

#if !defined (HAVE_ERFF)
extern OCTAVE_API float erff (float);
#endif

#if !defined (HAVE_ERFCF)
extern OCTAVE_API float erfcf (float);
#endif

#if !defined (HAVE_EXPM1)
extern OCTAVE_API double expm1 (double x);
#endif
extern OCTAVE_API Complex expm1 (const Complex& x);

#if !defined (HAVE_EXPM1F)
extern OCTAVE_API float expm1f (float x);
#endif
extern OCTAVE_API FloatComplex expm1f (const FloatComplex& x);

#if !defined (HAVE_LOG1P)
extern OCTAVE_API double log1p (double x);
#endif
extern OCTAVE_API Complex log1p (const Complex& x);

#if !defined (HAVE_LOG1PF)
extern OCTAVE_API float log1pf (float x);
#endif
extern OCTAVE_API FloatComplex log1pf (const FloatComplex& x);

extern OCTAVE_API double xgamma (double x);
extern OCTAVE_API double xlgamma (double x);
extern OCTAVE_API Complex xlgamma (const Complex& x);

extern OCTAVE_API float xgamma (float x);
extern OCTAVE_API float xlgamma (float x);
extern OCTAVE_API FloatComplex xlgamma (const FloatComplex& x);

extern OCTAVE_API Complex
besselj (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
bessely (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besseli (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besselk (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besselh1 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besselh2 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API ComplexMatrix
besselj (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (double alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (double alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const Matrix& alpha, const Complex& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const Matrix& alpha, const Complex& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (double alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (double alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (const NDArray& alpha, const Complex& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (const NDArray& alpha, const Complex& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplex
besselj (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
bessely (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besseli (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselk (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselh1 (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselh2 (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (float alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (float alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (float alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (float alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (float alpha, const FloatComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (float alpha, const FloatComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (float alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (float alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (float alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (float alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (float alpha, const FloatComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (float alpha, const FloatComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern OCTAVE_API Complex airy (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);
extern OCTAVE_API Complex biry (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API ComplexMatrix
airy (const ComplexMatrix& z, bool deriv, bool scaled, Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
biry (const ComplexMatrix& z, bool deriv, bool scaled, Array2<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
airy (const ComplexNDArray& z, bool deriv, bool scaled, ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
biry (const ComplexNDArray& z, bool deriv, bool scaled, ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplex airy (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr);
extern OCTAVE_API FloatComplex biry (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplexMatrix
airy (const FloatComplexMatrix& z, bool deriv, bool scaled, Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
biry (const FloatComplexMatrix& z, bool deriv, bool scaled, Array2<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
airy (const FloatComplexNDArray& z, bool deriv, bool scaled, ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
biry (const FloatComplexNDArray& z, bool deriv, bool scaled, ArrayN<octave_idx_type>& ierr);

extern OCTAVE_API double betainc (double x, double a, double b);
extern OCTAVE_API Matrix betainc (double x, double a, const Matrix& b);
extern OCTAVE_API Matrix betainc (double x, const Matrix& a, double b);
extern OCTAVE_API Matrix betainc (double x, const Matrix& a, const Matrix& b);

extern OCTAVE_API NDArray betainc (double x, double a, const NDArray& b);
extern OCTAVE_API NDArray betainc (double x, const NDArray& a, double b);
extern OCTAVE_API NDArray betainc (double x, const NDArray& a, const NDArray& b);

extern OCTAVE_API Matrix betainc (const Matrix& x, double a, double b);
extern OCTAVE_API Matrix betainc (const Matrix& x, double a, const Matrix& b);
extern OCTAVE_API Matrix betainc (const Matrix& x, const Matrix& a, double b);
extern OCTAVE_API Matrix betainc (const Matrix& x, const Matrix& a, const Matrix& b);

extern OCTAVE_API NDArray betainc (const NDArray& x, double a, double b);
extern OCTAVE_API NDArray betainc (const NDArray& x, double a, const NDArray& b);
extern OCTAVE_API NDArray betainc (const NDArray& x, const NDArray& a, double b);
extern OCTAVE_API NDArray betainc (const NDArray& x, const NDArray& a, const NDArray& b);

extern OCTAVE_API float betainc (float x, float a, float b);
extern OCTAVE_API FloatMatrix betainc (float x, float a, const FloatMatrix& b);
extern OCTAVE_API FloatMatrix betainc (float x, const FloatMatrix& a, float b);
extern OCTAVE_API FloatMatrix betainc (float x, const FloatMatrix& a, const FloatMatrix& b);

extern OCTAVE_API FloatNDArray betainc (float x, float a, const FloatNDArray& b);
extern OCTAVE_API FloatNDArray betainc (float x, const FloatNDArray& a, float b);
extern OCTAVE_API FloatNDArray betainc (float x, const FloatNDArray& a, const FloatNDArray& b);

extern OCTAVE_API FloatMatrix betainc (const FloatMatrix& x, float a, float b);
extern OCTAVE_API FloatMatrix betainc (const FloatMatrix& x, float a, const FloatMatrix& b);
extern OCTAVE_API FloatMatrix betainc (const FloatMatrix& x, const FloatMatrix& a, float b);
extern OCTAVE_API FloatMatrix betainc (const FloatMatrix& x, const FloatMatrix& a, const FloatMatrix& b);

extern OCTAVE_API FloatNDArray betainc (const FloatNDArray& x, float a, float b);
extern OCTAVE_API FloatNDArray betainc (const FloatNDArray& x, float a, const FloatNDArray& b);
extern OCTAVE_API FloatNDArray betainc (const FloatNDArray& x, const FloatNDArray& a, float b);
extern OCTAVE_API FloatNDArray betainc (const FloatNDArray& x, const FloatNDArray& a, const FloatNDArray& b);

extern OCTAVE_API double gammainc (double x, double a, bool& err);
extern OCTAVE_API Matrix gammainc (double x, const Matrix& a);
extern OCTAVE_API Matrix gammainc (const Matrix& x, double a);
extern OCTAVE_API Matrix gammainc (const Matrix& x, const Matrix& a);

extern OCTAVE_API NDArray gammainc (double x, const NDArray& a);
extern OCTAVE_API NDArray gammainc (const NDArray& x, double a);
extern OCTAVE_API NDArray gammainc (const NDArray& x, const NDArray& a);

inline double gammainc (double x, double a)
{
  bool err;
  return gammainc (x, a, err);
}

extern OCTAVE_API float gammainc (float x, float a, bool& err);
extern OCTAVE_API FloatMatrix gammainc (float x, const FloatMatrix& a);
extern OCTAVE_API FloatMatrix gammainc (const FloatMatrix& x, float a);
extern OCTAVE_API FloatMatrix gammainc (const FloatMatrix& x, const FloatMatrix& a);

extern OCTAVE_API FloatNDArray gammainc (float x, const FloatNDArray& a);
extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x, float a);
extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x, const FloatNDArray& a);

inline float gammainc (float x, float a)
{
  bool err;
  return gammainc (x, a, err);
}

#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
