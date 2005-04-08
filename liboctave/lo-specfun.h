/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

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
class Range;

#if !defined (HAVE_ACOSH)
extern double acosh (double);
#endif

#if !defined (HAVE_ASINH)
extern double asinh (double);
#endif

#if !defined (HAVE_ATANH)
extern double atanh (double);
#endif

#if !defined (HAVE_ERF)
extern double erf (double);
#endif

#if !defined (HAVE_ERFC)
extern double erfc (double);
#endif

extern double xgamma (double x);
extern double xlgamma (double x);

extern Complex
besselj (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern Complex
bessely (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern Complex
besseli (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern Complex
besselk (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern Complex
besselh1 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern Complex
besselh2 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern ComplexMatrix
besselj (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
bessely (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besseli (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselk (double alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh1 (double alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh2 (double alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselj (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
bessely (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besseli (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselk (const Matrix& alpha, const Complex& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh1 (const Matrix& alpha, const Complex& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh2 (const Matrix& alpha, const Complex& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselj (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
bessely (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besseli (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselk (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh1 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh2 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern ComplexNDArray
besselj (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
bessely (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besseli (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselk (double alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselh1 (double alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselh2 (double alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselj (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
bessely (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besseli (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselk (const NDArray& alpha, const Complex& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselh1 (const NDArray& alpha, const Complex& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselh2 (const NDArray& alpha, const Complex& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselj (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
bessely (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besseli (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselk (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	 ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselh1 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
besselh2 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
	  ArrayN<octave_idx_type>& ierr);

extern ComplexMatrix
besselj (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
bessely (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besseli (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselk (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	 Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh1 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern ComplexMatrix
besselh2 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
	  Array2<octave_idx_type>& ierr);

extern Complex airy (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);
extern Complex biry (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern ComplexMatrix
airy (const ComplexMatrix& z, bool deriv, bool scaled, Array2<octave_idx_type>& ierr);

extern ComplexMatrix
biry (const ComplexMatrix& z, bool deriv, bool scaled, Array2<octave_idx_type>& ierr);

extern ComplexNDArray
airy (const ComplexNDArray& z, bool deriv, bool scaled, ArrayN<octave_idx_type>& ierr);

extern ComplexNDArray
biry (const ComplexNDArray& z, bool deriv, bool scaled, ArrayN<octave_idx_type>& ierr);

extern double betainc (double x, double a, double b);
extern Matrix betainc (double x, double a, const Matrix& b);
extern Matrix betainc (double x, const Matrix& a, double b);
extern Matrix betainc (double x, const Matrix& a, const Matrix& b);

extern NDArray betainc (double x, double a, const NDArray& b);
extern NDArray betainc (double x, const NDArray& a, double b);
extern NDArray betainc (double x, const NDArray& a, const NDArray& b);

extern Matrix betainc (const Matrix& x, double a, double b);
extern Matrix betainc (const Matrix& x, double a, const Matrix& b);
extern Matrix betainc (const Matrix& x, const Matrix& a, double b);
extern Matrix betainc (const Matrix& x, const Matrix& a, const Matrix& b);

extern NDArray betainc (const NDArray& x, double a, double b);
extern NDArray betainc (const NDArray& x, double a, const NDArray& b);
extern NDArray betainc (const NDArray& x, const NDArray& a, double b);
extern NDArray betainc (const NDArray& x, const NDArray& a, const NDArray& b);

extern double gammainc (double x, double a, bool& err);
extern Matrix gammainc (double x, const Matrix& a);
extern Matrix gammainc (const Matrix& x, double a);
extern Matrix gammainc (const Matrix& x, const Matrix& a);

extern NDArray gammainc (double x, const NDArray& a);
extern NDArray gammainc (const NDArray& x, double a);
extern NDArray gammainc (const NDArray& x, const NDArray& a);

inline double gammainc (double x, double a)
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
