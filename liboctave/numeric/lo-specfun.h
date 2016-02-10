/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#if ! defined (octave_lo_specfun_h)
#define octave_lo_specfun_h 1

#include "octave-config.h"

#include "oct-cmplx.h"
#include "Array.h"

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

extern OCTAVE_API double xacosh (double);
extern OCTAVE_API float xacosh (float);
extern OCTAVE_API Complex xacosh (const Complex& x);
extern OCTAVE_API FloatComplex xacosh (const FloatComplex& x);

extern OCTAVE_API double xasinh (double);
extern OCTAVE_API float xasinh (float);
extern OCTAVE_API Complex xasinh (const Complex& x);
extern OCTAVE_API FloatComplex xasinh (const FloatComplex& x);

extern OCTAVE_API double xatanh (double);
extern OCTAVE_API float xatanh (float);
extern OCTAVE_API Complex xatanh (const Complex& x);
extern OCTAVE_API FloatComplex xatanh (const FloatComplex& x);

extern OCTAVE_API double xerf (double);
extern OCTAVE_API float xerf (float);
extern OCTAVE_API Complex xerf (const Complex& x);
extern OCTAVE_API FloatComplex xerf (const FloatComplex& x);

extern OCTAVE_API double xerfc (double);
extern OCTAVE_API float xerfc (float);
extern OCTAVE_API Complex xerfc (const Complex& x);
extern OCTAVE_API FloatComplex xerfc (const FloatComplex& x);

extern OCTAVE_API double xexpm1 (double x);
extern OCTAVE_API Complex xexpm1 (const Complex& x);

extern OCTAVE_API float xexpm1 (float x);
extern OCTAVE_API FloatComplex xexpm1 (const FloatComplex& x);

extern OCTAVE_API double xlog1p (double x);
extern OCTAVE_API Complex xlog1p (const Complex& x);

extern OCTAVE_API float xlog1p (float x);
extern OCTAVE_API FloatComplex xlog1p (const FloatComplex& x);

extern OCTAVE_API double xcbrt (double x);
extern OCTAVE_API float xcbrt (float x);

extern OCTAVE_API double xgamma (double x);
extern OCTAVE_API double xlgamma (double x);
extern OCTAVE_API Complex rc_lgamma (double x);

extern OCTAVE_API float xgamma (float x);
extern OCTAVE_API float xlgamma (float x);
extern OCTAVE_API FloatComplex rc_lgamma (float x);

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
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (double alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (double alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (double alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (double alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (double alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const Matrix& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const Matrix& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (double alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (double alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (const NDArray& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (const NDArray& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplex
besselj (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
bessely (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besseli (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselk (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselh1 (float alpha, const FloatComplex& x, bool scaled,
          octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselh2 (float alpha, const FloatComplex& x, bool scaled,
          octave_idx_type& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (float alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (float alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (float alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (float alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
          bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
          bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API Complex
airy (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
biry (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API ComplexMatrix
airy (const ComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
biry (const ComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
airy (const ComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
biry (const ComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplex
airy (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
biry (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplexMatrix
airy (const FloatComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
biry (const FloatComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
airy (const FloatComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
biry (const FloatComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API double
betainc (double x, double a, double b);
extern OCTAVE_API Array<double>
betainc (double x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betainc (double x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betainc (double x, const Array<double>& a, const Array<double>& b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, const Array<double>& a,
         const Array<double>& b);

extern OCTAVE_API float
betainc (float x, float a, float b);
extern OCTAVE_API Array<float>
betainc (float x, float a, const Array<float>& b);
extern OCTAVE_API Array<float>
betainc (float x, const Array<float>& a, float b);
extern OCTAVE_API Array<float>
betainc (float x, const Array<float>& a, const Array<float>& b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, float a, float b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, float a, float b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, float a, const Array<float>& b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, const Array<float>& a, float b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, const Array<float>& a, const Array<float>& b);

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
extern OCTAVE_API FloatMatrix
gammainc (const FloatMatrix& x, const FloatMatrix& a);

extern OCTAVE_API FloatNDArray gammainc (float x, const FloatNDArray& a);
extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x, float a);
extern OCTAVE_API FloatNDArray
gammainc (const FloatNDArray& x, const FloatNDArray& a);

inline float gammainc (float x, float a)
{
  bool err;
  return gammainc (x, a, err);
}

extern OCTAVE_API Complex rc_log1p (double);
extern OCTAVE_API FloatComplex rc_log1p (float);

extern OCTAVE_API double erfinv (double x);
extern OCTAVE_API float erfinv (float x);

extern OCTAVE_API double erfcinv (double x);
extern OCTAVE_API float erfcinv (float x);

extern OCTAVE_API float erfcx (float x);
extern OCTAVE_API double erfcx (double x);
extern OCTAVE_API Complex erfcx (const Complex& x);
extern OCTAVE_API FloatComplex erfcx (const FloatComplex& x);

extern OCTAVE_API float erfi (float x);
extern OCTAVE_API double erfi (double x);
extern OCTAVE_API Complex erfi (const Complex& x);
extern OCTAVE_API FloatComplex erfi (const FloatComplex& x);

extern OCTAVE_API float dawson (float x);
extern OCTAVE_API double dawson (double x);
extern OCTAVE_API Complex dawson (const Complex& x);
extern OCTAVE_API FloatComplex dawson (const FloatComplex& x);

extern OCTAVE_API double betaincinv (double x, double a, double b);
extern OCTAVE_API Array<double>
betaincinv (double x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betaincinv (double x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betaincinv (double x, const Array<double>& a, const Array<double>& b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, const Array<double>& a,
            const Array<double>& b);

extern OCTAVE_API void
ellipj (double u, double m, double& sn, double& cn, double& dn, double& err);
extern OCTAVE_API void
ellipj (const Complex& u, double m, Complex& sn, Complex& cn, Complex& dn,
        double& err);

//! Digamma function.
//!
//! Only defined for double and float.
template <typename T>
extern OCTAVE_API T psi (const T& z);

//! Digamma function for complex input.
//!
//! Only defined for double and float.
template <typename T>
extern OCTAVE_API std::complex<T> psi (const std::complex<T>& z);

//! Polygamma function.
//!
//! Only defined for double and float.
//! @param n must be non-negative.  If zero, the digamma function is computed.
//! @param z must be real and non-negative.
template <typename T>
extern OCTAVE_API T psi (const octave_idx_type n, const T z);

#endif
