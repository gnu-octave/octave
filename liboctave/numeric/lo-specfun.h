/*

Copyright (C) 1996-2016 John W. Eaton
Copyright (C) 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

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

namespace octave
{
  namespace math
  {
    extern OCTAVE_API double acosh (double x);
    extern OCTAVE_API float acosh (float x);
    extern OCTAVE_API Complex acosh (const Complex& x);
    extern OCTAVE_API FloatComplex acosh (const FloatComplex& x);

    extern OCTAVE_API double asinh (double x);
    extern OCTAVE_API float asinh (float x);
    extern OCTAVE_API Complex asinh (const Complex& x);
    extern OCTAVE_API FloatComplex asinh (const FloatComplex& x);

    extern OCTAVE_API double atanh (double x);
    extern OCTAVE_API float atanh (float x);
    extern OCTAVE_API Complex atanh (const Complex& x);
    extern OCTAVE_API FloatComplex atanh (const FloatComplex& x);

    extern OCTAVE_API double erf (double x);
    extern OCTAVE_API float erf (float x);
    extern OCTAVE_API Complex erf (const Complex& x);
    extern OCTAVE_API FloatComplex erf (const FloatComplex& x);

    extern OCTAVE_API double erfc (double x);
    extern OCTAVE_API float erfc (float x);
    extern OCTAVE_API Complex erfc (const Complex& x);
    extern OCTAVE_API FloatComplex erfc (const FloatComplex& x);

    extern OCTAVE_API double expm1 (double x);
    extern OCTAVE_API Complex expm1 (const Complex& x);

    extern OCTAVE_API float expm1 (float x);
    extern OCTAVE_API FloatComplex expm1 (const FloatComplex& x);

    extern OCTAVE_API double log1p (double x);
    extern OCTAVE_API Complex log1p (const Complex& x);

    extern OCTAVE_API float log1p (float x);
    extern OCTAVE_API FloatComplex log1p (const FloatComplex& x);

    extern OCTAVE_API double cbrt (double x);
    extern OCTAVE_API float cbrt (float x);

    extern OCTAVE_API double gamma (double x);
    extern OCTAVE_API double lgamma (double x);
    extern OCTAVE_API Complex rc_lgamma (double x);

    extern OCTAVE_API float gamma (float x);
    extern OCTAVE_API float lgamma (float x);
    extern OCTAVE_API FloatComplex rc_lgamma (float x);

    extern OCTAVE_API Complex besselj (double alpha, const Complex& x, bool scaled,
                                       octave_idx_type& ierr);
    extern OCTAVE_API Complex bessely (double alpha, const Complex& x, bool scaled,
                                       octave_idx_type& ierr);
    extern OCTAVE_API Complex besseli (double alpha, const Complex& x, bool scaled,
                                       octave_idx_type& ierr);
    extern OCTAVE_API Complex besselk (double alpha, const Complex& x, bool scaled,
                                       octave_idx_type& ierr);
    extern OCTAVE_API Complex besselh1 (double alpha, const Complex& x, bool scaled,
                                        octave_idx_type& ierr);
    extern OCTAVE_API Complex besselh2 (double alpha, const Complex& x, bool scaled,
                                        octave_idx_type& ierr);

    extern OCTAVE_API ComplexMatrix besselj (double alpha, const ComplexMatrix& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix bessely (double alpha, const ComplexMatrix& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besseli (double alpha, const ComplexMatrix& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselk (double alpha, const ComplexMatrix& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh1 (double alpha, const ComplexMatrix& x,
                                              bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh2 (double alpha, const ComplexMatrix& x,
                                              bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API ComplexMatrix besselj (const Matrix& alpha, const Complex& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix bessely (const Matrix& alpha, const Complex& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besseli (const Matrix& alpha, const Complex& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselk (const Matrix& alpha, const Complex& x,
                                             bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh1 (const Matrix& alpha, const Complex& x,
                                              bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh2 (const Matrix& alpha, const Complex& x,
                                              bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API ComplexMatrix besselj (const Matrix& alpha,
                                             const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix bessely (const Matrix& alpha,
                                             const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besseli (const Matrix& alpha,
                                             const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselk (const Matrix& alpha,
                                             const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh1 (const Matrix& alpha,
                                              const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh2 (const Matrix& alpha,
                                              const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API ComplexNDArray besselj (double alpha, const ComplexNDArray& x,
                                              bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray bessely (double alpha, const ComplexNDArray& x,
                                              bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besseli (double alpha, const ComplexNDArray& x,
                                              bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselk (double alpha, const ComplexNDArray& x,
                                              bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselh1 (double alpha,
                                               const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselh2 (double alpha,
                                               const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API ComplexNDArray besselj (const NDArray& alpha,
                                              const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray bessely (const NDArray& alpha,
                                              const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besseli (const NDArray& alpha,
                                              const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselk (const NDArray& alpha,
                                              const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselh1 (const NDArray& alpha,
                                               const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselh2 (const NDArray& alpha,
                                               const Complex& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API ComplexNDArray besselj (const NDArray& alpha,
                                              const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray bessely (const NDArray& alpha,
                                              const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besseli (const NDArray& alpha,
                                              const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselk (const NDArray& alpha,
                                              const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselh1 (const NDArray& alpha,
                                               const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray besselh2 (const NDArray& alpha,
                                               const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API ComplexMatrix besselj (const RowVector& alpha,
                                             const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix bessely (const RowVector& alpha,
                                             const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besseli (const RowVector& alpha,
                                             const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselk (const RowVector& alpha,
                                             const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh1 (const RowVector& alpha,
                                              const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix besselh2 (const RowVector& alpha,
                                              const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplex besselj (float alpha, const FloatComplex& x,
                                            bool scaled, octave_idx_type& ierr);
    extern OCTAVE_API FloatComplex bessely (float alpha, const FloatComplex& x,
                                            bool scaled, octave_idx_type& ierr);
    extern OCTAVE_API FloatComplex besseli (float alpha, const FloatComplex& x,
                                            bool scaled, octave_idx_type& ierr);
    extern OCTAVE_API FloatComplex besselk (float alpha, const FloatComplex& x,
                                            bool scaled, octave_idx_type& ierr);
    extern OCTAVE_API FloatComplex besselh1 (float alpha, const FloatComplex& x,
                                             bool scaled, octave_idx_type& ierr);
    extern OCTAVE_API FloatComplex besselh2 (float alpha, const FloatComplex& x,
                                             bool scaled, octave_idx_type& ierr);

    extern OCTAVE_API FloatComplexMatrix besselj (float alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix bessely (float alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besseli (float alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselk (float alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh1 (float alpha,
                                                   const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh2 (float alpha,
                                                   const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplexMatrix besselj (const FloatMatrix& alpha,
                                                  const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix bessely (const FloatMatrix& alpha,
                                                  const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besseli (const FloatMatrix& alpha,
                                                  const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselk (const FloatMatrix& alpha,
                                                  const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh1 (const FloatMatrix& alpha,
                                                   const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh2 (const FloatMatrix& alpha,
                                                   const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplexMatrix besselj (const FloatMatrix& alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix bessely (const FloatMatrix& alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besseli (const FloatMatrix& alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselk (const FloatMatrix& alpha,
                                                  const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh1 (const FloatMatrix& alpha,
                                                   const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh2 (const FloatMatrix& alpha,
                                                   const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplexNDArray besselj (float alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray bessely (float alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besseli (float alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselk (float alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselh1 (float alpha,
                                                    const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselh2 (float alpha,
                                                    const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplexNDArray besselj (const FloatNDArray& alpha,
                                                   const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray bessely (const FloatNDArray& alpha,
                                                   const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besseli (const FloatNDArray& alpha,
                                                   const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselk (const FloatNDArray& alpha,
                                                   const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselh1 (const FloatNDArray& alpha,
                                                    const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselh2 (const FloatNDArray& alpha,
                                                    const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplexNDArray besselj (const FloatNDArray& alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray bessely (const FloatNDArray& alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besseli (const FloatNDArray& alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselk (const FloatNDArray& alpha,
                                                   const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselh1 (const FloatNDArray& alpha,
                                                    const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray besselh2 (const FloatNDArray& alpha,
                                                    const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplexMatrix besselj (const FloatRowVector& alpha,
                                                  const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix bessely (const FloatRowVector& alpha,
                                                  const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besseli (const FloatRowVector& alpha,
                                                  const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselk (const FloatRowVector& alpha,
                                                  const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh1 (const FloatRowVector& alpha,
                                                   const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix besselh2 (const FloatRowVector& alpha,
                                                   const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API Complex airy (const Complex& z, bool deriv, bool scaled,
                                    octave_idx_type& ierr);
    extern OCTAVE_API Complex biry (const Complex& z, bool deriv, bool scaled,
                                    octave_idx_type& ierr);

    extern OCTAVE_API ComplexMatrix airy (const ComplexMatrix& z, bool deriv,
                                          bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexMatrix biry (const ComplexMatrix& z, bool deriv,
                                          bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API ComplexNDArray airy (const ComplexNDArray& z, bool deriv,
                                           bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API ComplexNDArray biry (const ComplexNDArray& z, bool deriv,
                                           bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplex airy (const FloatComplex& z, bool deriv,
                                         bool scaled, octave_idx_type& ierr);
    extern OCTAVE_API FloatComplex biry (const FloatComplex& z, bool deriv,
                                         bool scaled, octave_idx_type& ierr);

    extern OCTAVE_API FloatComplexMatrix airy (const FloatComplexMatrix& z,
                                               bool deriv, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexMatrix biry (const FloatComplexMatrix& z,
                                               bool deriv, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API FloatComplexNDArray airy (const FloatComplexNDArray& z,
                                                bool deriv, bool scaled, Array<octave_idx_type>& ierr);
    extern OCTAVE_API FloatComplexNDArray biry (const FloatComplexNDArray& z,
                                                bool deriv, bool scaled, Array<octave_idx_type>& ierr);

    extern OCTAVE_API double betainc (double x, double a, double b);
    extern OCTAVE_API Array<double> betainc (double x, double a,
                                             const Array<double>& b);
    extern OCTAVE_API Array<double> betainc (double x, const Array<double>& a,
                                             double b);
    extern OCTAVE_API Array<double> betainc (double x, const Array<double>& a,
                                             const Array<double>& b);

    extern OCTAVE_API Array<double> betainc (const Array<double>& x, double a,
                                             double b);
    extern OCTAVE_API Array<double> betainc (const Array<double>& x, double a,
                                             const Array<double>& b);
    extern OCTAVE_API Array<double> betainc (const Array<double>& x,
                                             const Array<double>& a, double b);
    extern OCTAVE_API Array<double> betainc (const Array<double>& x,
                                             const Array<double>& a, const Array<double>& b);

    extern OCTAVE_API float betainc (float x, float a, float b);
    extern OCTAVE_API Array<float> betainc (float x, float a,
                                            const Array<float>& b);
    extern OCTAVE_API Array<float> betainc (float x, const Array<float>& a,
                                            float b);
    extern OCTAVE_API Array<float> betainc (float x, const Array<float>& a,
                                            const Array<float>& b);

    extern OCTAVE_API Array<float> betainc (const Array<float>& x, float a,
                                            float b);
    extern OCTAVE_API Array<float> betainc (const Array<float>& x, float a,
                                            const Array<float>& b);
    extern OCTAVE_API Array<float> betainc (const Array<float>& x,
                                            const Array<float>& a, float b);
    extern OCTAVE_API Array<float> betainc (const Array<float>& x,
                                            const Array<float>& a, const Array<float>& b);

    extern OCTAVE_API double gammainc (double x, double a, bool& err);
    inline double gammainc (double x, double a)
    {
      bool err;
      return gammainc (x, a, err);
    }

    extern OCTAVE_API Matrix gammainc (double x, const Matrix& a);
    extern OCTAVE_API Matrix gammainc (const Matrix& x, double a);
    extern OCTAVE_API Matrix gammainc (const Matrix& x, const Matrix& a);

    extern OCTAVE_API NDArray gammainc (double x, const NDArray& a);
    extern OCTAVE_API NDArray gammainc (const NDArray& x, double a);
    extern OCTAVE_API NDArray gammainc (const NDArray& x, const NDArray& a);

    extern OCTAVE_API float gammainc (float x, float a, bool& err);
    inline float gammainc (float x, float a)
    {
      bool err;
      return gammainc (x, a, err);
    }

    extern OCTAVE_API FloatMatrix gammainc (float x, const FloatMatrix& a);
    extern OCTAVE_API FloatMatrix gammainc (const FloatMatrix& x, float a);
    extern OCTAVE_API FloatMatrix gammainc (const FloatMatrix& x,
                                            const FloatMatrix& a);

    extern OCTAVE_API FloatNDArray gammainc (float x, const FloatNDArray& a);
    extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x, float a);
    extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x,
                                             const FloatNDArray& a);

    extern OCTAVE_API Complex rc_log1p (double x);
    extern OCTAVE_API FloatComplex rc_log1p (float x);

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
    extern OCTAVE_API Array<double> betaincinv (double x, double a,
                                                const Array<double>& b);
    extern OCTAVE_API Array<double> betaincinv (double x, const Array<double>& a,
                                                double b);
    extern OCTAVE_API Array<double> betaincinv (double x, const Array<double>& a,
                                                const Array<double>& b);

    extern OCTAVE_API Array<double> betaincinv (const Array<double>& x, double a,
                                                double b);
    extern OCTAVE_API Array<double> betaincinv (const Array<double>& x, double a,
                                                const Array<double>& b);
    extern OCTAVE_API Array<double> betaincinv (const Array<double>& x,
                                                const Array<double>& a, double b);
    extern OCTAVE_API Array<double> betaincinv (const Array<double>& x,
                                                const Array<double>& a, const Array<double>& b);

    extern OCTAVE_API void ellipj (double u, double m, double& sn, double& cn,
                                   double& dn, double& err);
    extern OCTAVE_API void ellipj (const Complex& u, double m, Complex& sn,
                                   Complex& cn, Complex& dn, double& err);

    extern OCTAVE_API double psi (double x);
    extern OCTAVE_API float psi (float x);

    extern OCTAVE_API Complex psi (const Complex& x);
    extern OCTAVE_API FloatComplex psi (const FloatComplex& x);

    extern OCTAVE_API double psi (octave_idx_type n, double z);
    extern OCTAVE_API float psi (octave_idx_type n, float z);
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::math::acosh' instead")
inline double xacosh (double x) { return octave::math::acosh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::acosh' instead")
inline float xacosh (float x) { return octave::math::acosh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::acosh' instead")
inline Complex xacosh (const Complex& x) { return octave::math::acosh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::acosh' instead")
inline FloatComplex xacosh (const FloatComplex& x) { return octave::math::acosh (x); }

OCTAVE_DEPRECATED ("use 'octave::math::asinh' instead")
inline double xasinh (double x) { return octave::math::asinh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::asinh' instead")
inline float xasinh (float x) { return octave::math::asinh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::asinh' instead")
inline Complex xasinh (const Complex& x) { return octave::math::asinh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::asinh' instead")
inline FloatComplex xasinh (const FloatComplex& x) { return octave::math::asinh (x); }

OCTAVE_DEPRECATED ("use 'octave::math::atanh' instead")
inline double xatanh (double x) { return octave::math::atanh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::atanh' instead")
inline float xatanh (float x) { return octave::math::atanh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::atanh' instead")
inline Complex xatanh (const Complex& x) { return octave::math::atanh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::atanh' instead")
inline FloatComplex xatanh (const FloatComplex& x) { return octave::math::atanh (x); }

OCTAVE_DEPRECATED ("use 'octave::math::erf' instead")
inline double xerf (double x) { return octave::math::erf (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erf' instead")
inline float xerf (float x) { return octave::math::erf (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erf' instead")
inline Complex xerf (const Complex& x) { return octave::math::erf (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erf' instead")
inline FloatComplex xerf (const FloatComplex& x) { return octave::math::erf (x); }

OCTAVE_DEPRECATED ("use 'octave::math::erfc' instead")
inline double xerfc (double x) { return octave::math::erfc (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfc' instead")
inline float xerfc (float x) { return octave::math::erfc (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfc' instead")
inline Complex xerfc (const Complex& x) { return octave::math::erfc (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfc' instead")
inline FloatComplex xerfc (const FloatComplex& x) { return octave::math::erfc (x); }

OCTAVE_DEPRECATED ("use 'octave::math::expm1' instead")
inline double xexpm1 (double x) { return octave::math::expm1 (x); }
OCTAVE_DEPRECATED ("use 'octave::math::expm1' instead")
inline Complex xexpm1 (const Complex& x) { return octave::math::expm1 (x); }

OCTAVE_DEPRECATED ("use 'octave::math::expm1' instead")
inline float xexpm1 (float x) { return octave::math::expm1 (x); }
OCTAVE_DEPRECATED ("use 'octave::math::expm1' instead")
inline FloatComplex xexpm1 (const FloatComplex& x) { return octave::math::expm1 (x); }

OCTAVE_DEPRECATED ("use 'octave::math::log1p' instead")
inline double xlog1p (double x) { return octave::math::log1p (x); }
OCTAVE_DEPRECATED ("use 'octave::math::log1p' instead")
inline Complex xlog1p (const Complex& x) { return octave::math::log1p (x); }

OCTAVE_DEPRECATED ("use 'octave::math::log1p' instead")
inline float xlog1p (float x) { return octave::math::log1p (x); }
OCTAVE_DEPRECATED ("use 'octave::math::log1p' instead")
inline FloatComplex xlog1p (const FloatComplex& x) { return octave::math::log1p (x); }

OCTAVE_DEPRECATED ("use 'octave::math::cbrt' instead")
inline double xcbrt (double x) { return octave::math::cbrt (x); }
OCTAVE_DEPRECATED ("use 'octave::math::cbrt' instead")
inline float xcbrt (float x) { return octave::math::cbrt (x); }

OCTAVE_DEPRECATED ("use 'octave::math::gamma' instead")
inline double xgamma (double x) { return octave::math::gamma (x); }
OCTAVE_DEPRECATED ("use 'octave::math::lgamma' instead")
inline double xlgamma (double x) { return octave::math::lgamma (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_lgamma' instead")
inline Complex rc_lgamma (double x) { return octave::math::rc_lgamma (x); }

OCTAVE_DEPRECATED ("use 'octave::math::gamma' instead")
inline float xgamma (float x) { return octave::math::gamma (x); }
OCTAVE_DEPRECATED ("use 'octave::math::lgamma' instead")
inline float xlgamma (float x) { return octave::math::lgamma (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_lgamma' instead")
inline FloatComplex rc_lgamma (float x) { return octave::math::rc_lgamma (x); }

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
inline Complex besselj (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselj (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
inline Complex bessely (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr) { return octave::math::bessely (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
inline Complex besseli (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besseli (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
inline Complex besselk (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselk (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
inline Complex besselh1 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselh1 (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
inline Complex besselh2 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselh2 (alpha, x, scaled, ierr); }

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API ComplexMatrix besselj (double alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API ComplexMatrix bessely (double alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API ComplexMatrix besseli (double alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API ComplexMatrix besselk (double alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API ComplexMatrix besselh1 (double alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API ComplexMatrix besselh2 (double alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API ComplexMatrix besselj (const Matrix& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API ComplexMatrix bessely (const Matrix& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API ComplexMatrix besseli (const Matrix& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API ComplexMatrix besselk (const Matrix& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API ComplexMatrix besselh1 (const Matrix& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API ComplexMatrix besselh2 (const Matrix& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API ComplexMatrix besselj (const Matrix& alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API ComplexMatrix bessely (const Matrix& alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API ComplexMatrix besseli (const Matrix& alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API ComplexMatrix besselk (const Matrix& alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API ComplexMatrix besselh1 (const Matrix& alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API ComplexMatrix besselh2 (const Matrix& alpha, const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API ComplexNDArray besselj (double alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API ComplexNDArray bessely (double alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API ComplexNDArray besseli (double alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API ComplexNDArray besselk (double alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API ComplexNDArray besselh1 (double alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API ComplexNDArray besselh2 (double alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API ComplexNDArray besselj (const NDArray& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API ComplexNDArray bessely (const NDArray& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API ComplexNDArray besseli (const NDArray& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API ComplexNDArray besselk (const NDArray& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API ComplexNDArray besselh1 (const NDArray& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API ComplexNDArray besselh2 (const NDArray& alpha, const Complex& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API ComplexNDArray besselj (const NDArray& alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API ComplexNDArray bessely (const NDArray& alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API ComplexNDArray besseli (const NDArray& alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API ComplexNDArray besselk (const NDArray& alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API ComplexNDArray besselh1 (const NDArray& alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API ComplexNDArray besselh2 (const NDArray& alpha, const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API ComplexMatrix besselj (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API ComplexMatrix bessely (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API ComplexMatrix besseli (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API ComplexMatrix besselk (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API ComplexMatrix besselh1 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API ComplexMatrix besselh2 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
inline FloatComplex besselj (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselj (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octavh::bessely' instead")
inline FloatComplex bessely (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr) { return octave::math::bessely (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octavh::besseli' instead")
inline FloatComplex besseli (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besseli (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octavh::besselk' instead")
inline FloatComplex besselk (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselk (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octavh::besselh1' instead")
inline FloatComplex besselh1 (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselh1 (alpha, x, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octavh::besselh2' instead")
inline FloatComplex besselh2 (float alpha, const FloatComplex& x, bool scaled, octave_idx_type& ierr) { return octave::math::besselh2 (alpha, x, scaled, ierr); }

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API FloatComplexMatrix besselj (float alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API FloatComplexMatrix bessely (float alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API FloatComplexMatrix besseli (float alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API FloatComplexMatrix besselk (float alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API FloatComplexMatrix besselh1 (float alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API FloatComplexMatrix besselh2 (float alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API FloatComplexMatrix besselj (const FloatMatrix& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API FloatComplexMatrix bessely (const FloatMatrix& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API FloatComplexMatrix besseli (const FloatMatrix& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API FloatComplexMatrix besselk (const FloatMatrix& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API FloatComplexMatrix besselh1 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API FloatComplexMatrix besselh2 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API FloatComplexMatrix besselj (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API FloatComplexMatrix bessely (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API FloatComplexMatrix besseli (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API FloatComplexMatrix besselk (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API FloatComplexMatrix besselh1 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API FloatComplexMatrix besselh2 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API FloatComplexNDArray besselj (float alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API FloatComplexNDArray bessely (float alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API FloatComplexNDArray besseli (float alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API FloatComplexNDArray besselk (float alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API FloatComplexNDArray besselh1 (float alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API FloatComplexNDArray besselh2 (float alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API FloatComplexNDArray besselj (const FloatNDArray& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API FloatComplexNDArray bessely (const FloatNDArray& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API FloatComplexNDArray besseli (const FloatNDArray& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API FloatComplexNDArray besselk (const FloatNDArray& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API FloatComplexNDArray besselh1 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API FloatComplexNDArray besselh2 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API FloatComplexNDArray besselj (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API FloatComplexNDArray bessely (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API FloatComplexNDArray besseli (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API FloatComplexNDArray besselk (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API FloatComplexNDArray besselh1 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API FloatComplexNDArray besselh2 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::besselj' instead")
extern OCTAVE_API FloatComplexMatrix besselj (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::bessely' instead")
extern OCTAVE_API FloatComplexMatrix bessely (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besseli' instead")
extern OCTAVE_API FloatComplexMatrix besseli (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselk' instead")
extern OCTAVE_API FloatComplexMatrix besselk (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh1' instead")
extern OCTAVE_API FloatComplexMatrix besselh1 (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::besselh2' instead")
extern OCTAVE_API FloatComplexMatrix besselh2 (const FloatRowVector& alpha, const FloatComplexColumnVector& x, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::airy' instead")
inline Complex airy (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr) { return octave::math::airy (z, deriv, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octave::math::biry' instead")
inline Complex biry (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr) { return octave::math::biry (z, deriv, scaled, ierr); }

OCTAVE_DEPRECATED ("use 'octave::math::airy' instead")
extern OCTAVE_API ComplexMatrix airy (const ComplexMatrix& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::biry' instead")
extern OCTAVE_API ComplexMatrix biry (const ComplexMatrix& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::airy' instead")
extern OCTAVE_API ComplexNDArray airy (const ComplexNDArray& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::biry' instead")
extern OCTAVE_API ComplexNDArray biry (const ComplexNDArray& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::airy' instead")
inline FloatComplex airy (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr) { return octave::math::airy (z, deriv, scaled, ierr); }
OCTAVE_DEPRECATED ("use 'octave::math::biry' instead")
inline FloatComplex biry (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr) { return octave::math::biry (z, deriv, scaled, ierr); }

OCTAVE_DEPRECATED ("use 'octave::math::airy' instead")
extern OCTAVE_API FloatComplexMatrix airy (const FloatComplexMatrix& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::biry' instead")
extern OCTAVE_API FloatComplexMatrix biry (const FloatComplexMatrix& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::airy' instead")
extern OCTAVE_API FloatComplexNDArray airy (const FloatComplexNDArray& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);
OCTAVE_DEPRECATED ("use 'octave::math::biry' instead")
extern OCTAVE_API FloatComplexNDArray biry (const FloatComplexNDArray& z, bool deriv, bool scaled, Array<octave_idx_type>& ierr);

OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
inline double betainc (double x, double a, double b) { return octave::math::betainc (x, a, b); }
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<double> betainc (double x, double a, const Array<double>& b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<double> betainc (double x, const Array<double>& a, double b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<double> betainc (double x, const Array<double>& a, const Array<double>& b);

OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
inline float betainc (float x, float a, float b) { return octave::math::betainc (x, a, b); }
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<double> betainc (const Array<double>& x, double a, const Array<double>& b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<double> betainc (const Array<double>& x, const Array<double>& a, double b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<double> betainc (const Array<double>& x, const Array<double>& a, const Array<double>& b);

OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API float betainc (float x, float a, float b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<float> betainc (float x, float a, const Array<float>& b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<float> betainc (float x, const Array<float>& a, float b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<float> betainc (float x, const Array<float>& a, const Array<float>& b);

OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<float> betainc (const Array<float>& x, float a, float b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<float> betainc (const Array<float>& x, float a, const Array<float>& b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<float> betainc (const Array<float>& x, const Array<float>& a, float b);
OCTAVE_DEPRECATED ("use 'octave::math::betainc' instead")
extern OCTAVE_API Array<float> betainc (const Array<float>& x, const Array<float>& a, const Array<float>& b);

OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
inline double gammainc (double x, double a, bool& err) { return octave::math::gammainc (x, a, err); }
OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
inline double gammainc (double x, double a) { return octave::math::gammainc (x, a); }

OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
extern OCTAVE_API Matrix gammainc (double x, const Matrix& a);
OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
extern OCTAVE_API Matrix gammainc (const Matrix& x, double a);
OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
extern OCTAVE_API Matrix gammainc (const Matrix& x, const Matrix& a);

OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
extern OCTAVE_API NDArray gammainc (double x, const NDArray& a);
OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
extern OCTAVE_API NDArray gammainc (const NDArray& x, double a);
OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
extern OCTAVE_API NDArray gammainc (const NDArray& x, const NDArray& a);

OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
inline float gammainc (float x, float a, bool& err) { return octave::math::gammainc (x, a, err); }
OCTAVE_DEPRECATED ("use 'octave::math::gammainc' instead")
inline float gammainc (float x, float a) { return octave::math::gammainc (x, a); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
extern OCTAVE_API FloatMatrix gammainc (float x, const FloatMatrix& a);
OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
extern OCTAVE_API FloatMatrix gammainc (const FloatMatrix& x, float a);
OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
extern OCTAVE_API FloatMatrix gammainc (const FloatMatrix& x, const FloatMatrix& a);

OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
extern OCTAVE_API FloatNDArray gammainc (float x, const FloatNDArray& a);
OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x, float a);
OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x, const FloatNDArray& a);

OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
inline Complex rc_log1p (double x) { return octave::math::rc_log1p (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_log1p' instead")
inline FloatComplex rc_log1p (float x) { return octave::math::rc_log1p (x); }

OCTAVE_DEPRECATED ("use 'octave::math::erfinv' instead")
inline double erfinv (double x) { return octave::math::erfinv (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfinv' instead")
inline float erfinv (float x) { return octave::math::erfinv (x); }

OCTAVE_DEPRECATED ("use 'octave::math::erfcinv' instead")
inline double erfcinv (double x) { return octave::math::erfcinv (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfcinv' instead")
inline float erfcinv (float x) { return octave::math::erfcinv (x); }

OCTAVE_DEPRECATED ("use 'octave::math::erfcx' instead")
inline float erfcx (float x) { return octave::math::erfcx (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfcx' instead")
inline double erfcx (double x) { return octave::math::erfcx (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfcx' instead")
inline Complex erfcx (const Complex& x) { return octave::math::erfcx (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfcx' instead")
inline FloatComplex erfcx (const FloatComplex& x) { return octave::math::erfcx (x); }

OCTAVE_DEPRECATED ("use 'octave::math::erfi' instead")
inline float erfi (float x) { return octave::math::erfi (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfi' instead")
inline double erfi (double x) { return octave::math::erfi (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfi' instead")
inline Complex erfi (const Complex& x) { return octave::math::erfi (x); }
OCTAVE_DEPRECATED ("use 'octave::math::erfi' instead")
inline FloatComplex erfi (const FloatComplex& x) { return octave::math::erfi (x); }

OCTAVE_DEPRECATED ("use 'octave::math::dawson' instead")
inline float dawson (float x) { return octave::math::dawson (x); }
OCTAVE_DEPRECATED ("use 'octave::math::dawson' instead")
inline double dawson (double x) { return octave::math::dawson (x); }
OCTAVE_DEPRECATED ("use 'octave::math::dawson' instead")
inline Complex dawson (const Complex& x) { return octave::math::dawson (x); }
OCTAVE_DEPRECATED ("use 'octave::math::dawson' instead")
inline FloatComplex dawson (const FloatComplex& x) { return octave::math::dawson (x); }

OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
inline double betaincinv (double x, double a, double b) { return octave::math::betaincinv (x, a, b); }

OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API double betaincinv (double x, double a, double b);
OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API Array<double> betaincinv (double x, double a, const Array<double>& b);
OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API Array<double> betaincinv (double x, const Array<double>& a, double b);
OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API Array<double> betaincinv (double x, const Array<double>& a, const Array<double>& b);

OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API Array<double> betaincinv (const Array<double>& x, double a, double b);
OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API Array<double> betaincinv (const Array<double>& x, double a, const Array<double>& b);
OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API Array<double> betaincinv (const Array<double>& x, const Array<double>& a, double b);
OCTAVE_DEPRECATED ("use 'octave::math::betaincinv' instead")
extern OCTAVE_API Array<double> betaincinv (const Array<double>& x, const Array<double>& a, const Array<double>& b);

OCTAVE_DEPRECATED ("use 'octave::math::ellipj' instead")
inline void ellipj (double u, double m, double& sn, double& cn, double& dn, double& err) { octave::math::ellipj (u, m, sn, cn, dn, err); }
OCTAVE_DEPRECATED ("use 'octave::math::ellipj' instead")
inline void ellipj (const Complex& u, double m, Complex& sn, Complex& cn, Complex& dn, double& err) { octave::math::ellipj (u, m, sn, cn, dn, err); }

//! Digamma function.
//!
//! Only defined for double and float.
template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::psi' instead")
T
psi (T z);

template <>
inline double
psi (double z)
{
  return octave::math::psi (z);
}

template <>
inline float
psi (float z)
{
  return octave::math::psi (z);
}

//! Digamma function for complex input.
//!
//! Only defined for double and float.
template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::psi' instead")
std::complex<T>
psi (const std::complex<T>& z);

template <>
inline std::complex<double>
psi (const std::complex<double>& z)
{
  return octave::math::psi (z);
}

template <>
inline std::complex<float>
psi (const std::complex<float>& z)
{
  return octave::math::psi (z);
}

//! Polygamma function.
//!
//! Only defined for double and float.
//! @param n must be non-negative.  If zero, the digamma function is computed.
//! @param z must be real and non-negative.
template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::psi' instead")
T
psi (octave_idx_type n, T z);

template<>
inline double
psi (octave_idx_type n, double z)
{
  return octave::math::psi (n, z);
}

template<>
inline float
psi (octave_idx_type n, float z)
{
  return octave::math::psi (n, z);
}

#endif

#endif
