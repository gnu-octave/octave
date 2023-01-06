////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_lo_specfun_h)
#define octave_lo_specfun_h 1

#include "octave-config.h"

#include "mx-fwd.h"

#include "Array.h"
#include "oct-cmplx.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

inline double acosh (double x) { return std::acosh (x); }
inline float acosh (float x) { return std::acoshf (x); }
inline Complex acosh (const Complex& x) { return std::acosh (x); }
inline FloatComplex acosh (const FloatComplex& x) { return std::acosh (x); }

extern OCTAVE_API Complex airy (const Complex& z, bool deriv, bool scaled,
                                octave_idx_type& ierr);
extern OCTAVE_API ComplexMatrix airy (const ComplexMatrix& z, bool deriv,
                                      bool scaled, Array<octave_idx_type>& ierr);
extern OCTAVE_API ComplexNDArray airy (const ComplexNDArray& z, bool deriv,
                                       bool scaled, Array<octave_idx_type>& ierr);
extern OCTAVE_API FloatComplex airy (const FloatComplex& z, bool deriv,
                                     bool scaled, octave_idx_type& ierr);
extern OCTAVE_API FloatComplexMatrix airy (const FloatComplexMatrix& z,
                                           bool deriv, bool scaled, Array<octave_idx_type>& ierr);
extern OCTAVE_API FloatComplexNDArray airy (const FloatComplexNDArray& z,
                                            bool deriv, bool scaled, Array<octave_idx_type>& ierr);

inline double asinh (double x) { return std::asinh (x); }
inline float asinh (float x) { return std::asinhf (x); }
inline Complex asinh (const Complex& x) { return std::asinh (x); }
inline FloatComplex asinh (const FloatComplex& x) { return std::asinh (x); }

inline double atanh (double x) { return std::atanh (x); }
inline float atanh (float x) { return std::atanhf (x); }
inline Complex atanh (const Complex& x) { return std::atanh (x); }
inline FloatComplex atanh (const FloatComplex& x) { return std::atanh (x); }

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

extern OCTAVE_API Complex biry (const Complex& z, bool deriv, bool scaled,
                                octave_idx_type& ierr);
extern OCTAVE_API ComplexMatrix biry (const ComplexMatrix& z, bool deriv,
                                      bool scaled, Array<octave_idx_type>& ierr);
extern OCTAVE_API ComplexNDArray biry (const ComplexNDArray& z, bool deriv,
                                       bool scaled, Array<octave_idx_type>& ierr);
extern OCTAVE_API FloatComplex biry (const FloatComplex& z, bool deriv,
                                     bool scaled, octave_idx_type& ierr);
extern OCTAVE_API FloatComplexMatrix biry (const FloatComplexMatrix& z,
                                           bool deriv, bool scaled, Array<octave_idx_type>& ierr);
extern OCTAVE_API FloatComplexNDArray biry (const FloatComplexNDArray& z,
                                            bool deriv, bool scaled, Array<octave_idx_type>& ierr);

inline double cbrt (double x) { return std::cbrt (x); }
inline float cbrt (float x) { return std::cbrtf (x); }

extern OCTAVE_API double dawson (double x);
extern OCTAVE_API float dawson (float x);
extern OCTAVE_API Complex dawson (const Complex& x);
extern OCTAVE_API FloatComplex dawson (const FloatComplex& x);

extern OCTAVE_API void ellipj (double u, double m, double& sn, double& cn,
                               double& dn, double& err);
extern OCTAVE_API void ellipj (const Complex& u, double m, Complex& sn,
                               Complex& cn, Complex& dn, double& err);

inline double erf (double x) { return std::erf (x); }
inline float erf (float x) { return std::erff (x); }
extern OCTAVE_API Complex erf (const Complex& x);
extern OCTAVE_API FloatComplex erf (const FloatComplex& x);

inline double erfc (double x) { return std::erfc (x); }
inline float erfc (float x) { return std::erfcf (x); }
extern OCTAVE_API Complex erfc (const Complex& x);
extern OCTAVE_API FloatComplex erfc (const FloatComplex& x);

extern OCTAVE_API double erfcinv (double x);
extern OCTAVE_API float erfcinv (float x);

extern OCTAVE_API double erfcx (double x);
extern OCTAVE_API float erfcx (float x);
extern OCTAVE_API Complex erfcx (const Complex& x);
extern OCTAVE_API FloatComplex erfcx (const FloatComplex& x);

extern OCTAVE_API double erfi (double x);
extern OCTAVE_API float erfi (float x);
extern OCTAVE_API Complex erfi (const Complex& x);
extern OCTAVE_API FloatComplex erfi (const FloatComplex& x);

extern OCTAVE_API double erfinv (double x);
extern OCTAVE_API float erfinv (float x);

inline double expm1 (double x) { return std::expm1 (x); }
inline float expm1 (float x) { return std::expm1f (x); }
extern OCTAVE_API Complex expm1 (const Complex& x);
extern OCTAVE_API FloatComplex expm1 (const FloatComplex& x);

extern OCTAVE_API double gamma (double x);
extern OCTAVE_API float gamma (float x);

inline double lgamma (double x) { return std::lgamma (x); }
inline float lgamma (float x) { return std::lgammaf (x); }

inline double log1p (double x) { return std::log1p (x); }
inline float log1p (float x) { return std::log1pf (x); }
extern OCTAVE_API Complex log1p (const Complex& x);
extern OCTAVE_API FloatComplex log1p (const FloatComplex& x);

extern OCTAVE_API double psi (double x);
extern OCTAVE_API float psi (float x);
extern OCTAVE_API Complex psi (const Complex& x);
extern OCTAVE_API FloatComplex psi (const FloatComplex& x);
extern OCTAVE_API double psi (octave_idx_type n, double z);
extern OCTAVE_API float psi (octave_idx_type n, float z);

extern OCTAVE_API Complex rc_lgamma (double x);
extern OCTAVE_API FloatComplex rc_lgamma (float x);

extern OCTAVE_API Complex rc_log1p (double x);
extern OCTAVE_API FloatComplex rc_log1p (float x);

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
