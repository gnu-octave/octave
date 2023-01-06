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

#if ! defined (octave_lo_mappers_h)
#define octave_lo_mappers_h 1

#include "octave-config.h"

#include <cmath>

#include <limits>

#include "lo-ieee.h"
#include "oct-cmplx.h"
#include "oct-inttypes-fwd.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

extern OCTAVE_API bool isna (double x);
extern OCTAVE_API bool isna (float x);
extern OCTAVE_API bool isna (const Complex& x);
extern OCTAVE_API bool isna (const FloatComplex& x);

extern OCTAVE_API bool is_NaN_or_NA (const Complex& x);
extern OCTAVE_API bool is_NaN_or_NA (const FloatComplex& x);

inline double copysign (double x, double y) { return std::copysign (x, y); }
inline float copysign (float x, float y) { return std::copysignf (x, y); }

inline double signbit (double x) { return std::signbit (x); }
inline float signbit (float x) { return std::signbit (x); }

// Test for negative sign.
extern OCTAVE_API bool negative_sign (double x);
extern OCTAVE_API bool negative_sign (float x);

// Test for positive sign.
inline bool positive_sign (double x) { return ! negative_sign (x); }
inline bool positive_sign (float x) { return ! negative_sign (x); }

extern OCTAVE_API Complex acos (const Complex& x);
extern OCTAVE_API FloatComplex acos (const FloatComplex& x);

extern OCTAVE_API Complex asin (const Complex& x);
extern OCTAVE_API FloatComplex asin (const FloatComplex& x);

inline Complex atan (const Complex& x) { return std::atan (x); }
inline FloatComplex atan (const FloatComplex& x) { return std::atan (x); }

// The C++ standard would normally return a std::complex value for conj
// even when the input is fully real.  Octave overrides this.
inline double conj (double x) { return x; }
inline float conj (float x) { return x; }

template <typename T>
std::complex<T>
conj (const std::complex<T>& x)
{
  return std::conj (x);
}

inline double log2 (double x) { return std::log2 (x); }
inline float log2 (float x) { return std::log2f (x); }

extern OCTAVE_API Complex log2 (const Complex& x);
extern OCTAVE_API FloatComplex log2 (const FloatComplex& x);

extern OCTAVE_API double log2 (double x, int& exp);
extern OCTAVE_API float log2 (float x, int& exp);

extern OCTAVE_API Complex log2 (const Complex& x, int& exp);
extern OCTAVE_API FloatComplex log2 (const FloatComplex& x, int& exp);

inline double exp2 (double x) { return std::exp2 (x); }
inline float exp2 (float x) { return std::exp2f (x); }

template <typename T>
std::complex<T>
ceil (const std::complex<T>& x)
{
  return std::complex<T> (std::ceil (std::real (x)),
                          std::ceil (std::imag (x)));
}

template <typename T>
std::complex<T>
trunc (const std::complex<T>& x)
{
  return std::complex<T> (std::trunc (std::real (x)),
                          std::trunc (std::imag (x)));
}

// Provide alias for trunc under the more familiar name of fix.
inline double fix (double x) { return std::trunc (x); }
inline float fix (float x) { return std::trunc (x); }

template <typename T>
std::complex<T>
fix (const std::complex<T>& x)
{
  return trunc (x);
}

template <typename T>
std::complex<T>
floor (const std::complex<T>& x)
{
  return std::complex<T> (std::floor (std::real (x)),
                          std::floor (std::imag (x)));
}

inline double round (double x) { return std::round (x); }
inline float round (float x) { return std::roundf (x); }

template <typename T>
std::complex<T>
round (const std::complex<T>& x)
{
  return std::complex<T> (round (std::real (x)), round (std::imag (x)));
}

inline double
roundb (double x)
{
  double t = round (x);

  if (fabs (x - t) == 0.5)
    t = 2 * std::trunc (0.5 * t);

  return t;
}

inline float
roundb (float x)
{
  float t = round (x);

  if (fabsf (x - t) == 0.5f)
    t = 2 * std::trunc (0.5f * t);

  return t;
}

template <typename T>
std::complex<T>
roundb (const std::complex<T>& x)
{
  return std::complex<T> (roundb (std::real (x)), roundb (std::imag (x)));
}

extern OCTAVE_API double frexp (double x, int *expptr);
extern OCTAVE_API float frexp (float x, int *expptr);

inline bool isnan (bool) { return false; }
inline bool isnan (char) { return false; }

inline bool isnan (double x) { return std::isnan (x); }
inline bool isnan (float x) { return std::isnan (x); }

// FIXME: Do we need the isnan overload for complex?
template <typename T>
bool
isnan (const std::complex<T>& x)
{
  return (isnan (std::real (x)) || isnan (std::imag (x)));
}

inline bool isfinite (double x) { return std::isfinite (x); }
inline bool isfinite (float x) { return std::isfinite (x); }

// FIXME: Do we need isfinite overload for complex?
template <typename T>
bool
isfinite (const std::complex<T>& x)
{
  return (isfinite (std::real (x)) && isfinite (std::imag (x)));
}

inline bool isinf (double x) { return std::isinf (x); }
inline bool isinf (float x) { return std::isinf (x); }

template <typename T>
bool
isinf (const octave_int<T>&)
{
  return false;
}

// FIXME: Do we need isinf overload for complex?
template <typename T>
bool
isinf (const std::complex<T>& x)
{
  return (isinf (std::real (x)) || isinf (std::imag (x)));
}

// Some useful tests, that are commonly repeated.
// Test for a finite integer.

// FIXME: Benchmark whether trunc might be faster than round here.
inline bool isinteger (double x) { return isfinite (x) && x == round (x); }
inline bool isinteger (float x) { return isfinite (x) && x == round (x); }

inline double
signum (double x)
{
  double tmp = 0.0;

  if (x < 0.0)
    tmp = -1.0;
  else if (x > 0.0)
    tmp = 1.0;

  return isnan (x) ? numeric_limits<double>::NaN () : tmp;
}

inline float
signum (float x)
{
  float tmp = 0.0f;

  if (x < 0.0f)
    tmp = -1.0f;
  else if (x > 0.0f)
    tmp = 1.0f;

  return isnan (x) ? numeric_limits<float>::NaN () : tmp;
}

template <typename T>
std::complex<T>
signum (const std::complex<T>& x)
{
  T tmp = abs (x);

  return tmp == 0 ? 0.0 : x / tmp;
}

// Convert X to the nearest integer value.  Should not pass NaN to
// this function.

// For integer types?  Hmm.  Need to be sure T is an integer type...
template <typename T>
T
x_nint (T x)
{
  return x;
}

template <>
inline double x_nint (double x)
{
  return (isfinite (x) ? std::floor (x + 0.5) : x);
}

template <>
inline float x_nint (float x)
{
  return (isfinite (x) ? std::floor (x + 0.5f) : x);
}

extern OCTAVE_API octave_idx_type nint_big (double x);
extern OCTAVE_API octave_idx_type nint_big (float x);

extern OCTAVE_API int nint (double x);
extern OCTAVE_API int nint (float x);

template <typename T>
T
mod (T x, T y)
{
  T retval;

  if (y == 0)
    retval = x;
  else
    {
      T q = x / y;

      if (x_nint (y) != y
          && (std::abs ((q - x_nint (q)) / x_nint (q))
              < std::numeric_limits<T>::epsilon ()))
        retval = 0;
      else
        {
          T n = std::floor (q);

          // Prevent use of extra precision.
          volatile T tmp = y * n;

          retval = x - tmp;
        }
    }

  if (x != y && y != 0)
    retval = copysign (retval, y);

  return retval;
}

template <typename T>
T
rem (T x, T y)
{
  T retval;

  if (y == 0)
    retval = numeric_limits<T>::NaN ();
  else
    {
      T q = x / y;

      if (x_nint (y) != y
          && (std::abs ((q - x_nint (q)) / x_nint (q))
              < std::numeric_limits<T>::epsilon ()))
        retval = 0;
      else
        {
          T n = std::trunc (q);

          // Prevent use of extra precision.
          volatile T tmp = y * n;

          retval = x - tmp;
        }
    }

  if (x != y && y != 0)
    retval = copysign (retval, x);

  return retval;
}

// Generic min, max definitions
template <typename T>
T
min (T x, T y)
{
  return x <= y ? x : y;
}

template <typename T>
T
max (T x, T y)
{
  return x >= y ? x : y;
}

// This form is favorable.  GCC will translate (x <= y ? x : y) without a
// jump, hence the only conditional jump involved will be the first
// (isnan), infrequent and hence friendly to branch prediction.

inline double
min (double x, double y)
{
  return isnan (y) ? x : (x <= y ? x : y);
}

inline double
max (double x, double y)
{
  return isnan (y) ? x : (x >= y ? x : y);
}

inline float
min (float x, float y)
{
  return isnan (y) ? x : (x <= y ? x : y);
}

inline float
max (float x, float y)
{
  return isnan (y) ? x : (x >= y ? x : y);
}

inline std::complex<double>
min (const std::complex<double>& x, const std::complex<double>& y)
{
  return abs (x) <= abs (y) ? x : (isnan (x) ? x : y);
}

inline std::complex<float>
min (const std::complex<float>& x, const std::complex<float>& y)
{
  return abs (x) <= abs (y) ? x : (isnan (x) ? x : y);
}

inline std::complex<double>
max (const std::complex<double>& x, const std::complex<double>& y)
{
  return abs (x) >= abs (y) ? x : (isnan (x) ? x : y);
}

inline std::complex<float>
max (const std::complex<float>& x, const std::complex<float>& y)
{
  return abs (x) >= abs (y) ? x : (isnan (x) ? x : y);
}

template <typename T>
inline octave_int<T>
min (const octave_int<T>& x, const octave_int<T>& y)
{
  return xmin (x, y);
}

template <typename T>
inline octave_int<T>
max (const octave_int<T>& x, const octave_int<T>& y)
{
  return xmax (x, y);
}

// These map reals to Complex.

extern OCTAVE_API Complex rc_acos (double);
extern OCTAVE_API FloatComplex rc_acos (float);

extern OCTAVE_API Complex rc_acosh (double);
extern OCTAVE_API FloatComplex rc_acosh (float);

extern OCTAVE_API Complex rc_asin (double);
extern OCTAVE_API FloatComplex rc_asin (float);

extern OCTAVE_API Complex rc_atanh (double);
extern OCTAVE_API FloatComplex rc_atanh (float);

extern OCTAVE_API Complex rc_log (double);
extern OCTAVE_API FloatComplex rc_log (float);

extern OCTAVE_API Complex rc_log2 (double);
extern OCTAVE_API FloatComplex rc_log2 (float);

extern OCTAVE_API Complex rc_log10 (double);
extern OCTAVE_API FloatComplex rc_log10 (float);

extern OCTAVE_API Complex rc_sqrt (double);
extern OCTAVE_API FloatComplex rc_sqrt (float);

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)

#endif
