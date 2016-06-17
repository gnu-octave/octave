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

#if ! defined (octave_lo_mappers_h)
#define octave_lo_mappers_h 1

#include "octave-config.h"

#include <limits>

#include "oct-cmplx.h"
#include "lo-math.h"
#include "lo-ieee.h"

namespace octave
{
  namespace math
  {
    extern OCTAVE_API bool is_NA (double x);
    extern OCTAVE_API bool is_NA (float x);

    extern OCTAVE_API bool is_NA (const Complex& x);
    extern OCTAVE_API bool is_NA (const FloatComplex& x);

    extern OCTAVE_API bool is_NaN_or_NA (const Complex& x);
    extern OCTAVE_API bool is_NaN_or_NA (const FloatComplex& x);

    extern OCTAVE_API Complex acos (const Complex& x);
    extern OCTAVE_API FloatComplex acos (const FloatComplex& x);

    using std::acos;

    extern OCTAVE_API Complex asin (const Complex& x);
    extern OCTAVE_API FloatComplex asin (const FloatComplex& x);

    using std::asin;

    extern OCTAVE_API Complex atan (const Complex& x);
    extern OCTAVE_API FloatComplex atan (const FloatComplex& x);

    using std::atan;

    // C++ now provides versions of the following funtions for
    // arguments of type std::complex<T> and T so we no longer need to
    // provide our own wrappers for real-valued arguments.  Import
    // them to the octave::math namespace for convenience.

    using std::arg;
    using std::conj;
    using std::imag;
    using std::real;

    extern OCTAVE_API double log2 (double x);
    extern OCTAVE_API float log2 (float x);

    extern OCTAVE_API Complex log2 (const Complex& x);
    extern OCTAVE_API FloatComplex log2 (const FloatComplex& x);

    extern OCTAVE_API double log2 (double x, int& exp);
    extern OCTAVE_API float log2 (float x, int& exp);

    extern OCTAVE_API Complex log2 (const Complex& x, int& exp);
    extern OCTAVE_API FloatComplex log2 (const FloatComplex& x, int& exp);

    extern OCTAVE_API double exp2 (double x);
    extern OCTAVE_API float exp2 (float x);

    inline double ceil (double x) { return ::ceil (x); }
    inline float ceil (float x) { return ::ceilf (x); }

    template <typename T>
    std::complex<T>
    ceil (const std::complex<T>& x)
    {
      return std::complex<T> (ceil (real (x)), ceil (imag (x)));
    }

    extern OCTAVE_API double copysign (double x, double y);
    extern OCTAVE_API float copysign (float x, float y);

    extern OCTAVE_API double signbit (double x);
    extern OCTAVE_API float signbit (float x);

    // Test for negative sign.
    extern OCTAVE_API bool negative_sign (double x);
    extern OCTAVE_API bool negative_sign (float x);

    // Test for positive sign.
    inline bool positive_sign (double x) { return ! negative_sign (x); }
    inline bool positive_sign (float x) { return ! negative_sign (x); }

    extern OCTAVE_API double trunc (double x);
    extern OCTAVE_API float trunc (float x);

    template <typename T>
    std::complex<T>
    trunc (const std::complex<T>& x)
    {
      return std::complex<T> (trunc (real (x)), trunc (imag (x)));
    }

    inline double fix (double x) { return trunc (x); }
    inline float fix (float x) { return trunc (x); }

    template <typename T>
    std::complex<T>
    fix (const std::complex<T>& x)
    {
      return trunc (x);
    }

    extern OCTAVE_API double floor (double x);
    extern OCTAVE_API float floor (float x);

    template <typename T>
    std::complex<T>
    floor (const std::complex<T>& x)
    {
      return std::complex<T> (floor (real (x)), floor (imag (x)));
    }

    extern OCTAVE_API double round (double x);
    extern OCTAVE_API float round (float x);

    template <typename T>
    std::complex<T>
    round (const std::complex<T>& x)
    {
      return std::complex<T> (round (real (x)), round (imag (x)));
    }

    inline double
    roundb (double x)
    {
      double t = round (x);

      if (fabs (x - t) == 0.5)
        t = 2 * trunc (0.5 * t);

      return t;
    }

    inline float
    roundb (float x)
    {
      float t = round (x);

      if (fabsf (x - t) == 0.5)
        t = 2 * trunc (0.5 * t);

      return t;
    }

    template <typename T>
    std::complex<T>
    roundb (const std::complex<T>& x)
    {
      return std::complex<T> (roundb (real (x)), roundb (imag (x)));
    }

    inline bool isnan (bool) { return false; }
    inline bool isnan (char) { return false; }
    extern OCTAVE_API bool isnan (double x);
    extern OCTAVE_API bool isnan (float x);

    template <typename T>
    bool
    isnan (const std::complex<T>& x)
    {
      return (isnan (real (x)) || isnan (imag (x)));
    }

    extern OCTAVE_API bool finite (double x);
    extern OCTAVE_API bool finite (float x);

    template <typename T>
    bool
    finite (const std::complex<T>& x)
    {
      return (finite (real (x)) && finite (imag (x)));
    }

    extern OCTAVE_API bool isinf (double x);
    extern OCTAVE_API bool isinf (float x);

    template <typename T>
    bool
    isinf (const std::complex<T>& x)
    {
      return (isinf (real (x)) || isinf (imag (x)));
    }

    // Some useful tests, that are commonly repeated.
    // Test for a finite integer.

    inline bool isinteger (double x) { return finite (x) && x == round (x); }
    inline bool isinteger (float x) { return finite (x) && x == round (x); }

    inline double
    signum (double x)
    {
      double tmp = 0.0;

      if (x < 0.0)
        tmp = -1.0;
      else if (x > 0.0)
        tmp = 1.0;

      return isnan (x) ? octave::numeric_limits<double>::NaN () : tmp;
    }

    inline float
    signum (float x)
    {
      float tmp = 0.0f;

      if (x < 0.0f)
        tmp = -1.0f;
      else if (x > 0.0f)
        tmp = 1.0f;

      return isnan (x) ? octave::numeric_limits<float>::NaN () : tmp;
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
    inline double x_nint (double x) { return (finite (x) ? floor (x + 0.5) : x); }
    template <>
    inline float x_nint (float x) { return (finite (x) ? floor (x + 0.5f) : x); }

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
              T n = floor (q);

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
        retval = octave::numeric_limits<T>::NaN ();
      else
        {
          T q = x / y;

          if (x_nint (y) != y
              && (std::abs ((q - x_nint (q)) / x_nint (q))
                  < std::numeric_limits<T>::epsilon ()))
            retval = 0;
          else
            {
              T n = trunc (q);

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
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::math::is_NA' instead")
inline bool octave_is_NA (double x) { return octave::math::is_NA (x); }
OCTAVE_DEPRECATED ("use 'octave::math::is_NA' instead")
inline bool octave_is_NA (float x) { return octave::math::is_NA (x); }
OCTAVE_DEPRECATED ("use 'octave::math::is_NA' instead")
inline bool octave_is_NA (const Complex& x) { return octave::math::is_NA (x); }
OCTAVE_DEPRECATED ("use 'octave::math::is_NA' instead")
inline bool octave_is_NA (const FloatComplex& x) { return octave::math::is_NA (x); }

OCTAVE_DEPRECATED ("use 'octave::math::is_NaN_or_NA' instead")
inline bool octave_is_NaN_or_NA (const Complex& x) { return octave::math::is_NaN_or_NA (x); }
OCTAVE_DEPRECATED ("use 'octave::math::is_NaN_or_NA' instead")
inline bool octave_is_NaN_or_NA (const FloatComplex& x) { return octave::math::is_NaN_or_NA (x); }

OCTAVE_DEPRECATED ("use 'octave::math::acos' instead")
inline Complex acos (const Complex& x) { return octave::math::acos (x); }
OCTAVE_DEPRECATED ("use 'octave::math::acos' instead")
inline FloatComplex acos (const FloatComplex& x) { return octave::math::acos (x); }

OCTAVE_DEPRECATED ("use 'octave::math::asin' instead")
inline Complex asin (const Complex& x) { return octave::math::asin (x); }
OCTAVE_DEPRECATED ("use 'octave::math::asin' instead")
inline FloatComplex asin (const FloatComplex& x) { return octave::math::asin (x); }

OCTAVE_DEPRECATED ("use 'octave::math::atan' instead")
inline Complex atan (const Complex& x) { return octave::math::atan (x); }
OCTAVE_DEPRECATED ("use 'octave::math::atan' instead")
inline FloatComplex atan (const FloatComplex& x) { return octave::math::atan (x); }

OCTAVE_DEPRECATED ("use 'octave::math::arg' instead")
inline double arg (double x) { return octave::math::arg (x); }
OCTAVE_DEPRECATED ("use 'octave::math::arg' instead")
inline float arg (float x) { return octave::math::arg (x); }

OCTAVE_DEPRECATED ("use 'octave::math::conj' instead")
inline double conj (double x) { return x; }
OCTAVE_DEPRECATED ("use 'octave::math::conj' instead")
inline float conj (float x) { return x; }

OCTAVE_DEPRECATED ("use 'octave::math::imag' instead")
inline double imag (double x) { return octave::math::imag (x); }
OCTAVE_DEPRECATED ("use 'octave::math::imag' instead")
inline float imag (float x) { return octave::math::imag (x); }

OCTAVE_DEPRECATED ("use 'octave::math::real' instead")
inline double real (double x) { return octave::math::real (x); }
OCTAVE_DEPRECATED ("use 'octave::math::real' instead")
inline float real (float x) { return octave::math::real (x); }

OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline double xlog2 (double x) { return octave::math::log2 (x); }
OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline float xlog2 (float x) { return octave::math::log2 (x); }

OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline Complex xlog2 (const Complex& x) { return octave::math::log2 (x); }
OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline FloatComplex xlog2 (const FloatComplex& x) { return octave::math::log2 (x); }

OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline double xlog2 (double x, int& exp) { return octave::math::log2 (x, exp); }
OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline float xlog2 (float x, int& exp) { return octave::math::log2 (x, exp); }

OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline Complex xlog2 (const Complex& x, int& exp) { return octave::math::log2 (x, exp); }
OCTAVE_DEPRECATED ("use 'octave::math::log2' instead")
inline FloatComplex xlog2 (const FloatComplex& x, int& exp) { return octave::math::log2 (x, exp); }

OCTAVE_DEPRECATED ("use 'octave::math::exp2' instead")
inline double xexp2 (double x) { return octave::math::exp2 (x); }
OCTAVE_DEPRECATED ("use 'octave::math::exp2' instead")
inline float xexp2 (float x) { return octave::math::exp2 (x); }

OCTAVE_DEPRECATED ("use 'octave::math::ceil' instead")
inline double xceil (double x) { return octave::math::ceil (x); }
OCTAVE_DEPRECATED ("use 'octave::math::ceil' instead")
inline float xceil (float x) { return octave::math::ceil (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::ceil' instead")
std::complex<T>
ceil (const std::complex<T>& x)
{
  return octave::math::ceil (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::copysign' instead")
inline double xcopysign (double x, double y) { return octave::math::copysign (x, y); }
OCTAVE_DEPRECATED ("use 'octave::math::copysign' instead")
inline float xcopysign (float x, float y) { return octave::math::copysign (x, y); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::signbit' instead")
T
xsignbit (T x)
{
  return octave::math::signbit (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::negative_sign' instead")
inline bool xnegative_sign (double x) { return octave::math::negative_sign (x); }
OCTAVE_DEPRECATED ("use 'octave::math::negative_sign' instead")
inline bool xnegative_sign (float x) { return octave::math::negative_sign (x); }

OCTAVE_DEPRECATED ("use 'octave::math::positive_sign' instead")
inline bool xpositive_sign (double x) { return octave::math::positive_sign (x); }
OCTAVE_DEPRECATED ("use 'octave::math::positive_sign' instead")
inline bool xpositive_sign (float x) { return octave::math::positive_sign (x); }

OCTAVE_DEPRECATED ("use 'octave::math::signum' instead")
inline double signum (double x) { return octave::math::signum (x); }
OCTAVE_DEPRECATED ("use 'octave::math::signum' instead")
inline float signum (float x) { return octave::math::signum (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::signum' instead")
std::complex<T>
signum (const std::complex<T>& x)
{
  return octave::math::signum (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::trunc' instead")
inline double xtrunc (double x) { return octave::math::trunc (x); }
OCTAVE_DEPRECATED ("use 'octave::math::trunc' instead")
inline float xtrunc (float x) { return octave::math::trunc (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::trunc' instead")
std::complex<T>
xtrunc (const std::complex<T>& x)
{
  return octave::math::trunc (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::fix' instead")
inline double fix (double x) { return octave::math::fix (x); }
OCTAVE_DEPRECATED ("use 'octave::math::fix' instead")
inline float fix (float x) { return octave::math::fix (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::fix' instead")
std::complex<T>
fix (const std::complex<T>& x)
{
  return octave::math::fix (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::floor' instead")
inline double xfloor (double x) { return octave::math::floor (x); }
OCTAVE_DEPRECATED ("use 'octave::math::floor' instead")
inline float xfloor (float x) { return octave::math::floor (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::floor' instead")
std::complex<T>
floor (const std::complex<T>& x)
{
  return octave::math::floor (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::round' instead")
inline double xround (double x) { return octave::math::round (x); }
OCTAVE_DEPRECATED ("use 'octave::math::round' instead")
inline float xround (float x) { return octave::math::round (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::round' instead")
std::complex<T>
xround (const std::complex<T>& x)
{
  return octave::math::round (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::roundb' instead")
inline double xroundb (double x) { return octave::math::roundb (x); }
OCTAVE_DEPRECATED ("use 'octave::math::roundb' instead")
inline float xroundb (float x) { return octave::math::roundb (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::roundb' instead")
std::complex<T>
xroundb (const std::complex<T>& x)
{
  return octave::math::roundb (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::isnan' instead")
inline bool xisnan (bool x) { return octave::math::isnan (x); }
OCTAVE_DEPRECATED ("use 'octave::math::isnan' instead")
inline bool xisnan (char x) { return octave::math::isnan (x); }
OCTAVE_DEPRECATED ("use 'octave::math::isnan' instead")
inline bool xisnan (double x) { return octave::math::isnan (x); }
OCTAVE_DEPRECATED ("use 'octave::math::isnan' instead")
inline bool xisnan (float x) { return octave::math::isnan (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::isnan' instead")
bool
xisnan (const std::complex<T>& x)
{
  return octave::math::isnan (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::finite' instead")
inline bool xfinite (double x) { return octave::math::finite (x); }
OCTAVE_DEPRECATED ("use 'octave::math::finite' instead")
inline bool xfinite (float x) { return octave::math::finite (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::finite' instead")
bool
xfinite (const std::complex<T>& x)
{
  return octave::math::finite (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::isinf' instead")
inline bool xisinf (double x) { return octave::math::isinf (x); }
OCTAVE_DEPRECATED ("use 'octave::math::isinf' instead")
inline bool xisinf (float x) { return octave::math::isinf (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::isinf' instead")
bool
xisinf (const std::complex<T>& x)
{
  return octave::math::isinf (x);
}

// Some useful tests, that are commonly repeated.
// Test for a finite integer.

OCTAVE_DEPRECATED ("use 'octave::math::isinteger' instead")
inline bool
xisinteger (double x)
{
  return octave::math::isinteger (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::isinteger' instead")
inline bool
xisinteger (float x)
{
  return octave::math::isinteger (x);
}

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::x_nint' instead")
T
X_NINT (T x)
{
  return octave::math::x_nint (x);
}

OCTAVE_DEPRECATED ("use 'octave::math::x_nint (x)' instead")
inline double D_NINT (double x) { return octave::math::x_nint (x); }
OCTAVE_DEPRECATED ("use 'octave::math::x_nint (x)' instead")
inline float F_NINT (float x) { return octave::math::x_nint (x); }

OCTAVE_DEPRECATED ("use 'octave::math::nint_big' instead")
inline octave_idx_type NINTbig (double x) { return octave::math::nint_big (x); }
OCTAVE_DEPRECATED ("use 'octave::math::nint_big' instead")
inline octave_idx_type NINTbig (float x) { return octave::math::nint_big (x); }

OCTAVE_DEPRECATED ("use 'octave::math::nint' instead")
inline int NINT (double x) { return octave::math::nint (x); }
OCTAVE_DEPRECATED ("use 'octave::math::nint' instead")
inline int NINT (float x) { return octave::math::nint (x); }

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::mod' instead")
T
xmod (T x, T y)
{
  return octave::math::mod (x, y);
}

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::rem' instead")
T
xrem (T x, T y)
{
  return octave::math::rem (x, y);
}

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::min' instead")
T
xmin (T x, T y)
{
  return octave::math::min (x, y);
}

template <typename T>
OCTAVE_DEPRECATED ("use 'octave::math::max' instead")
T
xmax (T x, T y)
{
  return octave::math::max (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::min' instead")
inline double
xmin (double x, double y)
{
  return octave::math::min (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::max' instead")
inline double
xmax (double x, double y)
{
  return octave::math::max (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::min' instead")
inline float
xmin (float x, float y)
{
  return octave::math::min (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::max' instead")
inline float
xmax (float x, float y)
{
  return octave::math::max (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::min' instead")
inline Complex
xmin (const Complex& x, const Complex& y)
{
  return octave::math::min (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::max' instead")
inline Complex
xmax (const Complex& x, const Complex& y)
{
  return octave::math::max (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::min' instead")
inline OCTAVE_API FloatComplex
xmin (const FloatComplex& x, const FloatComplex& y)
{
  return octave::math::min (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::max' instead")
inline FloatComplex
xmax (const FloatComplex& x, const FloatComplex& y)
{
  return octave::math::max (x, y);
}

OCTAVE_DEPRECATED ("use 'octave::math::rc_acos' instead")
inline Complex rc_acos (double x) { return octave::math::rc_acos (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_acos' instead")
inline FloatComplex rc_acos (float x) { return octave::math::rc_acos (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_acosh' instead")
inline Complex rc_acosh (double x) { return octave::math::rc_acosh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_acosh' instead")
inline FloatComplex rc_acosh (float x) { return octave::math::rc_acosh (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_asin' instead")
inline Complex rc_asin (double x) { return octave::math::rc_asin (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_asin' instead")
inline FloatComplex rc_asin (float x) { return octave::math::rc_asin (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_atanh' instead")
inline Complex rc_atanh (double x) { return octave::math::rc_atanh (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_atanh' instead")
inline FloatComplex rc_atanh (float x) { return octave::math::rc_atanh (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_log' instead")
inline Complex rc_log (double x) { return octave::math::rc_log (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_log' instead")
inline FloatComplex rc_log (float x) { return octave::math::rc_log (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_log2' instead")
inline Complex rc_log2 (double x) { return octave::math::rc_log2 (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_log2' instead")
inline FloatComplex rc_log2 (float x) { return octave::math::rc_log2 (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_log10' instead")
inline Complex rc_log10 (double x) { return octave::math::rc_log10 (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_log10' instead")
inline FloatComplex rc_log10 (float x) { return octave::math::rc_log10 (x); }

OCTAVE_DEPRECATED ("use 'octave::math::rc_sqrt' instead")
inline Complex rc_sqrt (double x) { return octave::math::rc_sqrt (x); }
OCTAVE_DEPRECATED ("use 'octave::math::rc_sqrt' instead")
inline FloatComplex rc_sqrt (float x) { return octave::math::rc_sqrt (x); }

#endif

#endif
