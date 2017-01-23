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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cfloat>

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-math.h"
#include "lo-specfun.h"
#include "lo-utils.h"
#include "math-wrappers.h"
#include "oct-cmplx.h"

#include "f77-fcn.h"

// FIXME: We used to have this situation:
//
//   Functions that forward to gnulib belong here so we can keep
//   gnulib:: out of lo-mappers.h.
//
// but now we just use std:: and explicit wrappers in C++ code so maybe
// some of the forwarding functions can be defined inline here.

namespace octave
{
  namespace math
  {
    bool
    is_NA (double x)
    {
      return lo_ieee_is_NA (x);
    }

    bool
    is_NA (const Complex& x)
    {
      return (is_NA (std::real (x)) || is_NA (std::imag (x)));
    }

    bool
    is_NA (float x)
    {
      return lo_ieee_is_NA (x);
    }

    bool
    is_NA (const FloatComplex& x)
    {
      return (is_NA (std::real (x)) || is_NA (std::imag (x)));
    }

    bool
    is_NaN_or_NA (const Complex& x)
    {
      return (isnan (std::real (x)) || isnan (std::imag (x)));
    }

    bool
    is_NaN_or_NA (const FloatComplex& x)
    {
      return (isnan (std::real (x)) || isnan (std::imag (x)));
    }

    Complex
    acos (const Complex& x)
    {
#if defined (HAVE_COMPLEX_STD_ACOS)
      Complex y = std::acos (x);

      if (std::imag (x) == 0.0 && std::real (x) > 1.0)
        return std::conj (y);
      else
        return y;
#else
      static Complex i (0, 1);

      Complex tmp;

      if (std::imag (x) == 0.0)
        {
          // If the imaginary part of X is 0, then avoid generating an
          // imaginary part of -0 for the expression 1-x*x.
          // This chooses the same phase of the branch cut as Matlab.
          double xr = std::real (x);
          tmp = Complex (1.0 - xr*xr);
        }
      else
        tmp = 1.0 - x*x;

      return -i * log (x + i * sqrt (tmp));
#endif
    }

    FloatComplex
    acos (const FloatComplex& x)
    {
#if defined (HAVE_COMPLEX_STD_ACOS)
      FloatComplex y = std::acos (x);

      if (std::imag (x) == 0.0f && std::real (x) > 1.0f)
        return std::conj (y);
      else
        return y;
#else
      static FloatComplex i (0, 1);

      FloatComplex tmp;

      if (std::imag (x) == 0.0f)
        {
          // If the imaginary part of X is 0, then avoid generating an
          // imaginary part of -0 for the expression 1-x*x.
          // This chooses the same phase of the branch cut as Matlab.
          float xr = std::real (x);
          tmp = FloatComplex (1.0f - xr*xr);
        }
      else
        tmp = 1.0f - x*x;

      return -i * log (x + i * sqrt (tmp));
#endif
    }

    Complex
    asin (const Complex& x)
    {
#if defined (HAVE_COMPLEX_STD_ASIN)
      Complex y = std::asin (x);

      if (std::imag (x) == 0.0 && std::real (x) > 1.0)
        return std::conj (y);
      else
        return y;
#else
      static Complex i (0, 1);

      Complex tmp;

      if (std::imag (x) == 0.0)
        {
          // If the imaginary part of X is 0, then avoid generating an
          // imaginary part of -0 for the expression 1-x*x.
          // This chooses the same phase of the branch cut as Matlab.
          double xr = std::real (x);
          tmp = Complex (1.0 - xr*xr);
        }
      else
        tmp = 1.0 - x*x;

      return -i * log (i*x + sqrt (tmp));
#endif
    }

    FloatComplex
    asin (const FloatComplex& x)
    {
#if defined (HAVE_COMPLEX_STD_ASIN)
      FloatComplex y = std::asin (x);

      if (std::imag (x) == 0.0f && std::real (x) > 1.0f)
        return std::conj (y);
      else
        return y;
#else
      static FloatComplex i (0, 1);

      FloatComplex tmp;

      if (std::imag (x) == 0.0f)
        {
          // If the imaginary part of X is 0, then avoid generating an
          // imaginary part of -0 for the expression 1-x*x.
          // This chooses the same phase of the branch cut as Matlab.
          float xr = std::real (x);
          tmp = FloatComplex (1.0f - xr*xr);
        }
      else
        tmp = 1.0f - x*x;

      return -i * log (i*x + sqrt (tmp));
#endif
    }

    Complex
    atan (const Complex& x)
    {
#if defined (HAVE_COMPLEX_STD_ATAN)
      return std::atan (x);
#else
      static Complex i (0, 1);

      return i * log ((i + x) / (i - x)) / 2.0;
#endif
    }

    FloatComplex
    atan (const FloatComplex& x)
    {
#if defined (HAVE_COMPLEX_STD_ATAN)
      return std::atan (x);
#else
      static FloatComplex i (0, 1);

      return i * log ((i + x) / (i - x)) / 2.0f;
#endif
    }

    double log2 (double x) { return std::log2 (x); }
    float log2 (float x) { return std::log2 (x); }

    Complex
    log2 (const Complex& x)
    {
#if defined (M_LN2)
      static double ln2 = M_LN2;
#else
      static double ln2 = std::log (2.0);
#endif
      return std::log (x) / ln2;
    }

    FloatComplex
    log2 (const FloatComplex& x)
    {
#if defined (M_LN2)
      static float ln2 = M_LN2;
#else
      static float ln2 = log (2.0f);
#endif
      return std::log (x) / ln2;
    }

    double
    log2 (double x, int& exp)
    {
      return frexp (x, &exp);
    }

    float
    log2 (float x, int& exp)
    {
      return frexp (x, &exp);
    }

    Complex
    log2 (const Complex& x, int& exp)
    {
      double ax = std::abs (x);
      double lax = log2 (ax, exp);
      return (ax != lax) ? (x / ax) * lax : x;
    }

    FloatComplex
    log2 (const FloatComplex& x, int& exp)
    {
      float ax = std::abs (x);
      float lax = log2 (ax, exp);
      return (ax != lax) ? (x / ax) * lax : x;
    }

    double
    exp2 (double x)
    {
#if defined (HAVE_EXP2)
      return ::exp2 (x);
#else
#  if defined (M_LN2)
      static double ln2 = M_LN2;
#  else
      static double ln2 = std::log (2.0);
#  endif
      return exp (x * ln2);
#endif
    }

    float
    exp2 (float x)
    {
#if defined (HAVE_EXP2F)
      return exp2f (x);
#elif defined (HAVE_EXP2)
      return ::exp2 (x);
#else
#  if defined (M_LN2)
      static float ln2 = M_LN2;
#  else
      static float ln2 = log2 (2.0f);
#  endif
      return exp (x * ln2);
#endif
    }

    double copysign (double x, double y) { return std::copysign (x, y); }
    float copysign (float x, float y) { return std::copysign (x, y); }

    double signbit (double x) { return std::signbit (x); }
    float signbit (float x) { return std::signbit (x); }

    bool negative_sign (double x) { return __lo_ieee_signbit (x); }
    bool negative_sign (float x) { return __lo_ieee_float_signbit (x); }

    double trunc (double x) { return std::trunc (x); }
    float trunc (float x) { return std::trunc (x); }

    double floor (double x) { return std::floor (x); }
    float floor (float x) { return std::floor (x); }

    double round (double x) { return std::round (x); }
    float round (float x) { return std::round (x); }

    double frexp (double x, int *expptr)
    {
      return octave_frexp_wrapper (x, expptr);
    }

    float frexp (float x, int *expptr)
    {
      return octave_frexpf_wrapper (x, expptr);
    }

    bool
    isnan (double x)
    {
#if defined (HAVE_CMATH_ISNAN)
      return std::isnan (x);
#else
      return lo_ieee_isnan (x);
#endif
    }

    bool
    isnan (float x)
    {
#if defined (HAVE_CMATH_ISNANF)
      return std::isnan (x);
#else
      return lo_ieee_isnan (x);
#endif
    }

    bool
    finite (double x)
    {
#if defined (HAVE_CMATH_ISFINITE)
      return std::isfinite (x);
#else
      return lo_ieee_finite (x);
#endif
    }

    bool
    finite (float x)
    {
#if defined (HAVE_CMATH_ISFINITEF)
      return std::isfinite (x);
#else
      return lo_ieee_finite (x);
#endif
    }

    bool
    isinf (double x)
    {
#if defined (HAVE_CMATH_ISINF)
      return std::isinf (x);
#else
      return lo_ieee_isinf (x);
#endif
    }

    bool
    isinf (float x)
    {
#if defined (HAVE_CMATH_ISINFF)
      return std::isinf (x);
#else
      return lo_ieee_isinf (x);
#endif
    }

    // Sometimes you need a large integer, but not always.

    octave_idx_type
    nint_big (double x)
    {
      if (x > std::numeric_limits<octave_idx_type>::max ())
        return std::numeric_limits<octave_idx_type>::max ();
      else if (x < std::numeric_limits<octave_idx_type>::min ())
        return std::numeric_limits<octave_idx_type>::min ();
      else
        return static_cast<octave_idx_type> ((x > 0.0) ? (x + 0.5)
                                                       : (x - 0.5));
    }

    octave_idx_type
    nint_big (float x)
    {
      if (x > std::numeric_limits<octave_idx_type>::max ())
        return std::numeric_limits<octave_idx_type>::max ();
      else if (x < std::numeric_limits<octave_idx_type>::min ())
        return std::numeric_limits<octave_idx_type>::min ();
      else
        return static_cast<octave_idx_type> ((x > 0.0f) ? (x + 0.5f)
                                                        : (x - 0.5f));
    }

    int
    nint (double x)
    {
      if (x > std::numeric_limits<int>::max ())
        return std::numeric_limits<int>::max ();
      else if (x < std::numeric_limits<int>::min ())
        return std::numeric_limits<int>::min ();
      else
        return static_cast<int> ((x > 0.0) ? (x + 0.5) : (x - 0.5));
    }

    int
    nint (float x)
    {
      if (x > std::numeric_limits<int>::max ())
        return std::numeric_limits<int>::max ();
      else if (x < std::numeric_limits<int>::min ())
        return std::numeric_limits<int>::min ();
      else
        return static_cast<int> ((x > 0.0f) ? (x + 0.5f) : (x - 0.5f));
    }

    Complex
    rc_acos (double x)
    {
      return fabs (x) > 1.0 ? acos (Complex (x)) : Complex (::acos (x));
    }

    FloatComplex
    rc_acos (float x)
    {
      return fabsf (x) > 1.0f ? acos (FloatComplex (x))
                              : FloatComplex (::acosf (x));
    }

    Complex
    rc_acosh (double x)
    {
      return x < 1.0 ? acosh (Complex (x)) : Complex (acosh (x));
    }

    FloatComplex
    rc_acosh (float x)
    {
      return x < 1.0f ? acosh (FloatComplex (x)) : FloatComplex (acosh (x));
    }

    Complex
    rc_asin (double x)
    {
      return fabs (x) > 1.0 ? asin (Complex (x)) : Complex (::asin (x));
    }

    FloatComplex
    rc_asin (float x)
    {
      return fabsf (x) > 1.0f ? asin (FloatComplex (x))
                              : FloatComplex (::asinf (x));
    }

    Complex
    rc_atanh (double x)
    {
      return fabs (x) > 1.0 ? atanh (Complex (x)) : Complex (atanh (x));
    }

    FloatComplex
    rc_atanh (float x)
    {
      return fabsf (x) > 1.0f ? atanh (FloatComplex (x))
                              : FloatComplex (atanh (x));
    }

    Complex
    rc_log (double x)
    {
      const double pi = 3.14159265358979323846;
      return x < 0.0 ? Complex (std::log (-x), pi) : Complex (std::log (x));
    }

    FloatComplex
    rc_log (float x)
    {
      const float pi = 3.14159265358979323846f;
      return x < 0.0f ? FloatComplex (std::log (-x), pi)
                      : FloatComplex (std::log (x));
    }

    Complex
    rc_log2 (double x)
    {
      const double pil2 = 4.53236014182719380962; // = pi / log(2)
      return x < 0.0 ? Complex (log2 (-x), pil2) : Complex (log2 (x));
    }

    FloatComplex
    rc_log2 (float x)
    {
      const float pil2 = 4.53236014182719380962f; // = pi / log(2)
      return x < 0.0f ? FloatComplex (log2 (-x), pil2)
                      : FloatComplex (log2 (x));
    }

    Complex
    rc_log10 (double x)
    {
      const double pil10 = 1.36437635384184134748; // = pi / log(10)
      return x < 0.0 ? Complex (log10 (-x), pil10) : Complex (log10 (x));
    }

    FloatComplex
    rc_log10 (float x)
    {
      const float pil10 = 1.36437635384184134748f; // = pi / log(10)
      return x < 0.0f ? FloatComplex (log10 (-x), pil10)
                      : FloatComplex (log10f (x));
    }

    Complex
    rc_sqrt (double x)
    {
      return x < 0.0 ? Complex (0.0, sqrt (-x)) : Complex (sqrt (x));
    }

    FloatComplex
    rc_sqrt (float x)
    {
      return x < 0.0f ? FloatComplex (0.0f, sqrtf (-x))
                      : FloatComplex (sqrtf (x));
    }
  }
}
