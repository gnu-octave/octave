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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-mappers.h"
#include "lo-specfun.h"
#include "math-wrappers.h"

// FIXME: We used to have this situation:
//
//   Functions that forward to gnulib belong here so we can keep
//   gnulib:: out of lo-mappers.h.
//
// but now we just use std:: and explicit wrappers in C++ code so maybe
// some of the forwarding functions can be defined inline here.

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

bool
isna (double x)
{
  return lo_ieee_is_NA (x);
}

bool
isna (const Complex& x)
{
  return (isna (std::real (x)) || isna (std::imag (x)));
}

bool
isna (float x)
{
  return lo_ieee_is_NA (x);
}

bool
isna (const FloatComplex& x)
{
  return (isna (std::real (x)) || isna (std::imag (x)));
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

// Matlab returns a different phase for acos, asin then std library
// which requires a small function to remap the phase.
Complex
acos (const Complex& x)
{
  Complex y = std::acos (x);

  if (std::imag (x) == 0.0 && std::real (x) > 1.0)
    return std::conj (y);
  else
    return y;
}

FloatComplex
acos (const FloatComplex& x)
{
  FloatComplex y = std::acos (x);

  if (std::imag (x) == 0.0f && std::real (x) > 1.0f)
    return std::conj (y);
  else
    return y;
}

Complex
asin (const Complex& x)
{
  Complex y = std::asin (x);

  if (std::imag (x) == 0.0 && std::real (x) > 1.0)
    return std::conj (y);
  else
    return y;
}

FloatComplex
asin (const FloatComplex& x)
{
  FloatComplex y = std::asin (x);

  if (std::imag (x) == 0.0f && std::real (x) > 1.0f)
    return std::conj (y);
  else
    return y;
}

double frexp (double x, int *expptr)
{
  return octave_frexp_wrapper (x, expptr);
}

float frexp (float x, int *expptr)
{
  return octave_frexpf_wrapper (x, expptr);
}

Complex
log2 (const Complex& x)
{
  return std::log (x) / M_LN2;
}

FloatComplex
log2 (const FloatComplex& x)
{
  return std::log (x) / static_cast<float> (M_LN2);
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

bool negative_sign (double x) { return __lo_ieee_signbit (x); }
bool negative_sign (float x) { return __lo_ieee_float_signbit (x); }

// Sometimes you need a large integer, but not always.

octave_idx_type
nint_big (double x)
{
  static const double out_of_range_top
    = static_cast<double>(std::numeric_limits<octave_idx_type>::max ())+1.;
  if (x >= out_of_range_top)
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
  static const float out_of_range_top
    = static_cast<float>(std::numeric_limits<octave_idx_type>::max ())+1.;
  if (x >= out_of_range_top)
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
  static const float out_of_range_top
    = static_cast<float>(std::numeric_limits<int>::max ()) + 1.;
  if (x >= out_of_range_top)
    return std::numeric_limits<int>::max ();
  else if (x < std::numeric_limits<int>::min ())
    return std::numeric_limits<int>::min ();
  else
    return static_cast<int> ((x > 0.0f) ? (x + 0.5f) : (x - 0.5f));
}

Complex
rc_acos (double x)
{
  return fabs (x) > 1.0 ? acos (Complex (x)) : Complex (std::acos (x));
}

FloatComplex
rc_acos (float x)
{
  return fabsf (x) > 1.0f ? acos (FloatComplex (x))
         : FloatComplex (std::acos (x));
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
  return fabs (x) > 1.0 ? asin (Complex (x)) : Complex (std::asin (x));
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
  return x < 0.0 ? Complex (std::log (-x), M_PI) : Complex (std::log (x));
}

FloatComplex
rc_log (float x)
{
  return x < 0.0f ? FloatComplex (std::log (-x), static_cast<float> (M_PI))
         : FloatComplex (std::log (x));
}

Complex
rc_log2 (double x)
{
  constexpr double PI_LN2 = 4.53236014182719380962;  // = pi / log(2)
  return x < 0.0 ? Complex (log2 (-x), PI_LN2) : Complex (log2 (x));
}

FloatComplex
rc_log2 (float x)
{
  constexpr float PI_LN2 = 4.53236014182719380962f;  // = pi / log(2)
  return x < 0.0f ? FloatComplex (log2 (-x), PI_LN2)
         : FloatComplex (log2 (x));
}

Complex
rc_log10 (double x)
{
  constexpr double PI_LN10 = 1.36437635384184134748;  // = pi / log(10)
  return x < 0.0 ? Complex (log10 (-x), PI_LN10) : Complex (log10 (x));
}

FloatComplex
rc_log10 (float x)
{
  constexpr float PI_LN10 = 1.36437635384184134748f;  // = pi / log(10)
  return x < 0.0f ? FloatComplex (log10 (-x), PI_LN10)
         : FloatComplex (log10f (x));
}

Complex
rc_sqrt (double x)
{
  return x < 0.0 ? Complex (0.0, std::sqrt (-x)) : Complex (std::sqrt (x));
}

FloatComplex
rc_sqrt (float x)
{
  return x < 0.0f ? FloatComplex (0.0f, std::sqrt (-x))
         : FloatComplex (std::sqrt (x));
}

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
