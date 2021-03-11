////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2021 The Octave Project Developers
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

#if ! defined (octave_lo_ieee_h)
#define octave_lo_ieee_h 1

#include <cmath>

#include "octave-config.h"

#if defined (__cplusplus)
extern "C" {
#endif

/*  Octave's idea of infinity.  */
#define octave_Inf (lo_ieee_inf_value ())

/* Octave's idea of a missing value.  */
#define octave_NA (lo_ieee_na_value ())

/* Octave's idea of not a number.  */
#define octave_NaN (lo_ieee_nan_value ())

/*  Octave's idea of infinity.  */
#define octave_Float_Inf (lo_ieee_float_inf_value ())

/* Octave's idea of a missing value.  */
#define octave_Float_NA (lo_ieee_float_na_value ())

/* Octave's idea of not a number.  */
#define octave_Float_NaN (lo_ieee_float_nan_value ())

/* FIXME: This code assumes that a double has twice the
          number of bits as an int */

typedef union
{
  double value;
  unsigned int word[2];
} lo_ieee_double;

typedef union
{
  float value;
  unsigned int word;
} lo_ieee_float;

#if defined (HAVE_MIPS_NAN)
  #define LO_IEEE_NA_HW 0x7FF040F4
#else
  #define LO_IEEE_NA_HW 0x7FF840F4
#endif
#define LO_IEEE_NA_LW 0x40000000
#define LO_IEEE_NA_FLOAT   0x7FC207A2

extern OCTAVE_API void octave_ieee_init (void);

inline int __lo_ieee_isnan (double x) { return std::isnan (x); }
inline int __lo_ieee_finite (double x) { return std::isfinite (x); }
inline int __lo_ieee_isinf (double x) { return std::isinf (x); }

extern OCTAVE_API int __lo_ieee_is_NA (double);

extern OCTAVE_API double lo_ieee_inf_value (void);
extern OCTAVE_API double lo_ieee_na_value (void);
extern OCTAVE_API double lo_ieee_nan_value (void);

inline int __lo_ieee_signbit (double x) { return std::signbit (x); }

inline int __lo_ieee_float_isnan (float x) { return std::isnan (x); }
inline int __lo_ieee_float_finite (float x) { return std::isfinite (x); }
inline int __lo_ieee_float_isinf (float x) { return std::isinf (x); }

extern OCTAVE_API int __lo_ieee_float_is_NA (float);

extern OCTAVE_API float lo_ieee_float_inf_value (void);
extern OCTAVE_API float lo_ieee_float_na_value (void);
extern OCTAVE_API float lo_ieee_float_nan_value (void);

inline int __lo_ieee_float_signbit (float x) { return std::signbit (x); }

#if defined (__cplusplus)
}
#endif

#define lo_ieee_isnan(x)                                \
  (sizeof (x) == sizeof (float)                         \
   ? __lo_ieee_float_isnan (x) : __lo_ieee_isnan (x))

#define lo_ieee_finite(x)                               \
  (sizeof (x) == sizeof (float)                         \
   ? __lo_ieee_float_finite (x) : __lo_ieee_finite (x))

#define lo_ieee_isinf(x)                                \
  (sizeof (x) == sizeof (float)                         \
   ? __lo_ieee_float_isinf (x) : __lo_ieee_isinf (x))

#define lo_ieee_is_NA(x)                                \
  (sizeof (x) == sizeof (float)                         \
   ? __lo_ieee_float_is_NA (x) : __lo_ieee_is_NA (x))

#define lo_ieee_is_NaN_or_NA(x)                                         \
  (sizeof (x) == sizeof (float)                                         \
   ? __lo_ieee_float_is_NaN_or_NA (x) : __lo_ieee_is_NaN_or_NA (x))

#define lo_ieee_signbit(x)                                      \
  (sizeof (x) == sizeof (float)                                 \
   ? __lo_ieee_float_signbit (x) : __lo_ieee_signbit (x))

#if defined (__cplusplus)

namespace octave
{
  template <typename T>
  struct numeric_limits
  {
    static T NA (void) { return static_cast<T> (0); }
    static T NaN (void) { return static_cast<T> (0); }
    static T Inf (void) { return static_cast<T> (0); }
  };

  template <>
  struct numeric_limits<double>
  {
    static double NA (void) { return octave_NA; }
    static double NaN (void) { return octave_NaN; }
    static double Inf (void) { return octave_Inf; }
  };

  template <>
  struct numeric_limits<float>
  {
    static float NA (void) { return octave_Float_NA; }
    static float NaN (void) { return octave_Float_NaN; }
    static float Inf (void) { return octave_Float_Inf; }
  };
}

#endif

#endif
