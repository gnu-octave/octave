/*

Copyright (C) 1996, 1997, 2002, 2003, 2004, 2005, 2006, 2007, 2008
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

#if !defined (octave_liboctave_ieee_h)
#define octave_liboctave_ieee_h 1

#ifdef	__cplusplus
extern "C" {
#endif

/*  Octave's idea of infinity.  */
extern OCTAVE_API double octave_Inf;

/* Octave's idea of a missing value.  */
extern OCTAVE_API double octave_NA;

/* Octave's idea of not a number.  */
extern OCTAVE_API double octave_NaN;

/*  Octave's idea of infinity.  */
extern OCTAVE_API float octave_Float_Inf;

/* Octave's idea of a missing value.  */
extern OCTAVE_API float octave_Float_NA;

/* Octave's idea of not a number.  */
extern OCTAVE_API float octave_Float_NaN;

/* FIXME -- this code assumes that a double has twice the
   number of bits as an int */

extern OCTAVE_API int lo_ieee_hw;
extern OCTAVE_API int lo_ieee_lw;

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

#define LO_IEEE_NA_HW_OLD 0x7ff00000
#define LO_IEEE_NA_LW_OLD 1954
#define LO_IEEE_NA_HW 0x7FF840F4
#define LO_IEEE_NA_LW 0x40000000
#define LO_IEEE_NA_FLOAT   0x7FC207A2
 

extern OCTAVE_API void octave_ieee_init (void);

#if defined (SCO)
extern int __isnan (double);
extern int __isinf (double);
extern int __isnanf (float);
extern int __isinff (float);

#define isnan(x) (sizeof (x) == sizeof (float) ? __isnanf (x) : __isnan (x))
#define isinf(x) (sizeof (x) == sizeof (float) ? __isinff (x) : __isinf (x))
#endif

extern OCTAVE_API int __lo_ieee_isnan (double x);
extern OCTAVE_API int __lo_ieee_finite (double x);
extern OCTAVE_API int __lo_ieee_isinf (double x);

extern OCTAVE_API int __lo_ieee_is_NA (double);
extern OCTAVE_API int __lo_ieee_is_old_NA (double);
extern OCTAVE_API int __lo_ieee_is_NaN_or_NA (double) GCC_ATTR_DEPRECATED;
extern OCTAVE_API double __lo_ieee_replace_old_NA (double);

extern OCTAVE_API double lo_ieee_inf_value (void);
extern OCTAVE_API double lo_ieee_na_value (void);
extern OCTAVE_API double lo_ieee_nan_value (void);

extern OCTAVE_API int __lo_ieee_signbit (double);

extern OCTAVE_API int __lo_ieee_float_isnan (float x);
extern OCTAVE_API int __lo_ieee_float_finite (float x);
extern OCTAVE_API int __lo_ieee_float_isinf (float x);

extern OCTAVE_API int __lo_ieee_float_is_NA (float);
extern OCTAVE_API int __lo_ieee_float_is_NaN_or_NA (float) GCC_ATTR_DEPRECATED;

extern OCTAVE_API float lo_ieee_float_inf_value (void);
extern OCTAVE_API float lo_ieee_float_na_value (void);
extern OCTAVE_API float lo_ieee_float_nan_value (void);

extern OCTAVE_API int __lo_ieee_float_signbit (float);

#ifdef	__cplusplus
}
#endif

#define lo_ieee_isnan(x) (sizeof (x) == sizeof (float) ? \
			 __lo_ieee_float_isnan (x) : __lo_ieee_isnan (x))
#define lo_ieee_finite(x) (sizeof (x) == sizeof (float) ? \
			   __lo_ieee_float_finite (x) : __lo_ieee_finite (x))
#define lo_ieee_isinf(x) (sizeof (x) == sizeof (float) ? \
			  __lo_ieee_float_isinf (x) : __lo_ieee_isinf (x))


#define lo_ieee_is_NA(x) (sizeof (x) == sizeof (float) ? \
			  __lo_ieee_float_is_NA (x) : __lo_ieee_is_NA (x))
#define lo_ieee_is_NaN_or_NA(x) (sizeof (x) == sizeof (float) ? \
			  __lo_ieee_float_is_NaN_or_NA (x) : __lo_ieee_is_NaN_or_NA (x))
#define lo_ieee_signbit(x) (sizeof (x) == sizeof (float) ? \
			  __lo_ieee_float_signbit (x) : __lo_ieee_signbit (x))

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
