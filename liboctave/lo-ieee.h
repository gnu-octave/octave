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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_liboctave_ieee_h)
#define octave_liboctave_ieee_h 1

#ifdef	__cplusplus
extern "C" {
#endif

/*  Octave's idea of infinity.  */
extern double octave_Inf;

/* Octave's idea of a missing value.  */
extern double octave_NA;

/* Octave's idea of not a number.  */
extern double octave_NaN;

/* FIXME -- this code assumes that a double has twice the
   number of bits as an int */

extern int lo_ieee_hw;
extern int lo_ieee_lw;

typedef union
{
  double value;
  unsigned int word[2];
} lo_ieee_double;

#define LO_IEEE_NA_HW 0x7ff00000
#define LO_IEEE_NA_LW 1954

extern void octave_ieee_init (void);

#if defined (SCO)
extern int isnan (double);
extern int isinf (double);
#endif

extern int lo_ieee_isnan (double x);
extern int lo_ieee_finite (double x);
extern int lo_ieee_isinf (double x);

extern int lo_ieee_is_NA (double);
extern int lo_ieee_is_NaN_or_NA (double) GCC_ATTR_DEPRECATED;

extern double lo_ieee_inf_value (void);
extern double lo_ieee_na_value (void);
extern double lo_ieee_nan_value (void);

extern int lo_ieee_signbit (double);

#ifdef	__cplusplus
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
