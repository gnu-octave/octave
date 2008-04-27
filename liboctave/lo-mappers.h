/*

Copyright (C) 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

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

#if !defined (octave_liboctave_mappers_h)
#define octave_liboctave_mappers_h 1

#include "oct-cmplx.h"

// Double Precision 
extern OCTAVE_API double arg (double x);
extern OCTAVE_API double conj (double x);
extern OCTAVE_API double fix (double x);
extern OCTAVE_API double imag (double x);
extern OCTAVE_API double real (double x);
extern OCTAVE_API double xround (double x);
extern OCTAVE_API double xroundb (double x);
extern OCTAVE_API double signum (double x);
extern OCTAVE_API double xtrunc (double x);
extern OCTAVE_API double xlog2 (double x); 
extern OCTAVE_API Complex xlog2 (const Complex& x); 
extern OCTAVE_API double xlog2 (double x, int& exp);
extern OCTAVE_API Complex xlog2 (const Complex& x, int& exp);
extern OCTAVE_API double xexp2 (double x);

extern OCTAVE_API bool xisnan (double x);
extern OCTAVE_API bool xfinite (double x);
extern OCTAVE_API bool xisinf (double x);

extern OCTAVE_API bool octave_is_NA (double x);
extern OCTAVE_API bool octave_is_NaN_or_NA (double x) GCC_ATTR_DEPRECATED;

extern OCTAVE_API double xmin (double x, double y);
extern OCTAVE_API double xmax (double x, double y);

extern OCTAVE_API Complex acos (const Complex& x);
extern OCTAVE_API Complex acosh (const Complex& x);
extern OCTAVE_API Complex asin (const Complex& x);
extern OCTAVE_API Complex asinh (const Complex& x);
extern OCTAVE_API Complex atan (const Complex& x);
extern OCTAVE_API Complex atanh (const Complex& x);

extern OCTAVE_API Complex ceil (const Complex& x);
extern OCTAVE_API Complex fix (const Complex& x);
extern OCTAVE_API Complex floor (const Complex& x);
extern OCTAVE_API Complex xround (const Complex& x);
extern OCTAVE_API Complex xroundb (const Complex& x);
extern OCTAVE_API Complex signum (const Complex& x);

extern OCTAVE_API bool xisnan (const Complex& x);
extern OCTAVE_API bool xfinite (const Complex& x);
extern OCTAVE_API bool xisinf (const Complex& x);

extern OCTAVE_API bool octave_is_NA (const Complex& x);
extern OCTAVE_API bool octave_is_NaN_or_NA (const Complex& x);

extern OCTAVE_API Complex xmin (const Complex& x, const Complex& y);
extern OCTAVE_API Complex xmax (const Complex& x, const Complex& y);

// Single Precision 
extern OCTAVE_API float arg (float x);
extern OCTAVE_API float conj (float x);
extern OCTAVE_API float fix (float x);
extern OCTAVE_API float imag (float x);
extern OCTAVE_API float real (float x);
extern OCTAVE_API float xround (float x);
extern OCTAVE_API float xroundb (float x);
extern OCTAVE_API float signum (float x);
extern OCTAVE_API float xtrunc (float x);
extern OCTAVE_API float xlog2 (float x); 
extern OCTAVE_API FloatComplex xlog2 (const FloatComplex& x); 
extern OCTAVE_API float xlog2 (float x, int& exp);
extern OCTAVE_API FloatComplex xlog2 (const FloatComplex& x, int& exp);
extern OCTAVE_API float xexp2 (float x);

extern OCTAVE_API bool xisnan (float x);
extern OCTAVE_API bool xfinite (float x);
extern OCTAVE_API bool xisinf (float x);

extern OCTAVE_API bool octave_is_NA (float x);
extern OCTAVE_API bool octave_is_NaN_or_NA (float x) GCC_ATTR_DEPRECATED;

extern OCTAVE_API float xmin (float x, float y);
extern OCTAVE_API float xmax (float x, float y);

extern OCTAVE_API FloatComplex acos (const FloatComplex& x);
extern OCTAVE_API FloatComplex acosh (const FloatComplex& x);
extern OCTAVE_API FloatComplex asin (const FloatComplex& x);
extern OCTAVE_API FloatComplex asinh (const FloatComplex& x);
extern OCTAVE_API FloatComplex atan (const FloatComplex& x);
extern OCTAVE_API FloatComplex atanh (const FloatComplex& x);

extern OCTAVE_API FloatComplex ceil (const FloatComplex& x);
extern OCTAVE_API FloatComplex fix (const FloatComplex& x);
extern OCTAVE_API FloatComplex floor (const FloatComplex& x);
extern OCTAVE_API FloatComplex xround (const FloatComplex& x);
extern OCTAVE_API FloatComplex xroundb (const FloatComplex& x);
extern OCTAVE_API FloatComplex signum (const FloatComplex& x);

extern OCTAVE_API bool xisnan (const FloatComplex& x);
extern OCTAVE_API bool xfinite (const FloatComplex& x);
extern OCTAVE_API bool xisinf (const FloatComplex& x);

extern OCTAVE_API bool octave_is_NA (const FloatComplex& x);
extern OCTAVE_API bool octave_is_NaN_or_NA (const FloatComplex& x);

extern OCTAVE_API FloatComplex xmin (const FloatComplex& x, const FloatComplex& y);
extern OCTAVE_API FloatComplex xmax (const FloatComplex& x, const FloatComplex& y);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
