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

extern OCTAVE_API double arg (double x);
extern OCTAVE_API double conj (double x);
extern OCTAVE_API double fix (double x);
extern OCTAVE_API double imag (double x);
extern OCTAVE_API double real (double x);
extern OCTAVE_API double xround (double x);
extern OCTAVE_API double signum (double x);
extern OCTAVE_API double xlog2 (double x); 
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
extern OCTAVE_API Complex signum (const Complex& x);

extern OCTAVE_API bool xisnan (const Complex& x);
extern OCTAVE_API bool xfinite (const Complex& x);
extern OCTAVE_API bool xisinf (const Complex& x);

extern OCTAVE_API bool octave_is_NA (const Complex& x);
extern OCTAVE_API bool octave_is_NaN_or_NA (const Complex& x);

extern OCTAVE_API Complex xmin (const Complex& x, const Complex& y);
extern OCTAVE_API Complex xmax (const Complex& x, const Complex& y);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
