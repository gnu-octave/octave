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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_liboctave_mappers_h)
#define octave_liboctave_mappers_h 1

#include "oct-cmplx.h"

extern double arg (double x);
extern double conj (double x);
extern double fix (double x);
extern double imag (double x);
extern double real (double x);
extern double xround (double x);
extern double signum (double x);

extern bool xisnan (double x);
extern bool xfinite (double x);
extern bool xisinf (double x);

extern bool octave_is_NA (double x);
extern bool octave_is_NaN_or_NA (double x);

extern double xmin (double x, double y);
extern double xmax (double x, double y);

extern Complex acos (const Complex& x);
extern Complex acosh (const Complex& x);
extern Complex asin (const Complex& x);
extern Complex asinh (const Complex& x);
extern Complex atan (const Complex& x);
extern Complex atanh (const Complex& x);

extern Complex ceil (const Complex& x);
extern Complex fix (const Complex& x);
extern Complex floor (const Complex& x);
extern Complex xround (const Complex& x);
extern Complex signum (const Complex& x);

extern bool xisnan (const Complex& x);
extern bool xfinite (const Complex& x);
extern bool xisinf (const Complex& x);

extern bool octave_is_NA (const Complex& x);
extern bool octave_is_NaN_or_NA (const Complex& x);

extern Complex xmin (const Complex& x, const Complex& y);
extern Complex xmax (const Complex& x, const Complex& y);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
