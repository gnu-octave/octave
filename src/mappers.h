// mappers.h                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (_mappers_h)
#define _mappers_h 1

class Complex;

#include "missing-math.h"

extern double arg (double x);
extern double conj (double x);
extern double fix (double x);
extern double imag (double x);
extern double real (double x);
extern double round (double x);
extern double signum (double x);
extern double xfinite (double x);
extern double xisinf (double x);
extern double xisnan (double x);

extern double xfinite (const Complex& x);
extern double xisinf (const Complex& x);
extern double xisnan (const Complex& x);

extern Complex acos (const Complex& x);
extern Complex acosh (const Complex& x);
extern Complex asin (const Complex& x);
extern Complex asinh (const Complex& x);
extern Complex atan (const Complex& x);
extern Complex atanh (const Complex& x);
extern Complex ceil (const Complex& x);
extern Complex fix (const Complex& x);
extern Complex floor (const Complex& x);
extern Complex log10 (const Complex& x);
extern Complex round (const Complex& x);
extern Complex signum (const Complex& x);
extern Complex tan (const Complex& x);
extern Complex tanh (const Complex& x);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
