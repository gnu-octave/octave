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

#if !defined (octave_liboctave_specfun_h)
#define octave_liboctave_specfun_h 1

class ColumnVector;
class Matrix;
class Range;

#if !defined (HAVE_ACOSH)
extern double acosh (double);
#endif

#if !defined (HAVE_ASINH)
extern double asinh (double);
#endif

#if !defined (HAVE_ATANH)
extern double atanh (double);
#endif

#if !defined (HAVE_ERF)
extern double erf (double);
#endif

#if !defined (HAVE_ERFC)
extern double erfc (double);
#endif

#if !defined (HAVE_GAMMA)
extern double gamma (double);
#endif

#if !defined (HAVE_LGAMMA)
extern double lgamma (double);
#endif

extern Matrix besselj (double alpha, const Matrix& x);
extern Matrix bessely (double alpha, const Matrix& x);
extern Matrix besseli (double alpha, const Matrix& x);
extern Matrix besselk (double alpha, const Matrix& x);

extern Matrix besselj (const Range& alpha, const ColumnVector& x);
extern Matrix bessely (const Range& alpha, const ColumnVector& x);
extern Matrix besseli (const Range& alpha, const ColumnVector& x);
extern Matrix besselk (const Range& alpha, const ColumnVector& x);

extern double betainc (double x, double a, double b);
extern Matrix betainc (double x, double a, const Matrix& b);
extern Matrix betainc (double x, const Matrix& a, double b);
extern Matrix betainc (double x, const Matrix& a, const Matrix& b);

extern Matrix betainc (const Matrix& x, double a, double b);
extern Matrix betainc (const Matrix& x, double a, const Matrix& b);
extern Matrix betainc (const Matrix& x, const Matrix& a, double b);
extern Matrix betainc (const Matrix& x, const Matrix& a, const Matrix& b);

extern double gammainc (double x, double a);
extern Matrix gammainc (double x, const Matrix& a);
extern Matrix gammainc (const Matrix& x, double a);
extern Matrix gammainc (const Matrix& x, const Matrix& a);

#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
