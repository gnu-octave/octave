/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_BESSEL_h)
#define octave_BESSEL_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ColumnVector;
class Matrix;
class Range;

extern Matrix besselj (double alpha, const Matrix& x);
extern Matrix bessely (double alpha, const Matrix& x);
extern Matrix besseli (double alpha, const Matrix& x);
extern Matrix besselk (double alpha, const Matrix& x);

extern Matrix besselj (const Range& alpha, const ColumnVector& x);
extern Matrix bessely (const Range& alpha, const ColumnVector& x);
extern Matrix besseli (const Range& alpha, const ColumnVector& x);
extern Matrix besselk (const Range& alpha, const ColumnVector& x);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
