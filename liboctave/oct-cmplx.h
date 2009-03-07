/*

Copyright (C) 1995, 1996, 1997, 2000, 2001, 2004, 2005, 2007, 2008, 2009
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

#if !defined (octave_oct_cmplx_h)
#define octave_oct_cmplx_h 1

#include <complex>

typedef std::complex<double> Complex;
typedef std::complex<float> FloatComplex;

// The default comparison of complex number is to compare by abs, then by arg.
// FIXME: this could be speeded up significantly.
template <class T>
inline bool operator < (const std::complex<T>& a,
                        const std::complex<T>& b)
{
  T ax = std::abs (a), bx = std::abs (b);
  return ax < bx || (ax == bx && std::arg (a) < std::arg (b));
}

template <class T>
inline bool operator > (const std::complex<T>& a,
                        const std::complex<T>& b)
{
  T ax = std::abs (a), bx = std::abs (b);
  return ax > bx || (ax == bx && std::arg (a) > std::arg (b));
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
