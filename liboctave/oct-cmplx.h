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

// For complex-complex and complex-real comparisons, we use the following ordering:
// compare absolute values first; if they match, compare phase angles.
// This is partially inconsistent with M*b, which compares complex numbers only
// by their real parts; OTOH, it uses the same definition for max/min and sort.
// The abs/arg comparison is definitely more useful (the other one is emulated rather
// trivially), so let's be consistent and use that all over.

#define DEF_COMPLEXR_COMP(OP, OPS) \
template <class T> \
inline bool operator OP (const std::complex<T>& a, const std::complex<T>& b) \
{ \
  T ax = std::abs (a), bx = std::abs (b); \
  return ax OPS bx || (ax == bx && std::arg (a) OP std::arg (b)); \
} \
template <class T> \
inline bool operator OP (const std::complex<T>& a, T b) \
{ \
  T ax = std::abs (a); \
  return ax OPS b || (ax == b && std::arg (a) OP 0); \
} \
template <class T> \
inline bool operator OP (T a, const std::complex<T>& b) \
{ \
  T bx = std::abs (b); \
  return a OPS bx || (a == bx && 0 OP std::arg (b)); \
} \

DEF_COMPLEXR_COMP (>, >)
DEF_COMPLEXR_COMP (<, <)
DEF_COMPLEXR_COMP (<=, <=)
DEF_COMPLEXR_COMP (>=, >=)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
