/*

Copyright (C) 1994, 1995, 1996, 1997, 1998, 2001, 2003, 2004, 2005,
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// Instantiate Arrays of FloatComplex values.

#include "oct-cmplx.h"

#include "Array.h"
#include "Array.cc"

static float
xabs (const FloatComplex& x)
{
  return (xisinf (x.real ()) || xisinf (x.imag ())) ? octave_Float_Inf : abs (x);
}

static bool
operator < (const FloatComplex& a, const FloatComplex& b)
{
  return (xisnan (b) || (xabs (a) < xabs (b))
	  || ((xabs (a) == xabs (b)) && (arg (a) < arg (b))));
}

static bool
operator > (const FloatComplex& a, const FloatComplex& b)
{
  return (xisnan (a) || (xabs (a) > xabs (b))
	  || ((xabs (a) == xabs (b)) && (arg (a) > arg (b))));
}

// This file must be included after the < and > operators are
// defined to avoid errors with the Intel C++ compiler.
#include "oct-sort.cc"

template <>
bool
ascending_compare (FloatComplex a, FloatComplex b)
{
  return (xisnan (b) || (xabs (a) < xabs (b))
	  || ((xabs (a) == xabs (b)) && (arg (a) < arg (b))));
}

template <>
bool
ascending_compare (vec_index<FloatComplex> *a, vec_index<FloatComplex> *b)
{
  return (xisnan (b->vec)
	  || (xabs (a->vec) < xabs (b->vec))
	  || ((xabs (a->vec) == xabs (b->vec))
	      && (arg (a->vec) < arg (b->vec))));
}

template <>
bool
descending_compare (FloatComplex a, FloatComplex b)
{
  return (xisnan (a) || (xabs (a) > xabs (b))
	  || ((xabs (a) == xabs (b)) && (arg (a) > arg (b))));
}

template <>
bool
descending_compare (vec_index<FloatComplex> *a, vec_index<FloatComplex> *b)
{
  return (xisnan (a->vec)
	  || (xabs (a->vec) > xabs (b->vec))
	  || ((xabs (a->vec) == xabs (b->vec))
	      && (arg (a->vec) > arg (b->vec))));
}

INSTANTIATE_ARRAY_SORT (FloatComplex);

INSTANTIATE_ARRAY_AND_ASSIGN (FloatComplex, OCTAVE_API);

INSTANTIATE_ARRAY_ASSIGN (FloatComplex, float, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (FloatComplex, int, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (FloatComplex, short, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (FloatComplex, char, OCTAVE_API);

#include "Array2.h"

template class OCTAVE_API Array2<FloatComplex>;

#include "ArrayN.h"
#include "ArrayN.cc"

template class OCTAVE_API ArrayN<FloatComplex>;

template OCTAVE_API std::ostream& operator << (std::ostream&, const ArrayN<FloatComplex>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<FloatComplex>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
