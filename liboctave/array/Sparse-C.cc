////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

// Instantiate Sparse matrix of complex values.

#include "oct-cmplx.h"
#include "lo-mappers.h"
#include "lo-ieee.h"
#include "Array.h"
#include "Sparse.h"
#include "Sparse.cc"


static double
xabs (const Complex& x)
{
  return ((octave::math::isinf (x.real ()) || octave::math::isinf (x.imag ()))
          ? octave::numeric_limits<double>::Inf () : abs (x));
}

template <>
OCTAVE_API
bool
sparse_ascending_compare<Complex> (const Complex& a, const Complex& b)
{
  return (octave::math::isnan (b) || (xabs (a) < xabs (b))
          || ((xabs (a) == xabs (b)) && (arg (a) < arg (b))));
}

template <>
OCTAVE_API
bool
sparse_descending_compare<Complex> (const Complex& a, const Complex& b)
{
  return (octave::math::isnan (a) || (xabs (a) > xabs (b))
          || ((xabs (a) == xabs (b)) && (arg (a) > arg (b))));
}

INSTANTIATE_SPARSE (Complex);

#if 0
template std::ostream& operator << (std::ostream&, const Sparse<Complex>&);
#endif
