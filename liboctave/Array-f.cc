/*

Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2003, 2004,
              2005, 2006, 2007 John W. Eaton

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

// Instantiate Arrays of float values.

#include "lo-ieee.h"
#include "Array.h"
#include "Array.cc"
#include "oct-locbuf.h"

#define INLINE_ASCENDING_SORT
#define INLINE_DESCENDING_SORT
#include "oct-sort.cc"

template <>
inline bool _sort_isnan (float x)
{
  return lo_ieee_isnan (x);
}

INSTANTIATE_ARRAY_SORT (double);

INSTANTIATE_ARRAY_AND_ASSIGN (float, OCTAVE_API);

INSTANTIATE_ARRAY_ASSIGN (float, int, OCTAVE_API)
INSTANTIATE_ARRAY_ASSIGN (float, short, OCTAVE_API)
INSTANTIATE_ARRAY_ASSIGN (float, char, OCTAVE_API)

#include "Array2.h"

template class OCTAVE_API Array2<float>;

#include "ArrayN.h"
#include "ArrayN.cc"

template class OCTAVE_API ArrayN<float>;

template OCTAVE_API std::ostream& operator << (std::ostream&, const ArrayN<float>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<float>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
