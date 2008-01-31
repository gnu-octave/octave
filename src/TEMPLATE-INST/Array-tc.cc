/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2003, 2004, 2005, 2006,
              2007 John W. Eaton

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

// Instantiate Arrays of octave_values.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array.h"
#include "Array.cc"

#include "Array2.h"

#include "ArrayN.h"
#include "ArrayN.cc"

#include "DiagArray2.h"
#include "DiagArray2.cc"

#include "oct-obj.h"

#define IFLT(a, b) if (compare ? compare ((a), (b)) : true)

#include "oct-sort.cc"

template <>
bool
ascending_compare (octave_value a, octave_value b)
{
  return (a.string_value () < b.string_value ());
}

template <>
bool
ascending_compare (vec_index<octave_value> *a, vec_index<octave_value> *b)
{
  return (a->vec.string_value () < b->vec.string_value ());
}

template <>
bool
descending_compare (octave_value a, octave_value b)
{
  return (a.string_value () > b.string_value ());
}

template <>
bool
descending_compare (vec_index<octave_value> *a, vec_index<octave_value> *b)
{
  return (a->vec.string_value () > b->vec.string_value ());
}

INSTANTIATE_ARRAY_SORT (octave_value);

template class OCTINTERP_API Array<octave_value>;

INSTANTIATE_ARRAY_ASSIGN (octave_value, octave_value, OCTINTERP_API);

template class OCTINTERP_API Array2<octave_value>;

template class OCTINTERP_API ArrayN<octave_value>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
