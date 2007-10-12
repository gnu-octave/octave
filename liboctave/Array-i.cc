/*

Copyright (C) 1996, 1997 John W. Eaton

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

#include "oct-inttypes.h"

// Instantiate Arrays of integer values.

#include "Array.h"
#include "Array.cc"

INSTANTIATE_ARRAY_AND_ASSIGN (int, OCTAVE_API);
INSTANTIATE_ARRAY_AND_ASSIGN (long, OCTAVE_API);

INSTANTIATE_ARRAY_ASSIGN (int, short, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (int, char, OCTAVE_API);

INSTANTIATE_ARRAY_AND_ASSIGN (octave_int8, OCTAVE_API);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_int16, OCTAVE_API);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_int32, OCTAVE_API);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_int64, OCTAVE_API);

INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint8, OCTAVE_API);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint16, OCTAVE_API);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint32, OCTAVE_API);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint64, OCTAVE_API);

#include "Array2.h"

template class OCTAVE_API Array2<int>;

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<int>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
