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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-inttypes.h"

// Instantiate Arrays of integer values.

#include "Array.h"
#include "Array.cc"

INSTANTIATE_ARRAY_AND_ASSIGN (int);
INSTANTIATE_ARRAY_AND_ASSIGN (long);

INSTANTIATE_ARRAY_ASSIGN (int, short);
INSTANTIATE_ARRAY_ASSIGN (int, char);

INSTANTIATE_ARRAY_AND_ASSIGN (octave_int8);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_int16);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_int32);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_int64);

INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint8);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint16);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint32);
INSTANTIATE_ARRAY_AND_ASSIGN (octave_uint64);

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
