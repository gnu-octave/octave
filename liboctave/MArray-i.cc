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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-inttypes.h"

// Instantiate MArrays of int values.

#include "MArray.h"
#include "MArray.cc"

template class MArray<int>;
template class MArray<long>;

INSTANTIATE_MARRAY_FRIENDS (int)
INSTANTIATE_MARRAY_FRIENDS (long)

template class MArray<octave_int8>;
template class MArray<octave_int16>;
template class MArray<octave_int32>;
template class MArray<octave_int64>;

INSTANTIATE_MARRAY_FRIENDS (octave_int8)
INSTANTIATE_MARRAY_FRIENDS (octave_int16)
INSTANTIATE_MARRAY_FRIENDS (octave_int32)
INSTANTIATE_MARRAY_FRIENDS (octave_int64)

template class MArray<octave_uint8>;
template class MArray<octave_uint16>;
template class MArray<octave_uint32>;
template class MArray<octave_uint64>;

INSTANTIATE_MARRAY_FRIENDS (octave_uint8)
INSTANTIATE_MARRAY_FRIENDS (octave_uint16)
INSTANTIATE_MARRAY_FRIENDS (octave_uint32)
INSTANTIATE_MARRAY_FRIENDS (octave_uint64)

#include "MArray2.h"
#include "MArray2.cc"

template class MArray2<int>;

INSTANTIATE_MARRAY2_FRIENDS (int)

#include "MArrayN.h"
#include "MArrayN.cc"

template class MArrayN<int>;

INSTANTIATE_MARRAYN_FRIENDS (int)

template class MArrayN<octave_int8>;
template class MArrayN<octave_int16>;
template class MArrayN<octave_int32>;
template class MArrayN<octave_int64>;

INSTANTIATE_MARRAYN_FRIENDS (octave_int8)
INSTANTIATE_MARRAYN_FRIENDS (octave_int16)
INSTANTIATE_MARRAYN_FRIENDS (octave_int32)
INSTANTIATE_MARRAYN_FRIENDS (octave_int64)

template class MArrayN<octave_uint8>;
template class MArrayN<octave_uint16>;
template class MArrayN<octave_uint32>;
template class MArrayN<octave_uint64>;

INSTANTIATE_MARRAYN_FRIENDS (octave_uint8)
INSTANTIATE_MARRAYN_FRIENDS (octave_uint16)
INSTANTIATE_MARRAYN_FRIENDS (octave_uint32)
INSTANTIATE_MARRAYN_FRIENDS (octave_uint64)

#include "MDiagArray2.h"
#include "MDiagArray2.cc"

template class MDiagArray2<int>;

INSTANTIATE_MDIAGARRAY2_FRIENDS (int)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
