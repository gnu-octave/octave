/*

Copyright (C) 2004 John W. Eaton

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

#if !defined (octave_int8NDArray_h)
#define octave_int8NDArray_h 1

#include "intNDArray.h"
#include "mx-op-defs.h"
#include "oct-inttypes.h"

typedef intNDArray<octave_int8> int8NDArray;

NDS_CMP_OP_DECLS (int8NDArray, octave_int8, OCTAVE_API)
NDS_BOOL_OP_DECLS (int8NDArray, octave_int8, OCTAVE_API)

SND_CMP_OP_DECLS (octave_int8, int8NDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (octave_int8, int8NDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (int8NDArray, int8NDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (int8NDArray, int8NDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArrayN, int8NDArray, octave_int8)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
