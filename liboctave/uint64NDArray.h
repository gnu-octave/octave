/*

Copyright (C) 2004, 2005, 2007, 2009 John W. Eaton

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

#if !defined (octave_uint64NDArray_h)
#define octave_uint64NDArray_h 1

#include "intNDArray.h"
#include "mx-op-decl.h"
#include "oct-inttypes.h"

typedef intNDArray<octave_uint64> uint64NDArray;

NDS_CMP_OP_DECLS (uint64NDArray, octave_uint64, OCTAVE_API)
NDS_BOOL_OP_DECLS (uint64NDArray, octave_uint64, OCTAVE_API)

SND_CMP_OP_DECLS (octave_uint64, uint64NDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (octave_uint64, uint64NDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (uint64NDArray, uint64NDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (uint64NDArray, uint64NDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArrayN, uint64NDArray, octave_uint64)

MINMAX_DECLS (uint64)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
