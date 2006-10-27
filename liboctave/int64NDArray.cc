// N-D Array  manipulations.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "int64NDArray.h"

#include "intNDArray.cc"

template class OCTAVE_API intNDArray<octave_int64>;

template OCTAVE_API
std::ostream&
operator << (std::ostream& os, const intNDArray<octave_int64>& a);

template OCTAVE_API
std::istream&
operator >> (std::istream& is, intNDArray<octave_int64>& a);

NDS_CMP_OPS (int64NDArray, , octave_int64, )
NDS_BOOL_OPS (int64NDArray, octave_int64, octave_int64 (0))

SND_CMP_OPS (octave_int64, , int64NDArray, )
SND_BOOL_OPS (octave_int64, int64NDArray, octave_int64 (0))

NDND_CMP_OPS (int64NDArray, , int64NDArray, )
NDND_BOOL_OPS (int64NDArray, int64NDArray, octave_int64 (0))

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
