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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "uint16NDArray.h"

#include "intNDArray.cc"

template class intNDArray<octave_uint16>;

template
std::ostream&
operator << (std::ostream& os, const intNDArray<octave_uint16>& a);

template
std::istream&
operator >> (std::istream& is, intNDArray<octave_uint16>& a);

NDS_CMP_OPS (uint16NDArray, , octave_uint16, )
NDS_BOOL_OPS (uint16NDArray, octave_uint16, octave_uint16 (0))

SND_CMP_OPS (octave_uint16, , uint16NDArray, )
SND_BOOL_OPS (octave_uint16, uint16NDArray, octave_uint16 (0))

NDND_CMP_OPS (uint16NDArray, , uint16NDArray, )
NDND_BOOL_OPS (uint16NDArray, uint16NDArray, octave_uint16 (0))

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
