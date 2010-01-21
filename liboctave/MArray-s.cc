/*

Copyright (C) 1995, 1996, 1997, 2000, 2005, 2007 John W. Eaton

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

// Instantiate MArrays of short int values.

#include "MArray.h"
#include "MArray.cc"

template class OCTAVE_API MArray<short>;

INSTANTIATE_MARRAY_FRIENDS (short, OCTAVE_API)

#include "MArray2.h"
#include "MArray2.cc"

template class OCTAVE_API MArray2<short>;

INSTANTIATE_MARRAY2_FRIENDS (short, OCTAVE_API)

#include "MDiagArray2.h"
#include "MDiagArray2.cc"

template class OCTAVE_API MDiagArray2<short>;

INSTANTIATE_MDIAGARRAY2_FRIENDS (short, OCTAVE_API)
