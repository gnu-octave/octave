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

// Instantiate Arrays of Complex values.

#include "oct-cmplx.h"

#include "Array.h"
#include "Array.cc"

INSTANTIATE_ARRAY_AND_ASSIGN (Complex, OCTAVE_API);

INSTANTIATE_ARRAY_ASSIGN (Complex, double, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (Complex, int, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (Complex, short, OCTAVE_API);
INSTANTIATE_ARRAY_ASSIGN (Complex, char, OCTAVE_API);

#include "Array2.h"

template class OCTAVE_API Array2<Complex>;

#include "ArrayN.h"
#include "ArrayN.cc"

template class OCTAVE_API ArrayN<Complex>;

template OCTAVE_API std::ostream& operator << (std::ostream&, const ArrayN<Complex>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<Complex>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
