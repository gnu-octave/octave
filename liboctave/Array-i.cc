// Array-i.cc                                            -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Instantiate Arrays of integer values.

#include "Array.h"
#include "Array.cc"

template class ArrayRep<int>;
template class Array<int>;

// For now, we only need 1D arrays of integers.
//
// template class Array2<int>;
// template class DiagArray<int>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
