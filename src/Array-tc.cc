// Array-tc.cc                                            -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

// Instantiate Arrays of tree_constants.

#include "Array.h"
#include "Array.cc"

#include "tree-const.h"

extern template class ArrayRep<int>;
extern template class Array<int>;
extern template class Array2<int>;
extern template class DiagArray<int>;

extern template class ArrayRep<double>;
extern template class Array<double>;
extern template class Array2<double>;
extern template class DiagArray<double>;

extern template class ArrayRep<Complex>;
extern template class Array<Complex>;
extern template class Array2<Complex>;
extern template class DiagArray<Complex>;

template class ArrayRep<tree_constant>;
template class Array<tree_constant>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
