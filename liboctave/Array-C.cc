// Array-C.cc                                            -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// Instantiate Arrays of Complex values.

#include "Array.h"
#include "Array.cc"

#include "oct-cmplx.h"

template class Array<Complex>;
template class Array2<Complex>;
template class DiagArray<Complex>;

template void assign (Array<Complex>&, const Array<Complex>&);
template void assign (Array<Complex>&, const Array<double>&);
template void assign (Array<Complex>&, const Array<int>&);
template void assign (Array<Complex>&, const Array<short>&);
template void assign (Array<Complex>&, const Array<char>&);

template void assign (Array2<Complex>&, const Array2<Complex>&);
template void assign (Array2<Complex>&, const Array2<double>&);
template void assign (Array2<Complex>&, const Array2<int>&);
template void assign (Array2<Complex>&, const Array2<short>&);
template void assign (Array2<Complex>&, const Array2<char>&);

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
