// MArray-C.cc                                            -*- C++ -*-
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

// Instantiate MArrays of Complex values.

#include "MArray.h"
#include "MArray.cc"

#include <Complex.h>

template class MArray<Complex>;
template class MArray2<Complex>;
template class MDiagArray<Complex>;

template MArray<Complex>
operator + (const MArray<Complex>& a, const Complex& s);

template MArray<Complex>
operator - (const MArray<Complex>& a, const Complex& s);

template MArray<Complex>
operator * (const MArray<Complex>& a, const Complex& s);

template MArray<Complex>
operator / (const MArray<Complex>& a, const Complex& s);

template MArray<Complex>
operator + (const Complex& s, const MArray<Complex>& a);

template MArray<Complex>
operator - (const Complex& s, const MArray<Complex>& a);

template MArray<Complex>
operator * (const Complex& s, const MArray<Complex>& a);

template MArray<Complex>
operator / (const Complex& s, const MArray<Complex>& a);

template MArray<Complex>
operator + (const MArray<Complex>& a, const MArray<Complex>& b);

template MArray<Complex>
operator - (const MArray<Complex>& a, const MArray<Complex>& b);

template MArray<Complex>
product (const MArray<Complex>& a, const MArray<Complex>& b);

template MArray<Complex>
quotient (const MArray<Complex>& a, const MArray<Complex>& b);

template MArray<Complex>
operator - (const MArray<Complex>& a);

template MArray2<Complex>
operator + (const MArray2<Complex>& a, const Complex& s);

template MArray2<Complex>
operator - (const MArray2<Complex>& a, const Complex& s);

template MArray2<Complex>
operator * (const MArray2<Complex>& a, const Complex& s);

template MArray2<Complex>
operator / (const MArray2<Complex>& a, const Complex& s);

template MArray2<Complex>
operator + (const Complex& s, const MArray2<Complex>& a);

template MArray2<Complex>
operator - (const Complex& s, const MArray2<Complex>& a);

template MArray2<Complex>
operator * (const Complex& s, const MArray2<Complex>& a);

template MArray2<Complex>
operator / (const Complex& s, const MArray2<Complex>& a);

template MArray2<Complex>
operator + (const MArray2<Complex>& a, const MArray2<Complex>& b);

template MArray2<Complex>
operator - (const MArray2<Complex>& a, const MArray2<Complex>& b);

template MArray2<Complex>
product (const MArray2<Complex>& a, const MArray2<Complex>& b);

template MArray2<Complex>
quotient (const MArray2<Complex>& a, const MArray2<Complex>& b);

template MArray2<Complex>
operator - (const MArray2<Complex>& a);

template MDiagArray<Complex>
operator * (const MDiagArray<Complex>& a, const Complex& s);

template MDiagArray<Complex>
operator / (const MDiagArray<Complex>& a, const Complex& s);

template MDiagArray<Complex>
operator * (const Complex& s, const MDiagArray<Complex>& a);

template MDiagArray<Complex>
operator + (const MDiagArray<Complex>& a, const MDiagArray<Complex>& b);

template MDiagArray<Complex>
operator - (const MDiagArray<Complex>& a, const MDiagArray<Complex>& b);

template MDiagArray<Complex>
product (const MDiagArray<Complex>& a, const MDiagArray<Complex>& b);

template MDiagArray<Complex>
operator - (const MDiagArray<Complex>& a);

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
