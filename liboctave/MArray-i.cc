// MArray-i.cc                                            -*- C++ -*-
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

// Instantiate MArrays of int values.

#define NO_DIAG_ARRAY 1

#include "MArray.h"
#include "MArray.cc"

template class MArray<int>;
template class MArray2<int>;

#ifndef NO_DIAG_ARRAY
template class MDiagArray<int>;
#endif

template MArray<int>
operator + (const MArray<int>& a, const int& s);

template MArray<int>
operator - (const MArray<int>& a, const int& s);

template MArray<int>
operator * (const MArray<int>& a, const int& s);

template MArray<int>
operator / (const MArray<int>& a, const int& s);

template MArray<int>
operator + (const int& s, const MArray<int>& a);

template MArray<int>
operator - (const int& s, const MArray<int>& a);

template MArray<int>
operator * (const int& s, const MArray<int>& a);

template MArray<int>
operator / (const int& s, const MArray<int>& a);

template MArray<int>
operator + (const MArray<int>& a, const MArray<int>& b);

template MArray<int>
operator - (const MArray<int>& a, const MArray<int>& b);

template MArray<int>
product (const MArray<int>& a, const MArray<int>& b); 

template MArray<int>
quotient (const MArray<int>& a, const MArray<int>& b);

template MArray<int>
operator - (const MArray<int>& a);

template MArray2<int>
operator + (const MArray2<int>& a, const int& s);

template MArray2<int>
operator - (const MArray2<int>& a, const int& s);

template MArray2<int>
operator * (const MArray2<int>& a, const int& s);

template MArray2<int>
operator / (const MArray2<int>& a, const int& s);

template MArray2<int>
operator + (const int& s, const MArray2<int>& a);

template MArray2<int>
operator - (const int& s, const MArray2<int>& a);

template MArray2<int>
operator * (const int& s, const MArray2<int>& a);

template MArray2<int>
operator / (const int& s, const MArray2<int>& a);

template MArray2<int>
operator + (const MArray2<int>& a, const MArray2<int>& b);

template MArray2<int>
operator - (const MArray2<int>& a, const MArray2<int>& b);

template MArray2<int>
product (const MArray2<int>& a, const MArray2<int>& b);

template MArray2<int>
quotient (const MArray2<int>& a, const MArray2<int>& b);

template MArray2<int>
operator - (const MArray2<int>& a);

#ifndef NO_DIAG_ARRAY
template MDiagArray<int>
operator * (const MDiagArray<int>& a, const int& s);

template MDiagArray<int>
operator / (const MDiagArray<int>& a, const int& s);

template MDiagArray<int>
operator * (const int& s, const MDiagArray<int>& a);

template MDiagArray<int>
operator + (const MDiagArray<int>& a, const MDiagArray<int>& b);

template MDiagArray<int>
operator - (const MDiagArray<int>& a, const MDiagArray<int>& b);

template MDiagArray<int>
product (const MDiagArray<int>& a, const MDiagArray<int>& b);

template MDiagArray<int>
operator - (const MDiagArray<int>& a);
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
