// MArray-s.cc                                            -*- C++ -*-
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

// Instantiate MArrays of short int values.

#define NO_DIAG_ARRAY 1

#include "MArray.h"
#include "MArray.cc"

template class MArray<short>;
template class MArray2<short>;

#ifndef NO_DIAG_ARRAY
template class MDiagArray<short>;
#endif

template MArray<short>
operator + (const MArray<short>& a, const short& s);

template MArray<short>
operator - (const MArray<short>& a, const short& s);

template MArray<short>
operator * (const MArray<short>& a, const short& s);

template MArray<short>
operator / (const MArray<short>& a, const short& s);

template MArray<short>
operator + (const short& s, const MArray<short>& a);

template MArray<short>
operator - (const short& s, const MArray<short>& a);

template MArray<short>
operator * (const short& s, const MArray<short>& a);

template MArray<short>
operator / (const short& s, const MArray<short>& a);

template MArray<short>
operator + (const MArray<short>& a, const MArray<short>& b);

template MArray<short>
operator - (const MArray<short>& a, const MArray<short>& b);

template MArray<short>
product (const MArray<short>& a, const MArray<short>& b); 

template MArray<short>
quotient (const MArray<short>& a, const MArray<short>& b);

template MArray<short>
operator - (const MArray<short>& a);

template MArray2<short>
operator + (const MArray2<short>& a, const short& s);

template MArray2<short>
operator - (const MArray2<short>& a, const short& s);

template MArray2<short>
operator * (const MArray2<short>& a, const short& s);

template MArray2<short>
operator / (const MArray2<short>& a, const short& s);

template MArray2<short>
operator + (const short& s, const MArray2<short>& a);

template MArray2<short>
operator - (const short& s, const MArray2<short>& a);

template MArray2<short>
operator * (const short& s, const MArray2<short>& a);

template MArray2<short>
operator / (const short& s, const MArray2<short>& a);

template MArray2<short>
operator + (const MArray2<short>& a, const MArray2<short>& b);

template MArray2<short>
operator - (const MArray2<short>& a, const MArray2<short>& b);

template MArray2<short>
product (const MArray2<short>& a, const MArray2<short>& b);

template MArray2<short>
quotient (const MArray2<short>& a, const MArray2<short>& b);

template MArray2<short>
operator - (const MArray2<short>& a);

#ifndef NO_DIAG_ARRAY
template MDiagArray<short>
operator * (const MDiagArray<short>& a, const short& s);

template MDiagArray<short>
operator / (const MDiagArray<short>& a, const short& s);

template MDiagArray<short>
operator * (const short& s, const MDiagArray<short>& a);

template MDiagArray<short>
operator + (const MDiagArray<short>& a, const MDiagArray<short>& b);

template MDiagArray<short>
operator - (const MDiagArray<short>& a, const MDiagArray<short>& b);

template MDiagArray<short>
product (const MDiagArray<short>& a, const MDiagArray<short>& b);

template MDiagArray<short>
operator - (const MDiagArray<short>& a);
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
