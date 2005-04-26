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

#if !defined (octave_mx_base_h)
#define octave_mx_base_h 1

// Matrix classes.

#include "boolMatrix.h"
#include "chMatrix.h"
#include "dMatrix.h"
#include "CMatrix.h"

// Column Vector classes.

#include "dColVector.h"
#include "CColVector.h"

// Row Vector classes.

#include "dRowVector.h"
#include "CRowVector.h"

// Diagonal Matrix classes.

#include "dDiagMatrix.h"
#include "CDiagMatrix.h"

// Sparse Matrix classes.

#include "boolSparse.h"
#include "dSparse.h"
#include "CSparse.h"

// N-d Array classes.

#include "boolNDArray.h"
#include "chNDArray.h"
#include "dNDArray.h"
#include "CNDArray.h"

#include "int8NDArray.h"
#include "int16NDArray.h"
#include "int32NDArray.h"
#include "int64NDArray.h"

#include "uint8NDArray.h"
#include "uint16NDArray.h"
#include "uint32NDArray.h"
#include "uint64NDArray.h"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
