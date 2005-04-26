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

#if !defined (octave_mx_defs_h)
#define octave_mx_defs_h 1

// Classes we declare.

class Matrix;
class ComplexMatrix;
class boolMatrix;
class charMatrix;

class NDArray;
class ComplexNDArray;
class boolNDArray;
class charNDArray;

class ColumnVector;
class ComplexColumnVector;

class RowVector;
class ComplexRowVector;

class DiagMatrix;
class ComplexDiagMatrix;

class AEPBALANCE;
class ComplexAEPBALANCE;

class GEPBALANCE;

class CHOL;
class ComplexCHOL;

class DET;
class ComplexDET;

class EIG;

class HESS;
class ComplexHESS;

class SCHUR;
class ComplexSCHUR;

class SVD;
class ComplexSVD;

class LU;
class ComplexLU;

class QR;
class ComplexQR;

// Other data types we use but that don't always need to have full
// declarations.

#include "oct-cmplx.h"

#ifndef MAPPER_FCN_TYPEDEFS
#define MAPPER_FCN_TYPEDEFS 1

typedef bool (*b_d_Mapper)(double);
typedef bool (*b_c_Mapper)(const Complex&);

typedef double (*d_d_Mapper)(double);
typedef double (*d_c_Mapper)(const Complex&);
typedef Complex (*c_c_Mapper)(const Complex&);

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
