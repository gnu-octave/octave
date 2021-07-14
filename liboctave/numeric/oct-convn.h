////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2021 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_oct_convn_h)
#define octave_oct_convn_h 1

#include "octave-config.h"

class ColumnVector;
class RowVector;
class Matrix;
class NDArray;

class ComplexColumnVector;
class ComplexRowVector;
class ComplexMatrix;
class ComplexNDArray;

class FloatColumnVector;
class FloatRowVector;
class FloatMatrix;
class FloatNDArray;

class FloatComplexColumnVector;
class FloatComplexRowVector;
class FloatComplexMatrix;
class FloatComplexNDArray;

enum convn_type
{
  convn_full,
  convn_same,
  convn_valid
};

// double real X double real

extern OCTAVE_API NDArray
convn (const NDArray& a, const NDArray& b, convn_type ct);

extern OCTAVE_API Matrix
convn (const Matrix& a, const Matrix& b, convn_type ct);

extern OCTAVE_API Matrix
convn (const Matrix& a, const ColumnVector& c, const RowVector& r,
       convn_type ct);

// double complex X double real

extern OCTAVE_API ComplexNDArray
convn (const ComplexNDArray& a, const NDArray& b, convn_type ct);

extern OCTAVE_API ComplexMatrix
convn (const ComplexMatrix& a, const Matrix& b, convn_type ct);

extern OCTAVE_API ComplexMatrix
convn (const ComplexMatrix& a, const ColumnVector& c, const RowVector& r,
       convn_type ct);

// double complex X double complex

extern OCTAVE_API ComplexNDArray
convn (const ComplexNDArray& a, const ComplexNDArray& b, convn_type ct);

extern OCTAVE_API ComplexMatrix
convn (const ComplexMatrix& a, const ComplexMatrix& b, convn_type ct);

extern OCTAVE_API ComplexMatrix
convn (const ComplexMatrix& a, const ComplexColumnVector& c,
       const ComplexRowVector& r, convn_type ct);

// float real X float real

extern OCTAVE_API FloatNDArray
convn (const FloatNDArray& a, const FloatNDArray& b, convn_type ct);

extern OCTAVE_API FloatMatrix
convn (const FloatMatrix& a, const FloatMatrix& b, convn_type ct);

extern OCTAVE_API FloatMatrix
convn (const FloatMatrix& a, const FloatColumnVector& c,
       const FloatRowVector& r, convn_type ct);

// float complex X float real

extern OCTAVE_API FloatComplexNDArray
convn (const FloatComplexNDArray& a, const FloatNDArray& b, convn_type ct);

extern OCTAVE_API FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatMatrix& b, convn_type ct);

extern OCTAVE_API FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatColumnVector& c,
       const FloatRowVector& r, convn_type ct);

// float complex X float complex

extern OCTAVE_API FloatComplexNDArray
convn (const FloatComplexNDArray& a, const FloatComplexNDArray& b,
       convn_type ct);

extern OCTAVE_API FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
       convn_type ct);

extern OCTAVE_API FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatComplexColumnVector& c,
       const FloatComplexRowVector& r, convn_type ct);

#endif
