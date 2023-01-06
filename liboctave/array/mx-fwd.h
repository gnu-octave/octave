////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2023 The Octave Project Developers
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

#if ! defined (octave_mx_fwd_h)
#define octave_mx_fwd_h 1

#include "octave-config.h"

class OCTAVE_API Matrix;
class OCTAVE_API ComplexMatrix;
class OCTAVE_API FloatMatrix;
class OCTAVE_API FloatComplexMatrix;
class OCTAVE_API boolMatrix;
class OCTAVE_API charMatrix;

class OCTAVE_API NDArray;
class OCTAVE_API ComplexNDArray;
class OCTAVE_API FloatNDArray;
class OCTAVE_API FloatComplexNDArray;
class OCTAVE_API boolNDArray;
class OCTAVE_API charNDArray;

class OCTAVE_API ColumnVector;
class OCTAVE_API ComplexColumnVector;
class OCTAVE_API FloatColumnVector;
class OCTAVE_API FloatComplexColumnVector;

class OCTAVE_API RowVector;
class OCTAVE_API ComplexRowVector;
class OCTAVE_API FloatRowVector;
class OCTAVE_API FloatComplexRowVector;

class OCTAVE_API SparseMatrix;
class OCTAVE_API SparseComplexMatrix;
class OCTAVE_API SparseBoolMatrix;

class OCTAVE_API DiagMatrix;
class OCTAVE_API ComplexDiagMatrix;
class OCTAVE_API FloatDiagMatrix;
class OCTAVE_API FloatComplexDiagMatrix;

class OCTAVE_API PermMatrix;

#endif
