////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2021 The Octave Project Developers
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

#if ! defined (octave_oct_norm_h)
#define octave_oct_norm_h 1

#include "octave-config.h"

#include "oct-cmplx.h"

class Matrix;
class ColumnVector;
class RowVector;

class ComplexMatrix;
class ComplexColumnVector;
class ComplexRowVector;

class FloatMatrix;
class FloatColumnVector;
class FloatRowVector;

class FloatComplexMatrix;
class FloatComplexColumnVector;
class FloatComplexRowVector;

class SparseMatrix;
class SparseComplexMatrix;

extern OCTAVE_API double xnorm (const ColumnVector&, double p = 2);
extern OCTAVE_API double xnorm (const RowVector&, double p = 2);
extern OCTAVE_API double xnorm (const Matrix&, double p = 2);
extern OCTAVE_API double xfrobnorm (const Matrix&);

extern OCTAVE_API double xnorm (const ComplexColumnVector&, double p = 2);
extern OCTAVE_API double xnorm (const ComplexRowVector&, double p = 2);
extern OCTAVE_API double xnorm (const ComplexMatrix&, double p = 2);
extern OCTAVE_API double xfrobnorm (const ComplexMatrix&);

extern OCTAVE_API float xnorm (const FloatColumnVector&, float p = 2);
extern OCTAVE_API float xnorm (const FloatRowVector&, float p = 2);
extern OCTAVE_API float xnorm (const FloatMatrix&, float p = 2);
extern OCTAVE_API float xfrobnorm (const FloatMatrix&);

extern OCTAVE_API float xnorm (const FloatComplexColumnVector&, float p = 2);
extern OCTAVE_API float xnorm (const FloatComplexRowVector&, float p = 2);
extern OCTAVE_API float xnorm (const FloatComplexMatrix&, float p = 2);
extern OCTAVE_API float xfrobnorm (const FloatComplexMatrix&);

extern OCTAVE_API double xnorm (const SparseMatrix&, double p = 2);
extern OCTAVE_API double xfrobnorm (const SparseMatrix&);

extern OCTAVE_API double xnorm (const SparseComplexMatrix&, double p = 2);
extern OCTAVE_API double xfrobnorm (const SparseComplexMatrix&);

extern OCTAVE_API RowVector xcolnorms (const Matrix&, double p = 2);
extern OCTAVE_API ColumnVector xrownorms (const Matrix&, double p = 2);

extern OCTAVE_API RowVector xcolnorms (const ComplexMatrix&, double p = 2);
extern OCTAVE_API ColumnVector xrownorms (const ComplexMatrix&, double p = 2);

extern OCTAVE_API FloatRowVector xcolnorms (const FloatMatrix&, float p = 2);
extern OCTAVE_API FloatColumnVector xrownorms (const FloatMatrix&, float p = 2);

extern OCTAVE_API FloatRowVector xcolnorms (const FloatComplexMatrix&, float p = 2);
extern OCTAVE_API FloatColumnVector xrownorms (const FloatComplexMatrix&, float p = 2);

extern OCTAVE_API RowVector xcolnorms (const SparseMatrix&, double p = 2);
extern OCTAVE_API ColumnVector xrownorms (const SparseMatrix&, double p = 2);

extern OCTAVE_API RowVector xcolnorms (const SparseComplexMatrix&, double p = 2);
extern OCTAVE_API ColumnVector xrownorms (const SparseComplexMatrix&, double p = 2);

#endif
