////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#include "mx-fwd.h"

#include "oct-cmplx.h"

// The remaining includes can be removed when the deprecated functions
// at the end of this file are removed.

#include "dColVector.h"
#include "dRowVector.h"
#include "fColVector.h"
#include "fRowVector.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const ColumnVector& v, double p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const RowVector& v, double p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const Matrix& m, double p = 2)
{
  return octave::xnorm (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xfrobnorm' instead")
inline double xfrobnorm (const Matrix& m)
{
  return octave::xfrobnorm (m);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const ComplexColumnVector& v, double p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const ComplexRowVector& v, double p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const ComplexMatrix& m, double p = 2)
{
  return octave::xnorm (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xfrobnorm' instead")
inline double xfrobnorm (const ComplexMatrix& m)
{
  return octave::xfrobnorm (m);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline float xnorm (const FloatColumnVector& v, float p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline float xnorm (const FloatRowVector& v, float p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline float xnorm (const FloatMatrix& m, float p = 2)
{
  return octave::xnorm (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xfrobnorm' instead")
inline float xfrobnorm (const FloatMatrix& m)
{
  return octave::xfrobnorm (m);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline float xnorm (const FloatComplexColumnVector& v, float p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline float xnorm (const FloatComplexRowVector& v, float p = 2)
{
  return octave::xnorm (v, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline float xnorm (const FloatComplexMatrix& m, float p = 2)
{
  return octave::xnorm (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xfrobnorm' instead")
inline float xfrobnorm (const FloatComplexMatrix& m)
{
  return octave::xfrobnorm (m);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const SparseMatrix& m, double p = 2)
{
  return octave::xnorm (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xfrobnorm' instead")
inline double xfrobnorm (const SparseMatrix& m)
{
  return octave::xfrobnorm (m);
}

OCTAVE_DEPRECATED (7, "use 'octave::xnorm' instead")
inline double xnorm (const SparseComplexMatrix& m, double p = 2)
{
  return octave::xnorm (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xfrobnorm' instead")
inline double xfrobnorm (const SparseComplexMatrix& m)
{
  return octave::xfrobnorm (m);
}

OCTAVE_DEPRECATED (7, "use 'octave::xcolnorms' instead")
inline RowVector xcolnorms (const Matrix& m, double p = 2)
{
  return octave::xcolnorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xrownorms' instead")
inline ColumnVector xrownorms (const Matrix& m, double p = 2)
{
  return octave::xrownorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xcolnorms' instead")
inline RowVector xcolnorms (const ComplexMatrix& m, double p = 2)
{
  return octave::xcolnorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xrownorms' instead")
inline ColumnVector xrownorms (const ComplexMatrix& m, double p = 2)
{
  return octave::xrownorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xcolnorms' instead")
inline FloatRowVector xcolnorms (const FloatMatrix& m, float p = 2)
{
  return octave::xcolnorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xrownorms' instead")
inline FloatColumnVector xrownorms (const FloatMatrix& m, float p = 2)
{
  return octave::xrownorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xcolnorms' instead")
inline FloatRowVector xcolnorms (const FloatComplexMatrix& m, float p = 2)
{
  return octave::xcolnorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xrownorms' instead")
inline FloatColumnVector xrownorms (const FloatComplexMatrix& m, float p = 2)
{
  return octave::xrownorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xcolnorms' instead")
inline RowVector xcolnorms (const SparseMatrix& m, double p = 2)
{
  return octave::xcolnorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xrownorms' instead")
inline ColumnVector xrownorms (const SparseMatrix& m, double p = 2)
{
  return octave::xrownorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xcolnorms' instead")
inline RowVector xcolnorms (const SparseComplexMatrix& m, double p = 2)
{
  return octave::xcolnorms (m, p);
}

OCTAVE_DEPRECATED (7, "use 'octave::xrownorms' instead")
inline ColumnVector xrownorms (const SparseComplexMatrix& m, double p = 2)
{
  return octave::xrownorms (m, p);
}
#endif

#endif
