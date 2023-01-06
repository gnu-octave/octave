////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#include "mx-fwd.h"

// The remaining includes can be removed when the global enum
// declaration, the convert_enum function, and the deprecated functions
// at the end of this file are removed.

#include <cstdlib>
#include "dMatrix.h"
#include "dNDArray.h"
#include "CMatrix.h"
#include "CNDArray.h"
#include "fMatrix.h"
#include "fNDArray.h"
#include "fCMatrix.h"
#include "fCNDArray.h"

// FIXME: Is there any sane way to move a global enum to a namespace and
// tag the global one as deprecated when it is used as a parameter in
// public functions that also need to be tagged as deprecated?

enum convn_type
{
  convn_full,
  convn_same,
  convn_valid
};

OCTAVE_BEGIN_NAMESPACE(octave)

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

convn_type convert_enum (::convn_type ct)
{
  switch (ct)
    {
    case ::convn_full:
      return convn_full;

    case ::convn_same:
      return convn_same;

    case ::convn_valid:
      return convn_valid;

    default:
      abort ();
    }
}

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline NDArray
convn (const NDArray& a, const NDArray& b, convn_type ct)
{
  return octave::convn (a, b, static_cast<octave::convn_type> (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline Matrix
convn (const Matrix& a, const Matrix& b, convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline Matrix
convn (const Matrix& a, const ColumnVector& c, const RowVector& r,
       convn_type ct)
{
  return octave::convn (a, c, r, octave::convert_enum (ct));
}

// double complex X double real

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline ComplexNDArray
convn (const ComplexNDArray& a, const NDArray& b, convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline ComplexMatrix
convn (const ComplexMatrix& a, const Matrix& b, convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline ComplexMatrix
convn (const ComplexMatrix& a, const ColumnVector& c, const RowVector& r,
       convn_type ct)
{
  return octave::convn (a, c, r, octave::convert_enum (ct));
}

// double complex X double complex

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline ComplexNDArray
convn (const ComplexNDArray& a, const ComplexNDArray& b, convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline ComplexMatrix
convn (const ComplexMatrix& a, const ComplexMatrix& b, convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline ComplexMatrix
convn (const ComplexMatrix& a, const ComplexColumnVector& c,
       const ComplexRowVector& r, convn_type ct)
{
  return octave::convn (a, c, r, octave::convert_enum (ct));
}

// float real X float real

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatNDArray
convn (const FloatNDArray& a, const FloatNDArray& b, convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatMatrix
convn (const FloatMatrix& a, const FloatMatrix& b, convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatMatrix
convn (const FloatMatrix& a, const FloatColumnVector& c,
       const FloatRowVector& r, convn_type ct)
{
  return octave::convn (a, c, r, octave::convert_enum (ct));
}

// float complex X float real

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatComplexNDArray
convn (const FloatComplexNDArray& a, const FloatNDArray& b,
       convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatMatrix& b,
       convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatColumnVector& c,
       const FloatRowVector& r, convn_type ct)
{
  return octave::convn (a, c, r, octave::convert_enum (ct));
}

// float complex X float complex

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatComplexNDArray
convn (const FloatComplexNDArray& a, const FloatComplexNDArray& b,
       convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
       convn_type ct)
{
  return octave::convn (a, b, octave::convert_enum (ct));
}

OCTAVE_DEPRECATED (7, "use 'octave::convn' instead")
inline FloatComplexMatrix
convn (const FloatComplexMatrix& a, const FloatComplexColumnVector& c,
       const FloatComplexRowVector& r, convn_type ct)
{
  return octave::convn (a, c, r, octave::convert_enum (ct));
}
#endif

#endif
