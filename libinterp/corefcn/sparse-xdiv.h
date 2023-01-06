////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

#if ! defined (octave_sparse_xdiv_h)
#define octave_sparse_xdiv_h 1

#include "octave-config.h"

#include "oct-cmplx.h"
#include "MatrixType.h"

class DiagMatrix;
class ComplexDiagMatrix;
class SparseMatrix;
class SparseComplexMatrix;

OCTAVE_BEGIN_NAMESPACE(octave)

extern Matrix xdiv (const Matrix& a, const SparseMatrix& b, MatrixType& typ);
extern ComplexMatrix xdiv (const Matrix& a, const SparseComplexMatrix& b,
                           MatrixType& typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const SparseMatrix& b,
                           MatrixType& typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a,
                           const SparseComplexMatrix& b, MatrixType& typ);

extern SparseMatrix xdiv (const SparseMatrix& a, const SparseMatrix& b,
                          MatrixType& typ);
extern SparseComplexMatrix xdiv (const SparseMatrix& a,
                                 const SparseComplexMatrix& b, MatrixType& typ);
extern SparseComplexMatrix xdiv (const SparseComplexMatrix& a,
                                 const SparseMatrix& b, MatrixType& typ);
extern SparseComplexMatrix xdiv (const SparseComplexMatrix& a,
                                 const SparseComplexMatrix& b, MatrixType& typ);

extern SparseMatrix xdiv (const SparseMatrix& a,
                          const DiagMatrix& b, MatrixType& typ);
extern SparseComplexMatrix xdiv (const SparseMatrix& a,
                                 const ComplexDiagMatrix& b, MatrixType& typ);
extern SparseComplexMatrix xdiv (const SparseComplexMatrix& a,
                                 const DiagMatrix& b, MatrixType& typ);
extern SparseComplexMatrix xdiv (const SparseComplexMatrix& a,
                                 const ComplexDiagMatrix& b, MatrixType& typ);

extern Matrix elem_xdiv (double a, const SparseMatrix& b);
extern ComplexMatrix elem_xdiv (double a, const SparseComplexMatrix& b);
extern ComplexMatrix elem_xdiv (const Complex& a, const SparseMatrix& b);
extern ComplexMatrix elem_xdiv (const Complex& a,
                                const SparseComplexMatrix& b);

extern Matrix xleftdiv (const SparseMatrix& a, const Matrix& b,
                        MatrixType& typ);
extern ComplexMatrix xleftdiv (const SparseMatrix& a, const ComplexMatrix& b,
                               MatrixType& typ);
extern ComplexMatrix xleftdiv (const SparseComplexMatrix& a, const Matrix& b,
                               MatrixType& typ);
extern ComplexMatrix xleftdiv (const SparseComplexMatrix& a,
                               const ComplexMatrix& b, MatrixType& typ);

extern SparseMatrix xleftdiv (const SparseMatrix& a, const SparseMatrix& b,
                              MatrixType& typ);
extern SparseComplexMatrix xleftdiv (const SparseMatrix& a,
                                     const SparseComplexMatrix& b,
                                     MatrixType& typ);
extern SparseComplexMatrix xleftdiv (const SparseComplexMatrix& a,
                                     const SparseMatrix& b, MatrixType& typ);
extern SparseComplexMatrix xleftdiv (const SparseComplexMatrix& a,
                                     const SparseComplexMatrix& b,
                                     MatrixType& typ);

extern SparseMatrix xleftdiv (const DiagMatrix&, const SparseMatrix&,
                              MatrixType&);
extern SparseComplexMatrix xleftdiv (const ComplexDiagMatrix&,
                                     const SparseMatrix&,
                                     MatrixType&);
extern SparseComplexMatrix xleftdiv (const DiagMatrix&,
                                     const SparseComplexMatrix&,
                                     MatrixType&);
extern SparseComplexMatrix xleftdiv (const ComplexDiagMatrix&,
                                     const SparseComplexMatrix&,
                                     MatrixType&);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline Matrix
xdiv (const Matrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const Matrix& a, const SparseComplexMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const ComplexMatrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const ComplexMatrix& a, const SparseComplexMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseMatrix
xdiv (const SparseMatrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseComplexMatrix
xdiv (const SparseMatrix& a, const SparseComplexMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
      MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseMatrix
xdiv (const SparseMatrix& a, const DiagMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseComplexMatrix
xdiv (const SparseMatrix& a, const ComplexDiagMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const DiagMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const ComplexDiagMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline Matrix
x_el_div (double a, const SparseMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexMatrix
x_el_div (double a, const SparseComplexMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexMatrix
x_el_div (const Complex& a, const SparseMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexMatrix
x_el_div (const Complex& a, const SparseComplexMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline Matrix
xleftdiv (const SparseMatrix& a, const Matrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const SparseMatrix& a, const ComplexMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const Matrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const ComplexMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseMatrix
xleftdiv (const SparseMatrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseComplexMatrix
xleftdiv (const SparseMatrix& a, const SparseComplexMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
          MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseMatrix
xleftdiv (const DiagMatrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseComplexMatrix
xleftdiv (const ComplexDiagMatrix& a, const SparseMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseComplexMatrix
xleftdiv (const DiagMatrix& a, const SparseComplexMatrix& b, MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline SparseComplexMatrix
xleftdiv (const ComplexDiagMatrix& a, const SparseComplexMatrix& b,
          MatrixType& typ)
{
  return octave::xleftdiv (a, b, typ);
}

#endif

#endif
