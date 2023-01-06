////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_xdiv_h)
#define octave_xdiv_h 1

#include "octave-config.h"

#include "mx-defs.h"
#include "MatrixType.h"

OCTAVE_BEGIN_NAMESPACE(octave)

extern Matrix xdiv (const Matrix& a, const Matrix& b, MatrixType& typ);
extern ComplexMatrix xdiv (const Matrix& a, const ComplexMatrix& b,
                           MatrixType& typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const Matrix& b,
                           MatrixType& typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const ComplexMatrix& b,
                           MatrixType& typ);

extern Matrix elem_xdiv (double a, const Matrix& b);
extern ComplexMatrix elem_xdiv (double a, const ComplexMatrix& b);
extern ComplexMatrix elem_xdiv (const Complex a, const Matrix& b);
extern ComplexMatrix elem_xdiv (const Complex a, const ComplexMatrix& b);

extern NDArray elem_xdiv (double a, const NDArray& b);
extern ComplexNDArray elem_xdiv (double a, const ComplexNDArray& b);
extern ComplexNDArray elem_xdiv (const Complex a, const NDArray& b);
extern ComplexNDArray elem_xdiv (const Complex a, const ComplexNDArray& b);

extern Matrix xleftdiv (const Matrix& a, const Matrix& b, MatrixType& typ,
                        blas_trans_type transt = blas_no_trans);
extern ComplexMatrix xleftdiv (const Matrix& a, const ComplexMatrix& b,
                               MatrixType& typ,
                               blas_trans_type transt = blas_no_trans);
extern ComplexMatrix xleftdiv (const ComplexMatrix& a, const Matrix& b,
                               MatrixType& typ,
                               blas_trans_type transt = blas_no_trans);
extern ComplexMatrix xleftdiv (const ComplexMatrix& a, const ComplexMatrix& b,
                               MatrixType& typ,
                               blas_trans_type transt = blas_no_trans);

extern FloatMatrix xdiv (const FloatMatrix& a, const FloatMatrix& b,
                         MatrixType& typ);
extern FloatComplexMatrix xdiv (const FloatMatrix& a,
                                const FloatComplexMatrix& b,
                                MatrixType& typ);
extern FloatComplexMatrix xdiv (const FloatComplexMatrix& a,
                                const FloatMatrix& b,
                                MatrixType& typ);
extern FloatComplexMatrix xdiv (const FloatComplexMatrix& a,
                                const FloatComplexMatrix& b,
                                MatrixType& typ);

extern FloatMatrix elem_xdiv (float a, const FloatMatrix& b);
extern FloatComplexMatrix elem_xdiv (float a, const FloatComplexMatrix& b);
extern FloatComplexMatrix elem_xdiv (const FloatComplex a,
                                     const FloatMatrix& b);
extern FloatComplexMatrix elem_xdiv (const FloatComplex a,
                                     const FloatComplexMatrix& b);

extern FloatNDArray elem_xdiv (float a, const FloatNDArray& b);
extern FloatComplexNDArray elem_xdiv (float a, const FloatComplexNDArray& b);
extern FloatComplexNDArray elem_xdiv (const FloatComplex a,
                                      const FloatNDArray& b);
extern FloatComplexNDArray elem_xdiv (const FloatComplex a,
                                      const FloatComplexNDArray& b);

extern FloatMatrix xleftdiv (const FloatMatrix& a, const FloatMatrix& b,
                             MatrixType& typ,
                             blas_trans_type transt = blas_no_trans);
extern FloatComplexMatrix xleftdiv (const FloatMatrix& a,
                                    const FloatComplexMatrix& b,
                                    MatrixType& typ,
                                    blas_trans_type transt = blas_no_trans);
extern FloatComplexMatrix xleftdiv (const FloatComplexMatrix& a,
                                    const FloatMatrix& b,
                                    MatrixType& typ,
                                    blas_trans_type transt = blas_no_trans);
extern FloatComplexMatrix xleftdiv (const FloatComplexMatrix& a,
                                    const FloatComplexMatrix& b,
                                    MatrixType& typ,
                                    blas_trans_type transt = blas_no_trans);

extern Matrix xdiv (const Matrix& a, const DiagMatrix& b);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const DiagMatrix& b);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const ComplexDiagMatrix& b);

extern DiagMatrix xdiv (const DiagMatrix& a, const DiagMatrix& b);
extern ComplexDiagMatrix xdiv (const ComplexDiagMatrix& a, const DiagMatrix& b);
extern ComplexDiagMatrix xdiv (const ComplexDiagMatrix& a,
                               const ComplexDiagMatrix& b);

extern FloatMatrix xdiv (const FloatMatrix& a, const FloatDiagMatrix& b);
extern FloatComplexMatrix xdiv (const FloatComplexMatrix& a,
                                const FloatDiagMatrix& b);
extern FloatComplexMatrix xdiv (const FloatMatrix& a,
                                const FloatComplexDiagMatrix& b);
extern FloatComplexMatrix xdiv (const FloatComplexMatrix& a,
                                const FloatComplexDiagMatrix& b);

extern FloatDiagMatrix xdiv (const FloatDiagMatrix& a,
                             const FloatDiagMatrix& b);
extern FloatComplexDiagMatrix xdiv (const FloatComplexDiagMatrix& a,
                                    const FloatDiagMatrix& b);
extern FloatComplexDiagMatrix xdiv (const FloatComplexDiagMatrix& a,
                                    const FloatComplexDiagMatrix& b);

extern Matrix xleftdiv (const DiagMatrix& a, const Matrix& b);
extern ComplexMatrix xleftdiv (const DiagMatrix& a, const ComplexMatrix& b);
extern ComplexMatrix xleftdiv (const ComplexDiagMatrix& a,
                               const ComplexMatrix& b);

extern DiagMatrix xleftdiv (const DiagMatrix& a, const DiagMatrix& b);
extern ComplexDiagMatrix xleftdiv (const DiagMatrix& a,
                                   const ComplexDiagMatrix& b);
extern ComplexDiagMatrix xleftdiv (const ComplexDiagMatrix& a,
                                   const ComplexDiagMatrix& b);

extern FloatMatrix xleftdiv (const FloatDiagMatrix& a,
                             const FloatMatrix& b);
extern FloatComplexMatrix xleftdiv (const FloatDiagMatrix& a,
                                    const FloatComplexMatrix& b);
extern FloatComplexMatrix xleftdiv (const FloatComplexDiagMatrix& a,
                                    const FloatComplexMatrix& b);

extern FloatDiagMatrix xleftdiv (const FloatDiagMatrix& a,
                                 const FloatDiagMatrix& b);
extern FloatComplexDiagMatrix xleftdiv (const FloatDiagMatrix& a,
                                        const FloatComplexDiagMatrix& b);
extern FloatComplexDiagMatrix xleftdiv (const FloatComplexDiagMatrix& a,
                                        const FloatComplexDiagMatrix& b);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline Matrix
xdiv (const Matrix& a, const Matrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const Matrix& a, const ComplexMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const ComplexMatrix& a, const Matrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const ComplexMatrix& a, const ComplexMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline Matrix
x_el_div (double a, const Matrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexMatrix
x_el_div (double a, const ComplexMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexMatrix
x_el_div (const Complex a, const Matrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexMatrix
x_el_div (const Complex a, const ComplexMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline NDArray
x_el_div (double a, const NDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexNDArray
x_el_div (double a, const ComplexNDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexNDArray
x_el_div (const Complex a, const NDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline ComplexNDArray
x_el_div (const Complex a, const ComplexNDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline Matrix
xleftdiv (const Matrix& a, const Matrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const Matrix& a, const ComplexMatrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const ComplexMatrix& a, const Matrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const ComplexMatrix& a, const ComplexMatrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatMatrix
xdiv (const FloatMatrix& a, const FloatMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexMatrix
xdiv (const FloatMatrix& a, const FloatComplexMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexMatrix
xdiv (const FloatComplexMatrix& a, const FloatMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexMatrix
xdiv (const FloatComplexMatrix& a, const FloatComplexMatrix& b, MatrixType& typ)
{
  return octave::xdiv (a, b, typ);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatMatrix
x_el_div (float a, const FloatMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatComplexMatrix
x_el_div (float a, const FloatComplexMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatComplexMatrix
x_el_div (const FloatComplex a, const FloatMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatComplexMatrix
x_el_div (const FloatComplex a, const FloatComplexMatrix& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatNDArray
x_el_div (float a, const FloatNDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatComplexNDArray
x_el_div (float a, const FloatComplexNDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatComplexNDArray
x_el_div (const FloatComplex a, const FloatNDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xdiv' instead")
inline FloatComplexNDArray
x_el_div (const FloatComplex a, const FloatComplexNDArray& b)
{
  return octave::elem_xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatMatrix
xleftdiv (const FloatMatrix& a, const FloatMatrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatComplexMatrix
xleftdiv (const FloatMatrix& a, const FloatComplexMatrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatComplexMatrix
xleftdiv (const FloatComplexMatrix& a, const FloatMatrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatComplexMatrix
xleftdiv (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
          MatrixType& typ, blas_trans_type transt = blas_no_trans)
{
  return octave::xleftdiv (a, b, typ, transt);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline Matrix
xdiv (const Matrix& a, const DiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const ComplexMatrix& a, const DiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexMatrix
xdiv (const ComplexMatrix& a, const ComplexDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline DiagMatrix
xdiv (const DiagMatrix& a, const DiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexDiagMatrix
xdiv (const ComplexDiagMatrix& a, const DiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline ComplexDiagMatrix
xdiv (const ComplexDiagMatrix& a, const ComplexDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatMatrix
xdiv (const FloatMatrix& a, const FloatDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexMatrix
xdiv (const FloatComplexMatrix& a, const FloatDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexMatrix
xdiv (const FloatMatrix& a, const FloatComplexDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexMatrix
xdiv (const FloatComplexMatrix& a, const FloatComplexDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatDiagMatrix
xdiv (const FloatDiagMatrix& a, const FloatDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexDiagMatrix
xdiv (const FloatComplexDiagMatrix& a, const FloatDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xdiv' instead")
inline FloatComplexDiagMatrix
xdiv (const FloatComplexDiagMatrix& a, const FloatComplexDiagMatrix& b)
{
  return octave::xdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline Matrix
xleftdiv (const DiagMatrix& a, const Matrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const DiagMatrix& a, const ComplexMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexMatrix
xleftdiv (const ComplexDiagMatrix& a, const ComplexMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline DiagMatrix
xleftdiv (const DiagMatrix& a, const DiagMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexDiagMatrix
xleftdiv (const DiagMatrix& a, const ComplexDiagMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline ComplexDiagMatrix
xleftdiv (const ComplexDiagMatrix& a, const ComplexDiagMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatMatrix
xleftdiv (const FloatDiagMatrix& a, const FloatMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatComplexMatrix
xleftdiv (const FloatDiagMatrix& a, const FloatComplexMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatComplexMatrix
xleftdiv (const FloatComplexDiagMatrix& a, const FloatComplexMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatDiagMatrix
xleftdiv (const FloatDiagMatrix& a, const FloatDiagMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatComplexDiagMatrix
xleftdiv (const FloatDiagMatrix& a, const FloatComplexDiagMatrix& b)
{
  return octave::xleftdiv (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xleftdiv' instead")
inline FloatComplexDiagMatrix
xleftdiv (const FloatComplexDiagMatrix& a, const FloatComplexDiagMatrix& b)
{
  return octave::xleftdiv (a, b);
}

#endif

#endif
