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

#if ! defined (octave_sparse_xpow_h)
#define octave_sparse_xpow_h 1

#include "octave-config.h"

#include "oct-cmplx.h"

class SparseMatrix;
class SparseComplexMatrix;
class octave_value;

OCTAVE_BEGIN_NAMESPACE(octave)

extern octave_value xpow (const SparseMatrix& a, double b);
extern octave_value xpow (const SparseComplexMatrix& a, double b);

extern octave_value elem_xpow (double a, const SparseMatrix& b);
extern octave_value elem_xpow (double a, const SparseComplexMatrix& b);

extern octave_value elem_xpow (const SparseMatrix& a, double b);
extern octave_value elem_xpow (const SparseMatrix& a, const SparseMatrix& b);
extern octave_value elem_xpow (const SparseMatrix& a, const Complex& b);
extern octave_value elem_xpow (const SparseMatrix& a,
                               const SparseComplexMatrix& b);

extern octave_value elem_xpow (const Complex& a, const SparseMatrix& b);
extern octave_value elem_xpow (const Complex& a,
                               const SparseComplexMatrix& b);

extern octave_value elem_xpow (const SparseComplexMatrix& a, double b);
extern octave_value elem_xpow (const SparseComplexMatrix& a,
                               const SparseMatrix& b);
extern octave_value elem_xpow (const SparseComplexMatrix& a,
                               const Complex& b);
extern octave_value elem_xpow (const SparseComplexMatrix& a,
                               const SparseComplexMatrix& b);

OCTAVE_END_NAMESPACE(octave)

#if (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::xpow' instead")
inline octave_value
xpow (const SparseMatrix& a, double b)
{
  return octave::xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::xpow' instead")
inline octave_value
xpow (const SparseComplexMatrix& a, double b)
{
  return octave::xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (double a, const SparseMatrix& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (double a, const SparseComplexMatrix& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseMatrix& a, double b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseMatrix& a, const SparseMatrix& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseMatrix& a, const Complex& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseMatrix& a, const SparseComplexMatrix& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const Complex& a, const SparseMatrix& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const Complex& a, const SparseComplexMatrix& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseComplexMatrix& a, double b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseMatrix& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseComplexMatrix& a, const Complex& b)
{
  return octave::elem_xpow (a, b);
}

OCTAVE_DEPRECATED (7, "use 'octave::elem_xpow' instead")
inline octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseComplexMatrix& b)
{
  return octave::elem_xpow (a, b);
}

#endif

#endif
