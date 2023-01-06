////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_CDiagMatrix_h)
#define octave_CDiagMatrix_h 1

#include "octave-config.h"

#include "CColVector.h"
#include "CRowVector.h"
#include "DET.h"
#include "MDiagArray2.h"
#include "dColVector.h"
#include "dRowVector.h"
#include "mx-defs.h"

class
OCTAVE_API
ComplexDiagMatrix : public MDiagArray2<Complex>
{
public:

  typedef ComplexMatrix full_matrix_type;

  typedef double real_elt_type;
  typedef Complex complex_elt_type;
  typedef Complex element_type;

  ComplexDiagMatrix (void) = default;

  ComplexDiagMatrix (const ComplexDiagMatrix& a) = default;

  ComplexDiagMatrix& operator = (const ComplexDiagMatrix& a) = default;

  ~ComplexDiagMatrix (void) = default;

  ComplexDiagMatrix (octave_idx_type r, octave_idx_type c)
    : MDiagArray2<Complex> (r, c) { }

  ComplexDiagMatrix (octave_idx_type r, octave_idx_type c, const Complex& val)
    : MDiagArray2<Complex> (r, c, val) { }

  explicit ComplexDiagMatrix (const Array<Complex>& a)
    : MDiagArray2<Complex> (a) { }

  explicit ComplexDiagMatrix (const Array<double>& a)
    : MDiagArray2<Complex> (Array<Complex> (a)) { }

  ComplexDiagMatrix (const Array<Complex>& a, octave_idx_type r,
                     octave_idx_type c)
    : MDiagArray2<Complex> (a, r, c) { }

  explicit OCTAVE_API ComplexDiagMatrix (const DiagMatrix& a);

  ComplexDiagMatrix (const MDiagArray2<Complex>& a)
    : MDiagArray2<Complex> (a) { }

  template <typename U>
  ComplexDiagMatrix (const DiagArray2<U>& a)
    : MDiagArray2<Complex> (a) { }

  OCTAVE_API bool operator == (const ComplexDiagMatrix& a) const;
  OCTAVE_API bool operator != (const ComplexDiagMatrix& a) const;

  OCTAVE_API ComplexDiagMatrix& fill (double val);
  OCTAVE_API ComplexDiagMatrix& fill (const Complex& val);
  OCTAVE_API ComplexDiagMatrix&
  fill (double val, octave_idx_type beg, octave_idx_type end);
  OCTAVE_API ComplexDiagMatrix&
  fill (const Complex& val, octave_idx_type beg, octave_idx_type end);
  OCTAVE_API ComplexDiagMatrix& fill (const ColumnVector& a);
  OCTAVE_API ComplexDiagMatrix& fill (const ComplexColumnVector& a);
  OCTAVE_API ComplexDiagMatrix& fill (const RowVector& a);
  OCTAVE_API ComplexDiagMatrix& fill (const ComplexRowVector& a);
  OCTAVE_API ComplexDiagMatrix&
  fill (const ColumnVector& a, octave_idx_type beg);
  OCTAVE_API ComplexDiagMatrix&
  fill (const ComplexColumnVector& a, octave_idx_type beg);
  OCTAVE_API ComplexDiagMatrix& fill (const RowVector& a, octave_idx_type beg);
  OCTAVE_API ComplexDiagMatrix&
  fill (const ComplexRowVector& a, octave_idx_type beg);

  ComplexDiagMatrix hermitian (void) const
  { return MDiagArray2<Complex>::hermitian (std::conj); }
  ComplexDiagMatrix transpose (void) const
  { return MDiagArray2<Complex>::transpose (); }
  DiagMatrix abs (void) const;

  friend OCTAVE_API ComplexDiagMatrix conj (const ComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  OCTAVE_API ComplexMatrix
  extract (octave_idx_type r1, octave_idx_type c1,
           octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i

  OCTAVE_API ComplexRowVector row (octave_idx_type i) const;
  OCTAVE_API ComplexRowVector row (char *s) const;

  OCTAVE_API ComplexColumnVector column (octave_idx_type i) const;
  OCTAVE_API ComplexColumnVector column (char *s) const;

  OCTAVE_API ComplexDiagMatrix inverse (octave_idx_type& info) const;
  OCTAVE_API ComplexDiagMatrix inverse (void) const;
  OCTAVE_API ComplexDiagMatrix pseudo_inverse (double tol = 0.0) const;

  OCTAVE_API bool all_elements_are_real (void) const;

  // diagonal matrix by diagonal matrix -> diagonal matrix operations

  OCTAVE_API ComplexDiagMatrix& operator += (const DiagMatrix& a);
  OCTAVE_API ComplexDiagMatrix& operator -= (const DiagMatrix& a);

  // other operations

  ComplexColumnVector extract_diag (octave_idx_type k = 0) const
  { return MDiagArray2<Complex>::extract_diag (k); }

  OCTAVE_API ComplexDET determinant (void) const;
  OCTAVE_API double rcond (void) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const ComplexDiagMatrix& a);

};

OCTAVE_API ComplexDiagMatrix conj (const ComplexDiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

OCTAVE_API ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const ComplexDiagMatrix& b);

OCTAVE_API ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const DiagMatrix& b);

OCTAVE_API ComplexDiagMatrix
operator * (const DiagMatrix& a, const ComplexDiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, ComplexDiagMatrix, Complex)

#endif
