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

#if ! defined (octave_fCDiagMatrix_h)
#define octave_fCDiagMatrix_h 1

#include "octave-config.h"

#include "DET.h"
#include "MDiagArray2.h"
#include "fCColVector.h"
#include "fCRowVector.h"
#include "fColVector.h"
#include "fRowVector.h"
#include "mx-defs.h"

class
OCTAVE_API
FloatComplexDiagMatrix : public MDiagArray2<FloatComplex>
{
public:

  typedef FloatComplexMatrix full_matrix_type;

  typedef float real_elt_type;
  typedef FloatComplex complex_elt_type;
  typedef FloatComplex element_type;

  FloatComplexDiagMatrix (void) = default;

  FloatComplexDiagMatrix (const FloatComplexDiagMatrix& a) = default;

  FloatComplexDiagMatrix& operator = (const FloatComplexDiagMatrix& a) = default;

  ~FloatComplexDiagMatrix (void) = default;

  FloatComplexDiagMatrix (octave_idx_type r,
                          octave_idx_type c)
    : MDiagArray2<FloatComplex> (r, c) { }

  FloatComplexDiagMatrix (octave_idx_type r, octave_idx_type c,
                          const FloatComplex& val)
    : MDiagArray2<FloatComplex> (r, c, val) { }

  explicit FloatComplexDiagMatrix (const Array<FloatComplex>& a)
    : MDiagArray2<FloatComplex> (a) { }

  FloatComplexDiagMatrix (const Array<FloatComplex>& a, octave_idx_type r,
                          octave_idx_type c)
    : MDiagArray2<FloatComplex> (a, r, c) { }

  explicit FloatComplexDiagMatrix (const Array<float>& a)
    : MDiagArray2<FloatComplex> (Array<FloatComplex> (a)) { }

  explicit OCTAVE_API FloatComplexDiagMatrix (const FloatDiagMatrix& a);

  FloatComplexDiagMatrix (const MDiagArray2<FloatComplex>& a)
    : MDiagArray2<FloatComplex> (a) { }

  template <typename U>
  FloatComplexDiagMatrix (const DiagArray2<U>& a)
    : MDiagArray2<FloatComplex> (a) { }

  OCTAVE_API bool operator == (const FloatComplexDiagMatrix& a) const;
  OCTAVE_API bool operator != (const FloatComplexDiagMatrix& a) const;

  OCTAVE_API FloatComplexDiagMatrix& fill (float val);
  OCTAVE_API FloatComplexDiagMatrix& fill (const FloatComplex& val);
  OCTAVE_API FloatComplexDiagMatrix&
  fill (float val, octave_idx_type beg, octave_idx_type end);
  OCTAVE_API FloatComplexDiagMatrix&
  fill (const FloatComplex& val, octave_idx_type beg, octave_idx_type end);
  OCTAVE_API FloatComplexDiagMatrix& fill (const FloatColumnVector& a);
  OCTAVE_API FloatComplexDiagMatrix& fill (const FloatComplexColumnVector& a);
  OCTAVE_API FloatComplexDiagMatrix& fill (const FloatRowVector& a);
  OCTAVE_API FloatComplexDiagMatrix& fill (const FloatComplexRowVector& a);
  OCTAVE_API FloatComplexDiagMatrix&
  fill (const FloatColumnVector& a, octave_idx_type beg);
  OCTAVE_API FloatComplexDiagMatrix&
  fill (const FloatComplexColumnVector& a, octave_idx_type beg);
  OCTAVE_API FloatComplexDiagMatrix&
  fill (const FloatRowVector& a, octave_idx_type beg);
  OCTAVE_API FloatComplexDiagMatrix&
  fill (const FloatComplexRowVector& a, octave_idx_type beg);

  FloatComplexDiagMatrix hermitian (void) const
  { return MDiagArray2<FloatComplex>::hermitian (std::conj); }
  FloatComplexDiagMatrix transpose (void) const
  { return MDiagArray2<FloatComplex>::transpose (); }
  OCTAVE_API FloatDiagMatrix abs (void) const;

  friend OCTAVE_API FloatComplexDiagMatrix
  conj (const FloatComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  OCTAVE_API FloatComplexMatrix
  extract (octave_idx_type r1, octave_idx_type c1,
           octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i

  OCTAVE_API FloatComplexRowVector row (octave_idx_type i) const;
  OCTAVE_API FloatComplexRowVector row (char *s) const;

  OCTAVE_API FloatComplexColumnVector column (octave_idx_type i) const;
  OCTAVE_API FloatComplexColumnVector column (char *s) const;

  OCTAVE_API FloatComplexDiagMatrix inverse (octave_idx_type& info) const;
  OCTAVE_API FloatComplexDiagMatrix inverse (void) const;
  OCTAVE_API FloatComplexDiagMatrix pseudo_inverse (float tol = 0.0f) const;

  OCTAVE_API bool all_elements_are_real (void) const;

  // diagonal matrix by diagonal matrix -> diagonal matrix operations

  OCTAVE_API FloatComplexDiagMatrix& operator += (const FloatDiagMatrix& a);
  OCTAVE_API FloatComplexDiagMatrix& operator -= (const FloatDiagMatrix& a);

  // other operations

  FloatComplexColumnVector extract_diag (octave_idx_type k = 0) const
  { return MDiagArray2<FloatComplex>::extract_diag (k); }

  OCTAVE_API FloatComplexDET determinant (void) const;
  OCTAVE_API float rcond (void) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatComplexDiagMatrix& a);

};

OCTAVE_API FloatComplexDiagMatrix conj (const FloatComplexDiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

OCTAVE_API FloatComplexDiagMatrix
operator * (const FloatComplexDiagMatrix& a, const FloatComplexDiagMatrix& b);

OCTAVE_API FloatComplexDiagMatrix
operator * (const FloatComplexDiagMatrix& a, const FloatDiagMatrix& b);

OCTAVE_API FloatComplexDiagMatrix
operator * (const FloatDiagMatrix& a, const FloatComplexDiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, FloatComplexDiagMatrix, FloatComplex)

#endif
