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

#if ! defined (octave_fDiagMatrix_h)
#define octave_fDiagMatrix_h 1

#include "octave-config.h"

#include "DET.h"
#include "MDiagArray2.h"
#include "fColVector.h"
#include "fRowVector.h"
#include "mx-defs.h"

class
OCTAVE_API
FloatDiagMatrix : public MDiagArray2<float>
{
public:

  typedef FloatMatrix full_matrix_type;

  FloatDiagMatrix (void) = default;

  FloatDiagMatrix (const FloatDiagMatrix& a) = default;

  FloatDiagMatrix& operator = (const FloatDiagMatrix& a) = default;

  ~FloatDiagMatrix (void) = default;

  FloatDiagMatrix (octave_idx_type r, octave_idx_type c)
    : MDiagArray2<float> (r, c) { }

  FloatDiagMatrix (octave_idx_type r, octave_idx_type c, float val)
    : MDiagArray2<float> (r, c, val) { }

  FloatDiagMatrix (const MDiagArray2<float>& a) : MDiagArray2<float> (a) { }

  template <typename U>
  FloatDiagMatrix (const DiagArray2<U>& a) : MDiagArray2<float> (a) { }

  explicit FloatDiagMatrix (const Array<double>& a) : MDiagArray2<float> (a) { }

  FloatDiagMatrix (const Array<float>& a, octave_idx_type r, octave_idx_type c)
    : MDiagArray2<float> (a, r, c) { }

  OCTAVE_API bool operator == (const FloatDiagMatrix& a) const;
  OCTAVE_API bool operator != (const FloatDiagMatrix& a) const;

  OCTAVE_API FloatDiagMatrix& fill (float val);
  OCTAVE_API FloatDiagMatrix&
  fill (float val, octave_idx_type beg, octave_idx_type end);
  OCTAVE_API FloatDiagMatrix& fill (const FloatColumnVector& a);
  OCTAVE_API FloatDiagMatrix& fill (const FloatRowVector& a);
  OCTAVE_API FloatDiagMatrix&
  fill (const FloatColumnVector& a, octave_idx_type beg);
  OCTAVE_API FloatDiagMatrix&
  fill (const FloatRowVector& a, octave_idx_type beg);

  FloatDiagMatrix transpose (void) const
  { return MDiagArray2<float>::transpose (); }

  OCTAVE_API FloatDiagMatrix abs (void) const;

  friend OCTAVE_API FloatDiagMatrix real (const FloatComplexDiagMatrix& a);
  friend OCTAVE_API FloatDiagMatrix imag (const FloatComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  OCTAVE_API FloatMatrix
  extract (octave_idx_type r1, octave_idx_type c1,
           octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i.

  OCTAVE_API FloatRowVector row (octave_idx_type i) const;
  OCTAVE_API FloatRowVector row (char *s) const;

  OCTAVE_API FloatColumnVector column (octave_idx_type i) const;
  OCTAVE_API FloatColumnVector column (char *s) const;

  OCTAVE_API FloatDiagMatrix inverse (void) const;
  OCTAVE_API FloatDiagMatrix inverse (octave_idx_type& info) const;
  OCTAVE_API FloatDiagMatrix pseudo_inverse (float tol = 0.0f) const;

  // other operations

  FloatColumnVector extract_diag (octave_idx_type k = 0) const
  { return MDiagArray2<float>::extract_diag (k); }

  OCTAVE_API FloatDET determinant (void) const;
  OCTAVE_API float rcond (void) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatDiagMatrix& a);

};

OCTAVE_API FloatDiagMatrix real (const FloatComplexDiagMatrix& a);
OCTAVE_API FloatDiagMatrix imag (const FloatComplexDiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

OCTAVE_API FloatDiagMatrix
operator * (const FloatDiagMatrix& a, const FloatDiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, FloatDiagMatrix, float)

#endif
