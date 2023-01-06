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

#if ! defined (octave_fCRowVector_h)
#define octave_fCRowVector_h 1

#include "octave-config.h"

#include "MArray.h"
#include "fRowVector.h"
#include "mx-defs.h"

class
OCTAVE_API
FloatComplexRowVector : public MArray<FloatComplex>
{
  friend class FloatComplexColumnVector;

public:

  FloatComplexRowVector (void)
    : MArray<FloatComplex> (dim_vector (1, 0)) { }

  explicit FloatComplexRowVector (octave_idx_type n)
    : MArray<FloatComplex> (dim_vector (1, n)) { }

  explicit FloatComplexRowVector (const dim_vector& dv)
    : MArray<FloatComplex> (dv.as_row ()) { }

  FloatComplexRowVector (octave_idx_type n, const FloatComplex& val)
    : MArray<FloatComplex> (dim_vector (1, n), val) { }

  FloatComplexRowVector (const FloatComplexRowVector& a)
    : MArray<FloatComplex> (a) { }

  FloatComplexRowVector (const MArray<FloatComplex>& a)
    : MArray<FloatComplex> (a.as_row ()) { }

  FloatComplexRowVector (const Array<FloatComplex>& a)
    : MArray<FloatComplex> (a.as_row ()) { }

  explicit FloatComplexRowVector (const FloatRowVector& a)
    : MArray<FloatComplex> (a) { }

  FloatComplexRowVector& operator = (const FloatComplexRowVector& a)
  {
    MArray<FloatComplex>::operator = (a);
    return *this;
  }

  OCTAVE_API bool operator == (const FloatComplexRowVector& a) const;
  OCTAVE_API bool operator != (const FloatComplexRowVector& a) const;

  // destructive insert/delete/reorder operations

  OCTAVE_API FloatComplexRowVector&
  insert (const FloatRowVector& a, octave_idx_type c);
  OCTAVE_API FloatComplexRowVector&
  insert (const FloatComplexRowVector& a, octave_idx_type c);

  OCTAVE_API FloatComplexRowVector& fill (float val);
  OCTAVE_API FloatComplexRowVector& fill (const FloatComplex& val);
  OCTAVE_API FloatComplexRowVector&
  fill (float val, octave_idx_type c1, octave_idx_type c2);
  OCTAVE_API FloatComplexRowVector&
  fill (const FloatComplex& val, octave_idx_type c1, octave_idx_type c2);

  OCTAVE_API FloatComplexRowVector append (const FloatRowVector& a) const;
  OCTAVE_API FloatComplexRowVector
  append (const FloatComplexRowVector& a) const;

  OCTAVE_API FloatComplexColumnVector hermitian (void) const;
  OCTAVE_API FloatComplexColumnVector transpose (void) const;

  friend OCTAVE_API FloatComplexRowVector
  conj (const FloatComplexRowVector& a);

  // resize is the destructive equivalent for this one

  OCTAVE_API FloatComplexRowVector
  extract (octave_idx_type c1, octave_idx_type c2) const;

  OCTAVE_API FloatComplexRowVector
  extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by row vector -> row vector operations

  OCTAVE_API FloatComplexRowVector& operator += (const FloatRowVector& a);
  OCTAVE_API FloatComplexRowVector& operator -= (const FloatRowVector& a);

  // row vector by matrix -> row vector

  friend OCTAVE_API FloatComplexRowVector
  operator * (const FloatComplexRowVector& a, const FloatComplexMatrix& b);

  friend OCTAVE_API FloatComplexRowVector
  operator * (const FloatRowVector& a, const FloatComplexMatrix& b);

  // other operations

  OCTAVE_API FloatComplex min (void) const;
  OCTAVE_API FloatComplex max (void) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatComplexRowVector& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, FloatComplexRowVector& a);

  void resize (octave_idx_type n, const FloatComplex& rfv = FloatComplex (0))
  {
    Array<FloatComplex>::resize (dim_vector (1, n), rfv);
  }

  void clear (octave_idx_type n)
  { Array<FloatComplex>::clear (1, n); }

};

// row vector by column vector -> scalar

OCTAVE_API FloatComplex
operator * (const FloatComplexRowVector& a, const ColumnVector& b);

OCTAVE_API FloatComplex
operator * (const FloatComplexRowVector& a, const FloatComplexColumnVector& b);

// other operations

OCTAVE_API FloatComplexRowVector
linspace (const FloatComplex& x1, const FloatComplex& x2, octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, FloatComplexRowVector, FloatComplex)

#endif
