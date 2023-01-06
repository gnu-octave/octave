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

#if ! defined (octave_fRowVector_h)
#define octave_fRowVector_h 1

#include "octave-config.h"

#include "MArray.h"
#include "mx-defs.h"

class
OCTAVE_API
FloatRowVector : public MArray<float>
{
public:

  FloatRowVector (void) : MArray<float> (dim_vector (1, 0)) { }

  explicit FloatRowVector (octave_idx_type n)
    : MArray<float> (dim_vector (1, n)) { }

  explicit FloatRowVector (const dim_vector& dv)
    : MArray<float> (dv.as_row ()) { }

  FloatRowVector (octave_idx_type n, float val)
    : MArray<float> (dim_vector (1, n), val) { }

  FloatRowVector (const FloatRowVector& a) : MArray<float> (a) { }

  FloatRowVector (const MArray<float>& a) : MArray<float> (a.as_row ()) { }

  FloatRowVector (const Array<float>& a) : MArray<float> (a.as_row ()) { }

  FloatRowVector& operator = (const FloatRowVector& a)
  {
    MArray<float>::operator = (a);
    return *this;
  }

  OCTAVE_API bool operator == (const FloatRowVector& a) const;
  OCTAVE_API bool operator != (const FloatRowVector& a) const;

  // destructive insert/delete/reorder operations

  OCTAVE_API FloatRowVector&
  insert (const FloatRowVector& a, octave_idx_type c);

  OCTAVE_API FloatRowVector& fill (float val);
  OCTAVE_API FloatRowVector&
  fill (float val, octave_idx_type c1, octave_idx_type c2);

  OCTAVE_API FloatRowVector append (const FloatRowVector& a) const;

  OCTAVE_API FloatColumnVector transpose (void) const;

  friend OCTAVE_API FloatRowVector real (const FloatComplexRowVector& a);
  friend OCTAVE_API FloatRowVector imag (const FloatComplexRowVector& a);

  // resize is the destructive equivalent for this one

  OCTAVE_API FloatRowVector
  extract (octave_idx_type c1, octave_idx_type c2) const;

  OCTAVE_API FloatRowVector
  extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by matrix -> row vector

  friend OCTAVE_API FloatRowVector
  operator * (const FloatRowVector& a, const FloatMatrix& b);

  // other operations

  OCTAVE_API float min (void) const;
  OCTAVE_API float max (void) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatRowVector& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, FloatRowVector& a);

  void resize (octave_idx_type n, const float& rfv = 0)
  {
    Array<float>::resize (dim_vector (1, n), rfv);
  }

  void clear (octave_idx_type n)
  { Array<float>::clear (1, n); }

};

// row vector by column vector -> scalar

float OCTAVE_API operator * (const FloatRowVector& a,
                             const FloatColumnVector& b);

Complex OCTAVE_API operator * (const FloatRowVector& a,
                               const ComplexColumnVector& b);

// other operations

OCTAVE_API FloatRowVector linspace (float x1, float x2, octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, FloatRowVector, float)

#endif
