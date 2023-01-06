////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2004-2023 The Octave Project Developers
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

#if ! defined (octave_intNDArray_h)
#define octave_intNDArray_h 1

#include "octave-config.h"

#include "intNDArray-fwd.h"
#include "mx-fwd.h"
#include "MArray.h"
#include "boolNDArray.h"

template <typename T>
class
intNDArray : public MArray<T>
{
public:

  using typename MArray<T>::element_type;

  intNDArray (void) = default;

  intNDArray (const intNDArray<T>& a) = default;

  intNDArray& operator = (const intNDArray<T>& a) = default;

  ~intNDArray (void) = default;

  intNDArray (T val) : MArray<T> (dim_vector (1, 1), val) { }

  intNDArray (const dim_vector& dv) : MArray<T> (dv) { }

  intNDArray (const dim_vector& dv, T val)
    : MArray<T> (dv, val) { }

  template <typename U>
  intNDArray (const Array<U>& a) : MArray<T> (a) { }

  template <typename U>
  intNDArray (const MArray<U>& a) : MArray<T> (a) { }

  template <typename U>
  intNDArray (const intNDArray<U>& a) : MArray<T> (a) { }

  OCTAVE_API boolNDArray operator ! (void) const;

  bool any_element_is_nan (void) const { return false; }
  OCTAVE_API bool any_element_not_one_or_zero (void) const;

  OCTAVE_API intNDArray diag (octave_idx_type k = 0) const;

  OCTAVE_API intNDArray diag (octave_idx_type m, octave_idx_type n) const;

  intNDArray& changesign (void)
  {
    MArray<T>::changesign ();
    return *this;
  }

  // FIXME: this is not quite the right thing.

  OCTAVE_API boolNDArray all (int dim = -1) const;
  OCTAVE_API boolNDArray any (int dim = -1) const;

  OCTAVE_API intNDArray max (int dim = -1) const;
  OCTAVE_API intNDArray
  max (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API intNDArray min (int dim = -1) const;
  OCTAVE_API intNDArray
  min (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API intNDArray cummax (int dim = -1) const;
  OCTAVE_API intNDArray
  cummax (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API intNDArray cummin (int dim = -1) const;
  OCTAVE_API intNDArray
  cummin (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API intNDArray prod (int dim) const;
  OCTAVE_API intNDArray sum (int dim) const;
  OCTAVE_API NDArray dsum (int dim) const;
  OCTAVE_API intNDArray cumsum (int dim) const;

  OCTAVE_API intNDArray diff (octave_idx_type order = 1, int dim = -1) const;

  OCTAVE_API intNDArray abs (void) const;
  OCTAVE_API intNDArray signum (void) const;

  intNDArray squeeze (void) const
  { return intNDArray<T> (MArray<T>::squeeze ()); }

  intNDArray transpose (void) const
  { return intNDArray<T> (MArray<T>::transpose ()); }

  OCTAVE_API intNDArray
  concat (const intNDArray<T>& rb, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API intNDArray&
  insert (const intNDArray<T>& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API intNDArray&
  insert (const intNDArray<T>& a, const Array<octave_idx_type>& ra_idx);

  static OCTAVE_API void
  increment_index (Array<octave_idx_type>& ra_idx,
                   const dim_vector& dimensions, int start_dimension = 0);

  static OCTAVE_API octave_idx_type
  compute_index (Array<octave_idx_type>& ra_idx, const dim_vector& dimensions);
};

// i/o

template <typename T>
OCTAVE_API std::ostream&
operator << (std::ostream& os, const intNDArray<T>& a);

template <typename T>
OCTAVE_API std::istream&
operator >> (std::istream& is, intNDArray<T>& a);

#endif
