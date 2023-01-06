////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_fNDArray_h)
#define octave_fNDArray_h 1

#include "octave-config.h"

#include "intNDArray-fwd.h"
#include "MArray.h"
#include "bsxfun-decl.h"
#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
FloatNDArray : public MArray<float>
{
public:

  FloatNDArray (void) : MArray<float> () { }

  FloatNDArray (const dim_vector& dv) : MArray<float> (dv) { }

  FloatNDArray (const dim_vector& dv, float val)
    : MArray<float> (dv, val) { }

  FloatNDArray (const FloatNDArray& a) : MArray<float> (a) { }

  template <typename U>
  FloatNDArray (const MArray<U>& a) : MArray<float> (a) { }

  template <typename U>
  FloatNDArray (const Array<U>& a) : MArray<float> (a) { }

  template <typename U>
  explicit FloatNDArray (const intNDArray<U>& a) : MArray<float> (a) { }

  OCTAVE_API FloatNDArray (const charNDArray&);

  FloatNDArray& operator = (const FloatNDArray& a)
  {
    MArray<float>::operator = (a);
    return *this;
  }

  // unary operations

  OCTAVE_API boolNDArray operator ! (void) const;

  OCTAVE_API bool any_element_is_negative (bool = false) const;
  OCTAVE_API bool any_element_is_positive (bool = false) const;
  OCTAVE_API bool any_element_is_nan (void) const;
  OCTAVE_API bool any_element_is_inf_or_nan (void) const;
  OCTAVE_API bool any_element_not_one_or_zero (void) const;
  OCTAVE_API bool all_elements_are_zero (void) const;
  OCTAVE_API bool all_elements_are_int_or_inf_or_nan (void) const;
  OCTAVE_API bool all_integers (float& max_val, float& min_val) const;
  OCTAVE_API bool all_integers (void) const;
  OCTAVE_API bool too_large_for_float (void) const;

  // FIXME: this is not quite the right thing.

  OCTAVE_API boolNDArray all (int dim = -1) const;
  OCTAVE_API boolNDArray any (int dim = -1) const;

  OCTAVE_API FloatNDArray cumprod (int dim = -1) const;
  OCTAVE_API FloatNDArray cumsum (int dim = -1) const;
  OCTAVE_API FloatNDArray prod (int dim = -1) const;
  OCTAVE_API NDArray dprod (int dim = -1) const;
  OCTAVE_API FloatNDArray sum (int dim = -1) const;
  OCTAVE_API NDArray dsum (int dim = -1) const;
  OCTAVE_API FloatNDArray sumsq (int dim = -1) const;
  OCTAVE_API FloatNDArray
  concat (const FloatNDArray& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API FloatComplexNDArray
  concat (const FloatComplexNDArray& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API charNDArray
  concat (const charNDArray& rb, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API FloatNDArray max (int dim = -1) const;
  OCTAVE_API FloatNDArray
  max (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API FloatNDArray min (int dim = -1) const;
  OCTAVE_API FloatNDArray
  min (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API FloatNDArray cummax (int dim = -1) const;
  OCTAVE_API FloatNDArray
  cummax (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API FloatNDArray cummin (int dim = -1) const;
  OCTAVE_API FloatNDArray
  cummin (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API FloatNDArray diff (octave_idx_type order = 1, int dim = -1) const;

  OCTAVE_API FloatNDArray&
  insert (const FloatNDArray& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatNDArray&
  insert (const FloatNDArray& a, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API FloatNDArray abs (void) const;
  OCTAVE_API boolNDArray isnan (void) const;
  OCTAVE_API boolNDArray isinf (void) const;
  OCTAVE_API boolNDArray isfinite (void) const;

  OCTAVE_API FloatComplexNDArray fourier (int dim = 1) const;
  OCTAVE_API FloatComplexNDArray ifourier (int dim = 1) const;

  OCTAVE_API FloatComplexNDArray fourier2d (void) const;
  OCTAVE_API FloatComplexNDArray ifourier2d (void) const;

  OCTAVE_API FloatComplexNDArray fourierNd (void) const;
  OCTAVE_API FloatComplexNDArray ifourierNd (void) const;

  friend OCTAVE_API FloatNDArray real (const FloatComplexNDArray& a);
  friend OCTAVE_API FloatNDArray imag (const FloatComplexNDArray& a);

  friend class FloatComplexNDArray;

  FloatNDArray squeeze (void) const { return MArray<float>::squeeze (); }

  static OCTAVE_API void
  increment_index (Array<octave_idx_type>& ra_idx,
                   const dim_vector& dimensions, int start_dimension = 0);

  static OCTAVE_API octave_idx_type
  compute_index (Array<octave_idx_type>& ra_idx, const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatNDArray& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, FloatNDArray& a);

  OCTAVE_API FloatNDArray diag (octave_idx_type k = 0) const;

  OCTAVE_API FloatNDArray diag (octave_idx_type m, octave_idx_type n) const;

  FloatNDArray& changesign (void)
  {
    MArray<float>::changesign ();
    return *this;
  }

};

// Publish externally used friend functions.

extern OCTAVE_API FloatNDArray real (const FloatComplexNDArray& a);
extern OCTAVE_API FloatNDArray imag (const FloatComplexNDArray& a);

MINMAX_DECLS (FloatNDArray, float, OCTAVE_API)

NDS_CMP_OP_DECLS (FloatNDArray, float, OCTAVE_API)
NDS_BOOL_OP_DECLS (FloatNDArray, float, OCTAVE_API)

SND_CMP_OP_DECLS (float, FloatNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (float, FloatNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (FloatNDArray, FloatNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (FloatNDArray, FloatNDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, FloatNDArray, float)

BSXFUN_STDOP_DECLS (FloatNDArray, OCTAVE_API)
BSXFUN_STDREL_DECLS (FloatNDArray, OCTAVE_API)

BSXFUN_OP_DECL (pow, FloatNDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, FloatComplexNDArray, FloatComplexNDArray,
                 FloatNDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, FloatComplexNDArray, FloatNDArray,
                 FloatComplexNDArray, OCTAVE_API)

#endif
