////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
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

#if ! defined (octave_fCNDArray_h)
#define octave_fCNDArray_h 1

#include "octave-config.h"

#include "MArray.h"
#include "bsxfun-decl.h"
#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
FloatComplexNDArray : public MArray<FloatComplex>
{
public:

  FloatComplexNDArray (void) : MArray<FloatComplex> () { }

  FloatComplexNDArray (const dim_vector& dv) : MArray<FloatComplex> (dv) { }

  FloatComplexNDArray (const dim_vector& dv, const FloatComplex& val)
    : MArray<FloatComplex> (dv, val) { }

  FloatComplexNDArray (const FloatComplexNDArray& a)
    : MArray<FloatComplex> (a) { }

  template <typename U>
  FloatComplexNDArray (const MArray<U>& a) : MArray<FloatComplex> (a) { }

  template <typename U>
  FloatComplexNDArray (const Array<U>& a) : MArray<FloatComplex> (a) { }

  OCTAVE_API FloatComplexNDArray (const charNDArray&);

  FloatComplexNDArray& operator = (const FloatComplexNDArray& a)
  {
    MArray<FloatComplex>::operator = (a);
    return *this;
  }

  // unary operations

  OCTAVE_API boolNDArray operator ! (void) const;

  // FIXME: this is not quite the right thing.

  OCTAVE_API bool any_element_is_nan (void) const;
  OCTAVE_API bool any_element_is_inf_or_nan (void) const;
  OCTAVE_API bool all_elements_are_real (void) const;
  OCTAVE_API bool all_integers (float& max_val, float& min_val) const;
  OCTAVE_API bool too_large_for_float (void) const;

  OCTAVE_API boolNDArray all (int dim = -1) const;
  OCTAVE_API boolNDArray any (int dim = -1) const;

  OCTAVE_API FloatComplexNDArray cumprod (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray cumsum (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray prod (int dim = -1) const;
  OCTAVE_API ComplexNDArray dprod (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray sum (int dim = -1) const;
  OCTAVE_API ComplexNDArray dsum (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray sumsq (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray
  concat (const FloatComplexNDArray& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API FloatComplexNDArray
  concat (const FloatNDArray& rb, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API FloatComplexNDArray max (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray
  max (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API FloatComplexNDArray min (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray
  min (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API FloatComplexNDArray cummax (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray
  cummax (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API FloatComplexNDArray cummin (int dim = -1) const;
  OCTAVE_API FloatComplexNDArray
  cummin (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API FloatComplexNDArray
  diff (octave_idx_type order = 1, int dim = -1) const;

  OCTAVE_API FloatComplexNDArray&
  insert (const NDArray& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatComplexNDArray&
  insert (const FloatComplexNDArray& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatComplexNDArray&
  insert (const FloatComplexNDArray& a, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API FloatNDArray abs (void) const;
  OCTAVE_API boolNDArray isnan (void) const;
  OCTAVE_API boolNDArray isinf (void) const;
  OCTAVE_API boolNDArray isfinite (void) const;

  friend OCTAVE_API FloatComplexNDArray conj (const FloatComplexNDArray& a);

  OCTAVE_API FloatComplexNDArray fourier (int dim = 1) const;
  OCTAVE_API FloatComplexNDArray ifourier (int dim = 1) const;

  OCTAVE_API FloatComplexNDArray fourier2d (void) const;
  OCTAVE_API FloatComplexNDArray ifourier2d (void) const;

  OCTAVE_API FloatComplexNDArray fourierNd (void) const;
  OCTAVE_API FloatComplexNDArray ifourierNd (void) const;

  FloatComplexNDArray squeeze (void) const
  { return MArray<FloatComplex>::squeeze (); }

  static OCTAVE_API void
  increment_index (Array<octave_idx_type>& ra_idx,
                   const dim_vector& dimensions, int start_dimension = 0);

  static OCTAVE_API octave_idx_type
  compute_index (Array<octave_idx_type>& ra_idx, const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const FloatComplexNDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               FloatComplexNDArray& a);

  //  bool all_elements_are_real (void) const;
  //  bool all_integers (float& max_val, float& min_val) const;

  OCTAVE_API FloatComplexNDArray diag (octave_idx_type k = 0) const;

  OCTAVE_API FloatComplexNDArray
  diag (octave_idx_type m, octave_idx_type n) const;

  FloatComplexNDArray& changesign (void)
  {
    MArray<FloatComplex>::changesign ();
    return *this;
  }

};

extern OCTAVE_API FloatComplexNDArray conj (const FloatComplexNDArray& a);

MINMAX_DECLS (FloatComplexNDArray, FloatComplex, OCTAVE_API)

NDS_CMP_OP_DECLS (FloatComplexNDArray, FloatComplex, OCTAVE_API)
NDS_BOOL_OP_DECLS (FloatComplexNDArray, FloatComplex, OCTAVE_API)

SND_CMP_OP_DECLS (FloatComplex, FloatComplexNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (FloatComplex, FloatComplexNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (FloatComplexNDArray, FloatComplexNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (FloatComplexNDArray, FloatComplexNDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, FloatComplexNDArray, FloatComplex)

extern OCTAVE_API FloatComplexNDArray& operator *= (FloatComplexNDArray& a,
                                                    float s);
extern OCTAVE_API FloatComplexNDArray& operator /= (FloatComplexNDArray& a,
                                                    float s);

BSXFUN_STDOP_DECLS (FloatComplexNDArray, OCTAVE_API)
BSXFUN_STDREL_DECLS (FloatComplexNDArray, OCTAVE_API)

BSXFUN_OP_DECL (pow, FloatComplexNDArray, OCTAVE_API)

#endif
