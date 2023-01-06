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

#if ! defined (octave_dNDArray_h)
#define octave_dNDArray_h 1

#include "octave-config.h"

#include "intNDArray-fwd.h"
#include "MArray.h"
#include "bsxfun-decl.h"
#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
NDArray : public MArray<double>
{
public:

  NDArray (void) : MArray<double> () { }

  NDArray (const dim_vector& dv) : MArray<double> (dv) { }

  NDArray (const dim_vector& dv, double val)
    : MArray<double> (dv, val) { }

  NDArray (const NDArray& a) : MArray<double> (a) { }

  NDArray (const Array<octave_idx_type>& a, bool zero_based = false,
           bool negative_to_nan = false);

  template <typename U>
  NDArray (const MArray<U>& a) : MArray<double> (a) { }

  template <typename U>
  NDArray (const Array<U>& a) : MArray<double> (a) { }

  template <typename U>
  explicit NDArray (const intNDArray<U>& a) : MArray<double> (a) { }

  NDArray (const charNDArray&);

  NDArray& operator = (const NDArray& a)
  {
    MArray<double>::operator = (a);
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
  OCTAVE_API bool all_integers (double& max_val, double& min_val) const;
  OCTAVE_API bool all_integers (void) const;
  OCTAVE_API bool too_large_for_float (void) const;

  // FIXME: this is not quite the right thing.

  OCTAVE_API boolNDArray all (int dim = -1) const;
  OCTAVE_API boolNDArray any (int dim = -1) const;

  OCTAVE_API NDArray cumprod (int dim = -1) const;
  OCTAVE_API NDArray cumsum (int dim = -1) const;
  OCTAVE_API NDArray prod (int dim = -1) const;
  OCTAVE_API NDArray sum (int dim = -1) const;
  OCTAVE_API NDArray xsum (int dim = -1) const;
  OCTAVE_API NDArray sumsq (int dim = -1) const;
  OCTAVE_API NDArray
  concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API ComplexNDArray
  concat (const ComplexNDArray& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API charNDArray
  concat (const charNDArray& rb, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API NDArray max (int dim = -1) const;
  OCTAVE_API NDArray max (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API NDArray min (int dim = -1) const;
  OCTAVE_API NDArray min (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API NDArray cummax (int dim = -1) const;
  OCTAVE_API NDArray
  cummax (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API NDArray cummin (int dim = -1) const;
  OCTAVE_API NDArray
  cummin (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API NDArray diff (octave_idx_type order = 1, int dim = -1) const;

  OCTAVE_API NDArray&
  insert (const NDArray& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API NDArray&
  insert (const NDArray& a, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API NDArray abs (void) const;
  OCTAVE_API boolNDArray isnan (void) const;
  OCTAVE_API boolNDArray isinf (void) const;
  OCTAVE_API boolNDArray isfinite (void) const;

  OCTAVE_API ComplexNDArray fourier (int dim = 1) const;
  OCTAVE_API ComplexNDArray ifourier (int dim = 1) const;

  OCTAVE_API ComplexNDArray fourier2d (void) const;
  OCTAVE_API ComplexNDArray ifourier2d (void) const;

  OCTAVE_API ComplexNDArray fourierNd (void) const;
  OCTAVE_API ComplexNDArray ifourierNd (void) const;

  friend OCTAVE_API NDArray real (const ComplexNDArray& a);
  friend OCTAVE_API NDArray imag (const ComplexNDArray& a);

  friend class ComplexNDArray;

  NDArray squeeze (void) const { return MArray<double>::squeeze (); }

  static OCTAVE_API void
  increment_index (Array<octave_idx_type>& ra_idx,
                   const dim_vector& dimensions, int start_dimension = 0);

  static OCTAVE_API octave_idx_type
  compute_index (Array<octave_idx_type>& ra_idx, const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const NDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, NDArray& a);

  OCTAVE_API NDArray diag (octave_idx_type k = 0) const;

  OCTAVE_API NDArray diag (octave_idx_type m, octave_idx_type n) const;

  NDArray& changesign (void)
  {
    MArray<double>::changesign ();
    return *this;
  }

};

// Publish externally used friend functions.

extern OCTAVE_API NDArray real (const ComplexNDArray& a);
extern OCTAVE_API NDArray imag (const ComplexNDArray& a);

MINMAX_DECLS (NDArray, double, OCTAVE_API)

NDS_CMP_OP_DECLS (NDArray, double, OCTAVE_API)
NDS_BOOL_OP_DECLS (NDArray, double, OCTAVE_API)

SND_CMP_OP_DECLS (double, NDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (double, NDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (NDArray, NDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (NDArray, NDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, NDArray, double)

BSXFUN_STDOP_DECLS (NDArray, OCTAVE_API)
BSXFUN_STDREL_DECLS (NDArray, OCTAVE_API)

BSXFUN_OP_DECL (pow, NDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, ComplexNDArray, ComplexNDArray,
                 NDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, ComplexNDArray, NDArray,
                 ComplexNDArray, OCTAVE_API)

#endif
