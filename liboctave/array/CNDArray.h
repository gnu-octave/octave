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

#if ! defined (octave_CNDArray_h)
#define octave_CNDArray_h 1

#include "octave-config.h"

#include "MArray.h"
#include "bsxfun-decl.h"
#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
ComplexNDArray : public MArray<Complex>
{
public:

  ComplexNDArray (void) : MArray<Complex> () { }

  ComplexNDArray (const dim_vector& dv) : MArray<Complex> (dv) { }

  ComplexNDArray (const dim_vector& dv, const Complex& val)
    : MArray<Complex> (dv, val) { }

  ComplexNDArray (const ComplexNDArray& a) : MArray<Complex> (a) { }

  template <typename U>
  ComplexNDArray (const MArray<U>& a) : MArray<Complex> (a) { }

  template <typename U>
  ComplexNDArray (const Array<U>& a) : MArray<Complex> (a) { }

  ComplexNDArray (const charNDArray&);

  ComplexNDArray& operator = (const ComplexNDArray& a)
  {
    MArray<Complex>::operator = (a);
    return *this;
  }

  // unary operations

  OCTAVE_API boolNDArray operator ! (void) const;

  // FIXME: this is not quite the right thing.

  OCTAVE_API bool any_element_is_nan (void) const;
  OCTAVE_API bool any_element_is_inf_or_nan (void) const;
  OCTAVE_API bool all_elements_are_real (void) const;
  OCTAVE_API bool all_integers (double& max_val, double& min_val) const;
  OCTAVE_API bool too_large_for_float (void) const;

  OCTAVE_API boolNDArray all (int dim = -1) const;
  OCTAVE_API boolNDArray any (int dim = -1) const;

  OCTAVE_API ComplexNDArray cumprod (int dim = -1) const;
  OCTAVE_API ComplexNDArray cumsum (int dim = -1) const;
  OCTAVE_API ComplexNDArray prod (int dim = -1) const;
  OCTAVE_API ComplexNDArray sum (int dim = -1) const;
  OCTAVE_API ComplexNDArray xsum (int dim = -1) const;
  OCTAVE_API ComplexNDArray sumsq (int dim = -1) const;
  OCTAVE_API ComplexNDArray
  concat (const ComplexNDArray& rb, const Array<octave_idx_type>& ra_idx);
  OCTAVE_API ComplexNDArray
  concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API ComplexNDArray max (int dim = -1) const;
  OCTAVE_API ComplexNDArray
  max (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API ComplexNDArray min (int dim = -1) const;
  OCTAVE_API ComplexNDArray
  min (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API ComplexNDArray cummax (int dim = -1) const;
  OCTAVE_API ComplexNDArray
  cummax (Array<octave_idx_type>& index, int dim = -1) const;
  OCTAVE_API ComplexNDArray cummin (int dim = -1) const;
  OCTAVE_API ComplexNDArray
  cummin (Array<octave_idx_type>& index, int dim = -1) const;

  OCTAVE_API ComplexNDArray
  diff (octave_idx_type order = 1, int dim = -1) const;

  OCTAVE_API ComplexNDArray&
  insert (const NDArray& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexNDArray&
  insert (const ComplexNDArray& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexNDArray&
  insert (const ComplexNDArray& a, const Array<octave_idx_type>& ra_idx);

  OCTAVE_API NDArray abs (void) const;
  OCTAVE_API boolNDArray isnan (void) const;
  OCTAVE_API boolNDArray isinf (void) const;
  OCTAVE_API boolNDArray isfinite (void) const;

  friend OCTAVE_API ComplexNDArray conj (const ComplexNDArray& a);

  OCTAVE_API ComplexNDArray fourier (int dim = 1) const;
  OCTAVE_API ComplexNDArray ifourier (int dim = 1) const;

  OCTAVE_API ComplexNDArray fourier2d (void) const;
  OCTAVE_API ComplexNDArray ifourier2d (void) const;

  OCTAVE_API ComplexNDArray fourierNd (void) const;
  OCTAVE_API ComplexNDArray ifourierNd (void) const;

  ComplexNDArray squeeze (void) const { return MArray<Complex>::squeeze (); }

  static OCTAVE_API void
  increment_index (Array<octave_idx_type>& ra_idx,
                   const dim_vector& dimensions,
                   int start_dimension = 0);

  static OCTAVE_API octave_idx_type
  compute_index (Array<octave_idx_type>& ra_idx,
                 const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const ComplexNDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               ComplexNDArray& a);

  //  bool all_elements_are_real (void) const;
  //  bool all_integers (double& max_val, double& min_val) const;

  OCTAVE_API ComplexNDArray diag (octave_idx_type k = 0) const;

  OCTAVE_API ComplexNDArray diag (octave_idx_type m, octave_idx_type n) const;

  ComplexNDArray& changesign (void)
  {
    MArray<Complex>::changesign ();
    return *this;
  }

};

extern OCTAVE_API ComplexNDArray conj (const ComplexNDArray& a);

MINMAX_DECLS (ComplexNDArray, Complex, OCTAVE_API)

NDS_CMP_OP_DECLS (ComplexNDArray, Complex, OCTAVE_API)
NDS_BOOL_OP_DECLS (ComplexNDArray, Complex, OCTAVE_API)

SND_CMP_OP_DECLS (Complex, ComplexNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (Complex, ComplexNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (ComplexNDArray, ComplexNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (ComplexNDArray, ComplexNDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, ComplexNDArray, Complex)

extern OCTAVE_API ComplexNDArray& operator *= (ComplexNDArray& a, double s);
extern OCTAVE_API ComplexNDArray& operator /= (ComplexNDArray& a, double s);

BSXFUN_STDOP_DECLS (ComplexNDArray, OCTAVE_API)
BSXFUN_STDREL_DECLS (ComplexNDArray, OCTAVE_API)

BSXFUN_OP_DECL (pow, ComplexNDArray, OCTAVE_API)

#endif
