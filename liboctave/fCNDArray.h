/*

Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_FloatComplexNDArray_h)
#define octave_FloatComplexNDArray_h 1

#include "MArrayN.h"
#include "fCMatrix.h"

#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
FloatComplexNDArray : public MArrayN<FloatComplex>
{
public:

  FloatComplexNDArray (void) : MArrayN<FloatComplex> () { }

  FloatComplexNDArray (const dim_vector& dv) : MArrayN<FloatComplex> (dv) { }

  FloatComplexNDArray (const dim_vector& dv, const FloatComplex& val)
    : MArrayN<FloatComplex> (dv, val) { }
  
  FloatComplexNDArray (const FloatComplexNDArray& a) : MArrayN<FloatComplex> (a) { }

  FloatComplexNDArray (const FloatComplexMatrix& a) : MArrayN<FloatComplex> (a) { }

  template <class U>
  FloatComplexNDArray (const MArrayN<U>& a) : MArrayN<FloatComplex> (a) { }

  template <class U>
  FloatComplexNDArray (const ArrayN<U>& a) : MArrayN<FloatComplex> (a) { }

  FloatComplexNDArray (const charNDArray&); 

  FloatComplexNDArray& operator = (const FloatComplexNDArray& a)
    {
      MArrayN<FloatComplex>::operator = (a);
      return *this;
    }

  // unary operations

  boolNDArray operator ! (void) const;

  // FIXME -- this is not quite the right thing.

  bool any_element_is_nan (void) const;
  bool any_element_is_inf_or_nan (void) const;
  bool all_elements_are_real (void) const;
  bool all_integers (float& max_val, float& min_val) const;
  bool too_large_for_float (void) const;

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  FloatComplexNDArray cumprod (int dim = -1) const;
  FloatComplexNDArray cumsum (int dim = -1) const;
  FloatComplexNDArray prod (int dim = -1) const;
  FloatComplexNDArray sum (int dim = -1) const;
  FloatComplexNDArray sumsq (int dim = -1) const;
  FloatComplexNDArray concat (const FloatComplexNDArray& rb, const Array<octave_idx_type>& ra_idx);
  FloatComplexNDArray concat (const FloatNDArray& rb, const Array<octave_idx_type>& ra_idx);

  FloatComplexNDArray max (int dim = 0) const;
  FloatComplexNDArray max (ArrayN<octave_idx_type>& index, int dim = 0) const;
  FloatComplexNDArray min (int dim = 0) const;
  FloatComplexNDArray min (ArrayN<octave_idx_type>& index, int dim = 0) const;

  FloatComplexNDArray cummax (int dim = 0) const;
  FloatComplexNDArray cummax (ArrayN<octave_idx_type>& index, int dim = 0) const;
  FloatComplexNDArray cummin (int dim = 0) const;
  FloatComplexNDArray cummin (ArrayN<octave_idx_type>& index, int dim = 0) const;

  FloatComplexNDArray& insert (const NDArray& a, octave_idx_type r, octave_idx_type c);
  FloatComplexNDArray& insert (const FloatComplexNDArray& a, octave_idx_type r, octave_idx_type c);
  FloatComplexNDArray& insert (const FloatComplexNDArray& a, const Array<octave_idx_type>& ra_idx);
  
  FloatNDArray abs (void) const;
  boolNDArray isnan (void) const;
  boolNDArray isinf (void) const;
  boolNDArray isfinite (void) const;

  friend OCTAVE_API FloatComplexNDArray conj (const FloatComplexNDArray& a);

  FloatComplexNDArray fourier (int dim = 1) const;
  FloatComplexNDArray ifourier (int dim = 1) const;

  FloatComplexNDArray fourier2d (void) const;
  FloatComplexNDArray ifourier2d (void) const;

  FloatComplexNDArray fourierNd (void) const;
  FloatComplexNDArray ifourierNd (void) const;

  FloatComplexMatrix matrix_value (void) const;

  FloatComplexNDArray squeeze (void) const { return MArrayN<FloatComplex>::squeeze (); }

  static void increment_index (Array<octave_idx_type>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
			    const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const FloatComplexNDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, FloatComplexNDArray& a);

  static FloatComplex resize_fill_value (void) { return FloatComplex (0.0, 0.0); }

  //  bool all_elements_are_real (void) const;
  //  bool all_integers (float& max_val, float& min_val) const;

  FloatComplexNDArray diag (octave_idx_type k = 0) const;

  typedef float (*dmapper) (const FloatComplex&);
  typedef FloatComplex (*cmapper) (const FloatComplex&);
  typedef bool (*bmapper) (const FloatComplex&);

  FloatNDArray map (dmapper fcn) const;
  FloatComplexNDArray map (cmapper fcn) const;
  boolNDArray map (bmapper fcn) const;

private:

  FloatComplexNDArray (FloatComplex *d, const dim_vector& dv)
    : MArrayN<FloatComplex> (d, dv) { }
};

extern OCTAVE_API FloatComplexNDArray conj (const FloatComplexNDArray& a);

extern OCTAVE_API FloatComplexNDArray min (const FloatComplex& c, const FloatComplexNDArray& m);
extern OCTAVE_API FloatComplexNDArray min (const FloatComplexNDArray& m, const FloatComplex& c);
extern OCTAVE_API FloatComplexNDArray min (const FloatComplexNDArray& a, const FloatComplexNDArray& b);

extern OCTAVE_API FloatComplexNDArray max (const FloatComplex& c, const FloatComplexNDArray& m);
extern OCTAVE_API FloatComplexNDArray max (const FloatComplexNDArray& m, const FloatComplex& c);
extern OCTAVE_API FloatComplexNDArray max (const FloatComplexNDArray& a, const FloatComplexNDArray& b);

NDS_CMP_OP_DECLS (FloatComplexNDArray, FloatComplex, OCTAVE_API)
NDS_BOOL_OP_DECLS (FloatComplexNDArray, FloatComplex, OCTAVE_API)

SND_CMP_OP_DECLS (FloatComplex, FloatComplexNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (FloatComplex, FloatComplexNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (FloatComplexNDArray, FloatComplexNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (FloatComplexNDArray, FloatComplexNDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArrayN, FloatComplexNDArray, FloatComplex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
