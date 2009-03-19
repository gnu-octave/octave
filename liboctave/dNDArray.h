/*

Copyright (C) 1996, 1997, 2003, 2004, 2005, 2006, 2007, 2008,
              2009 John W. Eaton

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

#if !defined (octave_NDArray_h)
#define octave_NDArray_h 1

#include "MArrayN.h"
#include "dMatrix.h"
#include "intNDArray.h"

#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
NDArray : public MArrayN<double>
{
public:

  NDArray (void) : MArrayN<double> () { }

  NDArray (const dim_vector& dv) : MArrayN<double> (dv) { }

  NDArray (const dim_vector& dv, double val)
    : MArrayN<double> (dv, val) { }
  
  NDArray (const NDArray& a) : MArrayN<double> (a) { }

  NDArray (const Matrix& a) : MArrayN<double> (a) { }

  NDArray (const Array<octave_idx_type>& a, bool zero_based = false, 
	   bool negative_to_nan = false);

  template <class U>
  NDArray (const MArrayN<U>& a) : MArrayN<double> (a) { }

  template <class U>
  NDArray (const ArrayN<U>& a) : MArrayN<double> (a) { }

  template <class U>
  explicit NDArray (const intNDArray<U>& a) : MArrayN<double> (a) { }

  NDArray (const charNDArray&); 

  NDArray& operator = (const NDArray& a)
    {
      MArrayN<double>::operator = (a);
      return *this;
    }

  // unary operations

  boolNDArray operator ! (void) const;

  bool any_element_is_negative (bool = false) const;
  bool any_element_is_nan (void) const;
  bool any_element_is_inf_or_nan (void) const;
  bool any_element_not_one_or_zero (void) const;
  bool all_elements_are_zero (void) const;
  bool all_elements_are_int_or_inf_or_nan (void) const;
  bool all_integers (double& max_val, double& min_val) const;
  bool too_large_for_float (void) const;

  // FIXME -- this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  NDArray cumprod (int dim = -1) const;
  NDArray cumsum (int dim = -1) const;
  NDArray prod (int dim = -1) const;
  NDArray sum (int dim = -1) const;  
  NDArray sumsq (int dim = -1) const;
  NDArray concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx);
  ComplexNDArray concat (const ComplexNDArray& rb, const Array<octave_idx_type>& ra_idx);
  charNDArray concat (const charNDArray& rb, const Array<octave_idx_type>& ra_idx);

  NDArray max (int dim = 0) const;
  NDArray max (ArrayN<octave_idx_type>& index, int dim = 0) const;
  NDArray min (int dim = 0) const;
  NDArray min (ArrayN<octave_idx_type>& index, int dim = 0) const;
  
  NDArray cummax (int dim = 0) const;
  NDArray cummax (ArrayN<octave_idx_type>& index, int dim = 0) const;
  NDArray cummin (int dim = 0) const;
  NDArray cummin (ArrayN<octave_idx_type>& index, int dim = 0) const;

  NDArray& insert (const NDArray& a, octave_idx_type r, octave_idx_type c);
  NDArray& insert (const NDArray& a, const Array<octave_idx_type>& ra_idx);

  NDArray abs (void) const;
  boolNDArray isnan (void) const;
  boolNDArray isinf (void) const;
  boolNDArray isfinite (void) const;

  ComplexNDArray fourier (int dim = 1) const;
  ComplexNDArray ifourier (int dim = 1) const;

  ComplexNDArray fourier2d (void) const;
  ComplexNDArray ifourier2d (void) const;

  ComplexNDArray fourierNd (void) const;
  ComplexNDArray ifourierNd (void) const;

  friend OCTAVE_API NDArray real (const ComplexNDArray& a);
  friend OCTAVE_API NDArray imag (const ComplexNDArray& a);

  friend class ComplexNDArray;

  Matrix matrix_value (void) const;

  NDArray squeeze (void) const { return MArrayN<double>::squeeze (); }

  static void increment_index (Array<octave_idx_type>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
			    const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const NDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, NDArray& a);

  static double resize_fill_value (void) { return 0; }

  NDArray diag (octave_idx_type k = 0) const;

  typedef double (*dmapper) (double);
  typedef Complex (*cmapper) (const Complex&);
  typedef bool (*bmapper) (double);

  NDArray map (dmapper fcn) const;
  ComplexNDArray map (cmapper fcn) const;
  boolNDArray map (bmapper fcn) const;

private:

  NDArray (double *d, const dim_vector& dv) : MArrayN<double> (d, dv) { }
};

// Publish externally used friend functions.

extern OCTAVE_API NDArray real (const ComplexNDArray& a);
extern OCTAVE_API NDArray imag (const ComplexNDArray& a);

extern OCTAVE_API NDArray min (double d, const NDArray& m);
extern OCTAVE_API NDArray min (const NDArray& m, double d);
extern OCTAVE_API NDArray min (const NDArray& a, const NDArray& b);

extern OCTAVE_API NDArray max (double d, const NDArray& m);
extern OCTAVE_API NDArray max (const NDArray& m, double d);
extern OCTAVE_API NDArray max (const NDArray& a, const NDArray& b);

NDS_CMP_OP_DECLS (NDArray, double, OCTAVE_API)
NDS_BOOL_OP_DECLS (NDArray, double, OCTAVE_API)

SND_CMP_OP_DECLS (double, NDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (double, NDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (NDArray, NDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (NDArray, NDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArrayN, NDArray, double)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
