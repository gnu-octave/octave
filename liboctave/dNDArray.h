/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_NDArray_h)
#define octave_NDArray_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include "MArrayN.h"
#include "dMatrix.h"

#include "mx-defs.h"
#include "mx-op-defs.h"

#include "data-conv.h"
#include "mach-info.h"

class
NDArray : public MArrayN<double>
{
public:
  
  NDArray (void) : MArrayN<double> () { }

  NDArray (const dim_vector& dv) : MArrayN<double> (dv) { }

  NDArray (const dim_vector& dv, double val)
    : MArrayN<double> (dv, val) { }
  
  NDArray (const NDArray& a) : MArrayN<double> (a) { }

  NDArray (const Matrix& a) : MArrayN<double> (a) { }

  NDArray (const MArrayN<double>& a) : MArrayN<double> (a) { }

  NDArray (const ArrayN<double>& a) : MArrayN<double> (a) { }

  explicit NDArray (const boolNDArray& a);

  explicit NDArray (const charNDArray& a);

  NDArray& operator = (const NDArray& a)
    {
      MArrayN<double>::operator = (a);
      return *this;
    }

  // unary operations

  boolNDArray operator ! (void) const;

  bool any_element_is_negative (bool = false) const;
  bool any_element_is_inf_or_nan (void) const;
  bool all_elements_are_int_or_inf_or_nan (void) const;
  bool all_integers (double& max_val, double& min_val) const;
  bool too_large_for_float (void) const;

  // XXX FIXME XXX -- this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  NDArray cumprod (int dim = -1) const;
  NDArray cumsum (int dim = -1) const;
  NDArray prod (int dim = -1) const;
  NDArray sum (int dim = -1) const;  
  NDArray sumsq (int dim = -1) const;
  bool cat (const NDArray& ra_arg, int dim, int add_dim);
     
  NDArray abs (void) const;

  ComplexNDArray fourier (int dim = 1) const;
  ComplexNDArray ifourier (int dim = 1) const;

  ComplexNDArray fourier2d (void) const;
  ComplexNDArray ifourier2d (void) const;

  ComplexNDArray fourierNd (void) const;
  ComplexNDArray ifourierNd (void) const;

  friend NDArray real (const ComplexNDArray& a);
  friend NDArray imag (const ComplexNDArray& a);

  Matrix matrix_value (void) const;

  NDArray squeeze (void) const { return ArrayN<double>::squeeze (); }

  static void increment_index (Array<int>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension = 0);

  static int compute_index (Array<int>& ra_idx,
			    const dim_vector& dimensions);

  // i/o

  friend std::ostream& operator << (std::ostream& os, const NDArray& a);
  friend std::istream& operator >> (std::istream& is, NDArray& a);

  static double resize_fill_value (void) { return 0; }

private:

  NDArray (double *d, const dim_vector& dv) : MArrayN<double> (d, dv) { }
};

NDS_CMP_OP_DECLS (NDArray, double)
NDS_BOOL_OP_DECLS (NDArray, double)

SND_CMP_OP_DECLS (double, NDArray)
SND_BOOL_OP_DECLS (double, NDArray)

NDND_CMP_OP_DECLS (NDArray, NDArray)
NDND_BOOL_OP_DECLS (NDArray, NDArray)

MARRAY_FORWARD_DEFS (MArrayN, NDArray, double)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
