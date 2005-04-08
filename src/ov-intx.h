/*

Copyright (C) 2004 John W. Eaton

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

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "oct-alloc.h"
#include "so-array.h"
#include "str-vec.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-base-int.h"
#include "ov-typeinfo.h"
#include "gripes.h"

class
OCTAVE_VALUE_INT_MATRIX_T
  : public octave_base_int_matrix<OCTAVE_INT_NDARRAY_T>
{
public:

  OCTAVE_VALUE_INT_MATRIX_T (void)
    : octave_base_int_matrix<OCTAVE_INT_NDARRAY_T> () { }

  OCTAVE_VALUE_INT_MATRIX_T (const OCTAVE_INT_NDARRAY_T& nda)
    : octave_base_int_matrix<OCTAVE_INT_NDARRAY_T> (nda) { }

  ~OCTAVE_VALUE_INT_MATRIX_T (void) { }

  octave_value *clone (void) const
    { return new OCTAVE_VALUE_INT_MATRIX_T (*this); }

  octave_value *empty_clone (void) const
    { return new OCTAVE_VALUE_INT_MATRIX_T (); }

  OCTAVE_INT_NDARRAY_T
  OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION (void) const
    { return matrix; }

  double
  double_value (bool = false) const
    {
      double retval = lo_ieee_nan_value ();

      if (numel () > 0)
	{
	  // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
	  if (Vwarn_fortran_indexing)
	    gripe_implicit_conversion (type_name (), "real scalar");

	  retval = double (matrix(0));
	}
      else
	gripe_invalid_conversion (type_name (), "real scalar");

      return retval;
      
    }

  double scalar_value (bool = false) const { return double_value (); }

  NDArray
  array_value (bool = false) const
    { 
      NDArray retval (matrix.dims ()); 
      int nel = matrix.numel ();
      for (int i = 0; i < nel; i++)
        retval(i) = double (matrix(i));
      return retval;
    }

  ComplexNDArray
  complex_array_value (bool = false) const
    { 
      ComplexNDArray retval (matrix.dims ()); 
      int nel = matrix.numel ();
      for (int i = 0; i < nel; i++)
        retval(i) = Complex (double (matrix(i)));
      return retval;
    }

  idx_vector index_vector (void) const { return idx_vector (matrix); }

  int write (octave_stream& os, int block_size,
	     oct_data_conv::data_type output_type, int skip,
	     oct_mach_info::float_format flt_fmt) const
    { return os.write (matrix, block_size, output_type, skip, flt_fmt); }

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

class
OCTAVE_VALUE_INT_SCALAR_T
  : public octave_base_int_scalar<OCTAVE_INT_T>
{
public:

  OCTAVE_VALUE_INT_SCALAR_T (void)
    : octave_base_int_scalar<OCTAVE_INT_T> () { }

  OCTAVE_VALUE_INT_SCALAR_T (const OCTAVE_INT_T& nda)
    : octave_base_int_scalar<OCTAVE_INT_T> (nda) { }

  ~OCTAVE_VALUE_INT_SCALAR_T (void) { }

  octave_value *clone (void) const
    { return new OCTAVE_VALUE_INT_SCALAR_T (*this); }

  octave_value *empty_clone (void) const
    { return new OCTAVE_VALUE_INT_SCALAR_T (); }

  octave_value do_index_op (const octave_value_list& idx, int resize_ok)
    {
      octave_value retval;

      if (idx.valid_scalar_indices ())
	retval = scalar;
      else
	{
	  // XXX FIXME XXX -- this doesn't solve the problem of
	  //
	  //   a = 1; a([1,1], [1,1], [1,1])
	  //
	  // and similar constructions.  Hmm...

	  // XXX FIXME XXX -- using this constructor avoids narrowing the
	  // 1x1 matrix back to a scalar value.  Need a better solution
	  // to this problem.

	  octave_value tmp
	    (new OCTAVE_VALUE_INT_MATRIX_T
	     (OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION ())); 

	  retval = tmp.do_index_op (idx, resize_ok);
	}

      return retval;
    }

  OCTAVE_INT_T
  OCTAVE_VALUE_INT_SCALAR_EXTRACTOR_FUNCTION (void) const
    { return scalar; }

  OCTAVE_INT_NDARRAY_T
  OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION (void) const
    { return OCTAVE_INT_NDARRAY_T (dim_vector (1, 1), scalar); }

  octave_value resize (const dim_vector& dv) const
    {
      OCTAVE_INT_NDARRAY_T retval (dv);
      if (dv.numel())
	retval(0) = scalar;
      return retval;
    }

  double double_value (bool = false) const { return double (scalar); }

  double scalar_value (bool = false) const { return double (scalar); }

  NDArray
  array_value (bool = false) const
    { 
      NDArray retval (dim_vector (1,1)); 
      retval(0) = double (scalar);
      return retval;
    }

  ComplexNDArray
  complex_array_value (bool = false) const
    { 
      ComplexNDArray retval (dim_vector (1,1)); 
      retval(0) = Complex (double (scalar));
      return retval;
    }

  idx_vector index_vector (void) const { return idx_vector (scalar); }

  int write (octave_stream& os, int block_size,
	     oct_data_conv::data_type output_type, octave_idx_type skip,
	     oct_mach_info::float_format flt_fmt) const
    {
      return os.write (OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION (),
		       block_size, output_type, skip, flt_fmt);
    }

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
