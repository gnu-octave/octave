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
#include "ov-base.h"
#include "ov-base-int.h"
#include "ov-typeinfo.h"

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

  octave_value *
  clone (void) const
    { return new OCTAVE_VALUE_INT_MATRIX_T (*this); }

  octave_value *
  empty_clone (void) const
    { return new OCTAVE_VALUE_INT_MATRIX_T (); }

  OCTAVE_INT_NDARRAY_T
  OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION (void) const
    { return matrix; }

  NDArray
  array_value (bool = false) const
    { 
      NDArray retval (matrix.dims ()); 
      int nel = matrix.numel ();
      for (int i = 0; i < nel; i++)
        retval (i) = double (matrix(i));
      return retval;
    }

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

  octave_value *
  clone (void) const
    { return new OCTAVE_VALUE_INT_SCALAR_T (*this); }

  octave_value *
  empty_clone (void) const
    { return new OCTAVE_VALUE_INT_SCALAR_T (); }

  OCTAVE_INT_T
  OCTAVE_VALUE_INT_SCALAR_EXTRACTOR_FUNCTION (void) const
    { return scalar; }

  OCTAVE_INT_NDARRAY_T
  OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION (void) const
    { return OCTAVE_INT_NDARRAY_T (dim_vector (1, 1), scalar); }

  octave_value resize (const dim_vector& dv) const
    { OCTAVE_INT_NDARRAY_T retval (dv); if (dv.numel()) retval(0) = scalar; return retval; }

  NDArray
  array_value (bool = false) const
    { 
      NDArray retval (dim_vector (1,1)); 
      retval (0) = double (scalar);
      return retval;
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
