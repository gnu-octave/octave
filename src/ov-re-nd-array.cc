/*

Copyright (C) 2000 John W. Eaton

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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"

#include "gripes.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-base-nd-array.h"
#include "ov-base-nd-array.cc"
#include "ov-re-nd-array.h"
#include "pr-output.h"
#include "variables.h"

template class octave_base_nd_array<ArrayN<double> >;

DEFINE_OCTAVE_ALLOCATOR (octave_double_nd_array);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_double_nd_array,
				     "double-nd-array");

octave_value *
octave_double_nd_array::try_narrowing_conversion (void)
{
  octave_value *retval = 0;

  array.maybe_delete_dims ();

  int n_dims = array.dims ().length ();
  Array<int> dims = array.dims();

  // Scalar
  if (n_dims == 1 && dims(0) == 1)
    {
      retval = new octave_scalar (array(Array<int> (1,0)));
    }

  // Matrix
  if (n_dims == 2 || n_dims == 1)
    {
      retval = new octave_matrix (matrix_value ());
    }



  return retval;
}

void
octave_double_nd_array::assign (const octave_value_list& idx,
				const ArrayN<double>& rhs)
{
  int len = idx.length ();

  if ( len < 0)
    {
      error ("invalid number of indices (%d) for indexed matrix assignment",
	     len);
      return;
    }  

  // XXX FIXME XXX -- where should this go?
  array.set_max_indices (len);
  
  array.clear_index ();
  

  for (int i = 0; i < len; i++) 
    {
      idx_vector temp = idx(i).index_vector ();
      array.set_index(temp);
    }

  ::assign (array, rhs);

  // When subclasses of ArrayN are constructed, add an extra arguemnt
  // resize_fill_value ()
}

bool
octave_double_nd_array::valid_as_scalar_index (void) const
{
  // XXX FIXME XXX
  return false;
}

double
octave_double_nd_array::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  // XXX FIXME XXX

  gripe_invalid_conversion ("real nd-array", "real scalar");

  return retval;
}


Matrix
octave_double_nd_array::matrix_value (bool ) const
{
  Array<int> ra_idx (array.dimensions.length (), 0);

  return convert_slice_to_matrix (ra_idx);
}

Matrix
octave_double_nd_array::convert_slice_to_matrix (const Array<int>& ra_idx) const
{
  int n_dims = array.dimensions.length ();

  assert(ra_idx.length () == n_dims);

  Array<int> idx (ra_idx);
  Matrix retval;

  if (n_dims > 1)
    {
      int d1 = array.dimensions(0);
      int d2 = array.dimensions(1);

      retval = Matrix (d1, d2);

      for (int i = 0; i < d1; i++)
        {
          idx(0) = i;
          for (int j = 0; j < d2; j++)
            {
              idx(1) = j;
              retval(i,j) = array.elem (idx);
            }
        }
    }
  else if (n_dims == 1)
    {
      int d1 = array.dimensions(0);
      int d2 = 1;

      retval = Matrix (d1, d2);

      for (int i = 0; i < d1; i++)
        {
	  idx(0) = i;
	  retval(i, 0) = array.elem (idx);
	}

    }
  return retval;
}

Complex
octave_double_nd_array::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  gripe_invalid_conversion ("real matrix", "complex scalar");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
