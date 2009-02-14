// N-D Array  manipulations.
/*

Copyright (C) 1996, 1997, 2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array-util.h"
#include "CNDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"

// unary operations

boolNDArray
boolNDArray::operator ! (void) const
{
  boolNDArray b (dims ());

  for (octave_idx_type i = 0; i < length (); i++)
    b.elem (i) = ! elem (i);

  return b;
}

// FIXME -- this is not quite the right thing.

boolNDArray
boolNDArray::all (int dim) const
{
  return do_mx_red_op<boolNDArray> (*this, dim, mx_inline_all);
}

boolNDArray
boolNDArray::any (int dim) const
{
  return do_mx_red_op<boolNDArray> (*this, dim, mx_inline_any);
}

boolNDArray 
boolNDArray::sum (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) |= elem (iter_idx), true, boolNDArray);
}

boolNDArray
boolNDArray::concat (const boolNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

boolNDArray&
boolNDArray::insert (const boolNDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<bool>::insert (a, r, c);
  return *this;
}

boolNDArray&
boolNDArray::insert (const boolNDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<bool>::insert (a, ra_idx);
  return *this;
}



boolMatrix
boolNDArray::matrix_value (void) const
{
  boolMatrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = boolMatrix (Array2<bool> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = boolMatrix (Array2<bool> (*this, dimensions(0),
					 dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid conversion of boolNDArray to boolMatrix");
      break;
    }

  return retval;
}

void
boolNDArray::increment_index (Array<octave_idx_type>& ra_idx,
			      const dim_vector& dimensions,
			      int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type
boolNDArray::compute_index (Array<octave_idx_type>& ra_idx,
			    const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

boolNDArray
boolNDArray::diag (octave_idx_type k) const
{
  return ArrayN<bool>::diag (k);
}

NDND_BOOL_OPS (boolNDArray, boolNDArray, false)
NDND_CMP_OPS (boolNDArray, , boolNDArray, )

NDS_BOOL_OPS (boolNDArray, bool, false)
NDS_CMP_OPS (boolNDArray, , bool, )

SND_BOOL_OPS (bool, boolNDArray, false)
SND_CMP_OPS (bool, , boolNDArray, )

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
