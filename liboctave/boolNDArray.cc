// N-D Array  manipulations.
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

  for (int i = 0; i < length (); i++)
    b.elem (i) = ! elem (i);

  return b;
}

// XXX FIXME XXX -- this is not quite the right thing.

boolNDArray
boolNDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (MX_ND_ALL_EXPR), true);
}

boolNDArray
boolNDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ANY_EVAL (MX_ND_ANY_EXPR), false);
}

boolNDArray
boolNDArray::concat (const boolNDArray& rb, const Array<int>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

boolNDArray&
boolNDArray::insert (const boolNDArray& a, int r, int c)
{
  Array<bool>::insert (a, r, c);
  return *this;
}

boolNDArray&
boolNDArray::insert (const boolNDArray& a, const Array<int>& ra_idx)
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
boolNDArray::increment_index (Array<int>& ra_idx,
			      const dim_vector& dimensions,
			      int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

int
boolNDArray::compute_index (Array<int>& ra_idx,
			    const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
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
