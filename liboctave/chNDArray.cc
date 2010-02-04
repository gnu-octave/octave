// N-D Array  manipulations.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array-util.h"
#include "chNDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "mx-op-defs.h"

#include "bsxfun-defs.cc"

// FIXME -- this is not quite the right thing.

boolNDArray
charNDArray::all (int dim) const
{
  return do_mx_red_op<boolMatrix, char> (*this, dim, mx_inline_all);
}

boolNDArray
charNDArray::any (int dim) const
{
  return do_mx_red_op<boolMatrix, char> (*this, dim, mx_inline_any);
}

charNDArray
charNDArray::concat (const charNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

charNDArray
charNDArray::concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  charNDArray tmp (rb.dims ());
  octave_idx_type nel = rb.numel ();

  if (rb.numel () == 0)
    return *this;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double d = rb.elem (i);

      if (xisnan (d))
	{
	  (*current_liboctave_error_handler)
	    ("invalid conversion from NaN to character");
	  return *this;
	}
      else
	{
	  octave_idx_type ival = NINTbig (d);

	  if (ival < 0 || ival > UCHAR_MAX)
	    // FIXME -- is there something
	    // better we could do? Should we warn the user?
	    ival = 0;

	  tmp.elem (i) = static_cast<char>(ival);
	}
    }

  insert (tmp, ra_idx);
  return *this;
}

charNDArray&
charNDArray::insert (const charNDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<char>::insert (a, r, c);
  return *this;
}

charNDArray&
charNDArray::insert (const charNDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<char>::insert (a, ra_idx);
  return *this;
}

charMatrix
charNDArray::matrix_value (void) const
{
  charMatrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = charMatrix (Array2<char> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = charMatrix (Array2<char> (*this, dimensions(0),
					       dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid conversion of charNDArray to charMatrix");
      break;
    }

  return retval;
}

void
charNDArray::increment_index (Array<octave_idx_type>& ra_idx,
			      const dim_vector& dimensions,
			      int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type 
charNDArray::compute_index (Array<octave_idx_type>& ra_idx,
			    const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

charNDArray
charNDArray::diag (octave_idx_type k) const
{
  return Array<char>::diag (k);
}

NDS_CMP_OPS (charNDArray, char)
NDS_BOOL_OPS (charNDArray, char)

SND_CMP_OPS (char, charNDArray)
SND_BOOL_OPS (char, charNDArray)

NDND_CMP_OPS (charNDArray, charNDArray)
NDND_BOOL_OPS (charNDArray, charNDArray)

BSXFUN_STDREL_DEFS_MXLOOP (charNDArray)
