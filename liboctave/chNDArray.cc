// N-D Array  manipulations.
/*

Copyright (C) 2003 John W. Eaton

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
#include "chNDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"
#include "lo-mappers.h"

// XXX FIXME XXX -- this is not quite the right thing.

boolNDArray
charNDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (elem (iter_idx) == ' '), true);
}

boolNDArray
charNDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ANY_EVAL (elem (iter_idx) != ' '), false);
}

charNDArray
charNDArray::concat (const charNDArray& rb, const Array<int>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

charNDArray
charNDArray::concat (const NDArray& rb, const Array<int>& ra_idx)
{
  charNDArray tmp (rb.dims ());
  int nel = rb.numel ();

  if (rb.numel () == 0)
    return *this;

  for (int i = 0; i < nel; i++)
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
	  int ival = NINT (d);

	  if (ival < 0 || ival > UCHAR_MAX)
	    // XXX FIXME XXX -- is there something
	    // better we could do? Should we warn the user?
	    ival = 0;

	  tmp.elem (i) = static_cast<char>(ival);
	}
    }

  insert (tmp, ra_idx);
  return *this;
}

charNDArray&
charNDArray::insert (const charNDArray& a, int r, int c)
{
  Array<char>::insert (a, r, c);
  return *this;
}

charNDArray&
charNDArray::insert (const charNDArray& a, const Array<int>& ra_idx)
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
charNDArray::increment_index (Array<int>& ra_idx,
			      const dim_vector& dimensions,
			      int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

int 
charNDArray::compute_index (Array<int>& ra_idx,
			    const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
