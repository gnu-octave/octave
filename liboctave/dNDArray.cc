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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dNDArray.h"
#include "mx-base.h"
#include "lo-error.h"
#include "lo-ieee.h"

#include "ArrayN-inline.h"

NDArray::NDArray (const boolNDArray& a)
  : MArrayN<double> (a.dims ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i) = a.elem (i);
}

NDArray::NDArray (const charNDArray& a)
  : MArrayN<double> (a.dims ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i) = a.elem (i);
}

// unary operations

boolNDArray
NDArray::operator ! (void) const
{
  boolNDArray b (dims ());

  for (int i = 0; i < length (); i++)
    b.elem (i) = ! elem (i);

  return b;
}

// XXX FIXME XXX -- this is not quite the right thing.

boolNDArray
NDArray::all (int dim) const
{
  MX_ND_ALL_ANY (MX_ND_ALL_EVAL (MX_ND_ALL_EXPR));
}

boolNDArray
NDArray::any (int dim) const
{
  MX_ND_ALL_ANY (MX_ND_ANY_EVAL (MX_ND_ANY_EXPR));
}

Matrix
NDArray::matrix_value (void) const
{
  Matrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = Matrix (Array2<double> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = Matrix (Array2<double> (*this, dimensions(0), dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid converstion of NDArray to Matrix");
      break;
    }

  return retval;
}

void
NDArray::increment_index (Array<int>& ra_idx,
			  const dim_vector& dimensions,
			  int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

int
NDArray::compute_index (Array<int>& ra_idx,
			const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

bool
NDArray::any_element_is_negative (bool neg_zero) const
{
  int n = length (); 
  if (neg_zero)
    {
      for (int i = 0; i < n; i++)
	if (lo_ieee_signbit (Array<double>::elem (i)))
	  return true;
    }
  else
    {
      for (int i = 0; i < n; i++)
	if (Array<double>::elem (i) < 0)
	  return true;
    }
 
 return false;
}

bool
NDArray::all_integers (double& max_val, double& min_val) const
{
  int n = length ();

  if (n > 0)
    {
      max_val = Array<double>::elem (0);
      min_val = Array<double>::elem (0);
    }
  else 
    return false;

  for (int i = 0; i < n; i++)
    {
      double val = Array<double>::elem (0);
      
      if (val > max_val)
	max_val = val;

      if (val < min_val)
	min_val = val;

      if (D_NINT (val) != val)
	return false;
    }

  return true;
}

NDS_CMP_OPS(NDArray, , double, )
NDS_BOOL_OPS(NDArray, double, 0.0)

SND_CMP_OPS(double, , NDArray, )
SND_BOOL_OPS(double, NDArray, 0.0)

NDND_CMP_OPS(NDArray, , NDArray, )
NDND_BOOL_OPS(NDArray, NDArray, 0.0)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
