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

#include "CNDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"

#include "ArrayN-inline.h"

// XXX FIXME XXX -- could we use a templated mixed-type copy function
// here?

ComplexNDArray::ComplexNDArray (const NDArray& a)
  : MArrayN<Complex> (a.dims ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i) = a.elem (i);
}

ComplexNDArray::ComplexNDArray (const boolNDArray& a)
  : MArrayN<Complex> (a.dims ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i) = a.elem (i);
}

ComplexNDArray::ComplexNDArray (const charNDArray& a)
  : MArrayN<Complex> (a.dims ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i) = a.elem (i);
}

// unary operations

boolNDArray
ComplexNDArray::operator ! (void) const
{
  boolNDArray b (dims ());

  for (int i = 0; i < length (); i++)
    b.elem (i) = elem (i) != 0.0;

  return b;
}

// XXX FIXME XXX -- this is not quite the right thing.

boolNDArray
ComplexNDArray::all (int dim) const
{
  MX_ND_ANY_ALL (MX_ND_ALL_EVAL (real (elem (iter_idx)) == 0
				 && imag (elem (iter_idx)) == 0), true);
}

boolNDArray
ComplexNDArray::any (int dim) const
{
  MX_ND_ANY_ALL (MX_ND_ANY_EVAL (real (elem (iter_idx)) != 0
				 || imag (elem (iter_idx)) != 0), false);
}

ComplexMatrix
ComplexNDArray::matrix_value (void) const
{
  ComplexMatrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = ComplexMatrix (Array2<Complex> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = ComplexMatrix (Array2<Complex> (*this, dimensions(0),
					       dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid converstion of ComplexNDArray to ComplexMatrix");
      break;
    }

  return retval;
}

void
ComplexNDArray::increment_index (Array<int>& ra_idx,
				 const dim_vector& dimensions,
				 int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

int 
ComplexNDArray::compute_index (Array<int>& ra_idx,
			       const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

NDS_CMP_OPS(ComplexNDArray, real, Complex, real)
NDS_BOOL_OPS(ComplexNDArray, Complex, 0.0)

SND_CMP_OPS(Complex, real, ComplexNDArray, real)
SND_BOOL_OPS(Complex, ComplexNDArray, 0.0)

NDND_CMP_OPS(ComplexNDArray, real, ComplexNDArray, real)
NDND_BOOL_OPS(ComplexNDArray, ComplexNDArray, 0.0)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
