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

#include <cfloat>

#include "Array-util.h"
#include "CNDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"
#include "lo-mappers.h"

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

bool
ComplexNDArray::any_element_is_inf_or_nan (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      Complex val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }
  return false;
}

// Return true if no elements have imaginary components.

bool
ComplexNDArray::all_elements_are_real (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      double ip = imag (elem (i));

      if (ip != 0.0 || lo_ieee_signbit (ip))
	return false;
    }

  return true;
}

// Return nonzero if any element of CM has a non-integer real or
// imaginary part.  Also extract the largest and smallest (real or
// imaginary) values and return them in MAX_VAL and MIN_VAL. 

bool
ComplexNDArray::all_integers (double& max_val, double& min_val) const
{
  int nel = nelem ();

  if (nel > 0)
    {
      Complex val = elem (0);

      double r_val = real (val);
      double i_val = imag (val);
      
      max_val = r_val;
      min_val = r_val;

      if (i_val > max_val)
	max_val = i_val;

      if (i_val < max_val)
	min_val = i_val;
    }
  else
    return false;

  for (int i = 0; i < nel; i++)
    {
      Complex val = elem (i);

      double r_val = real (val);
      double i_val = imag (val);

      if (r_val > max_val)
	max_val = r_val;

      if (i_val > max_val)
	max_val = i_val;

      if (r_val < min_val)
	min_val = r_val;

      if (i_val < min_val)
	min_val = i_val;

      if (D_NINT (r_val) != r_val || D_NINT (i_val) != i_val)
	return false;
    }

  return true;
}

bool
ComplexNDArray::too_large_for_float (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      Complex val = elem (i);

      double r_val = real (val);
      double i_val = imag (val);

      if (r_val > FLT_MAX
	  || i_val > FLT_MAX
	  || r_val < FLT_MIN
	  || i_val < FLT_MIN)
	return true;
    }

  return false;
}

boolNDArray
ComplexNDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ALL_EVAL (elem (iter_idx) == Complex (0, 0)), true);
}

boolNDArray
ComplexNDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ANY_EVAL (elem (iter_idx) != Complex (0, 0)), false);
}

ComplexNDArray
ComplexNDArray::cumprod (int dim) const
{
  MX_ND_CUMULATIVE_OP (ComplexNDArray, Complex, Complex (1, 0), *);
}

ComplexNDArray
ComplexNDArray::cumsum (int dim) const
{
  MX_ND_CUMULATIVE_OP (ComplexNDArray, Complex, Complex (0, 0), +);
}

ComplexNDArray
ComplexNDArray::prod (int dim) const
{
  MX_ND_COMPLEX_OP_REDUCTION (*= elem (iter_idx), Complex (1, 0));
}

ComplexNDArray
ComplexNDArray::sumsq (int dim) const
{
  MX_ND_COMPLEX_OP_REDUCTION
    (+= imag (elem (iter_idx))
     ? elem (iter_idx) * conj (elem (iter_idx))
     : std::pow (elem (iter_idx), 2), Complex (0, 0));
}

ComplexNDArray 
ComplexNDArray::sum (int dim) const
{
  MX_ND_COMPLEX_OP_REDUCTION (+= elem (iter_idx), Complex (0, 0));
}

bool
ComplexNDArray::cat (const ComplexNDArray& ra_arg, int dim, int add_dim)
{
  MX_ND_CAT;
}

NDArray
ComplexNDArray::abs (void) const
{
  NDArray retval (dims ());

  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    retval(i) = ::abs (elem (i));
      
  return retval;
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


// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const ComplexNDArray& a)
{
  int nel = a.nelem ();

  for (int i = 0; i < nel; i++)
    {
      os << " ";
      octave_write_complex (os, a.elem (i));
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, ComplexNDArray& a)
{
  int nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      Complex tmp;
      for (int i = 0; i < nel; i++)
	  {
	    tmp = octave_read_complex (is);
	    if (is)
	      a.elem (i) = tmp;
	    else
	      goto done;
	  }
    }

 done:

  return is;
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
