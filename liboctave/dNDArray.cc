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
#include "dNDArray.h"
#include "mx-base.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"

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

bool
NDArray::any_element_is_negative (bool neg_zero) const
{
  int nel = nelem ();

  if (neg_zero)
    {
      for (int i = 0; i < nel; i++)
	if (lo_ieee_signbit (elem (i)))
	  return true;
    }
  else
    {
      for (int i = 0; i < nel; i++)
	if (elem (i) < 0)
	  return true;
    }

  return false;
}


bool
NDArray::any_element_is_inf_or_nan (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }

  return false;
}

bool
NDArray::all_elements_are_int_or_inf_or_nan (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (xisnan (val) || D_NINT (val) == val)
	continue;
      else
	return false;
    }

  return true;
}

// Return nonzero if any element of M is not an integer.  Also extract
// the largest and smallest values and return them in MAX_VAL and MIN_VAL.

bool
NDArray::all_integers (double& max_val, double& min_val) const
{
  int nel = nelem ();

  if (nel > 0)
    {
      max_val = elem (0);
      min_val = elem (0);
    }
  else
    return false;

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);

      if (val > max_val)
	max_val = val;

      if (val < min_val)
	min_val = val;

      if (D_NINT (val) != val)
	return false;
    }

  return true;
}

bool
NDArray::too_large_for_float (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);

      if (val > FLT_MAX || val < FLT_MIN)
	return true;
    }

  return false;
}

// XXX FIXME XXX -- this is not quite the right thing.

boolNDArray
NDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (MX_ND_ALL_EXPR), true);
}

boolNDArray
NDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ANY_EVAL (MX_ND_ANY_EXPR), false);
}

NDArray
NDArray::cumprod (int dim) const
{
  MX_ND_CUMULATIVE_OP (NDArray, double, 1, *);
}

NDArray
NDArray::cumsum (int dim) const
{
  MX_ND_CUMULATIVE_OP (NDArray, double, 0, +);
}

NDArray
NDArray::prod (int dim) const
{
  MX_ND_REAL_OP_REDUCTION (*= elem (iter_idx), 1);
}

NDArray
NDArray::sumsq (int dim) const
{
  MX_ND_REAL_OP_REDUCTION (+= std::pow (elem (iter_idx), 2), 0);
}

NDArray 
NDArray::sum (int dim) const
{
  MX_ND_REAL_OP_REDUCTION (+= elem (iter_idx), 0);
}

bool
NDArray::cat (NDArray& cat_arr, int dim, int add_dim) const
{
  MX_ND_CAT;
}

NDArray
real (const ComplexNDArray& a)
{
  int a_len = a.length ();
  NDArray retval;
  if (a_len > 0)
    retval = NDArray (mx_inline_real_dup (a.data (), a_len), a.dims ());
  return retval;
}

NDArray
imag (const ComplexNDArray& a)
{
  int a_len = a.length ();
  NDArray retval;
  if (a_len > 0)
    retval = NDArray (mx_inline_imag_dup (a.data (), a_len), a.dims ());
  return retval;
}

NDArray
NDArray::abs (void) const
{
  NDArray retval (dims ());

  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    retval(i) = fabs (elem (i));

  return retval;
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

// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const NDArray& a)
{
  int nel = a.nelem ();

  for (int i = 0; i < nel; i++)
    {
      os << " ";
      octave_write_double (os, a.elem (i));
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, NDArray& a)
{
  int nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      double tmp;
      for (int i = 0; i < nel; i++)
	  {
	    tmp = octave_read_double (is);
	    if (is)
	      a.elem (i) = tmp;
	    else
	      goto done;
	  }
    }

 done:

  return is;
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
