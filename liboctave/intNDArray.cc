// N-D Array  manipulations.
/*

Copyright (C) 2004, 2005, 2006, 2007 John W. Eaton

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
#include "mx-base.h"
#include "lo-ieee.h"

// unary operations

template <class T>
boolNDArray
intNDArray<T>::operator ! (void) const
{
  boolNDArray b (this->dims ());

  for (octave_idx_type i = 0; i < this->length (); i++)
    b.elem (i) = ! this->elem (i);

  return b;
}

template <class T>
bool
intNDArray<T>::any_element_not_one_or_zero (void) const
{
  octave_idx_type nel = this->nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      T val = this->elem (i);

      if (val != 0.0 && val != 1.0)
	return true;
    }

  return false;
}

template <class T>
intNDArray<T>
intNDArray<T>::diag (octave_idx_type k) const
{
  return MArrayN<T>::diag (k);
}

// FIXME -- this is not quite the right thing.

template <class T>
boolNDArray
intNDArray<T>::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (this->elem (iter_idx) == T (0)), true);
}

template <class T>
boolNDArray
intNDArray<T>::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ANY_EVAL (this->elem (iter_idx) != T (0)), false);
}

template <class T>
void
intNDArray<T>::increment_index (Array<octave_idx_type>& ra_idx,
			       const dim_vector& dimensions,
			       int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

template <class T>
octave_idx_type 
intNDArray<T>::compute_index (Array<octave_idx_type>& ra_idx,
			      const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

template <class T>
intNDArray<T>
intNDArray<T>::concat (const intNDArray<T>& rb, const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

template <class T>
intNDArray<T>&
intNDArray<T>::insert (const intNDArray<T>& a, octave_idx_type r, octave_idx_type c)
{
  Array<T>::insert (a, r, c);
  return *this;
}

template <class T>
intNDArray<T>&
intNDArray<T>::insert (const intNDArray<T>& a, const Array<octave_idx_type>& ra_idx)
{
  Array<T>::insert (a, ra_idx);
  return *this;
}

// This contains no information on the array structure !!!

template <class T>
std::ostream&
operator << (std::ostream& os, const intNDArray<T>& a)
{
  octave_idx_type nel = a.nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    os << " " << a.elem (i) << "\n";

  return os;
}

template <class T>
std::istream&
operator >> (std::istream& is, intNDArray<T>& a)
{
  octave_idx_type nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      T tmp;

      for (octave_idx_type i = 0; i < nel; i++)
	{
	  is >> tmp;

	  if (is)
	    a.elem (i) = tmp;
	  else
	    goto done;
	}
    }

 done:

  return is;
}

// FIXME -- should abs and signum just be mapper functions?

template <class T>
intNDArray<T>
intNDArray<T>::abs (void) const
{
  octave_idx_type nel = this->nelem ();
  intNDArray<T> ret (*this);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      T val = this->elem (i);
      ret.xelem (i) = val.abs ();
    }

  return ret;
}

template <class T>
intNDArray<T>
intNDArray<T>::signum (void) const
{
  octave_idx_type nel = this->nelem ();
  intNDArray<T> ret (*this);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      T val = this->elem (i);
      ret.xelem (i) = val.signum ();
    }

  return ret;
}

template <class T>
intNDArray<T>
intNDArray<T>::sum (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) += intNDArray<T>::elem (iter_idx), 0, intNDArray<T>);
}

template <class T>
intNDArray<T>
intNDArray<T>::max (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return max (dummy_idx, dim);
}

template <class T>
intNDArray<T>
intNDArray<T>::max (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = this->dims ();
  dim_vector dr = this->dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return intNDArray<T> ();
  
  dr(dim) = 1;

  intNDArray<T> result (dr);
  idx_arg.resize (dr);

  octave_idx_type x_stride = 1;
  octave_idx_type x_len = dv(dim);
  for (int i = 0; i < dim; i++)
    x_stride *= dv(i);

  for (octave_idx_type i = 0; i < dr.numel (); i++)
    {
      octave_idx_type x_offset;
      if (x_stride == 1)
	x_offset = i * x_len;
      else
	{
	  octave_idx_type x_offset2 = 0;
	  x_offset = i;
	  while (x_offset >= x_stride)
	    {
	      x_offset -= x_stride;
	      x_offset2++;
	    }
	  x_offset += x_offset2 * x_stride * x_len;
	}

      octave_idx_type idx_j = 0;

      T tmp_max = this->elem (x_offset);

      for (octave_idx_type j = 1; j < x_len; j++)
	{
	  T tmp = this->elem (j * x_stride + x_offset);

	  if (tmp > tmp_max)
	    {
	      idx_j = j;
	      tmp_max = tmp;
	    }
	}

      result.elem (i) = tmp_max;
      idx_arg.elem (i) = idx_j;
    }

  result.chop_trailing_singletons ();
  idx_arg.chop_trailing_singletons ();

  return result;
}

template <class T>
intNDArray<T>
intNDArray<T>::min (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return min (dummy_idx, dim);
}

template <class T>
intNDArray<T>
intNDArray<T>::min (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = this->dims ();
  dim_vector dr = this->dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return intNDArray<T> ();
  
  dr(dim) = 1;

  intNDArray<T> result (dr);
  idx_arg.resize (dr);

  octave_idx_type x_stride = 1;
  octave_idx_type x_len = dv(dim);
  for (int i = 0; i < dim; i++)
    x_stride *= dv(i);

  for (octave_idx_type i = 0; i < dr.numel (); i++)
    {
      octave_idx_type x_offset;
      if (x_stride == 1)
	x_offset = i * x_len;
      else
	{
	  octave_idx_type x_offset2 = 0;
	  x_offset = i;
	  while (x_offset >= x_stride)
	    {
	      x_offset -= x_stride;
	      x_offset2++;
	    }
	  x_offset += x_offset2 * x_stride * x_len;
	}

      octave_idx_type idx_j = 0;

      T tmp_min = this->elem (x_offset);

      for (octave_idx_type j = 1; j < x_len; j++)
	{
	  T tmp = this->elem (j * x_stride + x_offset);

	  if (tmp < tmp_min)
	    {
	      idx_j = j;
	      tmp_min = tmp;
	    }
	}

      result.elem (i) = tmp_min;
      idx_arg.elem (i) = idx_j;
    }

  result.chop_trailing_singletons ();
  idx_arg.chop_trailing_singletons ();

  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
