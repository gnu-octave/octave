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
intNDArray<T>::diag (void) const
{
  return diag (0);
}

template <class T>
intNDArray<T>
intNDArray<T>::diag (octave_idx_type k) const
{
  dim_vector dv = this->dims ();
  octave_idx_type nd = dv.length ();

  if (nd > 2)
    {
      (*current_liboctave_error_handler) ("Matrix must be 2-dimensional");    
      return intNDArray<T>();
    }
  else
    {
      octave_idx_type nnr = dv (0);
      octave_idx_type nnc = dv (1);

      if (k > 0)
	nnc -= k;
      else if (k < 0)
	nnr += k;

      intNDArray<T> d;

      if (nnr > 0 && nnc > 0)
	{
	  octave_idx_type ndiag = (nnr < nnc) ? nnr : nnc;

	  d.resize (dim_vector (ndiag, 1));

	  if (k > 0)
	    {
	      for (octave_idx_type i = 0; i < ndiag; i++)
		d.xelem (i) = this->elem (i, i+k);
	    }
	  else if (k < 0)
	    {
	      for (octave_idx_type i = 0; i < ndiag; i++)
		d.xelem (i) = this->elem (i-k, i);
	    }
	  else
	    {
	      for (octave_idx_type i = 0; i < ndiag; i++)
		d.xelem (i) = this->elem (i, i);
	    }
	}
      else
	(*current_liboctave_error_handler)
	  ("diag: requested diagonal out of range");

      return d;
    }
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
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (this->elem (iter_idx) == T (0)), false);
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
