// Inline functions used by ArrayN-idx.h and ArrayN.cc
/*

Copyright (C) 2000 John W. Eaton

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

#ifndef octave_ArrayN_inline_h
#define octave_ArrayN_inline_h 1

#include "idx-vector.h"

static inline bool
index_in_bounds (const Array<int>& ra_idx, const dim_vector& dimensions)
{
  bool retval = true;

  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      for (int i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || ra_idx(i) > dimensions (i))
	    {
	      retval = false;
	      break;
	    }
	}
    }
  else
    retval = false;

  return retval;
}

static inline void
increment_index (Array<int>& ra_idx, const dim_vector& dimensions,
		 int start_dimension = 0)
{
  ra_idx(start_dimension)++;

  int n = ra_idx.length () - 1;

  for (int i = start_dimension; i < n; i++)
    {
      if (ra_idx(i) < dimensions(i))
 	break;
      else
 	{
 	  ra_idx(i) = 0;
 	  ra_idx(i+1)++;
 	}
    }
}

static inline int
get_scalar_idx (Array<int>& idx, dim_vector& dims)
{
  int retval (-1);

  int n = idx.length ();

  if (n > 0)
    {
      retval = idx(--n);

      while (--n >= 0)
	{      		
	  retval *= dims (n);
	
	  retval += idx(n);
	}
    }
  return retval;
}

static inline int
num_ones (const Array<int>& ra_idx)
{
  int retval (0);
  for (int i = 0; i < ra_idx.length (); i++)
    {
      if (ra_idx (i) == 1)
	retval++;
    }
  return retval;
}

static inline bool
is_scalar (const dim_vector& dim)
{
  bool retval = true;

  int n = dim.length ();

  if (n == 0)
    {
      retval = false;
    }
  else
    {
      for (int i = 0; i < n; i ++)
	{
	  if (dim (i) != 1)
	    {
	      retval = false;
	
	      break;
	    }
	}
    }
  return retval;
}

static inline bool
any_ones (const Array<int>& arr)
{
  bool retval = false;

  for (int i = 0; i < arr.length (); i++)
    {
      if (arr (i) == 1)
	{
	  retval = true;
	
	  break;
	}
    }
  return retval;
}

static inline int
compute_index (const Array<int>& ra_idx, const dim_vector& dims)
{
  int retval = -1;

  int n = dims.length ();

  if (n > 0 && n == ra_idx.length ())
    {
      retval = ra_idx(--n);

      while (--n >= 0)
	{
	  retval *= dims(n);
	
	  retval += ra_idx(n);
	}
    }
  else
    (*current_liboctave_error_handler)
      ("ArrayN<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

static inline Array<int>
conv_to_int_array (const Array<idx_vector>& a)
{
  Array<int> retval (a.length ());

  for (int i = 0; i < a.length (); i++)
    retval (i) = a(i).elem (0);

  return retval;
}

static inline Array<idx_vector>
conv_to_array (const idx_vector *tmp, const int len)
{
  Array<idx_vector> retval (len);

  for (int i = 0; i < len; i++)
      retval (i) = tmp[i];

  return retval;
}

static inline dim_vector
freeze (Array<idx_vector>& ra_idx, const dim_vector& dimensions, int resize_ok)
{
  dim_vector retval;

  int n = ra_idx.length ();

  assert (n == dimensions.length ());

  retval.resize (n);

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(i).freeze (dimensions(i), "XXX FIXME XXX", resize_ok);

  return retval;
}

static inline bool
vector_equivalent (const Array<int>& ra_idx)
{
  int n = ra_idx.length ();

  bool found_first = false;

  for (int i = 0; i < n; i++)
    {
      if (ra_idx(i) != 1)
        {
	  if (! found_first)
	    found_first = true;
	  else
	    return false;
	}
    }

  return true;
}

static inline bool
equal_arrays (const dim_vector& a, const dim_vector& b)
{
  bool retval = true;

  if (a.length () != b.length ())
    retval = false;
  else
    {
      for (int i = 0; i < a.length (); i++)
	{
	  if (a(i) != b(i))
	    retval = false;
	}
    }

  return retval;
}

static inline bool
all_ok (const Array<idx_vector>& ra_idx)
{
  bool retval = true;

  int n = ra_idx.length ();

  for (int i = 0; i < n; i++)
    {
      if (! ra_idx(i))
	{
	  retval = false;
	  break;
	}
    }

  return retval;
}

static inline bool
any_orig_empty (const Array<idx_vector>& ra_idx)
{
  bool retval = false;

  int n = ra_idx.length ();

  for (int i = 0; i < n; i++)
    {
      if (ra_idx(i).orig_empty ())
	{
	  retval = true;
	  break;
	}
    }

  return retval;
}

static inline bool
any_zero_len (const dim_vector& frozen_lengths)
{
  bool retval = false;

  int n = frozen_lengths.length ();

  for (int i = 0; i < n; i++)
    {
      if (frozen_lengths(i) == 0)
	{
	  retval = true;
	  break;
	}
    }

  return retval;
}

static inline dim_vector
get_zero_len_size (const dim_vector& frozen_lengths,
		   const dim_vector& dimensions)
{
  dim_vector retval;
  assert (0);
  return retval;
}

static inline bool
all_colon_equiv (const Array<idx_vector>& ra_idx,
		 const dim_vector& frozen_lengths)
{
  bool retval = true;

  int idx_n = ra_idx.length ();

  int n = frozen_lengths.length ();

  assert (idx_n == n);

  for (int i = 0; i < n; i++)
    {
      if (! ra_idx(i).is_colon_equiv (frozen_lengths(i)))
	{
	  retval = false;
	  break;
	}
    }

  return retval;
}

static inline bool
is_in (int num, const idx_vector& idx)
{
  int n = idx.capacity ();

  for (int i = 0; i < n; i++)
    if (idx.elem (i) == num)
      return true;

  return false;
}

static inline int
how_many_lgt (const int num, idx_vector& idxv)
{
  int retval = 0;

  int n = idxv.capacity ();

  for (int i = 0; i < n; i++)
    if (num > idxv.elem (i))
      retval++;

  return retval;
}

static inline bool
all_ones (const Array<int>& arr)
{
  bool retval = true;

  for (int i = 0; i < arr.length (); i++)
    {
      if (arr(i) != 1)
	{
	  retval = false;
	  break;
	}
    }

  return retval;
}

static Array<int>
get_elt_idx (const Array<idx_vector>& ra_idx, const Array<int>& result_idx)
{
  int n = ra_idx.length ();

  Array<int> retval (n);

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(i).elem (result_idx(i));

  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
