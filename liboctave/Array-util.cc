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

bool
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

void
increment_index (Array<int>& ra_idx, const dim_vector& dimensions,
		 int start_dimension)
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

int
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

int
num_ones (const Array<int>& ra_idx)
{
  int retval = 0;

  for (int i = 0; i < ra_idx.length (); i++)
    {
      if (ra_idx (i) == 1)
	retval++;
    }

  return retval;
}

bool
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

bool
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

int
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

Array<int>
conv_to_int_array (const Array<idx_vector>& a)
{
  Array<int> retval (a.length ());

  for (int i = 0; i < a.length (); i++)
    retval (i) = a(i).elem (0);

  return retval;
}

Array<idx_vector>
conv_to_array (const idx_vector *tmp, const int len)
{
  Array<idx_vector> retval (len);

  for (int i = 0; i < len; i++)
      retval (i) = tmp[i];

  return retval;
}

dim_vector
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

bool
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

bool
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

bool
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

bool
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

bool
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

dim_vector
get_zero_len_size (const dim_vector& /* frozen_lengths */,
		   const dim_vector& /* dimensions */)
{
  dim_vector retval;
  assert (0);
  return retval;
}

bool
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

bool
is_in (int num, const idx_vector& idx)
{
  int n = idx.capacity ();

  for (int i = 0; i < n; i++)
    if (idx.elem (i) == num)
      return true;

  return false;
}

int
how_many_lgt (const int num, idx_vector& idxv)
{
  int retval = 0;

  int n = idxv.capacity ();

  for (int i = 0; i < n; i++)
    {
      if (num > idxv.elem (i))
	retval++;
    }

  return retval;
}

bool
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

Array<int>
get_elt_idx (const Array<idx_vector>& ra_idx, const Array<int>& result_idx)
{
  int n = ra_idx.length ();

  Array<int> retval (n);

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(i).elem (result_idx(i));

  return retval;
}

int
number_of_elements (const dim_vector ra_idx)
{
  int retval = 1;

  int n = ra_idx.length ();

  if (n == 0)
    retval = 0;

  for (int i = 0; i < n; i++)
    retval *= ra_idx(i);

  return retval;
}

Array<int>
get_ra_idx (int idx, const dim_vector& dims)
{
  Array<int> retval;

  int n_dims = dims.length ();

  retval.resize (n_dims);

  for (int i = 0; i < n_dims; i++)
    retval(i) = 0;

  assert (idx > 0 || idx < number_of_elements (dims));

  for (int i = 0; i < idx; i++)
    increment_index (retval, dims);

  // XXX FIXME XXX -- the solution using increment_index is not
  // efficient.

#if 0
  int var = 1;
  for (int i = 0; i < n_dims; i++)
    {
      std::cout << "idx: " << idx << ", var: " << var << ", dims(" << i << "): " << dims(i) <<"\n";
      retval(i) = ((int)floor(((idx) / (double)var))) % dims(i);
      idx -= var * retval(i);
      var = dims(i);
    }
#endif

  return retval;
}

dim_vector
short_freeze (Array<idx_vector>& ra_idx, const dim_vector& dimensions,
	      int resize_ok)
{
  dim_vector retval;

  int n = ra_idx.length ();

  int n_dims = dimensions.length ();

  if (n == n_dims)
    {
      retval = freeze (ra_idx, dimensions, resize_ok);
    }
  else if (n < n_dims)
    {
      retval.resize (n);
      
      for (int i = 0; i < n - 1; i++)
        retval(i) = ra_idx(i).freeze (dimensions(i), "dimension", resize_ok);

      int size_left = 1;

      for (int i = n - 1; i < n_dims; i++)
        size_left *= dimensions(i); 
 
      if (ra_idx(n-1).is_colon())
        {
	  retval(n-1) = size_left;
	}
      else
	{
	  int last_ra_idx = ra_idx(n-1)(0);

	  if (last_ra_idx < dimensions(n - 1))
            {
              retval(n - 1) = ra_idx(n - 1).freeze (dimensions(n-1),
						    "dimension", resize_ok);
            }
          else
            {
              if (size_left <= last_ra_idx)
                {
         	  // Make it larger than it should be to get an error
         	  // later.

                  retval.resize(n_dims + 1);

                  (*current_liboctave_error_handler)
                    ("index exceeds N-d array dimensions");
                }
              else
                {
                  retval(n-1) = 1;
                }
	    }
	}
    }

  return retval;
}

Array<int>
calc_permutated_idx (const Array<int>& old_idx, 
		     const Array<int>& perm_vec, bool inv)
{
  int n_el = old_idx.length ();

  Array<int> retval (n_el);

  for (int i = 0; i < n_el; i++)
    {
      if (inv)
	retval(perm_vec(i)-1) = old_idx(i);
      else
	retval(i) = old_idx(perm_vec(i)-1);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
