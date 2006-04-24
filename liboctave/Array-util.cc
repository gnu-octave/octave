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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array-util.h"
#include "dim-vector.h"
#include "lo-error.h"

bool
index_in_bounds (const Array<octave_idx_type>& ra_idx, const dim_vector& dimensions)
{
  bool retval = true;

  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      for (int i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || ra_idx(i) >= dimensions(i))
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
increment_index (Array<octave_idx_type>& ra_idx, const dim_vector& dimensions,
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

octave_idx_type
get_scalar_idx (Array<octave_idx_type>& idx, dim_vector& dims)
{
  octave_idx_type retval (-1);

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

octave_idx_type
num_ones (const Array<octave_idx_type>& ra_idx)
{
  octave_idx_type retval = 0;

  for (octave_idx_type i = 0; i < ra_idx.length (); i++)
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
any_ones (const Array<octave_idx_type>& arr)
{
  bool retval = false;

  for (octave_idx_type i = 0; i < arr.length (); i++)
    {
      if (arr (i) == 1)
	{
	  retval = true;
	
	  break;
	}
    }
  return retval;
}

octave_idx_type
compute_index (const Array<octave_idx_type>& ra_idx, const dim_vector& dims)
{
  octave_idx_type retval = -1;

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

Array<octave_idx_type>
conv_to_int_array (const Array<idx_vector>& a)
{
  Array<octave_idx_type> retval (a.length ());

  for (octave_idx_type i = 0; i < a.length (); i++)
    retval (i) = a(i).elem (0);

  return retval;
}

Array<idx_vector>
conv_to_array (const idx_vector *tmp, const octave_idx_type len)
{
  Array<idx_vector> retval (len);

  for (octave_idx_type i = 0; i < len; i++)
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

  static const char *tag[3] = { "row", "column", 0 };

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(i).freeze (dimensions(i), tag[i < 2 ? i : 3],
				  resize_ok);

  return retval;
}

bool
vector_equivalent (const dim_vector& dv)
{
  int n = dv.length ();

  bool found_first = false;

  for (int i = 0; i < n; i++)
    {
      if (dv(i) != 1)
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
all_ok (const Array<idx_vector>& ra_idx)
{
  bool retval = true;

  octave_idx_type n = ra_idx.length ();

  for (octave_idx_type i = 0; i < n; i++)
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

  octave_idx_type n = ra_idx.length ();

  for (octave_idx_type i = 0; i < n; i++)
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
all_colon_equiv (const Array<idx_vector>& ra_idx,
		 const dim_vector& frozen_lengths)
{
  bool retval = true;

  octave_idx_type idx_n = ra_idx.length ();

  int n = frozen_lengths.length ();

  assert (idx_n == n);

  for (octave_idx_type i = 0; i < n; i++)
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
is_in (octave_idx_type num, const idx_vector& idx)
{
  octave_idx_type n = idx.capacity ();

  for (octave_idx_type i = 0; i < n; i++)
    if (idx.elem (i) == num)
      return true;

  return false;
}

octave_idx_type
how_many_lgt (const octave_idx_type num, idx_vector& idxv)
{
  octave_idx_type retval = 0;

  octave_idx_type n = idxv.capacity ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (num > idxv.elem (i))
	retval++;
    }

  return retval;
}

bool
all_ones (const Array<octave_idx_type>& arr)
{
  bool retval = true;

  for (octave_idx_type i = 0; i < arr.length (); i++)
    {
      if (arr(i) != 1)
	{
	  retval = false;
	  break;
	}
    }

  return retval;
}

Array<octave_idx_type>
get_elt_idx (const Array<idx_vector>& ra_idx, const Array<octave_idx_type>& result_idx)
{
  octave_idx_type n = ra_idx.length ();

  Array<octave_idx_type> retval (n);

  for (octave_idx_type i = 0; i < n; i++)
    retval(i) = ra_idx(i).elem (result_idx(i));

  return retval;
}

Array<octave_idx_type>
get_ra_idx (octave_idx_type idx, const dim_vector& dims)
{
  Array<octave_idx_type> retval;

  int n_dims = dims.length ();

  retval.resize (n_dims);

  for (int i = 0; i < n_dims; i++)
    retval(i) = 0;

  assert (idx > 0 || idx < dims.numel ());

  for (octave_idx_type i = 0; i < idx; i++)
    increment_index (retval, dims);

  // FIXME -- the solution using increment_index is not
  // efficient.

#if 0
  octave_idx_type var = 1;
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
	  octave_idx_type last_ra_idx = ra_idx(n-1)(0);
	  for (octave_idx_type i = 1; i < ra_idx (n - 1).capacity (); i++)
	    last_ra_idx = (last_ra_idx > ra_idx(n-1)(i) ? last_ra_idx : 
			   ra_idx(n-1)(i));

	  if (last_ra_idx < size_left)
            {
              retval(n-1) = ra_idx(n-1).freeze (size_left,
						"dimension", resize_ok);
            }
          else
            {
	      // Make it larger than it should be to get an error
	      // later.

	      retval.resize (n_dims+1);

	      (*current_liboctave_error_handler)
		("index exceeds N-d array dimensions");
	    }
	}
    }

  return retval;
}

void
gripe_nonconformant (const char *op, int op1_len, int op2_len)
{
  (*current_liboctave_error_handler)
    ("%s: nonconformant arguments (op1 len: %d, op2 len: %d)",
     op, op1_len, op2_len);
}

void
gripe_nonconformant (const char *op, int op1_nr, int op1_nc,
		     int op2_nr, int op2_nc)
{
  (*current_liboctave_error_handler)
    ("%s: nonconformant arguments (op1 is %dx%d, op2 is %dx%d)",
     op, op1_nr, op1_nc, op2_nr, op2_nc);
}

void
gripe_nonconformant (const char *op, dim_vector& op1_dims,
		     dim_vector& op2_dims)
{
  std::string op1_dims_str = op1_dims.str ();
  std::string op2_dims_str = op2_dims.str ();

  (*current_liboctave_error_handler)
    ("%s: nonconformant arguments (op1 is %s, op2 is %s)",
     op, op1_dims_str.c_str (), op2_dims_str.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
