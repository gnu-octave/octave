// Template array classes
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

#include "Array-flags.h"
#include "idx-vector.h"
#include "lo-error.h"

template <class T>
void
ArrayN<T>::maybe_delete_elements (Array<idx_vector>&)
{
  assert (0);
}

template <class T>
ArrayN<T>
ArrayN<T>::value (void)
{
  ArrayN<T> retval;

  int n_idx = index_count ();

  if (n_idx > 1)
    {
      Array<idx_vector> idx (n_idx);

      idx_vector *tmp = get_idx ();

      for (int i = 0; i < n_idx; i++)
	idx(i) = tmp[i];

      return index (idx);
    }
  else if (n_idx == 1)
    {
      idx_vector *tmp = get_idx ();

      idx_vector idx = tmp[0];

      return index (idx);
    }
  else
    (*current_liboctave_error_handler)
      ("invalid number of indices for array expression");

  clear_index ();

  return retval;
}

template <class T>
ArrayN<T>
ArrayN<T>::index (idx_vector& idx, int resize_ok,
		  const T& resize_fill_value) const
{
  ArrayN<T> retval;
  assert (0);
  return retval;
}

static inline Array<int>
freeze (Array<idx_vector>& idx, const Array<int>& dimensions, int resize_ok)
{
  Array<int> retval;

  int n = idx.length ();

  assert (n == dimensions.length ());

  retval.resize (n);

  for (int i = 0; i < n; i++)
    retval(i) = idx(i).freeze (dimensions(i), "XXX FIXME XXX", resize_ok);

  return retval;
}

static inline bool
all_ok (const Array<idx_vector>& idx)
{
  bool retval = true;

  int n = idx.length ();

  for (int i = 0; i < n; i++)
    {
      if (! idx(i))
	{
	  retval = false;
	  break;
	}
    }

  return retval;
}

static inline bool
any_orig_empty (const Array<idx_vector>& idx)
{
  bool retval = false;

  int n = idx.length ();

  for (int i = 0; i < n; i++)
    {
      if (idx(i).orig_empty ())
	{
	  retval = true;
	  break;
	}
    }

  return retval;
}

static inline bool
any_zero_len (const Array<int>& frozen_lengths)
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

static inline Array<int>
get_zero_len_size (const Array<int>& frozen_lengths,
		   const Array<int>& dimensions)
{
  Array<int> retval;
  assert (0);
  return retval;
}

static inline bool
all_colon_equiv (const Array<idx_vector>& idx,
		 const Array<int>& frozen_lengths)
{
  bool retval = true;

  int idx_n = idx.length ();

  int n = frozen_lengths.length ();

  assert (idx_n == n);

  for (int i = 0; i < n; i++)
    {
      if (! idx(i).is_colon_equiv (frozen_lengths(i)))
	{
	  retval = false;
	  break;
	}
    }

  return retval;
}

static Array<int>
get_elt_idx (const Array<idx_vector>& idx, const Array<int>& result_idx)
{
  int n = idx.length ();

  Array<int> retval (n);

  for (int i = 0; i < n; i++)
    retval(i) = idx(result_idx(i));

  return retval;
}

template <class T>
ArrayN<T>
ArrayN<T>::index (Array<idx_vector>& arr_idx, int resize_ok,
		  const T& resize_fill_value) const
{
  ArrayN<T> retval;

  int n_dims = dimensions.length ();

  Array<int> frozen_lengths = freeze (arr_idx, dimensions, resize_ok);

  if (frozen_lengths.length () == n_dims)
    {
      if (all_ok (arr_idx))
	{
	  if (any_orig_empty (arr_idx))
	    {
	      retval.resize (frozen_lengths);
	    }
	  else if (any_zero_len (frozen_lengths))
	    {
	      Array<int> new_size = get_zero_len_size (frozen_lengths,
						       dimensions);

	      retval.resize (new_size);
	    }
	  else if (all_colon_equiv (arr_idx, frozen_lengths))
	    {
	      retval = *this;
	    }
	  else
	    {
#if 0
	      retval.resize (frozen_lengths);

	      int n = Array<T>::get_size (frozen_lengths);

	      Array<int> result_idx (n_dims, 0);

	      for (int i = 0; i < n; i++)
		{
		  Array<int> elt_idx = get_elt_idx (result_idx);

		  if (elt_idx > orig_len)
		    retval.elem (result_idx) = resize_fill_value;
		  else
		    retval.elem (result_idx) = elem (elt_idx);

		  increment_index (result_idx, frozen_lengths);
		}
#endif
	    }
	}
      // idx_vector::freeze() printed an error message for us.
    }
  else
    (*current_liboctave_error_handler)
      ("invalid number of dimensions for N-dimensional array index");

  return retval;
}

template <class LT, class RT>
int
assign (ArrayN<LT>&, const ArrayN<RT>&, const LT&)
{
  assert (0);
  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
