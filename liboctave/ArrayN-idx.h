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
      Array<idx_vector> ra_idx (n_idx);

      idx_vector *tmp = get_idx ();

      for (int i = 0; i < n_idx; i++)
	ra_idx(i) = tmp[i];

      return index (ra_idx);
    }
  else if (n_idx == 1)
    {
      idx_vector *tmp = get_idx ();

      idx_vector ra_idx = tmp[0];

      return index (ra_idx);
    }
  else
    (*current_liboctave_error_handler)
      ("invalid number of indices for array expression");

  clear_index ();

  return retval;
}

template <class T>
ArrayN<T>
ArrayN<T>::index (idx_vector& ra_idx, int resize_ok, const T& rfv) const
{
  ArrayN<T> retval;
  assert (0);
  return retval;
}

static inline Array<int>
freeze (Array<idx_vector>& ra_idx, const Array<int>& dimensions, int resize_ok)
{
  Array<int> retval;

  int n = ra_idx.length ();

  assert (n == dimensions.length ());

  retval.resize (n);

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(i).freeze (dimensions(i), "XXX FIXME XXX", resize_ok);

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
all_colon_equiv (const Array<idx_vector>& ra_idx,
		 const Array<int>& frozen_lengths)
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

static Array<int>
get_elt_idx (const Array<idx_vector>& ra_idx, const Array<int>& result_idx)
{
  int n = ra_idx.length ();

  Array<int> retval (n);

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(result_idx(i));

  return retval;
}

template <class T>
ArrayN<T>
ArrayN<T>::index (Array<idx_vector>& ra_idx, int resize_ok, const T& rfv) const
{
  ArrayN<T> retval;

  int n_dims = dimensions.length ();

  Array<int> frozen_lengths = freeze (ra_idx, dimensions, resize_ok);

  if (frozen_lengths.length () == n_dims)
    {
      if (all_ok (ra_idx))
	{
	  if (any_orig_empty (ra_idx))
	    {
	      retval.resize (frozen_lengths);
	    }
	  else if (any_zero_len (frozen_lengths))
	    {
	      Array<int> new_size = get_zero_len_size (frozen_lengths,
						       dimensions);

	      retval.resize (new_size);
	    }
	  else if (all_colon_equiv (ra_idx, frozen_lengths))
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
		    retval.elem (result_idx) = rfv;
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
