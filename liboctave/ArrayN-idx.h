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
ArrayN<T>::maybe_delete_elements (Array<idx_vector>& idx, const T& rfv)
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

#define MAYBE_RESIZE_ND_DIMS \
  do \
    { \
      if (n_idx >= lhs_dims.length () && ! rhs_is_empty) \
	{ \
	  Array<int> max_idx (n_idx); \
	  Array<int> new_idx (n_idx); \
 \
	  for (int i = 0; i < n_idx; i++) \
	    { \
	      if (lhs_dims.length () == 0 || i >= lhs_dims.length ()) \
		new_idx(i) = idx(i).max () + 1; \
	      else \
		{ \
		  if (i < rhs_dims.length ()) \
		    max_idx(i) = idx(i).is_colon () ? rhs_dims(i) : idx(i).max () + 1; \
		  else \
		    max_idx(i) = idx(i).max () + 1; \
 \
		  new_idx(i) = max_idx(i) > lhs_dims(i) ? max_idx(i) : lhs_dims(i); \
		} \
            } \
 \
	  lhs.resize (new_idx, rfv); \
	  lhs_dims = lhs.dims ();  \
        } \
    } \
  while (0)

template <class LT, class RT>
int
assign (ArrayN<LT>& lhs, const ArrayN<RT>& rhs, const LT& rfv)
{
  int retval = 1;

  int n_idx = lhs.index_count ();

  Array<int> lhs_dims = lhs.dims ();
  Array<int> rhs_dims = rhs.dims ();

  idx_vector *tmp = lhs.get_idx ();

  Array<idx_vector> idx = conv_to_array (tmp, n_idx);

  // This needs to be defined before MAYBE_RESIZE_ND_DIMS.

  bool rhs_is_empty = rhs_dims.length () == 0 ? true : any_zero_len (rhs_dims);

  // Maybe expand to more dimensions.

  MAYBE_RESIZE_ND_DIMS;

  Array<int> idx_is_colon (n_idx, 0);
  Array<int> idx_is_colon_equiv (n_idx, 0);

  for (int i = 0; i < n_idx; i++)
    {
      idx_is_colon_equiv(i) = idx(i).is_colon_equiv (lhs_dims(i), 1);

      idx_is_colon(i) = idx(i).is_colon ();
    }

  int resize_ok = 1;

  Array<int> frozen_len;

  if (n_idx == lhs_dims.length ())
    frozen_len = freeze (idx, lhs_dims, resize_ok);

  bool rhs_is_scalar = is_scalar (rhs_dims);

  bool idx_is_empty = any_zero_len (frozen_len);

  if (rhs_is_empty)
    {
      lhs.maybe_delete_elements (idx, rfv);
    }
  else if (rhs_is_scalar)
    {
      if (n_idx == 0)
	(*current_liboctave_error_handler)
	  ("number of indices is zero.");
		
      else if (n_idx < lhs_dims.length ())
	{
	  // Number of indices is less than dimensions.

	  if (any_ones (idx_is_colon)|| any_ones (idx_is_colon_equiv))
	    {
	      (*current_liboctave_error_handler)
		("number of indices is less than number of dimensions, one or more indices are colons.");
	    }
	  else
	    {
	      // Fewer indices than dimensions, no colons.

	      bool resize = false;

	      // Subtract one since the last idx do not tell us
	      // anything about dimensionality.

	      for (int i = 0; i < idx.length () - 1; i++)
		{
		  // Subtract one since idx counts from 0 while dims
		  // count from 1.

		  if (idx(i).elem (0) + 1 > lhs_dims(i))
		    resize = true;
		}

	      if (resize)
		{
		  Array<int> new_dims (lhs_dims.length ());

		  for (int i = 0; i < lhs_dims.length (); i++)
		    {
		      if (i < idx.length () - 1
			  && idx(i).elem (0) + 1 > lhs_dims(i))
			new_dims(i) = idx(i).elem (0)+1;
		      else
			new_dims(i) = lhs_dims(i);
		    }

		  lhs.resize (new_dims, rfv);

		  lhs_dims = lhs.dims ();
		}

	      Array<int> one_arg_temp (1, 0);
		
	      RT scalar = rhs.elem (one_arg_temp);

	      Array<int> int_arr = conv_to_int_array (idx);

	      int numelem = get_scalar_idx (int_arr, lhs_dims);
	
	      if (numelem > lhs.length () || numelem < 0)
		(*current_liboctave_error_handler)
		  ("attempt to grow array along ambiguous dimension.");
	      else
		lhs.Array<LT>::checkelem (numelem) = scalar;
	    }
	}
      else
	{
	  // Scalar to matrix assignment with as many indices as lhs
	  // dimensions.

	  int n = ArrayN<LT>::get_size (frozen_len);
	
	  Array<int> result_idx (lhs_dims.length (), 0);

	  Array<int> elt_idx;
	
	  Array<int> one_arg_temp(1,0);		
	  RT scalar = rhs.elem (one_arg_temp);
	
	  for (int i = 0; i < n; i++)
	    {
	      elt_idx = get_elt_idx (idx, result_idx);
	
	      Array<int> lhs_inc(lhs_dims.length());
	
	      for (int i = 0; i < lhs_dims.length (); i++)
		lhs_inc(i) = lhs_dims(i) + 1;
	
	      if (index_in_bounds(elt_idx, lhs_inc))
		lhs.checkelem (elt_idx) = scalar;
	      else
		lhs.checkelem (elt_idx) = rfv;

	      increment_index (result_idx, frozen_len);
	    }
	}
    }
  else if (rhs_dims.length () >= 2)
    {
      // RHS is matrix or higher dimension.

      // Subtracting number of dimensions of length 1 will catch
      // cases where: A(2,1,2)=3  A(:,1,:)=[2,3;4,5]

      if (rhs_dims.length () != num_ones(idx_is_colon_equiv) - num_ones(lhs_dims))
	{
	  (*current_liboctave_error_handler)
	    ("dimensions do not match in matrix assignment.");
	}
      else
	{
	  bool dim_ok(true);

	  int jj = 0;

	  // Check that RHS dimensions are the same length as the
	  // corresponding LHS dimensions.

	  for (int j = 0; j < idx_is_colon.length (); j++)
	    {
	      if (idx_is_colon(j) || idx_is_colon_equiv(j))
		{
		  if (rhs_dims(jj) < lhs_dims(j))
		    {
		      dim_ok = false;

		      break;
		    }

		  jj++;
		}
	    }

	  if (! dim_ok)
	    (*current_liboctave_error_handler)
	      ("subscripted assignment dimension mismatch.");
	  else
	    {
	      Array<int> new_dims (n_idx);
	
	      bool resize = false;
	
	      int ii = 0;

	      // Update idx vectors.

	      for (int i = 0; i < n_idx; i++)
		{
		  if (idx(i).is_colon ())
		    {
		      // Add appropriate idx_vector to idx(i) since
		      // index with : contains no indexes.

		      frozen_len(i) = lhs_dims(i) > rhs_dims(ii) ? lhs_dims(i) : rhs_dims(ii);
		
		      new_dims(i) = lhs_dims(i) > rhs_dims(ii) ? lhs_dims(i) : rhs_dims(ii);
		
		      ii++;
		
		      Range idxrange (1, frozen_len(i), 1);
		
		      idx_vector idxv (idxrange);
		
		      idx(i) = idxv;
		    }
		  else
		    {
		      new_dims(i) = lhs_dims(i) > idx(i).max () + 1 ? lhs_dims(i) : idx(i).max () + 1;
		
		      if (frozen_len(i) > 1)
			ii++;
		    }
		  if (new_dims(i) != lhs_dims(i))
		    resize = true;
		}
	
	      // Resize LHS if dimensions have changed.

	      if (resize)
		{
		  lhs.resize (new_dims, rfv);
		
		  lhs_dims = lhs.dims ();
		}
	
	      // Number of elements which need to be set.

	      int n = ArrayN<LT>::get_size (frozen_len);
	
	      Array<int> result_idx (lhs_dims.length (), 0);
	      Array<int> elt_idx;
	
	      Array<int> result_rhs_idx (rhs_dims.length (), 0);
	      Array<int> frozen_rhs (rhs_dims.length(), 0);
	
	      for (int i = 0; i < rhs_dims.length (); i++)
		frozen_rhs(i) = rhs_dims(i);
	
	      Array<int> lhs_inc (lhs_dims.length ());
	
	      for (int i = 0; i < lhs_dims.length (); i++)
		lhs_inc(i) = lhs_dims(i) + 1;
	
	      for (int i = 0; i < n; i++)
		{
		  elt_idx = get_elt_idx (idx, result_idx);
		
		  if (index_in_bounds (elt_idx, lhs_inc))
		    {
		      int s = compute_index (result_rhs_idx,rhs_dims);
		
		      lhs.checkelem (elt_idx) = rhs.Array<RT>::elem (s);
		
		      increment_index (result_rhs_idx, frozen_rhs);
		    }
		  else
		    lhs.checkelem (elt_idx) = rfv;
		
		  increment_index (result_idx, frozen_len);
		}
	    }
	}
    }
  else if (idx_is_empty)
    {
      // Assignment to matrix with at least one empty index.

      if (! rhs_is_empty || ! rhs_is_scalar)
	{
	  (*current_liboctave_error_handler)
	    ("A([], []) = X: X must be an empty matrix or a scalar");
	
	  retval = 0;
	}
    }
  else if (lhs_dims.length () != rhs_dims.length ())
    {
      (*current_liboctave_error_handler)
	("A(I) = X: X must be a scalar or a matrix with the same size as I");
      retval = 0;
    }

  return retval;
}

static inline int
get_scalar_idx (Array<int>& idx, Array<int>& dims)
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
num_ones (const Array<int> ra_idx)
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
is_scalar (const Array<int>& dim)
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
any_ones (const Array<int> arr)
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
compute_index (const Array<int>& ra_idx, const Array<int>& dims)
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
/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
