// Template array classes
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

#include <cassert>
#include <climits>

#include <iostream>

#include "Array.h"
#include "Array-flags.h"
#include "Array-util.h"
#include "Range.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "lo-sstream.h"

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
Array<T>::Array (const Array<T>& a, const dim_vector& dv)
  : rep (a.rep), dimensions (dv), idx (0), idx_count (0)
{
  rep->count++;

  if (a.numel () < dv.numel ())
    (*current_liboctave_error_handler)
      ("Array::Array (const Array&, const dim_vector&): dimension mismatch");
}

template <class T>
Array<T>::~Array (void)
{
  if (--rep->count <= 0)
    delete rep;

  delete [] idx;
}

template <class T>
Array<T>
Array<T>::squeeze (void) const
{
  Array<T> retval = *this;

  if (ndims () > 2)
    {
      bool dims_changed = false;

      dim_vector new_dimensions = dimensions;

      int k = 0;

      for (int i = 0; i < ndims (); i++)
	{
	  if (dimensions(i) == 1)
	    dims_changed = true;
	  else
	    new_dimensions(k++) = dimensions(i);
	}

      if (dims_changed)
	{
	  switch (k)
	    {
	    case 0:
	      new_dimensions = dim_vector (1, 1);
	      break;

	    case 1:
	      {
		int tmp = new_dimensions(0);

		new_dimensions.resize (2);

		new_dimensions(0) = tmp;
		new_dimensions(1) = 1;
	      }
	      break;

	    default:
	      new_dimensions.resize (k);
	      break;
	    }
	}

      // XXX FIXME XXX -- it would be better if we did not have to do
      // this, so we could share the data while still having different
      // dimension vectors.

      retval.make_unique ();

      retval.dimensions = new_dimensions;
    }

  return retval;
}

// A guess (should be quite conservative).
#define MALLOC_OVERHEAD 1024

template <class T>
int
Array<T>::get_size (int r, int c)
{
  // XXX KLUGE XXX

  // If an allocation of an array with r * c elements of type T
  // would cause an overflow in the allocator when computing the
  // size of the allocation, then return a value which, although
  // not equivalent to the actual request, should be too large for
  // most current hardware, but not so large to cause the
  // allocator to barf on computing retval * sizeof (T).

  static int nl;
  static double dl
    = frexp (static_cast<double>
	     (INT_MAX - MALLOC_OVERHEAD) / sizeof (T), &nl);

  // This value should be an integer.  If we return this value and
  // things work the way we expect, we should be paying a visit to
  // new_handler in no time flat.
  static int max_items = static_cast<int> (ldexp (dl, nl));

  int nr, nc;
  double dr = frexp (static_cast<double> (r), &nr);
  double dc = frexp (static_cast<double> (c), &nc);

  int nt = nr + nc;
  double dt = dr * dc;

  if (dt < 0.5)
    {
      nt--;
      dt *= 2;
    }

  return (nt < nl || (nt == nl && dt < dl)) ? r * c : max_items;
}

template <class T>
int
Array<T>::get_size (int r, int c, int p)
{
  // XXX KLUGE XXX

  // If an allocation of an array with r * c * p elements of type T
  // would cause an overflow in the allocator when computing the
  // size of the allocation, then return a value which, although
  // not equivalent to the actual request, should be too large for
  // most current hardware, but not so large to cause the
  // allocator to barf on computing retval * sizeof (T).

  static int nl;
  static double dl
    = frexp (static_cast<double>
	     (INT_MAX - MALLOC_OVERHEAD) / sizeof (T), &nl);

  // This value should be an integer.  If we return this value and
  // things work the way we expect, we should be paying a visit to
  // new_handler in no time flat.
  static int max_items = static_cast<int> (ldexp (dl, nl));

  int nr, nc, np;
  double dr = frexp (static_cast<double> (r), &nr);
  double dc = frexp (static_cast<double> (c), &nc);
  double dp = frexp (static_cast<double> (p), &np);

  int nt = nr + nc + np;
  double dt = dr * dc * dp;

  if (dt < 0.5)
    {
      nt--;
      dt *= 2;

      if (dt < 0.5)
	{
	  nt--;
	  dt *= 2;
	}
    }

  return (nt < nl || (nt == nl && dt < dl)) ? r * c * p : max_items;
}

template <class T>
int
Array<T>::get_size (const dim_vector& ra_idx)
{
  // XXX KLUGE XXX

  // If an allocation of an array with r * c elements of type T
  // would cause an overflow in the allocator when computing the
  // size of the allocation, then return a value which, although
  // not equivalent to the actual request, should be too large for
  // most current hardware, but not so large to cause the
  // allocator to barf on computing retval * sizeof (T).

  static int nl;
  static double dl
    = frexp (static_cast<double>
	     (INT_MAX - MALLOC_OVERHEAD) / sizeof (T), &nl);

  // This value should be an integer.  If we return this value and
  // things work the way we expect, we should be paying a visit to
  // new_handler in no time flat.

  static int max_items = static_cast<int> (ldexp (dl, nl));

  int retval = max_items;

  int n = ra_idx.length ();

  int nt = 0;
  double dt = 1;

  for (int i = 0; i < n; i++)
    {
      int nra_idx;
      double dra_idx = frexp (static_cast<double> (ra_idx(i)), &nra_idx);

      nt += nra_idx;
      dt *= dra_idx;

      if (dt < 0.5)
	{
	  nt--;
	  dt *= 2;
	}
    }

  if (nt < nl || (nt == nl && dt < dl))
    {
      retval = 1;

      for (int i = 0; i < n; i++)
	retval *= ra_idx(i);
    }

  return retval;
}

#undef MALLOC_OVERHEAD

template <class T>
int
Array<T>::compute_index (const Array<int>& ra_idx) const
{
  int retval = -1;

  int n = dimensions.length ();

  if (n > 0 && n == ra_idx.length ())
    {
      retval = ra_idx(--n);

      while (--n >= 0)
	{
	  retval *= dimensions(n);
	  retval += ra_idx(n);
	}
    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

template <class T>
T
Array<T>::range_error (const char *fcn, int n) const
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, int n)
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  static T foo;
  return foo;
}

template <class T>
T
Array<T>::range_error (const char *fcn, int i, int j) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, int i, int j)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  static T foo;
  return foo;
}

template <class T>
T
Array<T>::range_error (const char *fcn, int i, int j, int k) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d, %d): range error", fcn, i, j, k);
  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, int i, int j, int k)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d, %d): range error", fcn, i, j, k);
  static T foo;
  return foo;
}

template <class T>
T
Array<T>::range_error (const char *fcn, const Array<int>& ra_idx) const
{
  OSSTREAM buf;

  buf << fcn << " (";

  int n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (int i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";

  buf << OSSTREAM_ENDS;

  (*current_liboctave_error_handler) (OSSTREAM_C_STR (buf));

  OSSTREAM_FREEZE (buf);

  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, const Array<int>& ra_idx)
{
  OSSTREAM buf;

  buf << fcn << " (";

  int n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (int i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";

  buf << OSSTREAM_ENDS;

  (*current_liboctave_error_handler) (OSSTREAM_C_STR (buf));

  OSSTREAM_FREEZE (buf);

  static T foo;
  return foo;
}

template <class T>
Array<T>
Array<T>::reshape (const dim_vector& new_dims) const
{
  Array<T> retval;

  if (dimensions != new_dims)
    {
      if (dimensions.numel () == new_dims.numel ())
	retval = Array<T> (*this, new_dims);
      else
	(*current_liboctave_error_handler) ("reshape: size mismatch");
    }
  else
    retval = *this;

  return retval;
}

template <class T>
Array<T>
Array<T>::permute (const Array<int>& perm_vec, bool inv) const
{
  Array<T> retval;

  dim_vector dv = dims ();
  dim_vector dv_new;

  int nd = dv.length ();

  dv_new.resize (nd);

  // Need this array to check for identical elements in permutation array.
  Array<bool> checked (nd, false);

  // Find dimension vector of permuted array.
  for (int i = 0; i < nd; i++)
    {
      int perm_el = perm_vec.elem (i);

      if (perm_el > dv.length () || perm_el < 1)
	{
	  (*current_liboctave_error_handler)
	    ("permutation vector contains an invalid element");

	  return retval;
	}

      if (checked.elem(perm_el - 1))
	{
	  (*current_liboctave_error_handler)
	    ("PERM cannot contain identical elements");

	  return retval;
	}
      else
	checked.elem(perm_el - 1) = true;

      dv_new (i) = dv (perm_el - 1);
    }

  retval.resize (dv_new);

  // Index array to the original array.
  Array<int> old_idx (nd, 0);

  // Number of elements in Array (should be the same for
  // both the permuted array and original array).
  int n = retval.length ();

  // Permute array.
  for (int i = 0; i < n; i++)
    {
      // Get the idx of permuted array.
      Array<int> new_idx = calc_permutated_idx (old_idx, perm_vec, inv);

      retval.elem (new_idx) = elem (old_idx);

      increment_index (old_idx, dv);
    }

  return retval;
}

template <class T>
void
Array<T>::resize_no_fill (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (n == length ())
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new typename Array<T>::ArrayRep (n);

  dimensions = dim_vector (n);

  if (n > 0 && old_data && old_len > 0)
    {
      int min_len = old_len < n ? old_len : n;

      for (int i = 0; i < min_len; i++)
	xelem (i) = old_data[i];
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_no_fill (const dim_vector& dv)
{
  int n = dv.length ();

  for (int i = 0; i < n; i++)
    {
      if (dv(i) < 0)
	{
	  (*current_liboctave_error_handler)
	    ("can't resize to negative dimension");
	  return;
	}
    }

  bool same_size = true;

  if (dimensions.length () != n)
    {
      same_size = false;
    }
  else
    {
      for (int i = 0; i < n; i++)
	{
	  if (dv(i) != dimensions(i))
	    {
	      same_size = false;
	      break;
	    }
	}
    }

  if (same_size)
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int ts = get_size (dv);

  rep = new typename Array<T>::ArrayRep (ts);

  dim_vector dv_old = dimensions;
  int dv_old_orig_len = dv_old.length ();
  dimensions = dv;
  int ts_old = get_size (dv_old);

  if (ts > 0 && ts_old > 0 && dv_old_orig_len > 0)
    {
      Array<int> ra_idx (dimensions.length (), 0);

      if (n > dv_old_orig_len)
	{
	  dv_old.resize (n);

	  for (int i = dv_old_orig_len; i < n; i++)
	    dv_old.elem (i) = 1;
	}

      for (int i = 0; i < ts; i++)
	{
	  if (index_in_bounds (ra_idx, dv_old))
	    rep->elem (i) = old_data[get_scalar_idx (ra_idx, dv_old)];

	  increment_index (ra_idx, dimensions);
	}
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_no_fill (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  int n = ndims ();

  if (n == 0)
    dimensions = dim_vector (0, 0);

  assert (ndims () == 2);

  if (r == dim1 () && c == dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = Array<T>::rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  int ts = get_size (r, c);

  rep = new typename Array<T>::ArrayRep (ts);

  dimensions = dim_vector (r, c);

  if (ts > 0 && old_data && old_len > 0)
    {
      int min_r = old_d1 < r ? old_d1 : r;
      int min_c = old_d2 < c ? old_d2 : c;

      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  xelem (i, j) = old_data[old_d1*j+i];
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_no_fill (int r, int c, int p)
{
  if (r < 0 || c < 0 || p < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  int n = ndims ();

  if (n == 0)
    dimensions = dim_vector (0, 0, 0);

  assert (ndims () == 3);

  if (r == dim1 () && c == dim2 () && p == dim3 ())
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_d3 = dim3 ();
  int old_len = length ();

  int ts = get_size (get_size (r, c), p);

  rep = new typename Array<T>::ArrayRep (ts);

  dimensions = dim_vector (r, c, p);

  if (ts > 0 && old_data && old_len > 0)
    {
      int min_r = old_d1 < r ? old_d1 : r;
      int min_c = old_d2 < c ? old_d2 : c;
      int min_p = old_d3 < p ? old_d3 : p;

      for (int k = 0; k < min_p; k++)
	for (int j = 0; j < min_c; j++)
	  for (int i = 0; i < min_r; i++)
	    xelem (i, j, k) = old_data[old_d1*(old_d2*k+j)+i];
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_and_fill (int n, const T& val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (n == length ())
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new typename Array<T>::ArrayRep (n);

  dimensions = dim_vector (n);

  if (n > 0)
    {
      int min_len = old_len < n ? old_len : n;

      if (old_data && old_len > 0)
	{
	  for (int i = 0; i < min_len; i++)
	    xelem (i) = old_data[i];
	}

      for (int i = old_len; i < n; i++)
	xelem (i) = val;
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_and_fill (int r, int c, const T& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (ndims () == 0)
    dimensions = dim_vector (0, 0);

  assert (ndims () == 2);

  if (r == dim1 () && c == dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = Array<T>::rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  int ts = get_size (r, c);

  rep = new typename Array<T>::ArrayRep (ts);

  dimensions = dim_vector (r, c);

  if (ts > 0)
    {
      int min_r = old_d1 < r ? old_d1 : r;
      int min_c = old_d2 < c ? old_d2 : c;

      if (old_data && old_len > 0)
	{
	  for (int j = 0; j < min_c; j++)
	    for (int i = 0; i < min_r; i++)
	      xelem (i, j) = old_data[old_d1*j+i];
	}

      for (int j = 0; j < min_c; j++)
	for (int i = min_r; i < r; i++)
	  xelem (i, j) = val;

      for (int j = min_c; j < c; j++)
	for (int i = 0; i < r; i++)
	  xelem (i, j) = val;
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_and_fill (int r, int c, int p, const T& val)
{
  if (r < 0 || c < 0 || p < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (ndims () == 0)
    dimensions = dim_vector (0, 0, 0);

  assert (ndims () == 3);

  if (r == dim1 () && c == dim2 () && p == dim3 ())
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_d3 = dim3 ();

  int old_len = length ();

  int ts = get_size (get_size (r, c), p);

  rep = new typename Array<T>::ArrayRep (ts);

  dimensions = dim_vector (r, c, p);

  if (ts > 0)
    {
      int min_r = old_d1 < r ? old_d1 : r;
      int min_c = old_d2 < c ? old_d2 : c;
      int min_p = old_d3 < p ? old_d3 : p;

      if (old_data && old_len > 0)
	for (int k = 0; k < min_p; k++)
	  for (int j = 0; j < min_c; j++)
	    for (int i = 0; i < min_r; i++)
	      xelem (i, j, k) = old_data[old_d1*(old_d2*k+j)+i];

      // XXX FIXME XXX -- if the copy constructor is expensive, this
      // may win.  Otherwise, it may make more sense to just copy the
      // value everywhere when making the new ArrayRep.

      for (int k = 0; k < min_p; k++)
	for (int j = min_c; j < c; j++)
	  for (int i = 0; i < min_r; i++)
	    xelem (i, j, k) = val;

      for (int k = 0; k < min_p; k++)
	for (int j = 0; j < c; j++)
	  for (int i = min_r; i < r; i++)
	    xelem (i, j, k) = val;

      for (int k = min_p; k < p; k++)
	for (int j = 0; j < c; j++)
	  for (int i = 0; i < r; i++)
	    xelem (i, j, k) = val;
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_and_fill (const dim_vector& dv, const T& val)
{
  int n = dv.length ();

  for (int i = 0; i < n; i++)
    {
      if (dv(i) < 0)
	{
	  (*current_liboctave_error_handler)
	    ("can't resize to negative dimension");
	  return;
	}
    }

  bool same_size = true;

  if (dimensions.length () != n)
    {
      same_size = false;
    }
  else
    {
      for (int i = 0; i < n; i++)
	{
	  if (dv(i) != dimensions(i))
	    {
	      same_size = false;
	      break;
	    }
	}
    }

  if (same_size)
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int len = get_size (dv);

  rep = new typename Array<T>::ArrayRep (len);

  dim_vector dv_old = dimensions;
  int dv_old_orig_len = dv_old.length ();
  dimensions = dv;

  if (len > 0 && dv_old_orig_len > 0)
    {
      Array<int> ra_idx (dimensions.length (), 0);
      
      if (n > dv_old_orig_len)
	{
	  dv_old.resize (n);

	  for (int i = dv_old_orig_len; i < n; i++)
	    dv_old.elem (i) = 1;
	}

      for (int i = 0; i < len; i++)
	{
	  if (index_in_bounds (ra_idx, dv_old))
	    rep->elem (i) = old_data[get_scalar_idx (ra_idx, dv_old)];
	  else
	    rep->elem (i) = val;
	  
	  increment_index (ra_idx, dimensions);
	}
    }
  else
    for (int i = 0; i < len; i++)
      rep->elem (i) = val;

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
Array<T>&
Array<T>::insert (const Array<T>& a, int r, int c)
{
  if (ndims () == 2 && a.ndims () == 2)
    insert2 (a, r, c);
  else
    insertN (a, r, c);

  return *this;
}


template <class T>
Array<T>&
Array<T>::insert2 (const Array<T>& a, int r, int c)
{
  int a_rows = a.rows ();
  int a_cols = a.cols ();

  if (r < 0 || r + a_rows > rows () || c < 0 || c + a_cols > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int j = 0; j < a_cols; j++)
    for (int i = 0; i < a_rows; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

template <class T>
Array<T>&
Array<T>::insertN (const Array<T>& a, int r, int c)
{
  dim_vector dv = dims ();

  dim_vector a_dv = a.dims ();

  int n = a_dv.length ();

  if (n == dimensions.length ())
    {
      Array<int> a_ra_idx (a_dv.length (), 0);

      a_ra_idx.elem (0) = r;
      a_ra_idx.elem (1) = c;

      for (int i = 0; i < n; i++)
	{
	  if (a_ra_idx(i) < 0 || (a_ra_idx(i) + a_dv(i)) > dv(i))
	    {
	      (*current_liboctave_error_handler)
		("Array<T>::insert: range error for insert");
	      return *this;
	    }
	}

      int n_elt = a.numel ();
      
      const T *a_data = a.data ();   
   
      int iidx = 0;
	  
      int a_rows = a_dv(0);

      int this_rows = dv(0);
	  
      int numel_page = a_dv(0) * a_dv(1);	  

      int count_pages = 0;
	  
      for (int i = 0; i < n_elt; i++)
	{
	  if (i != 0 && i % a_rows == 0)
	    iidx += (this_rows - a_rows);	      
	  
	  if (i % numel_page == 0)
	    iidx = c * dv(0) + r + dv(0) * dv(1) * count_pages++;

	  elem (iidx++) = a_data[i];
	}
    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::insert: invalid indexing operation");

  return *this;
}

template <class T>
Array<T>&
Array<T>::insert (const Array<T>& a, const Array<int>& ra_idx)
{
  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      dim_vector dva = a.dims ();
      dim_vector dv = dims ();
      int len_a = dva.length ();

      for (int i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || (ra_idx(i) + 
				(i < len_a ? dva(i) : 1)) > dimensions(i))
	    {
	      (*current_liboctave_error_handler)
		("Array<T>::insert: range error for insert");
	      return *this;
	    }
	}

      if (dva.numel ())
        {
	  const T *a_data = a.data ();   
	  int numel_to_move = dva (0);
	  int skip = dv (0);
	  for (int i = 0; i < len_a - 1; i++)
	    if (ra_idx(i) == 0 && dva(i) == dv(i))
	      {
		numel_to_move *= dva(i+1);
		skip *= dv(i+1);
	      }
	    else
	      {
		skip -= dva(i);
		break;
	      }

	  int jidx = ra_idx (n - 1);
	  for (int i = n-2; i >= 0; i--)
	    {
	      jidx *= dv (i);
	      jidx += ra_idx (i);
	    }

	  int iidx = 0;
	  int moves = dva.numel () / numel_to_move;
	  for (int i = 0; i < moves; i++)
	    {
	      for (int j = 0; j < numel_to_move; j++)
		elem (jidx++) = a_data[iidx++];
	      jidx += skip;
	    }
	}
    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::insert: invalid indexing operation");

  return *this;
}

template <class T>
Array<T>
Array<T>::transpose (void) const
{
  assert (ndims () == 2);

  int nr = dim1 ();
  int nc = dim2 ();

  if (nr > 1 && nc > 1)
    {
      Array<T> result (dim_vector (nc, nr));

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.xelem (j, i) = xelem (i, j);

      return result;
    }
  else
    {
      // Fast transpose for vectors and empty matrices
      return Array<T> (*this, dim_vector (nc, nr));
    }
}

template <class T>
T *
Array<T>::fortran_vec (void)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new typename Array<T>::ArrayRep (*rep);
    }
  return rep->data;
}

template <class T>
void
Array<T>::maybe_delete_dims (void)
{
  int nd = dimensions.length ();

  dim_vector new_dims (1, 1);

  bool delete_dims = true;

  for (int i = nd - 1; i >= 0; i--)
    {
      if (delete_dims)
        {
          if (dimensions(i) != 1)
	    {
	      delete_dims = false;

	      new_dims = dim_vector (i + 1, dimensions(i));
	    }
        }
      else
	new_dims(i) = dimensions(i);
    }

  if (nd != new_dims.length ())
    dimensions = new_dims;
}

template <class T>
void
Array<T>::clear_index (void)
{
  delete [] idx;
  idx = 0;
  idx_count = 0;
}

template <class T>
void
Array<T>::set_index (const idx_vector& idx_arg)
{
  int nd = ndims ();

  if (! idx && nd > 0)
    idx = new idx_vector [nd];

  if (idx_count < nd)
    {
      idx[idx_count++] = idx_arg;
    }
  else
    {
      idx_vector *new_idx = new idx_vector [idx_count+1];

      for (int i = 0; i < idx_count; i++)
	new_idx[i] = idx[i];

      new_idx[idx_count++] = idx_arg;

      delete [] idx;

      idx = new_idx;
    }
}

template <class T>
void
Array<T>::maybe_delete_elements (idx_vector& idx_arg)
{
  switch (ndims ())
    {
    case 1:
      maybe_delete_elements_1 (idx_arg);
      break;

    case 2:
      maybe_delete_elements_2 (idx_arg);
      break;

    default:
      (*current_liboctave_error_handler)
	("Array<T>::maybe_delete_elements: invalid operation");
      break;
    }
}

template <class T>
void
Array<T>::maybe_delete_elements_1 (idx_vector& idx_arg)
{
  int len = length ();

  if (len == 0)
    return;

  if (idx_arg.is_colon_equiv (len, 1))
    resize_no_fill (0);
  else
    {
      int num_to_delete = idx_arg.length (len);

      if (num_to_delete != 0)
	{
	  int new_len = len;

	  int iidx = 0;

	  for (int i = 0; i < len; i++)
	    if (i == idx_arg.elem (iidx))
	      {
		iidx++;
		new_len--;

		if (iidx == num_to_delete)
		  break;
	      }

	  if (new_len > 0)
	    {
	      T *new_data = new T [new_len];

	      int ii = 0;
	      iidx = 0;
	      for (int i = 0; i < len; i++)
		{
		  if (iidx < num_to_delete && i == idx_arg.elem (iidx))
		    iidx++;
		  else
		    {
		      new_data[ii] = elem (i);
		      ii++;
		    }
		}

	      if (--rep->count <= 0)
		delete rep;

	      rep = new typename Array<T>::ArrayRep (new_data, new_len);

	      dimensions.resize (1);
	      dimensions(0) = new_len;
	    }
	  else
	    (*current_liboctave_error_handler)
	      ("A(idx) = []: index out of range");
	}
    }
}

template <class T>
void
Array<T>::maybe_delete_elements_2 (idx_vector& idx_arg)
{
  assert (ndims () == 2);

  int nr = dim1 ();
  int nc = dim2 ();

  if (nr == 0 && nc == 0)
    return;

  int n;
  if (nr == 1)
    n = nc;
  else if (nc == 1)
    n = nr;
  else
    {
      // Reshape to row vector for Matlab compatibility.

      n = nr * nc;
      nr = 1;
      nc = n;
    }

  if (idx_arg.is_colon_equiv (n, 1))
    {
      // Either A(:) = [] or A(idx) = [] with idx enumerating all
      // elements, so we delete all elements and return [](0x0).  To
      // preserve the orientation of the vector, you have to use
      // A(idx,:) = [] (delete rows) or A(:,idx) (delete columns).

      resize_no_fill (0, 0);
      return;
    }

  idx_arg.sort (true);

  int num_to_delete = idx_arg.length (n);

  if (num_to_delete != 0)
    {
      int new_n = n;

      int iidx = 0;

      for (int i = 0; i < n; i++)
	if (i == idx_arg.elem (iidx))
	  {
	    iidx++;
	    new_n--;

	    if (iidx == num_to_delete)
	      break;
	  }

      if (new_n > 0)
	{
	  T *new_data = new T [new_n];

	  int ii = 0;
	  iidx = 0;
	  for (int i = 0; i < n; i++)
	    {
	      if (iidx < num_to_delete && i == idx_arg.elem (iidx))
		iidx++;
	      else
		{
		  new_data[ii] = elem (i);

		  ii++;
		}
	    }

	  if (--(Array<T>::rep)->count <= 0)
	    delete Array<T>::rep;

	  Array<T>::rep = new typename Array<T>::ArrayRep (new_data, new_n);

	  dimensions.resize (2);

	  if (nr == 1)
	    {
	      dimensions(0) = 1;
	      dimensions(1) = new_n;
	    }
	  else
	    {
	      dimensions(0) = new_n;
	      dimensions(1) = 1;
	    }
	}
      else
	(*current_liboctave_error_handler)
	  ("A(idx) = []: index out of range");
    }
}

template <class T>
void
Array<T>::maybe_delete_elements (idx_vector& idx_i, idx_vector& idx_j)
{
  assert (ndims () == 2);

  int nr = dim1 ();
  int nc = dim2 ();

  if (nr == 0 && nc == 0)
    return;

  if (idx_i.is_colon ())
    {
      if (idx_j.is_colon ())
	{
	  // A(:,:) -- We are deleting columns and rows, so the result
	  // is [](0x0).

	  resize_no_fill (0, 0);
	  return;
	}

      if (idx_j.is_colon_equiv (nc, 1))
	{
	  // A(:,j) -- We are deleting columns by enumerating them,
	  // If we enumerate all of them, we should have zero columns
	  // with the same number of rows that we started with.

	  resize_no_fill (nr, 0);
	  return;
	}
    }

  if (idx_j.is_colon () && idx_i.is_colon_equiv (nr, 1))
    {
      // A(i,:) -- We are deleting rows by enumerating them.  If we
      // enumerate all of them, we should have zero rows with the
      // same number of columns that we started with.

      resize_no_fill (0, nc);
      return;
    }

  if (idx_i.is_colon_equiv (nr, 1))
    {
      if (idx_j.is_colon_equiv (nc, 1))
	resize_no_fill (0, 0);
      else
	{
	  idx_j.sort (true);

	  int num_to_delete = idx_j.length (nc);

	  if (num_to_delete != 0)
	    {
	      if (nr == 1 && num_to_delete == nc)
		resize_no_fill (0, 0);
	      else
		{
		  int new_nc = nc;

		  int iidx = 0;

		  for (int j = 0; j < nc; j++)
		    if (j == idx_j.elem (iidx))
		      {
			iidx++;
			new_nc--;

			if (iidx == num_to_delete)
			  break;
		      }

		  if (new_nc > 0)
		    {
		      T *new_data = new T [nr * new_nc];

		      int jj = 0;
		      iidx = 0;
		      for (int j = 0; j < nc; j++)
			{
			  if (iidx < num_to_delete && j == idx_j.elem (iidx))
			    iidx++;
			  else
			    {
			      for (int i = 0; i < nr; i++)
				new_data[nr*jj+i] = elem (i, j);
			      jj++;
			    }
			}

		      if (--(Array<T>::rep)->count <= 0)
			delete Array<T>::rep;

		      Array<T>::rep = new typename Array<T>::ArrayRep (new_data, nr * new_nc);

		      dimensions.resize (2);
		      dimensions(1) = new_nc;
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("A(idx) = []: index out of range");
		}
	    }
	}
    }
  else if (idx_j.is_colon_equiv (nc, 1))
    {
      if (idx_i.is_colon_equiv (nr, 1))
	resize_no_fill (0, 0);
      else
	{
	  idx_i.sort (true);

	  int num_to_delete = idx_i.length (nr);

	  if (num_to_delete != 0)
	    {
	      if (nc == 1 && num_to_delete == nr)
		resize_no_fill (0, 0);
	      else
		{
		  int new_nr = nr;

		  int iidx = 0;

		  for (int i = 0; i < nr; i++)
		    if (i == idx_i.elem (iidx))
		      {
			iidx++;
			new_nr--;

			if (iidx == num_to_delete)
			  break;
		      }

		  if (new_nr > 0)
		    {
		      T *new_data = new T [new_nr * nc];

		      int ii = 0;
		      iidx = 0;
		      for (int i = 0; i < nr; i++)
			{
			  if (iidx < num_to_delete && i == idx_i.elem (iidx))
			    iidx++;
			  else
			    {
			      for (int j = 0; j < nc; j++)
				new_data[new_nr*j+ii] = elem (i, j);
			      ii++;
			    }
			}

		      if (--(Array<T>::rep)->count <= 0)
			delete Array<T>::rep;

		      Array<T>::rep = new typename Array<T>::ArrayRep (new_data, new_nr * nc);

		      dimensions.resize (2);
		      dimensions(0) = new_nr;
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("A(idx) = []: index out of range");
		}
	    }
	}
    }
}

template <class T>
void
Array<T>::maybe_delete_elements (idx_vector&, idx_vector&, idx_vector&)
{
  assert (0);
}

template <class T>
void
Array<T>::maybe_delete_elements (Array<idx_vector>& ra_idx, const T& rfv)
{
  int n_idx = ra_idx.length ();

  dim_vector lhs_dims = dims ();

  if (lhs_dims.all_zero ())
    return;

  int n_lhs_dims = lhs_dims.length ();

  Array<int> idx_is_colon (n_idx, 0);

  Array<int> idx_is_colon_equiv (n_idx, 0);

  // Initialization of colon arrays.

  for (int i = 0; i < n_idx; i++)
    {
      idx_is_colon_equiv(i) = ra_idx(i).is_colon_equiv (lhs_dims(i), 1);

      idx_is_colon(i) = ra_idx(i).is_colon ();
    }

  bool idx_ok = true;

  // Check for index out of bounds.

  for (int i = 0 ; i < n_idx - 1; i++)
    {
      if (! (idx_is_colon(i) || idx_is_colon_equiv(i)))
	{
	  ra_idx(i).sort (true);

	  if (ra_idx(i).max () > lhs_dims(i))
	    {
	      (*current_liboctave_error_handler)
		("index exceeds array dimensions");

	      idx_ok = false;
	      break;
	    }
	  else if (ra_idx(i).min () < 0) // I believe this is checked elsewhere
	    {
	      (*current_liboctave_error_handler)
		("index must be one or larger");

	      idx_ok = false;
	      break;
	    }
	}
    }

  if (n_idx <= n_lhs_dims)
    {
      int last_idx = ra_idx(n_idx-1).max ();

      int sum_el = lhs_dims(n_idx-1);

      for (int i = n_idx; i < n_lhs_dims; i++)
	  sum_el *= lhs_dims(i);

      if (last_idx > sum_el - 1)
	{
	  (*current_liboctave_error_handler)
	    ("index exceeds array dimensions");

	  idx_ok = false;
	}
    }

  if (idx_ok)
    {
      if (n_idx > 1
	  && (all_ones (idx_is_colon) || all_ones (idx_is_colon_equiv)))
	{
	  // A(:,:,:) -- we are deleting elements in all dimensions, so
	  // the result is [](0x0x0).

	  dim_vector zeros;
	  zeros.resize (n_idx);

	  for (int i = 0; i < n_idx; i++)
	    zeros(i) = 0;

	  resize (zeros, rfv);
	}

      else if (n_idx > 1
	       && num_ones (idx_is_colon) == n_idx - 1
	       && num_ones (idx_is_colon_equiv) == n_idx)
	{
	  // A(:,:,j) -- we are deleting elements in one dimension by
	  // enumerating them.
	  //
	  // If we enumerate all of the elements, we should have zero
	  // elements in that dimension with the same number of elements
	  // in the other dimensions that we started with.

	  dim_vector temp_dims;
	  temp_dims.resize (n_idx);

	  for (int i = 0; i < n_idx; i++)
	    {
	      if (idx_is_colon (i))
		temp_dims(i) =  lhs_dims(i);
	      else
		temp_dims(i) = 0;
	    }

	  resize (temp_dims);
	}
      else if (n_idx > 1 && num_ones (idx_is_colon) == n_idx - 1)
	{
	  // We have colons in all indices except for one.
	  // This index tells us which slice to delete

	  if (n_idx < n_lhs_dims)
	    {
	      // Collapse dimensions beyond last index.

	      if (liboctave_wfi_flag && ! (ra_idx(n_idx-1).is_colon ()))
		(*current_liboctave_warning_handler)
		  ("fewer indices than dimensions for N-d array");

	      for (int i = n_idx; i < n_lhs_dims; i++)
		lhs_dims(n_idx-1) *= lhs_dims(i);

	      lhs_dims.resize (n_idx);

	      // Reshape *this.
	      dimensions = lhs_dims;
	    }

	  int non_col = 0;

	  // Find the non-colon column.

	  for (int i = 0; i < n_idx; i++)
	    {
	      if (! idx_is_colon(i))
		non_col = i;
	    }

	  // The length of the non-colon dimension.

	  int non_col_dim = lhs_dims (non_col);

	  int num_to_delete = ra_idx(non_col).length (lhs_dims (non_col));

	  if (num_to_delete > 0)
	    {
	      int temp = lhs_dims.num_ones ();

	      if (non_col_dim == 1)
		temp--;

	      if (temp == n_idx - 1 && num_to_delete == non_col_dim)
		{
		  // We have A with (1x1x4), where A(1,:,1:4)
		  // Delete all (0x0x0)

		  dim_vector zero_dims (n_idx, 0);

		  resize (zero_dims, rfv);
		}
	      else
		{
		  // New length of non-colon dimension
		  // (calculated in the next for loop)

		  int new_dim = non_col_dim;

		  int iidx = 0;

		  for (int j = 0; j < non_col_dim; j++)
		    if (j == ra_idx(non_col).elem (iidx))
		      {
			iidx++;

			new_dim--;

			if (iidx == num_to_delete)
			  break;
		      }

		  // Creating the new nd array after deletions.

		  if (new_dim > 0)
		    {
		      // Calculate number of elements in new array.

		      int num_new_elem=1;

		      for (int i = 0; i < n_idx; i++)
			{
			  if (i == non_col)
			    num_new_elem *= new_dim;

			  else
			    num_new_elem *= lhs_dims(i);
			}

		      T *new_data = new T [num_new_elem];

		      Array<int> result_idx (n_lhs_dims, 0);

		      dim_vector new_lhs_dim = lhs_dims;

		      new_lhs_dim(non_col) = new_dim;

		      int num_elem = 1;

		      int numidx = 0;

		      int n = length ();

		      for (int i = 0; i < n_lhs_dims; i++)
			if (i != non_col)
			  num_elem *= lhs_dims(i);

		      num_elem *= ra_idx(non_col).capacity ();

		      for (int i = 0; i < n; i++)
			{
			  if (numidx < num_elem
			      && is_in (result_idx(non_col), ra_idx(non_col)))
			    numidx++;

			  else
			    {
			      Array<int> temp_result_idx = result_idx;

			      int num_lgt = how_many_lgt (result_idx(non_col),
							  ra_idx(non_col));

			      temp_result_idx(non_col) -= num_lgt;

			      int kidx
				= ::compute_index (temp_result_idx, new_lhs_dim);

			      new_data[kidx] = elem (result_idx);
			    }

			  increment_index (result_idx, lhs_dims);
			}

		      if (--rep->count <= 0)
			delete rep;

		      rep = new typename Array<T>::ArrayRep (new_data,
							     num_new_elem);

		      dimensions = new_lhs_dim;
		    }
		}
	    }
	}
      else if (n_idx == 1)
	{
	  // This handle cases where we only have one index (not
	  // colon).  The index denotes which elements we should
	  // delete in the array which can be of any dimension. We
	  // return a column vector, except for the case where we are
	  // operating on a row vector. The elements are numerated
	  // column by column.
	  //
	  // A(3,3,3)=2;
	  // A(3:5) = []; A(6)=[]

	  int lhs_numel = numel ();

	  idx_vector idx_vec = ra_idx(0);

	  idx_vec.freeze (lhs_numel, 0, true, liboctave_wrore_flag);
      
	  idx_vec.sort (true);

	  int num_to_delete = idx_vec.length (lhs_numel);

	  if (num_to_delete > 0)
	    {
	      int new_numel = lhs_numel - num_to_delete;

	      T *new_data = new T[new_numel];

	      Array<int> lhs_ra_idx (ndims (), 0);

	      int ii = 0;
	      int iidx = 0;

	      for (int i = 0; i < lhs_numel; i++)
		{
		  if (iidx < num_to_delete && i == idx_vec.elem (iidx))
		    {
		      iidx++;
		    }
		  else
		    {
		      new_data[ii++] = elem (lhs_ra_idx);
		    }

		  increment_index (lhs_ra_idx, lhs_dims);
		}

	      if (--(Array<T>::rep)->count <= 0)
		delete Array<T>::rep;

	      Array<T>::rep = new typename Array<T>::ArrayRep (new_data, new_numel);

	      dimensions.resize (2);

	      if (lhs_dims.length () == 2 && lhs_dims(1) == 1)
		{
		  dimensions(0) = new_numel;
		  dimensions(1) = 1;
		}
	      else
		{
		  dimensions(0) = 1;
		  dimensions(1) = new_numel;
		}
	    }
	}
      else if (num_ones (idx_is_colon) < n_idx)
	{
	  (*current_liboctave_error_handler)
	    ("a null assignment can have only one non-colon index");
	}
    }
}

template <class T>
Array<T>
Array<T>::value (void)
{
  Array<T> retval;

  int n_idx = index_count ();

  if (n_idx == 2)
    {
      idx_vector *tmp = get_idx ();

      idx_vector idx_i = tmp[0];
      idx_vector idx_j = tmp[1];

      retval = index (idx_i, idx_j);
    }
  else if (n_idx == 1)
    {
      retval = index (idx[0]);
    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::value: invalid number of indices specified");

  clear_index ();

  return retval;
}

template <class T>
Array<T>
Array<T>::index (idx_vector& idx_arg, int resize_ok, const T& rfv) const
{
  Array<T> retval;

  dim_vector dv = idx_arg.orig_dimensions ();

  if (dv.length () > 2 || ndims () > 2)
    retval = indexN (idx_arg, resize_ok, rfv);
  else
    {
      switch (ndims ())
	{
	case 1:
	  retval = index1 (idx_arg, resize_ok, rfv);
	  break;

	case 2:
	  retval = index2 (idx_arg, resize_ok, rfv);
	  break;

	default:
	  (*current_liboctave_error_handler)
	    ("invalid array (internal error)");
	  break;
	}
    }

  return retval;
}

template <class T>
Array<T>
Array<T>::index1 (idx_vector& idx_arg, int resize_ok, const T& rfv) const
{
  Array<T> retval;

  int len = length ();

  int n = idx_arg.freeze (len, "vector", resize_ok);

  if (idx_arg)
    {
      if (idx_arg.is_colon_equiv (len))
	{
	  retval = *this;
	}
      else if (n == 0)
	{
	  retval.resize_no_fill (0);
	}
      else if (len == 1 && n > 1
	       && idx_arg.one_zero_only ()
	       && idx_arg.ones_count () == n)
	{
	  retval.resize_and_fill (n, elem (0));
	}
      else
	{
	  retval.resize_no_fill (n);

	  for (int i = 0; i < n; i++)
	    {
	      int ii = idx_arg.elem (i);
	      if (ii >= len)
		retval.elem (i) = rfv;
	      else
		retval.elem (i) = elem (ii);
	    }
	}
    }

  // idx_vector::freeze() printed an error message for us.

  return retval;
}

template <class T>
Array<T>
Array<T>::index2 (idx_vector& idx_arg, int resize_ok, const T& rfv) const
{
  Array<T> retval;

  assert (ndims () == 2);

  int nr = dim1 ();
  int nc = dim2 ();

  int orig_len = nr * nc;

  dim_vector idx_orig_dims = idx_arg.orig_dimensions ();

  int idx_orig_rows = idx_arg.orig_rows ();
  int idx_orig_columns = idx_arg.orig_columns ();

  if (idx_arg.is_colon ())
    {
      // Fast magic colon processing.

      int result_nr = nr * nc;
      int result_nc = 1;

      retval = Array<T> (*this, dim_vector (result_nr, result_nc));
    }
  else if (nr == 1 && nc == 1)
    {
      Array<T> tmp = Array<T>::index1 (idx_arg, resize_ok);

      int len = tmp.length ();

      if (len == 0 && idx_arg.one_zero_only ())
	retval = Array<T> (tmp, dim_vector (0, 0));
      else if (len >= idx_orig_dims.numel ())
	retval = Array<T> (tmp, idx_orig_dims);
    }
  else if (nr == 1 || nc == 1)
    {
      // If indexing a vector with a matrix, return value has same
      // shape as the index.  Otherwise, it has same orientation as
      // indexed object.

      Array<T> tmp = Array<T>::index1 (idx_arg, resize_ok);

      int len = tmp.length ();

      if ((len != 0 && idx_arg.one_zero_only ())
	  || idx_orig_rows == 1 || idx_orig_columns == 1)
	{
	  if (nr == 1)
	    retval = Array<T> (tmp, dim_vector (1, len));
	  else
	    retval = Array<T> (tmp, dim_vector (len, 1));
	}
      else if (len >= idx_orig_dims.numel ())
	retval = Array<T> (tmp, idx_orig_dims);
    }
  else
    {
      if (liboctave_wfi_flag
	  && ! (idx_arg.one_zero_only ()
		&& idx_orig_rows == nr
		&& idx_orig_columns == nc))
	(*current_liboctave_warning_handler) ("single index used for matrix");

      // This code is only for indexing matrices.  The vector
      // cases are handled above.

      idx_arg.freeze (nr * nc, "matrix", resize_ok);

      if (idx_arg)
	{
	  int result_nr = idx_orig_rows;
	  int result_nc = idx_orig_columns;

	  if (idx_arg.one_zero_only ())
	    {
	      result_nr = idx_arg.ones_count ();
	      result_nc = (result_nr > 0 ? 1 : 0);
	    }

	  retval.resize_no_fill (result_nr, result_nc);

	  int k = 0;
	  for (int j = 0; j < result_nc; j++)
	    {
	      for (int i = 0; i < result_nr; i++)
		{
		  int ii = idx_arg.elem (k++);
		  if (ii >= orig_len)
		    retval.elem (i, j) = rfv;
		  else
		    {
		      int fr = ii % nr;
		      int fc = (ii - fr) / nr;
		      retval.elem (i, j) = elem (fr, fc);
		    }
		}
	    }
	}
      // idx_vector::freeze() printed an error message for us.
    }

  return retval;
}

template <class T>
Array<T>
Array<T>::indexN (idx_vector& ra_idx, int resize_ok, const T& rfv) const
{
  Array<T> retval;

  int n_dims = dims().length ();

  int orig_len = dims().numel ();

  dim_vector idx_orig_dims = ra_idx.orig_dimensions ();

  if (ra_idx.is_colon ())
    {
      // Fast magic colon processing.

      retval = Array<T> (*this, dim_vector (orig_len, 1));
    }
  else if (length () == 1)
    {
      // Only one element in array.

      Array<T> tmp = Array<T>::index (ra_idx, resize_ok);

      int len = tmp.length ();

      if (len != 0)
	{
	  if (len >= idx_orig_dims.numel ())
	    retval = Array<T> (tmp, idx_orig_dims);
	}
      else
	retval = Array<T> (tmp, dim_vector (0, 0));
    }
  else if (vector_equivalent (dims ()))
    {
      // We're getting elements from a vector equivalent i.e. (1x4x1).

      Array<T> tmp = Array<T>::index (ra_idx, resize_ok);

      int len = tmp.length ();

      if (len == 0)
	{
	  if (idx_orig_dims.any_zero ())
	    retval = Array<T> (idx_orig_dims);
	  else
	    {
	      dim_vector new_dims;

	      new_dims.resize (n_dims);

	      for (int i = 0; i < n_dims; i++)
	        {
		  if ((dims ())(i) == 1)
		    new_dims(i) = 1;
		}

	      new_dims.chop_trailing_singletons ();

	      retval = Array<T> (new_dims);
	    }
	}
      else
	{
	  if (vector_equivalent (idx_orig_dims))
	    {
	      // Array<int> index (n_dims, len);
	      dim_vector new_dims;

	      new_dims.resize (n_dims);

	      for (int i = 0; i < n_dims; i++)
	        {
		  if ((dims ())(i) == 1)
		    new_dims(i) = 1;
	        }

	      new_dims.chop_trailing_singletons ();

	      retval = Array<T> (tmp, new_dims);
	    }
	  else if (tmp.length () >= idx_orig_dims.numel ())
	    retval = Array<T> (tmp, idx_orig_dims);

	  (*current_liboctave_error_handler)
	    ("I do not know what to do here yet!");
	}
    }
  else
    {
      if (liboctave_wfi_flag
	  && ! (ra_idx.is_colon ()
		|| (ra_idx.one_zero_only () && idx_orig_dims == dims ())))
	(*current_liboctave_warning_handler)
	  ("single index used for N-d array");

      ra_idx.freeze (orig_len, "nd-array", resize_ok);

      if (ra_idx)
	{
	  dim_vector result_dims (idx_orig_dims);

	  if (ra_idx.one_zero_only ())
	    {
	      result_dims.resize (2);
	      int ntot = ra_idx.ones_count ();
	      result_dims(0) = ntot;
	      result_dims(1) = (ntot > 0 ? 1 : 0);
	    }

	  result_dims.chop_trailing_singletons ();

	  retval.resize (result_dims);

	  int n = result_dims.numel ();

	  int r_dims = result_dims.length ();

	  Array<int> iidx (r_dims, 0);

	  int k = 0;

	  for (int i = 0; i < n; i++)
	    {
	      int ii = ra_idx.elem (k++);

	      if (ii >= orig_len)
	        retval.elem (iidx) = rfv;
	      else
	        {
		  Array<int> temp = get_ra_idx (ii, dims ());

		  retval.elem (iidx) = elem (temp);
		}
	      if (i != n - 1)
		increment_index (iidx, result_dims);
	    }
	}
    }

  return retval;
}

template <class T>
Array<T>
Array<T>::index (idx_vector& idx_i, idx_vector& idx_j, int resize_ok,
		 const T& rfv) const
{
  Array<T> retval;

  assert (ndims () == 2);

  int nr = dim1 ();
  int nc = dim2 ();

  int n = idx_i.freeze (nr, "row", resize_ok);
  int m = idx_j.freeze (nc, "column", resize_ok);

  if (idx_i && idx_j)
    {
      if (idx_i.orig_empty () || idx_j.orig_empty () || n == 0 || m == 0)
	{
	  retval.resize_no_fill (n, m);
	}
      else if (idx_i.is_colon_equiv (nr) && idx_j.is_colon_equiv (nc))
	{
	  retval = *this;
	}
      else
	{
	  retval.resize_no_fill (n, m);

	  for (int j = 0; j < m; j++)
	    {
	      int jj = idx_j.elem (j);
	      for (int i = 0; i < n; i++)
		{
		  int ii = idx_i.elem (i);
		  if (ii >= nr || jj >= nc)
		    retval.elem (i, j) = rfv;
		  else
		    retval.elem (i, j) = elem (ii, jj);
		}
	    }
	}
    }

  // idx_vector::freeze() printed an error message for us.

  return retval;
}

template <class T>
Array<T>
Array<T>::index (Array<idx_vector>& ra_idx, int resize_ok, const T&) const
{
  // This function handles all calls with more than one idx.
  // For (3x3x3), the call can be A(2,5), A(2,:,:), A(3,2,3) etc.

  Array<T> retval;

  int n_dims = dimensions.length ();

  // Remove trailing singletons in ra_idx, but leave at least ndims
  // elements.

  int ra_idx_len = ra_idx.length ();

  bool trim_trailing_singletons = true;
  for (int j = ra_idx_len; j > n_dims; j--)
    {
      idx_vector iidx = ra_idx (ra_idx_len-1);
      if (iidx.capacity () == 1 && trim_trailing_singletons)
	ra_idx_len--;
      else
	trim_trailing_singletons = false;

      for (int i = 0; i < iidx.capacity (); i++)
	if (iidx (i) != 0)
	  {
	    (*current_liboctave_error_handler)
	      ("index exceeds N-d array dimensions");
	    
	    return retval;
	  }
    }

  ra_idx.resize (ra_idx_len);

  dim_vector new_dims = dims ();
  dim_vector frozen_lengths;

  if (! any_orig_empty (ra_idx) && ra_idx_len < n_dims)
    frozen_lengths = short_freeze (ra_idx, dimensions, resize_ok);
  else
    {
      new_dims.resize (ra_idx_len, 1);
      frozen_lengths = freeze (ra_idx, new_dims, resize_ok);
    }

  if (all_ok (ra_idx))
    {
      if (any_orig_empty (ra_idx) || frozen_lengths.any_zero ())
	{
	  frozen_lengths.chop_trailing_singletons ();

	  retval.resize (frozen_lengths);
	}
      else if (frozen_lengths.length () == n_dims
	       && all_colon_equiv (ra_idx, dimensions))
	{
	  retval = *this;
	}
      else
	{
	  dim_vector frozen_lengths_for_resize = frozen_lengths;

	  frozen_lengths_for_resize.chop_trailing_singletons ();

	  retval.resize (frozen_lengths_for_resize);

	  int n = retval.length ();

	  Array<int> result_idx (ra_idx.length (), 0);

	  Array<int> elt_idx;

	  for (int i = 0; i < n; i++)
	    {
	      elt_idx = get_elt_idx (ra_idx, result_idx);

	      int numelem_elt = get_scalar_idx (elt_idx, new_dims);

	      if (numelem_elt > length () || numelem_elt < 0)
		(*current_liboctave_error_handler)
		  ("invalid N-d array index");
	      else
		retval.elem (i) = elem (numelem_elt);

	      increment_index (result_idx, frozen_lengths);

	    }
	}
    }

  return retval;
}

// XXX FIXME XXX -- this is a mess.

template <class LT, class RT>
int
assign (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv)
{
  int retval = 0;

  switch (lhs.ndims ())
    {
    case 0:
      {
	if (lhs.index_count () < 3)
	  {
	    // kluge...
	    lhs.resize_no_fill (0, 0);
	    retval = assign2 (lhs, rhs, rfv);
	  }
	else
	  retval = assignN (lhs, rhs, rfv);
      }
      break;

    case 1:
      {
	if (lhs.index_count () > 1)
	  retval = assignN (lhs, rhs, rfv);
	else
	  retval = assign1 (lhs, rhs, rfv);
      }
      break;

    case 2:
      {
	if (lhs.index_count () > 2)
	  retval = assignN (lhs, rhs, rfv);
	else
	  retval = assign2 (lhs, rhs, rfv);
      }
      break;

    default:
      retval = assignN (lhs, rhs, rfv);
      break;
    }

  return retval;
}

template <class LT, class RT>
int
assign1 (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv)
{
  int retval = 1;

  idx_vector *tmp = lhs.get_idx ();

  idx_vector lhs_idx = tmp[0];

  int lhs_len = lhs.length ();
  int rhs_len = rhs.length ();

  int n = lhs_idx.freeze (lhs_len, "vector", true, liboctave_wrore_flag);

  if (n != 0)
    {
      if (rhs_len == n || rhs_len == 1)
	{
	  int max_idx = lhs_idx.max () + 1;
	  if (max_idx > lhs_len)
	    lhs.resize_and_fill (max_idx, rfv);
	}

      if (rhs_len == n)
	{
	  for (int i = 0; i < n; i++)
	    {
	      int ii = lhs_idx.elem (i);
	      lhs.elem (ii) = rhs.elem (i);
	    }
	}
      else if (rhs_len == 1)
	{
	  RT scalar = rhs.elem (0);

	  for (int i = 0; i < n; i++)
	    {
	      int ii = lhs_idx.elem (i);
	      lhs.elem (ii) = scalar;
	    }
	}
      else
	{
	  (*current_liboctave_error_handler)
	    ("A(I) = X: X must be a scalar or a vector with same length as I");

	  retval = 0;
	}
    }
  else if (lhs_idx.is_colon ())
    {
      if (lhs_len == 0)
	{
	  lhs.resize_no_fill (rhs_len);

	  for (int i = 0; i < rhs_len; i++)
	    lhs.elem (i) = rhs.elem (i);
	}
      else
	(*current_liboctave_error_handler)
	  ("A(:) = X: A must be the same size as X");
    }
  else if (! (rhs_len == 1 || rhs_len == 0))
    {
      (*current_liboctave_error_handler)
	("A([]) = X: X must also be an empty matrix or a scalar");

      retval = 0;
    }

  lhs.clear_index ();

  return retval;
}

#define MAYBE_RESIZE_LHS \
  do \
    { \
      int max_row_idx = idx_i_is_colon ? rhs_nr : idx_i.max () + 1; \
      int max_col_idx = idx_j_is_colon ? rhs_nc : idx_j.max () + 1; \
 \
      int new_nr = max_row_idx > lhs_nr ? max_row_idx : lhs_nr; \
      int new_nc = max_col_idx > lhs_nc ? max_col_idx : lhs_nc; \
 \
      lhs.resize_and_fill (new_nr, new_nc, rfv); \
    } \
  while (0)

template <class LT, class RT>
int
assign2 (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv)
{
  int retval = 1;

  int n_idx = lhs.index_count ();

  int lhs_nr = lhs.rows ();
  int lhs_nc = lhs.cols ();

  Array<RT> xrhs = rhs;

  int rhs_nr = xrhs.rows ();
  int rhs_nc = xrhs.cols ();

  if (xrhs.ndims () > 2)
    {
      xrhs = xrhs.squeeze ();

      dim_vector dv_tmp = xrhs.dims ();

      switch (dv_tmp.length ())
	{
	case 1:
	  // XXX FIXME XXX -- this case should be unnecessary, because
	  // squeeze should always return an object with 2 dimensions.
	  if (rhs_nr == 1)
	    rhs_nc = dv_tmp.elem (0);
	  break;

	case 2:
	  rhs_nr = dv_tmp.elem (0);
	  rhs_nc = dv_tmp.elem (1);
	  break;

	default:
	  (*current_liboctave_error_handler)
	    ("Array<T>::assign2: Dimension mismatch");
	  return 0;
	}
    }

  idx_vector *tmp = lhs.get_idx ();

  idx_vector idx_i;
  idx_vector idx_j;

  if (n_idx > 1)
    idx_j = tmp[1];

  if (n_idx > 0)
    idx_i = tmp[0];

  if (n_idx == 2)
    {
      int n = idx_i.freeze (lhs_nr, "row", true, liboctave_wrore_flag);

      int m = idx_j.freeze (lhs_nc, "column", true, liboctave_wrore_flag);

      int idx_i_is_colon = idx_i.is_colon ();
      int idx_j_is_colon = idx_j.is_colon ();

      if (idx_i_is_colon)
	n = lhs_nr > 0 ? lhs_nr : rhs_nr;

      if (idx_j_is_colon)
	m = lhs_nc > 0 ? lhs_nc : rhs_nc;

      if (idx_i && idx_j)
	{
	  if (rhs_nr == 0 && rhs_nc == 0)
	    {
	      lhs.maybe_delete_elements (idx_i, idx_j);
	    }
	  else
	    {
	      if (rhs_nr == 1 && rhs_nc == 1 && n >= 0 && m >= 0)
		{
		  // No need to do anything if either of the indices
		  // are empty.

		  if (n > 0 && m > 0)
		    {
		      MAYBE_RESIZE_LHS;

		      RT scalar = xrhs.elem (0, 0);

		      for (int j = 0; j < m; j++)
			{
			  int jj = idx_j.elem (j);
			  for (int i = 0; i < n; i++)
			    {
			      int ii = idx_i.elem (i);
			      lhs.elem (ii, jj) = scalar;
			    }
			}
		    }
		}
	      else if (n == rhs_nr && m == rhs_nc)
		{
		  if (n > 0 && m > 0)
		    {
		      MAYBE_RESIZE_LHS;

		      for (int j = 0; j < m; j++)
			{
			  int jj = idx_j.elem (j);
			  for (int i = 0; i < n; i++)
			    {
			      int ii = idx_i.elem (i);
			      lhs.elem (ii, jj) = xrhs.elem (i, j);
			    }
			}
		    }
		}
	      else if (n == 0 && m == 0)
		{
		  if (! ((rhs_nr == 1 && rhs_nc == 1)
			 || (rhs_nr == 0 || rhs_nc == 0)))
		    {
		      (*current_liboctave_error_handler)
		("A([], []) = X: X must be an empty matrix or a scalar");

		      retval = 0;
		    }
		}
	      else
		{
		  (*current_liboctave_error_handler)
    ("A(I, J) = X: X must be a scalar or the number of elements in I must");
		  (*current_liboctave_error_handler)
    ("match the number of rows in X and the number of elements in J must");
		  (*current_liboctave_error_handler)
    ("match the number of columns in X");

		  retval = 0;
		}
	    }
	}
      // idx_vector::freeze() printed an error message for us.
    }
  else if (n_idx == 1)
    {
      int lhs_is_empty = lhs_nr == 0 || lhs_nc == 0;

      if (lhs_is_empty || (lhs_nr == 1 && lhs_nc == 1))
	{
	  int lhs_len = lhs.length ();

	  int n = idx_i.freeze (lhs_len, 0, true, liboctave_wrore_flag);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		{
		  if (n != 0 && (lhs_nr != 0 || lhs_nc != 0))
		    lhs.maybe_delete_elements (idx_i);
		}
	      else
		{
		  if (liboctave_wfi_flag)
		    {
		      if (lhs_is_empty
			  && idx_i.is_colon ()
			  && ! (rhs_nr == 1 || rhs_nc == 1))
			{
			  (*current_liboctave_warning_handler)
			    ("A(:) = X: X is not a vector or scalar");
			}
		      else
			{
			  int idx_nr = idx_i.orig_rows ();
			  int idx_nc = idx_i.orig_columns ();

			  if (! (rhs_nr == idx_nr && rhs_nc == idx_nc))
			    (*current_liboctave_warning_handler)
			      ("A(I) = X: X does not have same shape as I");
			}
		    }

		  if (assign1 (lhs, xrhs, rfv))
		    {
		      int len = lhs.length ();

		      if (len > 0)
			{
			  // The following behavior is much simplified
			  // over previous versions of Octave.  It
			  // seems to be compatible with Matlab.

			  lhs.dimensions = dim_vector (1, lhs.length ());
			}
		      else
			lhs.dimensions = dim_vector (0, 0);
		    }
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nr == 1)
	{
	  idx_i.freeze (lhs_nc, "vector", true, liboctave_wrore_flag);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else
		{
		  if (assign1 (lhs, xrhs, rfv))
		    lhs.dimensions = dim_vector (1, lhs.length ());
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nc == 1)
	{
	  idx_i.freeze (lhs_nr, "vector", true, liboctave_wrore_flag);

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else
		{
		  if (assign1 (lhs, xrhs, rfv))
		    lhs.dimensions = dim_vector (lhs.length (), 1);
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else
	{
	  if (liboctave_wfi_flag
	      && ! (idx_i.is_colon ()
		    || (idx_i.one_zero_only ()
			&& idx_i.orig_rows () == lhs_nr
			&& idx_i.orig_columns () == lhs_nc)))
	    (*current_liboctave_warning_handler)
	      ("single index used for matrix");

	  int len = idx_i.freeze (lhs_nr * lhs_nc, "matrix");

	  if (idx_i)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		lhs.maybe_delete_elements (idx_i);
	      else if (len == 0)
		{
		  if (! ((rhs_nr == 1 && rhs_nc == 1)
			 || (rhs_nr == 0 || rhs_nc == 0)))
		    (*current_liboctave_error_handler)
		      ("A([]) = X: X must be an empty matrix or scalar");
		}
	      else if (len == rhs_nr * rhs_nc)
		{
		  int k = 0;
		  for (int j = 0; j < rhs_nc; j++)
		    {
		      for (int i = 0; i < rhs_nr; i++)
			{
			  int ii = idx_i.elem (k++);
			  int fr = ii % lhs_nr;
			  int fc = (ii - fr) / lhs_nr;
			  lhs.elem (fr, fc) = xrhs.elem (i, j);
			}
		    }
		}
	      else if (rhs_nr == 1 && rhs_nc == 1)
		{
		  RT scalar = rhs.elem (0, 0);

		  for (int i = 0; i < len; i++)
		    {
		      int ii = idx_i.elem (i);
		      lhs.elem (ii) = scalar;
		    }
		}
	      else
		{
		  (*current_liboctave_error_handler)
      ("A(I) = X: X must be a scalar or a matrix with the same size as I");

		  retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
    }
  else
    {
      (*current_liboctave_error_handler)
	("invalid number of indices for matrix expression");

      retval = 0;
    }

  lhs.clear_index ();

  return retval;
}

template <class LT, class RT>
int
assignN (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv)
{
  int retval = 1;

  dim_vector rhs_dims = rhs.dims ();

  int rhs_dims_len = rhs_dims.length ();

  bool rhs_is_scalar = is_scalar (rhs_dims);

  int n_idx = lhs.index_count ();

  idx_vector *idx_vex = lhs.get_idx ();

  Array<idx_vector> idx = conv_to_array (idx_vex, n_idx);

  if (rhs_dims_len == 2 && rhs_dims(0) == 0 && rhs_dims(1) == 0)
    {
      lhs.maybe_delete_elements (idx, rfv);
    }
  else if (n_idx == 1)
    {
      idx_vector iidx = idx(0);

      if (liboctave_wfi_flag
	  && ! (iidx.is_colon ()
		|| (iidx.one_zero_only ()
		    && iidx.orig_dimensions () == lhs.dims ())))
	(*current_liboctave_warning_handler)
	  ("single index used for N-d array");

      int lhs_len = lhs.length ();

      int len = iidx.freeze (lhs_len, "N-d arrray");

      if (iidx)
	{
	  if (len == 0)
	    {
	      if (! (rhs_dims.all_ones () || rhs_dims.any_zero ()))
		{
		  (*current_liboctave_error_handler)
		    ("A([]) = X: X must be an empty matrix or scalar");

		  retval = 0;
		}
	    }
	  else if (len == rhs.length ())
	    {
	      for (int i = 0; i < len; i++)
		{
		  int ii = iidx.elem (i);

		  lhs.elem (ii) = rhs.elem (i);
		}
	    }
	  else if (rhs_is_scalar)
	    {
	      RT scalar = rhs.elem (0);

	      for (int i = 0; i < len; i++)
		{
		  int ii = iidx.elem (i);

		  lhs.elem (ii) = scalar;
		}
	    }
	  else
	    {
	      (*current_liboctave_error_handler)
		("A(I) = X: X must be a scalar or a matrix with the same size as I");

	      retval = 0;
	    }

	  // idx_vector::freeze() printed an error message for us.
	}
    }
  else
    {
      // Maybe expand to more dimensions.

      dim_vector lhs_dims = lhs.dims ();

      int lhs_dims_len = lhs_dims.length ();

      dim_vector final_lhs_dims = lhs_dims;

      dim_vector frozen_len;

      int orig_lhs_dims_len = lhs_dims_len;

      bool orig_empty = lhs_dims.all_zero ();

      if (n_idx < lhs_dims_len)
	{
	  // Collapse dimensions beyond last index.  Note that we
	  // delay resizing LHS until we know that the assignment will
	  // succeed.

	  if (liboctave_wfi_flag && ! (idx(n_idx-1).is_colon ()))
	    (*current_liboctave_warning_handler)
	      ("fewer indices than dimensions for N-d array");

	  for (int i = n_idx; i < lhs_dims_len; i++)
	    lhs_dims(n_idx-1) *= lhs_dims(i);

	  lhs_dims.resize (n_idx);

	  lhs_dims_len = lhs_dims.length ();
	}

      // Resize.

      dim_vector new_dims;
      new_dims.resize (n_idx);

      for (int i = 0; i < n_idx; i++)
	{
	  if (orig_empty)
	    {
	      // If index is a colon, resizing to RHS dimensions is
	      // allowed because we started out empty.

	      new_dims(i)
		= (i < rhs_dims.length () && idx(i).is_colon ())
		? rhs_dims(i) : idx(i).max () + 1;
	    }
	  else
	    {
	      // We didn't start out with all zero dimensions, so if
	      // index is a colon, it refers to the current LHS
	      // dimension.  Otherwise, it is OK to enlarge to a
	      // dimension given by the largest index, but if that 
	      // index is a colon the new dimension is singleton.

	      if (i < lhs_dims_len
		  && (idx(i).is_colon () || idx(i).max () < lhs_dims(i)))
		new_dims(i) = lhs_dims(i);
	      else if (! idx(i).is_colon ())
		new_dims(i) = idx(i).max () + 1;
	      else
		new_dims(i) = 1;
	    }
	}

      if (retval != 0)
	{
	  if (! orig_empty
	      && n_idx < orig_lhs_dims_len
	      && new_dims(n_idx-1) != lhs_dims(n_idx-1))
	    {
	      // We reshaped and the last dimension changed.  This has to
	      // be an error, because we don't know how to undo that
	      // later...

	      (*current_liboctave_error_handler)
		("array index %d (= %d) for assignment requires invalid resizing operation",
		 n_idx, new_dims(n_idx-1));

	      retval = 0;
	    }
	  else
	    {
	      // Determine final dimensions for LHS and reset the
	      // current size of the LHS.  Note that we delay actually
	      // resizing LHS until we know that the assignment will
	      // succeed.

	      if (n_idx < orig_lhs_dims_len)
		{
		  for (int i = 0; i < n_idx-1; i++)
		    final_lhs_dims(i) = new_dims(i);
		}
	      else
		final_lhs_dims = new_dims;

	      lhs_dims = new_dims;

	      lhs_dims_len = lhs_dims.length ();

	      frozen_len = freeze (idx, lhs_dims, true);

	      if (rhs_is_scalar)
		{
		  lhs.resize_and_fill (new_dims, rfv);

		  if  (! final_lhs_dims.any_zero ())
		    {
		      int n = Array<LT>::get_size (frozen_len);

		      Array<int> result_idx (lhs_dims_len, 0);

		      RT scalar = rhs.elem (0);

		      for (int i = 0; i < n; i++)
			{
			  Array<int> elt_idx = get_elt_idx (idx, result_idx);

			  lhs.elem (elt_idx) = scalar;

			  increment_index (result_idx, frozen_len);
			}
		    }
		}
	      else
		{
		  // RHS is matrix or higher dimension.

		  // Check that non-singleton RHS dimensions conform to
		  // non-singleton LHS index dimensions.

		  dim_vector t_rhs_dims = rhs_dims.squeeze ();
		  dim_vector t_frozen_len = frozen_len.squeeze ();

		  // If after sqeezing out singleton dimensions, RHS is
		  // vector and LHS is vector, force them to have the same
		  // orientation so that operations like
		  //
		  //   a = zeros (3, 3, 3);
		  //   a(1:3,1,1) = [1,2,3];
		  //
		  // will work.

		  if (t_rhs_dims.length () == 2 && t_frozen_len.length () == 2
		      && ((t_rhs_dims.elem(1) == 1
			   && t_frozen_len.elem(0) == 1)
			  || (t_rhs_dims.elem(0) == 1
			      && t_frozen_len.elem(1) == 1)))
		    {
		      int t0 = t_rhs_dims.elem(0);
		      t_rhs_dims.elem(0) = t_rhs_dims.elem(1);
		      t_rhs_dims.elem(1) = t0;
		    }

		  if (t_rhs_dims != t_frozen_len)
		    {
		      (*current_liboctave_error_handler)
			("A(IDX-LIST) = X: X must be a scalar or size of X must equal number of elements indexed by IDX-LIST");

			  retval = 0;
		    }
		  else
		    {
		      lhs.resize_and_fill (new_dims, rfv);

		      if  (! final_lhs_dims.any_zero ())
			{
			  int n = Array<LT>::get_size (frozen_len);

			  Array<int> result_idx (lhs_dims_len, 0);

			  for (int i = 0; i < n; i++)
			    {
			      Array<int> elt_idx = get_elt_idx (idx, result_idx);

			      lhs.elem (elt_idx) = rhs.elem (i);

			      increment_index (result_idx, frozen_len);
			    }
			}
		    }
		}
	    }
	}

      if (retval != 0)
	lhs.resize (final_lhs_dims);
    }

  if (retval != 0)
    lhs.chop_trailing_singletons ();

  lhs.clear_index ();

  return retval;
}

template <class T>
void
Array<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "rep address: " << rep << "\n"
     << prefix << "rep->len:    " << rep->len << "\n"
     << prefix << "rep->data:   " << static_cast<void *> (rep->data) << "\n"
     << prefix << "rep->count:  " << rep->count << "\n";

  // 2D info:
  //
  //     << pefix << "rows: " << rows () << "\n"
  //     << prefix << "cols: " << cols () << "\n";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
