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

#include <iostream>

#include "Array.h"
#include "Array-idx.h"
#include "idx-vector.h"
#include "lo-error.h"

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
Array<T>::~Array (void)
{
  if (--rep->count <= 0)
    delete rep;

  delete [] idx;
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

  if (dt <= 0.5)
    {
      nt--;
      dt *= 2;

      if (dt <= 0.5)
	nt--;
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

  if (dt <= 0.5)
    {
      nt--;
      dt *= 2;

      if (dt <= 0.5)
	nt--;
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
    }

  if (dt <= 0.5)
    {
      nt--;
      dt *= 2;

      if (dt <= 0.5)
	nt--;
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

#if 0

template <class T>
int
Array<T>::compute_index (int i, int j) const
{
  int retval = -1;

  int n = dimensions.length ();

  if (n == 2)
    retval = j*dimensions(0)+i;
  else if (n == 1 && j == 0)
    retval = i;
  else
    (*current_liboctave_error_handler)
      ("Array<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

#endif

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
  // XXX FIXME XXX -- report index values too!

  (*current_liboctave_error_handler) ("range error in Array");

  return T ();
}

template <class T>
T&
Array<T>::range_error (const char *fcn, const Array<int>& ra_idx)
{
  // XXX FIXME XXX -- report index values too!

  (*current_liboctave_error_handler) ("range error in Array");

  static T foo;
  return foo;
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

  if (old_data && old_len > 0)
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
Array<T>::resize_no_fill (const dim_vector& dims)
{
  int n = dims.length ();

  for (int i = 0; i < n; i++)
    {
      if (dims(i) < 0)
	{
	  (*current_liboctave_error_handler)
	    ("can't resize to negative dimension");
	  return;
	}
    }

  bool no_change = true;

  for (int i = 0; i < n; i++)
    {
      if (dims(i) != dimensions(i))
	{
	  no_change = false;
	  break;
	}
    }

  if (no_change)
    return;

  int old_len = length ();

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  rep = new typename Array<T>::ArrayRep (get_size (dims));

  dim_vector old_dimensions = dimensions;

  dimensions = dims;

  Array<int> ra_idx (dimensions.length (), 0);

  for (int i = 0; i < old_len; i++)
    {
      if (index_in_bounds (ra_idx, dimensions))
	xelem (ra_idx) = old_data[i];

      increment_index (ra_idx, dimensions);
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

  if (r == dim1 () && c == dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = Array<T>::rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new typename Array<T>::ArrayRep (get_size (r, c));

  dimensions = dim_vector (r, c);

  if (old_data && old_len > 0)
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

  if (old_data && old_len > 0)
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

  int min_len = old_len < n ? old_len : n;

  if (old_data && old_len > 0)
    {
      for (int i = 0; i < min_len; i++)
	xelem (i) = old_data[i];
    }

  for (int i = old_len; i < n; i++)
    xelem (i) = val;

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

  if (r == dim1 () && c == dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = Array<T>::rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new typename Array<T>::ArrayRep (get_size (r, c));

  dimensions = dim_vector (r, c);

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

  int min_r = old_d1 < r ? old_d1 : r;
  int min_c = old_d2 < c ? old_d2 : c;
  int min_p = old_d3 < p ? old_d3 : p;

  if (old_data && old_len > 0)
    for (int k = 0; k < min_p; k++)
      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  xelem (i, j, k) = old_data[old_d1*(old_d2*k+j)+i];

  // XXX FIXME XXX -- if the copy constructor is expensive, this may
  // win.  Otherwise, it may make more sense to just copy the value
  // everywhere when making the new ArrayRep.

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

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array<T>::resize_and_fill (const dim_vector& dims, const T& val)
{
  int n = dims.length ();

  for (int i = 0; i < n; i++)
    {
      if (dims(i) < 0)
	{
	  (*current_liboctave_error_handler)
	    ("can't resize to negative dimension");
	  return;
	}
    }

  bool no_change = true;

  for (int i = 0; i < n; i++)
    {
      if (dims(i) != dimensions(i))
	{
	  no_change = false;
	  break;
	}
    }

  if (no_change)
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_len = length ();

  int len = get_size (dims);

  rep = new typename Array<T>::ArrayRep (len);

  dim_vector old_dimensions = dimensions;

  dimensions = dims;

  Array<int> ra_idx (dimensions.length (), 0);

  // XXX FIXME XXX -- it is much simpler to fill the whole array
  // first, but probably slower for large arrays, or if the assignment
  // operator for the type T is expensive.  OTOH, the logic for
  // deciding whether an element needs the copied value or the filled
  // value might be more expensive.

  for (int i = 0; i < len; i++)
    rep->elem (i) = val;

  for (int i = 0; i < old_len; i++)
    {
      if (index_in_bounds (ra_idx, dimensions))
	xelem (ra_idx) = old_data[i];

      increment_index (ra_idx, dimensions);
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
Array<T>&
Array<T>::insert (const Array<T>& a, int r, int c)
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
Array<T>::insert (const Array<T>& a, const Array<int>& ra_idx)
{
  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      dim_vector a_dims = a.dims ();

      for (int i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || ra_idx(i) + a_dims(i) > dimensions(i))
	    {
	      (*current_liboctave_error_handler)
		("Array<T>::insert: range error for insert");
	      return *this;
	    }
	}

#if 0
      // XXX FIXME XXX -- need to copy elements

      for (int j = 0; j < a_cols; j++)
	for (int i = 0; i < a_rows; i++)
	  elem (r+i, c+j) = a.elem (i, j);
#endif

    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::insert: invalid indexing operation");

  return *this;
}

template <class T>
void
Array<T>::maybe_delete_dims (void)
{
  int ndims = dimensions.length ();

  dim_vector new_dims (1, 1);

  bool delete_dims = true;

  for (int i = ndims - 1; i >= 0; i--)
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
    
  if (ndims != new_dims.length ())
    dimensions = new_dims;
}

template <class T>
Array<T>
Array<T>::transpose (void) const
{
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
Array<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "rep address: " << rep << "\n"
     << prefix << "rep->len:    " << rep->len << "\n"
     << prefix << "rep->data:   " << static_cast<void *> (rep->data) << "\n"
     << prefix << "rep->count:  " << rep->count << "\n";

  // 2D info:
  //
  //     << prefix << "rows: " << rows () << "\n"
  //     << prefix << "cols: " << cols () << "\n";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
