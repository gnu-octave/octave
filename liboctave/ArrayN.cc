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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include <iostream>

#include "ArrayN.h"

#if defined (HEAVYWEIGHT_INDEXING)
#include "idx-vector.h"
#include "ArrayN-idx.h"
#endif

#include "lo-error.h"

// N-dimensional array class.

template <class T>
int
ArrayN<T>::compute_index (const Array<int>& ra_idx) const
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
      ("ArrayN<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

// A guess (should be quite conservative).
#define MALLOC_OVERHEAD 1024

template <class T>
int
ArrayN<T>::get_size (const Array<int>& ra_idx)
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
T
ArrayN<T>::range_error (const char *fcn, const Array<int>& ra_idx) const
{
  // XXX FIXME XXX -- report index values too!

  (*current_liboctave_error_handler) ("range error in ArrayN");

  return T ();
}

template <class T>
T&
ArrayN<T>::range_error (const char *fcn, const Array<int>& ra_idx)
{
  // XXX FIXME XXX -- report index values too!

  (*current_liboctave_error_handler) ("range error in ArrayN");

  static T foo;
  return foo;
}

static inline bool
index_in_bounds (const Array<int>& ra_idx, const Array<int>& dimensions)
{
  bool retval = true;

  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      for (int i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || ra_idx(i) >= dimensions (i))
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
increment_index (Array<int>& ra_idx, const Array<int>& dimensions)
{
  ra_idx(0)++;

  int n = ra_idx.length () - 1;

  for (int i = 0; i < n; i++)
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

template <class T>
void
ArrayN<T>::resize (const Array<int>& dims)
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

  rep = new typename Array<T>::ArrayRep (get_size (dims));

  Array<int> old_dimensions = dimensions;

  int old_len = length ();

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
ArrayN<T>::resize (const Array<int>& dims, const T& val)
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

  int len = get_size (dims);

  rep = new typename Array<T>::ArrayRep (len);

  Array<int> old_dimensions = dimensions;

  int old_len = length ();

  dimensions = dims;

  Array<int> ra_idx (dimensions.length (), 0);

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
ArrayN<T>&
ArrayN<T>::insert (const ArrayN<T>& a, const Array<int>& ra_idx)
{
  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      Array<int> a_dims = a.dims ();

      for (int i = 0; i < n; i++)
	{
	  if (ra_idx(i) < 0 || ra_idx(i) + a_dims(i) > dimensions(i))
	    {
	      (*current_liboctave_error_handler)
		("ArrayN<T>::insert: range error for insert");
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
      ("ArrayN<T>::insert: invalid indexing operation");

  return *this;
}

template <class T>
std::ostream&
operator << (std::ostream& os, const ArrayN<T>& a)
{
  Array<int> dims = a.dimensions;

  int n_dims = dims.length ();

  os << n_dims << "-dimensional array (";

  for (int i = 0; i < n_dims - 1; i++)
    os << dims(i) << "x";
  os << dims(n_dims-1) << ")\n\n";

  os << "data:\n";

  int n = ArrayN<T>::get_size (dims);

  //  for (int i = 0; i < n; i++)
  //    os << a.elem (i) << "\n";

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
