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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include <iostream>

#include "Array2.h"

#if defined (HEAVYWEIGHT_INDEXING)
#include "idx-vector.h"
#include "Array2-idx.h"
#endif

#include "lo-error.h"

// Two dimensional array class.

template <class T>
int
Array2<T>::get_size (int r, int c) const
{
  // XXX KLUGE XXX

  // If an allocation of an array with r * c elements of type T
  // would cause an overflow in the allocator when computing the
  // size of the allocation, then return a value which, although
  // not equivalent to the actual request, should be too large for
  // most current hardware, but not so large to cause the
  // allocator to barf on computing retval * sizeof (T).

  // A guess (should be quite conservative).
  static const int MALLOC_OVERHEAD = 1024;

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
T
Array2<T>::range_error (const char *fcn, int i, int j) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  return T ();
}

template <class T>
T&
Array2<T>::range_error (const char *fcn, int i, int j)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  static T foo;
  return foo;
}

template <class T>
void
Array2<T>::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new typename Array<T>::ArrayRep (get_size (r, c));

  d1 = r;
  d2 = c;

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
Array2<T>::resize (int r, int c, const T& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new typename Array<T>::ArrayRep (get_size (r, c));

  d1 = r;
  d2 = c;

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
Array2<T>&
Array2<T>::insert (const Array2<T>& a, int r, int c)
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
Array2<T>
Array2<T>::transpose (void) const
{
  if (d1 > 1 && d2 > 1)
    {
      Array2<T> result (d2, d1);

      for (int j = 0; j < d2; j++)
	for (int i = 0; i < d1; i++)
	  result.xelem (j, i) = xelem (i, j);

      return result;
    }
  else
    {
      // Fast transpose for vectors and empty matrices
      return Array2<T> (*this, d2, d1);
    }
}

template <class T>
void
Array2<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << "\n"
     << prefix << "rows: " << rows () << "\n"
     << prefix << "cols: " << cols () << "\n";

  Array<T>::print_info (os, prefix + "  ");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
