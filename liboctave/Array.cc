// Template array classes                              -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

#include <iostream.h>

#include "Array.h"

#if defined (HEAVYWEIGHT_INDEXING)
#include "idx-vector.h"
#include "Array-idx.h"
#endif

#include "lo-error.h"

// The real representation of all arrays.

template <class T>
ArrayRep<T>::ArrayRep (int n)
{
  len = n;
  data = new T [len];

#ifdef HEAVYWEIGHT_INDEXING
  idx = 0;
  max_indices = 0;
  idx_count = 0;
#endif
}

template <class T>
ArrayRep<T>::ArrayRep (const ArrayRep<T>& a)
{
  len = a.len;
  count = a.count;

  data = new T [len];
  for (int i = 0; i < len; i++)
    data[i] = a.data[i];

#ifdef HEAVYWEIGHT_INDEXING
  max_indices = a.max_indices;
  idx_count = a.idx_count;
  if (a.idx)
    {
      idx_vector *idx = new idx_vector [max_indices];
      for (int i = 0; i < max_indices; i++)
	idx[i] = a.idx[i];
    }
  else
    idx = 0;
#endif
}

template <class T>
ArrayRep<T>::~ArrayRep (void)
{
  delete [] data;
  delete [] idx;
}

template <class T>
T&
ArrayRep<T>::elem (int n)
{
  return data[n];
}

template <class T>
T
ArrayRep<T>::elem (int n) const
{
  return data[n];
}

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
Array<T>::Array (int n, const T& val)
{
  rep = new ArrayRep<T> (n);
  rep->count = 1;
  for (int i = 0; i < n; i++)
    rep->data[i] = val;
}

template <class T>
Array<T>&
Array<T>::operator = (const Array<T>& a)
{
  if (this != &a)
    {
      if (--rep->count <= 0)
	delete rep;

      rep = a.rep;
      rep->count++;
    }
  return *this;
}

template <class T>
T&
Array<T>::checkelem (int n)
{
  if (n < 0 || n >= rep->length ())
    {
      (*current_liboctave_error_handler) ("range error");
      static T foo;
      return foo;
    }
  return elem (n);
}

template <class T>
T
Array<T>::elem (int n) const
{
  return rep->elem (n);
}

template <class T>
T
Array<T>::checkelem (int n) const
{
  if (n < 0 || n >= rep->length ())
    {
      (*current_liboctave_error_handler) ("range error");
      T foo;
      static T *bar = &foo;
      return foo;
    }
  return elem (n);
}

template <class T>
T
Array<T>::operator () (int n) const
{
  return checkelem (n);
}

template <class T>
void
Array<T>::resize (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimension");
      return;
    }

  if (n == length ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new ArrayRep<T> (n);
  rep->count = 1;

  SET_MAX_INDICES (1);

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
Array<T>::resize (int n, const T& val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimension");
      return;
    }

  if (n == length ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new ArrayRep<T> (n);
  rep->count = 1;

  SET_MAX_INDICES (1);

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
T *
Array<T>::fortran_vec (void)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new ArrayRep<T> (*rep);
      rep->count = 1;
    }
  return rep->data;
}

// Two dimensional array class.

template <class T>
T&
Array2<T>::checkelem (int i, int j)
{
  if (i < 0 || j < 0 || i >= d1 || j >= d2)
    {
      (*current_liboctave_error_handler) ("range error");
      static T foo;
      return foo;
    }
  return Array<T>::elem (d1*j+i);
}

template <class T>
T
Array2<T>::elem (int i, int j) const
{
  return Array<T>::elem (d1*j+i);
}

template <class T>
T
Array2<T>::checkelem (int i, int j) const
{
  if (i < 0 || j < 0 || i >= d1 || j >= d2)
    {
      (*current_liboctave_error_handler) ("range error");
      T foo;
      static T *bar = &foo;
      return foo;
    }
  return Array<T>::elem (d1*j+i);
}

template <class T>
T
Array2<T>::operator () (int i, int j) const
{
  if (i < 0 || j < 0 || i >= d1 || j >= d2)
    {
      (*current_liboctave_error_handler) ("range error");
      T foo;
      static T *bar = &foo;
      return foo;
    }
  return Array<T>::elem (d1*j+i);
}

template <class T>
void
Array2<T>::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new ArrayRep<T> (r*c);
  rep->count = 1;

  SET_MAX_INDICES (2);

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
      (*current_liboctave_error_handler) ("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new ArrayRep<T> (r*c);
  rep->count = 1;

  SET_MAX_INDICES (2);

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

// Three dimensional array class.

template <class T>
T&
Array3<T>::checkelem (int i, int j, int k)
{
  if (i < 0 || j < 0 || k < 0 || i >= d1 || j >= d2 || k >= d3)
    {
      (*current_liboctave_error_handler) ("range error");
      static T foo;
      return foo;
    }
  return Array2<T>::elem (i, d1*k+j);
}

template <class T>
T
Array3<T>::elem (int i, int j, int k) const
{
  return Array2<T>::elem (i, d2*k+j);
}

template <class T>
T
Array3<T>::checkelem (int i, int j, int k) const
{
  if (i < 0 || j < 0 || k < 0 || i >= d1 || j >= d2 || k >= d3)
    {
      (*current_liboctave_error_handler) ("range error");
      T foo;
      static T *bar = &foo;
      return foo;
    }
  return Array2<T>::elem (i, d1*k+j);
}

template <class T>
T
Array3<T>::operator () (int i, int j, int k) const
{
  if (i < 0 || j < 0 || k < 0 || i >= d1 || j >= d2 || k >= d3)
    {
      (*current_liboctave_error_handler) ("range error");
      T foo;
      static T *bar = &foo;
      return foo;
    }
  return Array2<T>::elem (i, d2*k+j);
}

template <class T>
void
Array3<T>::resize (int n, int m, int k)
{
  assert (0); // XXX FIXME XXX
}

template <class T>
void
Array3<T>::resize (int n, int m, int k, const T& val)
{
  assert (0); // XXX FIXME XXX
}

// A two-dimensional array with diagonal elements only.

#if 1
template <class T>
T&
DiagArray<T>::elem (int r, int c)
{
  static T foo (0);
  return (r == c) ? Array<T>::elem (r) : foo;
}

template <class T>
T&
DiagArray<T>::checkelem (int r, int c)
{
  static T foo (0);
  if (r < 0 || c < 0 || r >= nr || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      return foo;
    }
  return (r == c) ? Array<T>::elem (r) : foo;
}

template <class T>
T&
DiagArray<T>::operator () (int r, int c)
{
  static T foo (0);
  if (r < 0 || c < 0 || r >= nr || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      return foo;
    }
  return (r == c) ? Array<T>::elem (r) : foo;
}
#endif

template <class T>
T&
DiagArray<T>::xelem (int r, int c)
{
  static T foo (0);
  return (r == c) ? Array<T>::xelem (r) : foo;
}

template <class T>
T
DiagArray<T>::elem (int r, int c) const
{
  return (r == c) ? Array<T>::elem (r) : T (0);
}

template <class T>
T
DiagArray<T>::checkelem (int r, int c) const
{
  if (r < 0 || c < 0 || r >= nr || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      T foo;
      static T *bar = &foo;
      return foo;
    }
  return (r == c) ? Array<T>::elem (r) : T (0);
}

template <class T>
T
DiagArray<T>::operator () (int r, int c) const
{
  if (r < 0 || c < 0 || r >= nr || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      T foo;
      static T *bar = &foo;
      return foo;
    }
  return (r == c) ? Array<T>::elem (r) : T (0);
}

template <class T>
void
DiagArray<T>::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimensions");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  int new_len = r < c ? r : c;

  rep = new ArrayRep<T> (new_len);
  rep->count = 1;

  SET_MAX_INDICES (2);

  nr = r;
  nc = c;

  if (old_data && old_len > 0)
    {
      int min_len = old_len < new_len ? old_len : new_len;

      for (int i = 0; i < min_len; i++)
	xelem (i, i) = old_data[i];
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
DiagArray<T>::resize (int r, int c, const T& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimensions");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  int new_len = r < c ? r : c;

  rep = new ArrayRep<T> (new_len);
  rep->count = 1;

  SET_MAX_INDICES (2);

  nr = r;
  nc = c;

  int min_len = old_len < new_len ? old_len : new_len;

  if (old_data && old_len > 0)
    {
      for (int i = 0; i < min_len; i++)
	xelem (i, i) = old_data[i];
    }

  for (int i = min_len; i < new_len; i++)
    xelem (i, i) = val;

  if (--old_rep->count <= 0)
    delete old_rep;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
