// Template array classes                              -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>

#if defined (__GNUG__) && defined (USE_EXTERNAL_TEMPLATES)
#pragma implementation
#endif

#include "Array.h"

/*
 * The real representation of all arrays.
 */

template <class T>
ArrayRep<T>::ArrayRep (T *d, int l)
{
  data = d;
  len = l;
}

template <class T>
ArrayRep<T>::ArrayRep (void)
{
  len = 0;
  data = (T *) 0;
}

template <class T>
ArrayRep<T>::ArrayRep (int n)
{
  len = n;
  data = new T [len];
}

template <class T>
ArrayRep<T>::ArrayRep (const ArrayRep<T>& a)
{
  len = a.len;
  count = a.count;
  data = new T [len];
  for (int i = 0; i < len; i++)
    data[i] = a.data[i];
}

template <class T>
ArrayRep<T>::~ArrayRep (void)
{
  delete [] data;
  data = (T *) 0;
}

template <class T>
int
ArrayRep<T>::length (void) const
{
  return len;
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

/*
 * One dimensional array class.  Handles the reference counting for
 * all the derived classes.
 */

template <class T>
Array<T>::Array (T *d, int l)
{
  rep = new ArrayRep<T> (d, l);
  rep->count = 1;
}

template <class T>
Array<T>::Array (void)
{
  rep = new ArrayRep<T>;
  rep->count = 1;
}

template <class T>
Array<T>::Array (int n)
{
  rep = new ArrayRep<T> (n);
  rep->count = 1;
}

template <class T>
Array<T>::Array (int n, const T& val)
{
  rep = new ArrayRep<T> (n);
  rep->count = 1;
  for (int i = 0; i < n; i++)
    rep->data[i] = val;
}

template <class T>
Array<T>::Array (const Array<T>& a)
{
  rep = a.rep;
  rep->count++;
}

template <class T>
Array<T>::~Array (void)
{
  if (--rep->count <= 0)
    delete rep;
}

template <class T>
Array<T>&
Array<T>::operator = (const Array<T>& a)
{
  if (--rep->count <= 0)
    delete rep;

  rep = a.rep;
  rep->count++;
  return *this;
}

template <class T>
int
Array<T>::capacity (void) const
{
  return rep->length ();
}

template <class T>
int
Array<T>::length (void) const
{
  return rep->length ();
}

template <class T>
T&
Array<T>::elem (int n)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new ArrayRep<T> (*rep);
      rep->count = 1;
    }
  return rep->elem (n);
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
T&
Array<T>::operator () (int n)
{
  return checkelem (n);
}

template <class T>
T&
Array<T>::xelem (int n)
{
  return rep->elem (n);
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
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (n == length ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new ArrayRep<T> (n);
  rep->count = 1;

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
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (n == length ())
    return;

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new ArrayRep<T> (n);
  rep->count = 1;

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
const T *
Array<T>::data (void) const
{
  return rep->data;
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

/*
 * Two dimensional array class.
 */

template <class T>
Array2<T>::Array2 (T *d, int n, int m) : Array<T> (d, n*m)
{
  d1 = n;
  d2 = m;
}

template <class T>
Array2<T>::Array2 (void) : Array<T> ()
{
  d1 = 0;
  d2 = 0;
}

template <class T>
Array2<T>::Array2 (int n, int m) : Array<T> (n*m)
{
  d1 = n;
  d2 = m;
}

template <class T>
Array2<T>::Array2 (int n, int m, const T& val) : Array<T> (n*m, val)
{
  d1 = n;
  d2 = m;
}

template <class T>
Array2<T>::Array2 (const Array2<T>& a) : Array<T> (a)
{
  d1 = a.d1;
  d2 = a.d2;
}

template <class T>
Array2<T>::Array2 (const DiagArray<T>& a)
  : Array<T> (a.rows () * a.cols (), T (0))
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

template <class T>
Array2<T>&
Array2<T>::operator = (const Array2<T>& a)
{
  Array<T>::operator = (a);
  d1 = a.d1;
  d2 = a.d2;
  return *this;
}

template <class T>
int
Array2<T>::dim1 (void) const
{
  return d1;
}

template <class T>
int
Array2<T>::dim2 (void) const
{
  return d2;
}

template <class T>
int
Array2<T>::rows (void) const
{
  return d1;
}

template <class T>
int
Array2<T>::cols (void) const
{
  return d2;
}

template <class T>
int
Array2<T>::columns (void) const
{
  return d2;
}

template <class T>
T&
Array2<T>::elem (int i, int j)
{
  return Array<T>::elem (d1*j+i);
}

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
T&
Array2<T>::operator () (int i, int j)
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
T&
Array2<T>::xelem (int i, int j)
{
  return Array<T>::xelem (d1*j+i);
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
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
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

  ArrayRep<T> *old_rep = rep;
  const T *old_data = data ();
  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new ArrayRep<T> (r*c);
  rep->count = 1;

  d1 = r;
  d2 = c;

  int min_r = old_d1 < r ? old_d1 : r;
  int min_c = old_d2 < c ? old_d2 : c;

  int i, j;

  if (old_data && old_len > 0)
    {
      for (j = 0; j < min_c; j++)
	for (i = 0; i < min_r; i++)
	  xelem (i, j) = old_data[old_d1*j+i];
    }

  for (j = 0; j < min_c; j++)
    for (i = min_r; i < r; i++)
      xelem (i, j) = val;

  for (j = min_c; j < c; j++)
    for (i = 0; i < r; i++)
      xelem (i, j) = val;

  if (--old_rep->count <= 0)
    delete old_rep;
}

/*
 * Three dimensional array class.
 */

template <class T>
Array3<T>::Array3 (T *d, int n, int m, int k) : Array2<T> (d, n, m*k)
{
  d2 = m;
  d3 = k;
}

template <class T>
Array3<T>::Array3 (void) : Array2<T> ()
{
  d2 = 0;
  d3 = 0;
}

template <class T>
Array3<T>::Array3 (int n, int m, int k) : Array2<T> (n, m*k)
{
  d2 = m;
  d3 = k;
}

template <class T>
Array3<T>::Array3 (int n, int m, int k, const T& val) : Array2<T> (n, m*k, val)
{
  d2 = m;
  d3 = k;
}

template <class T>
Array3<T>::Array3 (const Array3<T>& a) : Array2<T> (a)
{
  d2 = a.d2;
  d3 = a.d3;
}

template <class T>
Array3<T>&
Array3<T>::operator = (const Array3<T>& a)
{
  Array<T>::operator = (a);
  d1 = a.d1;
  d2 = a.d2;
  d3 = a.d3;
  return *this;
}

template <class T>
int
Array3<T>::dim3 (void) const
{
  return d3;
}

template <class T>
T&
Array3<T>::elem (int i, int j, int k)
{
  return Array2<T>::elem (i, d2*k+j);
}

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
T&
Array3<T>::operator () (int i, int j, int k)
{
  if (i < 0 || j < 0 || k < 0 || i >= d1 || j >= d2 || k >= d3)
    {
      (*current_liboctave_error_handler) ("range error");
      static T foo;
      return foo;
    }
  return Array2<T>::elem (i, d2*k+j);
}

template <class T>
T&
Array3<T>::xelem (int i, int j, int k)
{
  return Array2<T>::xelem (i, d2*k+j);
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
      return foo;
    }
  return Array2<T>::elem (i, d2*k+j);
}

template <class T>
void
Array3<T>::resize (int n, int m, int k)
{
  assert (0); /* XXX FIXME XXX */
}

template <class T>
void
Array3<T>::resize (int n, int m, int k, const T& val)
{
  assert (0); /* XXX FIXME XXX */
}

/*
 * A two-dimensional array with diagonal elements only.
 */

template <class T>
DiagArray<T>::DiagArray (T *d, int r, int c) : Array<T> (d, r < c ? r : c)
{
  nr = r;
  nc = c;
}

template <class T>
DiagArray<T>::DiagArray (void) : Array<T> ()
{
  nr = 0;
  nc = 0;
}

template <class T>
DiagArray<T>::DiagArray (int n) : Array<T> (n)
{
  nr = n;
  nc = n;
}

template <class T>
DiagArray<T>::DiagArray (int n, const T& val) : Array<T> (n, val)
{
  nr = nc = n;
}

template <class T>
DiagArray<T>::DiagArray (int r, int c) : Array<T> (r < c ? r : c)
{
  nr = r;
  nc = c;
}

template <class T>
DiagArray<T>::DiagArray (int r, int c, const T& val)
  : Array<T> (r < c ? r : c, val)
{
  nr = r;
  nc = c;
}

template <class T>
DiagArray<T>::DiagArray (const Array<T>& a) : Array<T> (a)
{
  nr = nc = a.length ();
}

template <class T>
DiagArray<T>::DiagArray (const DiagArray<T>& a) : Array<T> (a)
{
  nr = a.nr;
  nc = a.nc;
}

template <class T>
DiagArray<T>&
DiagArray<T>::operator = (const DiagArray<T>& a)
{
  Array<T>::operator = (a);
  nr = a.nr;
  nc = a.nc;
  return *this;
}

template <class T>
int
DiagArray<T>::dim1 (void) const
{
  return nr;
}

template <class T>
int
DiagArray<T>::dim2 (void) const
{
  return nc;
}

template <class T>
int
DiagArray<T>::rows (void) const
{
  return nr;
}

template <class T>
int
DiagArray<T>::cols (void) const
{
  return nc;
}

template <class T>
int
DiagArray<T>::columns (void) const
{
  return nc;
}

#if defined (_AIX)
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
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
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
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
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

#if defined (__GNUG__) && defined (USE_EXTERNAL_TEMPLATES)
#if defined (OCTAVE_SOURCE)

typedef Array<double> array_type_double;
typedef Array2<double> array2_type_double;
typedef DiagArray<double> diag_array_type_double;

#include <Complex.h>
typedef Array<Complex> array_type_complex;
typedef Array2<Complex> array2_type_complex;
typedef DiagArray<Complex> diag_array_type_complex;

#elif defined (USER_TYPEDEFS)

// Users can generate their own .o files with their own types, as many
// times as they like.  USER_TYPEDEFS should be defined to be the name
// of an include file that contains typdefs for the desired types.
//
// For example, if my-types.h contains typedefs for the Array types
// you are interested in, you might compile this file with the command
//
//   g++ -fexternal-templates -DUSER_EXTERNAL_TEMPLATES \
//       -DUSER_TYPEDEFS=\"my-types.h\"

#include USER_TYPEDEFS

#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
