// Template array classes                              -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#include "Array2.h"

#if defined (HEAVYWEIGHT_INDEXING)
#include "idx-vector.h"
#include "Array2-idx.h"
#endif

#include "lo-error.h"

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

  ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new ArrayRep (r*c);

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

  ArrayRep *old_rep = rep;
  const T *old_data = data ();
  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_len = length ();

  rep = new ArrayRep (r*c);

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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
