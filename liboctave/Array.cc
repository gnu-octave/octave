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

#include "Array.h"

#if defined (HEAVYWEIGHT_INDEXING)
#include "idx-vector.h"
#include "Array-idx.h"
#endif

#include "lo-error.h"

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
Array<T>::Array (int n, const T& val)
{
  rep = new ArrayRep (n);

  for (int i = 0; i < n; i++)
    rep->data[i] = val;

#ifdef HEAVYWEIGHT_INDEXING
  max_indices = 1;
  idx_count = 0;
  idx = 0;
#endif
}

template <class T>
Array<T>::~Array (void)
{
  if (--rep->count <= 0)
    delete rep;

#ifdef HEAVYWEIGHT_INDEXING
  delete [] idx;
#endif
}

template <class T>
Array<T>&
Array<T>::operator = (const Array<T>& a)
{
  if (this != &a && rep != a.rep)
    {
      if (--rep->count <= 0)
	delete rep;

      rep = a.rep;
      rep->count++;
    }

#ifdef HEAVYWEIGHT_INDEXING
  max_indices = 1;
  idx_count = 0;
  idx = 0;
#endif

  return *this;
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

  ArrayRep *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new ArrayRep (n);

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

  ArrayRep *old_rep = rep;
  const T *old_data = data ();
  int old_len = length ();

  rep = new ArrayRep (n);

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
      rep = new ArrayRep (*rep);
    }
  return rep->data;
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
