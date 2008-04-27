// Template array classes
/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2003, 2004, 2005, 2007
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include <iostream>

#include "DiagArray2.h"

#include "lo-error.h"

template <class T>
DiagArray2<T>
DiagArray2<T>::transpose (void) const
{
  DiagArray2<T> retval (*this);
  retval.dimensions = dim_vector (this->dim2 (), this->dim1 ());
  return retval;
}

template <class T>
DiagArray2<T>
DiagArray2<T>::hermitian (T (* fcn) (const T&)) const
{
  DiagArray2<T> retval (this->dim2 (), this->dim1 ());
  const T *p = this->data ();
  T *q = retval.fortran_vec ();
  for (octave_idx_type i = 0; i < this->length (); i++)
    q [i] = fcn (p [i]);
  return retval;
}

// A two-dimensional array with diagonal elements only.

template <class T>
T
DiagArray2<T>::elem (octave_idx_type r, octave_idx_type c) const
{
  return (r == c) ? Array<T>::xelem (r) : T (0);
}

template <class T>
T
DiagArray2<T>::checkelem (octave_idx_type r, octave_idx_type c) const
{
  if (r < 0 || c < 0 || r >= this->dim1 () || c >= this->dim2 ())
    {
      (*current_liboctave_error_handler) ("range error in DiagArray2");
      return T ();
    }
  return (r == c) ? Array<T>::xelem (r) : T (0);
}

template <class T>
T
DiagArray2<T>::operator () (octave_idx_type r, octave_idx_type c) const
{
  if (r < 0 || c < 0 || r >= this->dim1 () || c >= this->dim2 ())
    {
      (*current_liboctave_error_handler) ("range error in DiagArray2");
      return T ();
    }
  return (r == c) ? Array<T>::xelem (r) : T (0);
}

template <class T>
T&
DiagArray2<T>::xelem (octave_idx_type r, octave_idx_type c)
{
  static T foo (0);
  return (r == c) ? Array<T>::xelem (r) : foo;
}

template <class T>
T
DiagArray2<T>::xelem (octave_idx_type r, octave_idx_type c) const
{
  return (r == c) ? Array<T>::xelem (r) : T (0);
}

template <class T>
void
DiagArray2<T>::resize (octave_idx_type r, octave_idx_type c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimensions");
      return;
    }

  if (r == this->dim1 () && c == this->dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = Array<T>::rep;
  const T *old_data = this->data ();
  octave_idx_type old_len = this->length ();

  octave_idx_type new_len = r < c ? r : c;

  Array<T>::rep = new typename Array<T>::ArrayRep (new_len);

  this->dimensions = dim_vector (r, c);

  if (old_data && old_len > 0)
    {
      octave_idx_type min_len = old_len < new_len ? old_len : new_len;

      for (octave_idx_type i = 0; i < min_len; i++)
	xelem (i, i) = old_data[i];
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
DiagArray2<T>::resize (octave_idx_type r, octave_idx_type c, const T& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimensions");
      return;
    }

  if (r == this->dim1 () && c == this->dim2 ())
    return;

  typename Array<T>::ArrayRep *old_rep = Array<T>::rep;
  const T *old_data = this->data ();
  octave_idx_type old_len = this->length ();

  octave_idx_type new_len = r < c ? r : c;

  Array<T>::rep = new typename Array<T>::ArrayRep (new_len);

  this->dimensions = dim_vector (r, c);

  octave_idx_type min_len = old_len < new_len ? old_len : new_len;

  if (old_data && old_len > 0)
    {
      for (octave_idx_type i = 0; i < min_len; i++)
	xelem (i, i) = old_data[i];
    }

  for (octave_idx_type i = min_len; i < new_len; i++)
    xelem (i, i) = val;

  if (--old_rep->count <= 0)
    delete old_rep;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
