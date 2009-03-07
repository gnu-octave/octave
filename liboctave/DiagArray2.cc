// Template array classes
/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2003, 2004, 2005, 2007,
              2008, 2009 John W. Eaton

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

#include <algorithm>

#include "DiagArray2.h"

#include "lo-error.h"

template <class T>
const typename DiagArray2<T>::Proxy& 
DiagArray2<T>::Proxy::operator = (const T& val) const
{
  if (i == j)
    {
      if (object)
        object->set (val, i);
    }
  else
    (*current_liboctave_error_handler)
      ("invalid assignment to off-diagonal in diagonal array");

  return *this;
}

template <class T>
DiagArray2<T>::Proxy::operator T () const
{
  if (object && i == j)
    return object->get (i);
  else
    {
      static T foo;
      return foo;
    }
}

template <class T>
Array<T>
DiagArray2<T>::diag (octave_idx_type k) const
{
  Array<T> d;

  if (k == 0)
    // The main diagonal is shallow-copied.
    d = *this;
  else if (k > 0 && k < cols ())
    d = Array<T> (std::min (cols () - k, rows ()), T ());
  else if (k < 0 && -k < rows ())
    d = Array<T> (std::min (rows () + k, cols ()), T ());
  else
    (*current_liboctave_error_handler)
      ("diag: requested diagonal out of range");

  return d;
}

template <class T>
DiagArray2<T>
DiagArray2<T>::transpose (void) const
{
  DiagArray2<T> retval (*this);
  retval.d1 = d2;
  retval.d2 = d1;
  return retval;
}

template <class T>
DiagArray2<T>
DiagArray2<T>::hermitian (T (* fcn) (const T&)) const
{
  DiagArray2<T> retval (dim2 (), dim1 ());
  const T *p = this->data ();
  T *q = retval.fortran_vec ();
  for (octave_idx_type i = 0; i < this->length (); i++)
    q [i] = fcn (p [i]);
  return retval;
}

// A two-dimensional array with diagonal elements only.

template <class T>
T
DiagArray2<T>::checkelem (octave_idx_type r, octave_idx_type c) const
{
  if (r < 0 || c < 0 || r >= dim1 () || c >= dim2 ())
    {
      (*current_liboctave_error_handler) ("range error in DiagArray2");
      return T ();
    }
  return elem (r, c);
}

template <class T>
typename DiagArray2<T>::Proxy
DiagArray2<T>::checkelem (octave_idx_type r, octave_idx_type c) 
{
  if (r < 0 || c < 0 || r >= dim1 () || c >= dim2 ())
    {
      (*current_liboctave_error_handler) ("range error in DiagArray2");
      return Proxy (0, r, c);
    }
  else
    return Proxy (this, r, c);
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

  if (r != dim1 () || c != dim2 ())
    {
      Array<T>::resize (std::min (r, c));
      d1 = r; d2 = c;
    }
}

template <class T>
void
DiagArray2<T>::resize_fill (octave_idx_type r, octave_idx_type c, const T& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler) ("can't resize to negative dimensions");
      return;
    }

  if (r != dim1 () || c != dim2 ())
    {
      Array<T>::resize_fill (std::min (r, c), val);
      d1 = r; d2 = c;
    }
}

template <class T>
DiagArray2<T>::operator Array2<T> (void) const
{
  Array2<T> result (dim1 (), dim2 ());
  for (octave_idx_type i = 0, len = length (); i < len; i++)
    result.xelem (i, i) = dgelem (i);

  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
