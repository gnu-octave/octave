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

#if !defined (octave_ArrayN_h)
#define octave_ArrayN_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <iostream>

#include <cassert>
#include <climits>
#include <cmath>
#include <cstdlib>

#include "Array.h"
#include "Array2.h"
#include "lo-error.h"

class idx_vector;

// N-dimensional array class.

template <class T>
class
ArrayN : public Array<T>
{
protected:

  static int get_size (const dim_vector& dims)
    { return Array<T>::get_size (dims); }

  ArrayN (T *d, const dim_vector& dims) : Array<T> (d, dims) { }

public:

  // These really need to be protected (and they will be in the
  // future, so don't depend on them being here!), but they can't be
  // until template friends work correctly in g++.

  ArrayN (void) : Array<T> () { }

  ArrayN (const dim_vector& dims) : Array<T> (dims) { }

  ArrayN (const dim_vector& dims, const T& val)
    : Array<T> (dims) { fill (val); }

  ArrayN (const Array2<T>& a) : Array<T> (a, a.dims ()) { }

  ArrayN (const ArrayN<T>& a) : Array<T> (a, a.dims ()) { }

  ArrayN (const Array<T>& a) : Array<T> (a) { }

  ArrayN (const Array<T>& a, const dim_vector& dims) : Array<T> (a, dims) { }

  ~ArrayN (void) { }

  ArrayN<T>& operator = (const ArrayN<T>& a)
    {
      if (this != &a)
	Array<T>::operator = (a);

      return *this;
    }

  void resize (const dim_vector& dims)
    { Array<T>resize_no_fill (dims); }

  void resize (const dim_vector& dims, const T& val)
    { Array<T>::resize (dims, val); }

  ArrayN<T> squeeze (void) const { return Array<T>::squeeze (); }

  ArrayN<T>& insert (const ArrayN<T>& a, const dim_vector& dims)
    {
      Array<T>::insert (a, dims);
      return *this;
    }

  ArrayN<T> index (idx_vector& i, int resize_ok = 0,
		   const T& rfv = resize_fill_value (T ())) const
    {
      Array<T> tmp = Array<T>::index (i, resize_ok, rfv);
      return ArrayN<T> (tmp, tmp.dims ());
    }

  ArrayN<T> index (idx_vector& i, idx_vector& j, int resize_ok = 0,
		   const T& rfv = resize_fill_value (T ())) const
    {
      Array<T> tmp = Array<T>::index (i, j, resize_ok, rfv);
      return ArrayN<T> (tmp, tmp.dims ());
    }

  ArrayN<T> index (Array<idx_vector>& ra_idx, int resize_ok = 0,
		   const T& rfv = resize_fill_value (T ())) const
    {
      Array<T> tmp = Array<T>::index (ra_idx, resize_ok, rfv);
      return ArrayN<T> (tmp, tmp.dims ());
    }
};

template <class T>
std::ostream&
operator << (std::ostream&, const ArrayN<T>&);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
