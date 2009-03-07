// Template array classes
/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
              2005, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_Array2_h)
#define octave_Array2_h 1

#include <cassert>
#include <climits>
#include <cstdlib>

#include "Array.h"
#include "lo-error.h"
#include "lo-math.h"

class idx_vector;

// Two dimensional array class.

template <class T>
class
Array2 : public Array<T>
{
protected:

  static octave_idx_type get_size (octave_idx_type r, octave_idx_type c) { return Array<T>::get_size (r, c); }

  Array2 (T *d, octave_idx_type r, octave_idx_type c) : Array<T> (d, dim_vector (r, c)) { }

public:

  Array2 (void) : Array<T> (dim_vector (0, 0)) { }

  Array2 (octave_idx_type r, octave_idx_type c) : Array<T> (dim_vector (r, c)) { }

  Array2 (octave_idx_type r, octave_idx_type c, const T& val)
    : Array<T> (dim_vector (r, c), val) { }

  Array2 (const dim_vector& dv) : Array<T> (dv) 
    { 
      if (dv.length () != 2)
	(*current_liboctave_error_handler) ("too many dimensions");
    }

  Array2 (const dim_vector& dv, const T& val) : Array<T> (dv) 
    { 
      if (dv.length () != 2)
	(*current_liboctave_error_handler) ("too many dimensions");
      else
	Array<T>::fill (val); 
    }

  Array2 (const Array2<T>& a) : Array<T> (a, a.dims ()) { }

  Array2 (const Array<T>& a, octave_idx_type r, octave_idx_type c)
    : Array<T> (a, dim_vector (r, c)) { }

  template <class U>
  Array2 (const Array<U>& a) : Array<T> (a) { }

  template <class U>
  Array2 (const Array<U>& a, const dim_vector& dv)
    : Array<T> (a, dv) { }

  ~Array2 (void) { }

  Array2<T>& operator = (const Array2<T>& a)
    {
      if (this != &a)
	Array<T>::operator = (a);

      return *this;
    }

  void resize (octave_idx_type r, octave_idx_type c)
    { Array<T>::resize_fill (r, c, Array<T>::resize_fill_value ()); }

  void resize (octave_idx_type r, octave_idx_type c, const T& val)
    { Array<T>::resize_fill (r, c, val); }

  Array2<T>& insert (const Array2<T>& a, octave_idx_type r, octave_idx_type c)
    {
      Array<T>::insert (a, r, c);
      return *this;
    }

  Array2<T> transpose (void) const
    {
      Array<T> tmp = Array<T>::transpose ();
      return Array2<T> (tmp, tmp.rows (), tmp.columns ());
    }

  Array2<T> hermitian (T (*fcn) (const T&) = 0) const
    {
      Array<T> tmp = Array<T>::hermitian (fcn);
      return Array2<T> (tmp, tmp.rows (), tmp.columns ());
    }

  Array2<T> index (const idx_vector& i, bool resize_ok = false,
		   const T& rfv = Array<T>::resize_fill_value ()) const
    {
      Array<T> tmp = Array<T>::index (i, resize_ok, rfv);
      return Array2<T> (tmp, tmp.rows (), tmp.columns ());
    }

  Array2<T> index (const idx_vector& i, const idx_vector& j, bool resize_ok = false,
		   const T& rfv = Array<T>::resize_fill_value ()) const
    {
      Array<T> tmp = Array<T>::index (i, j, resize_ok, rfv);
      return Array2<T> (tmp, tmp.rows (), tmp.columns ());
    }

  Array2<T> sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
    {
      Array<T> tmp = Array<T>::sort (dim, mode);
      return Array2<T> (tmp, tmp.rows (), tmp.columns ());
    }

  Array2<T> sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
		 sortmode mode = ASCENDING) const
    {
      Array<T> tmp = Array<T>::sort (sidx, dim, mode);
      return Array2<T> (tmp, tmp.rows (), tmp.columns ());
    }

  Array2<T> diag (octave_idx_type k) const
  {
    return Array<T>::diag (k);
  }

  template <class U, class F>
  Array2<U> map (F fcn) const
  {
    return Array<T>::template map<U> (fcn);
  }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
