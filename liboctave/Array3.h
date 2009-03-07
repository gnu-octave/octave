// Template array classes
/*

Copyright (C) 1996, 1997, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008
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

#if !defined (octave_Array3_h)
#define octave_Array3_h 1

#include <cassert>
#include <cstdlib>

#include "Array.h"
#include "lo-error.h"

class idx_vector;

// Three dimensional array class.

template <class T>
class
Array3 : public Array<T>
{
protected:

  static octave_idx_type get_size (octave_idx_type r, octave_idx_type c, octave_idx_type p)
    { return Array<T>::get_size (r, c, p); }

  Array3 (T *d, octave_idx_type r, octave_idx_type c, octave_idx_type p) : Array<T> (d, dim_vector (r, c, p)) { }

public:

  Array3 (void) : Array<T> (dim_vector (0, 0, 0)) { }

  Array3 (octave_idx_type r, octave_idx_type c, octave_idx_type p) : Array<T> (dim_vector (r, c, p)) { }

  Array3 (octave_idx_type r, octave_idx_type c, octave_idx_type p, const T& val)
    : Array<T> (dim_vector (r, c, p), val) { }

  Array3 (const Array3<T>& a)
    : Array<T> (a, a.dims ()) { }

  Array3 (const Array<T>& a, octave_idx_type r, octave_idx_type c, octave_idx_type p)
    : Array<T> (a, dim_vector (r, c, p)) { }

  ~Array3 (void) { }

  Array3<T>& operator = (const Array3<T>& a)
    {
      if (this != &a)
	Array<T>::operator = (a);

      return *this;
    }

  void resize (octave_idx_type r, octave_idx_type c, octave_idx_type p) 
    { Array<T>::resize (dim_vector (r, c, p)); }

  void resize (octave_idx_type r, octave_idx_type c, octave_idx_type p, const T& val)
    { Array<T>::resize_fill (dim_vector (r, c, p), val); }

  Array3<T> sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
    {
      Array<T> tmp = Array<T>::sort (dim, mode);
      return Array3<T> (tmp, tmp.rows (), tmp.columns (), tmp.pages ());
    }

  Array3<T> sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
		 sortmode mode = ASCENDING) const
    {
      Array<T> tmp = Array<T>::sort (sidx, dim, mode);
      return Array3<T> (tmp, tmp.rows (), tmp.columns (), tmp.pages ());
    }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
