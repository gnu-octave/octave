// Template array classes
/*

Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
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

#if !defined (octave_ArrayN_h)
#define octave_ArrayN_h 1

#include <cassert>
#include <climits>
#include <cstdlib>

#include <iosfwd>

#include "Array.h"
#include "Array2.h"
#include "lo-error.h"
#include "lo-math.h"

class idx_vector;

// N-dimensional array class.

template <class T>
class
ArrayN : public Array<T>
{
protected:

  ArrayN (T *d, const dim_vector& dv) : Array<T> (d, dv) { }

public:

  // These really need to be protected (and they will be in the
  // future, so don't depend on them being here!), but they can't be
  // until template friends work correctly in g++.

  ArrayN (void) : Array<T> () { }

  ArrayN (const dim_vector& dv) : Array<T> (dv) { }

  ArrayN (const dim_vector& dv, const T& val)
    : Array<T> (dv) { Array<T>::fill (val); }

  template <class U>
  explicit ArrayN (const Array2<U>& a) : Array<T> (a, a.dims ()) { }

  template <class U>
  ArrayN (const ArrayN<U>& a) : Array<T> (a, a.dims ()) { }

  template <class U>
  ArrayN (const Array<U>& a) : Array<T> (a) { }

  template <class U>
  ArrayN (const Array<U>& a, const dim_vector& dv)
    : Array<T> (a, dv) { }

  ~ArrayN (void) { }

  ArrayN<T>& operator = (const ArrayN<T>& a)
    {
      if (this != &a)
	Array<T>::operator = (a);

      return *this;
    }

  ArrayN<T> reshape (const dim_vector& new_dims) const
    { return Array<T>::reshape (new_dims); }

  ArrayN<T> permute (const Array<octave_idx_type>& vec, bool inv = false) const
    { return Array<T>::permute (vec, inv); }

  ArrayN<T> ipermute (const Array<octave_idx_type>& vec) const
    { return Array<T>::ipermute (vec); }

  ArrayN<T> squeeze (void) const { return Array<T>::squeeze (); }

  ArrayN<T> transpose (void) const { return Array<T>::transpose (); }
  ArrayN<T> hermitian (T (*fcn) (const T&) = 0) const { return Array<T>::hermitian (fcn); }

  ArrayN<T>& insert (const ArrayN<T>& a, const dim_vector& dv)
    {
      Array<T>::insert (a, dv);
      return *this;
    }

  ArrayN<T>& insert (const ArrayN<T>& a, octave_idx_type r, octave_idx_type c)
  {
    Array<T>::insert (a, r, c);
    return *this;
  }

  ArrayN<T> index (const idx_vector& i, bool resize_ok = false,
		   const T& rfv = Array<T>::resize_fill_value ()) const
    {
      Array<T> tmp = Array<T>::index (i, resize_ok, rfv);
      return ArrayN<T> (tmp, tmp.dims ());
    }

  ArrayN<T> index (const idx_vector& i, const idx_vector& j, bool resize_ok = false,
		   const T& rfv = Array<T>::resize_fill_value ()) const
    {
      Array<T> tmp = Array<T>::index (i, j, resize_ok, rfv);
      return ArrayN<T> (tmp, tmp.dims ());
    }

  ArrayN<T> index (const Array<idx_vector>& ra_idx, bool resize_ok = false,
		   const T& rfv = Array<T>::resize_fill_value ()) const
    {
      Array<T> tmp = Array<T>::index (ra_idx, resize_ok, rfv);
      return ArrayN<T> (tmp, tmp.dims ());
    }

  ArrayN<T> sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
    {
      Array<T> tmp = Array<T>::sort (dim, mode);
      return ArrayN<T> (tmp, tmp.dims ());
    }

  ArrayN<T> sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
		 sortmode mode = ASCENDING) const
    {
      Array<T> tmp = Array<T>::sort (sidx, dim, mode);
      return ArrayN<T> (tmp, tmp.dims ());
    }

  ArrayN<T> diag (octave_idx_type k) const
  {
    return Array<T>::diag (k);
  }

  template <class U, class F>
  ArrayN<U> map (F fcn) const
  {
    return Array<T>::template map<U> (fcn);
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
