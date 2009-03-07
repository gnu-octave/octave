// Template array classes with like-type math ops
/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2007,
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

#if !defined (octave_MArray2_h)
#define octave_MArray2_h 1

#include "Array2.h"

// Two dimensional array with math ops.

// But first, some preprocessor abuse...

#include "MArray-decl.h"

MARRAY_OPS_FORWARD_DECLS (MArray2, )

template <class T>
class
MArray2 : public Array2<T>
{
protected:

  MArray2 (T *d, octave_idx_type n, octave_idx_type m) : Array2<T> (d, n, m) { }

public:

  MArray2 (void) : Array2<T> () { }

  MArray2 (octave_idx_type n, octave_idx_type m) : Array2<T> (n, m) { }

  MArray2 (octave_idx_type n, octave_idx_type m, const T& val) : Array2<T> (n, m, val) { }

  MArray2 (const dim_vector& dv) : Array2<T> (dv) { }

  MArray2 (const dim_vector& dv, const T& val) : Array2<T> (dv, val) { }

  MArray2 (const MArray2<T>& a) : Array2<T> (a) { }

  MArray2 (const Array2<T>& a) : Array2<T> (a) { }

  template <class U>
  MArray2 (const Array2<U>& a) : Array2<T> (a) { }

  template <class U>
  MArray2 (const MArray2<U>& a) : Array2<T> (a) { }

  ~MArray2 (void) { }

  MArray2<T>& operator = (const MArray2<T>& a)
    {
      Array2<T>::operator = (a);
      return *this;
    }

  MArray2<T>& insert (const Array2<T>& a, octave_idx_type r, octave_idx_type c)
  {
    Array2<T>::insert (a, r, c);
    return *this;
  }

  MArray2<T> transpose (void) const { return Array2<T>::transpose (); }
  MArray2<T> hermitian (T (*fcn) (const T&) = 0) const { return Array2<T>::hermitian (fcn); }

  MArray2<T> diag (octave_idx_type k) const
  {
    return Array2<T>::diag (k);
  }

  template <class U, class F>
  MArray2<U> map (F fcn) const
  {
    return Array2<T>::template map<U> (fcn);
  }

  // Currently, the OPS functions don't need to be friends, but that
  // may change.

  // MARRAY_OPS_FRIEND_DECLS (MArray2)
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
