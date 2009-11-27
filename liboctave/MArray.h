// Template array classes with like-type math ops
/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2002, 2003, 2004,
              2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_MArray_h)
#define octave_MArray_h 1

#include "Array.h"

// One dimensional array with math ops.

// But first, some preprocessor abuse...

#include "MArray-decl.h"

MARRAY_OPS_FORWARD_DECLS (MArray, )

template <class T>
class
MArray : public Array<T>
{
protected:

  MArray (T *d, octave_idx_type l) : Array<T> (d, l) { }

public:
  
  MArray (void) : Array<T> () { }

  explicit MArray (octave_idx_type n) : Array<T> (n) { }

  MArray (octave_idx_type n, const T& val) : Array<T> (n, val) { }

  MArray (const MArray<T>& a) : Array<T> (a) { }

  // FIXME: kluge.
  MArray (const Array<T>& a) : Array<T> (a, dim_vector (a.length ())) { }

  MArray (const dim_vector& dv) : Array<T> (dv) { }

  ~MArray (void) { }

  MArray<T>& operator = (const MArray<T>& a)
    {
      Array<T>::operator = (a);
      return *this;
    }

  // FIXME: kluge again. This design really sucks.
  void resize (octave_idx_type n, const T& val = Array<T>::resize_fill_value ())
    { Array<T>::resize_fill (n, 1, val); }

  MArray<T> transpose (void) const { return Array<T>::transpose (); }
  MArray<T> hermitian (T (*fcn) (const T&) = 0) const { return Array<T>::hermitian (fcn); }

  double norm (double p) const;
  float norm (float p) const;

  // FIXME: should go away.
  template <class U>
  MArray<U>
  map (U (&fcn) (T)) const
  { return Array<T>::template map<U> (fcn); }

  template <class U>
  MArray<U>
  map (U (&fcn) (const T&)) const
  { return Array<T>::template map<U> (fcn); }

  // Currently, the OPS functions don't need to be friends, but that
  // may change.

  // MARRAY_OPS_FRIEND_DECLS (MArray)
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
