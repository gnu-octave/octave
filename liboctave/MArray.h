// Template array classes with like-type math ops
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

#if !defined (octave_MArray_h)
#define octave_MArray_h 1

#include "Array.h"

// One dimensional array with math ops.

// But first, some preprocessor abuse...

#include "MArray-defs.h"

MARRAY_OPS_FORWARD_DECLS (MArray)

template <class T>
class
MArray : public Array<T>
{
protected:

  MArray (T *d, int l) : Array<T> (d, l) { }

public:
  
  MArray (void) : Array<T> () { }

  explicit MArray (int n) : Array<T> (n) { }

  MArray (int n, const T& val) : Array<T> (n, val) { }

  MArray (const MArray<T>& a) : Array<T> (a) { }

  MArray (const Array<T>& a) : Array<T> (a) { }

  ~MArray (void) { }

  MArray<T>& operator = (const MArray<T>& a)
    {
      Array<T>::operator = (a);
      return *this;
    }

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
