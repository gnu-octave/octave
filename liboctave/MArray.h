// Template array classes with like-type math ops          -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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
#pragma interface
#endif

#if !defined (octave_MArray_h)
#define octave_MArray_h 1

#include "Array.h"

// One dimensional array with math ops.

template <class T>
class MArray : public Array<T>
{
protected:

  MArray (T *d, int l) : Array<T> (d, l) { }

public:
  
  MArray (void) : Array<T> () { }
  MArray (int n) : Array<T> (n) { }
  MArray (int n, const T& val) : Array<T> (n, val) { }
//  MArray (const Array<T>& a) : Array<T> (a) { }
  MArray (const MArray<T>& a) : Array<T> (a) { }

  ~MArray (void) { }

  MArray<T>& operator = (const MArray<T>& a)
    {
      Array<T>::operator = (a);
      return *this;
    }

  // element by element MArray by scalar ops

  friend MArray<T>& operator += (MArray<T>& a, const T& s);
  friend MArray<T>& operator -= (MArray<T>& a, const T& s);

  // element by element MArray by MArray ops

  friend MArray<T>& operator += (MArray<T>& a, const MArray<T>& b);
  friend MArray<T>& operator -= (MArray<T>& a, const MArray<T>& b);

  // element by element MArray by scalar ops

  friend MArray<T> operator + (const MArray<T>& a, const T& s);
  friend MArray<T> operator - (const MArray<T>& a, const T& s);
  friend MArray<T> operator * (const MArray<T>& a, const T& s);
  friend MArray<T> operator / (const MArray<T>& a, const T& s);

  // element by element scalar by MArray ops

  friend MArray<T> operator + (const T& s, const MArray<T>& a);
  friend MArray<T> operator - (const T& s, const MArray<T>& a);
  friend MArray<T> operator * (const T& s, const MArray<T>& a);
  friend MArray<T> operator / (const T& s, const MArray<T>& a);

  // element by element MArray by MArray ops

  friend MArray<T> operator + (const MArray<T>& a, const MArray<T>& b);

  friend MArray<T> operator - (const MArray<T>& a, const MArray<T>& b);

  friend MArray<T> product (const MArray<T>& a, const MArray<T>& b);
  friend MArray<T> quotient (const MArray<T>& a, const MArray<T>& b);

  friend MArray<T> operator - (const MArray<T>& a);
};

#define INSTANTIATE_MARRAY_FRIENDS(T) \
  template MArray<T> operator + (const MArray<T>& a, const T& s); \
  template MArray<T> operator - (const MArray<T>& a, const T& s); \
  template MArray<T> operator * (const MArray<T>& a, const T& s); \
  template MArray<T> operator / (const MArray<T>& a, const T& s); \
  template MArray<T> operator + (const T& s, const MArray<T>& a); \
  template MArray<T> operator - (const T& s, const MArray<T>& a); \
  template MArray<T> operator * (const T& s, const MArray<T>& a); \
  template MArray<T> operator / (const T& s, const MArray<T>& a); \
  template MArray<T> operator + (const MArray<T>& a, const MArray<T>& b); \
  template MArray<T> operator - (const MArray<T>& a, const MArray<T>& b); \
  template MArray<T> product (const MArray<T>& a, const MArray<T>& b); \
  template MArray<T> quotient (const MArray<T>& a, const MArray<T>& b); \
  template MArray<T> operator - (const MArray<T>& a);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
