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

#if defined (__GNUG__)
#pragma interface
#endif

#if !defined (octave_MArray_h)
#define octave_MArray_h 1

#include "Array.h"

#if defined (LTGT)
#undef LTGT
#endif

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
#define LTGT
#else

#define LTGT <>

template <class T> 
class MArray;

template <typename T> MArray<T>& 
operator += (MArray<T>& a, const T& s);

template <typename T> MArray<T>& 
operator -= (MArray<T>& a, const T& s);

template <typename T> MArray<T>& 
operator += (MArray<T>& a, const MArray<T>& b);

template <typename T> MArray<T>& 
operator -= (MArray<T>& a, const MArray<T>& b);

template <typename T> MArray<T> 
operator + (const MArray<T>& a, const T& s);

template <typename T> MArray<T> 
operator - (const MArray<T>& a, const T& s);

template <typename T> MArray<T> 
operator * (const MArray<T>& a, const T& s);

template <typename T> MArray<T> 
operator / (const MArray<T>& a, const T& s);

template <typename T> MArray<T> 
operator + (const T& s, const MArray<T>& a);

template <typename T> MArray<T> 
operator - (const T& s, const MArray<T>& a);

template <typename T> MArray<T> 
operator * (const T& s, const MArray<T>& a);

template <typename T> MArray<T> 
operator / (const T& s, const MArray<T>& a);

template <typename T> MArray<T> 
operator + (const MArray<T>& a, const MArray<T>& b);

template <typename T> MArray<T> 
operator - (const MArray<T>& a, const MArray<T>& b);

template <class T> MArray<T> 
product (const MArray<T>& a, const MArray<T>& b);

template <typename T> MArray<T> 
quotient (const MArray<T>& a, const MArray<T>& b);

template <typename T> MArray<T> 
operator - (const MArray<T>& a);
#endif

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
  MArray (const Array<T>& a) : Array<T> (a) { }
  MArray (const MArray<T>& a) : Array<T> (a) { }

  ~MArray (void) { }

  MArray<T>& operator = (const MArray<T>& a)
    {
      Array<T>::operator = (a);
      return *this;
    }
};

#undef LTGT

extern void
gripe_nonconformant (const char *op, int op1_len, int op2_len);

#define INSTANTIATE_MARRAY_FRIENDS(T) \
  template MArray<T>& operator += (MArray<T>& a, const T& s); \
  template MArray<T>& operator -= (MArray<T>& a, const T& s); \
  template MArray<T>& operator += (MArray<T>& a, const MArray<T>& b); \
  template MArray<T>& operator -= (MArray<T>& a, const MArray<T>& b); \
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
;;; End: ***
*/
