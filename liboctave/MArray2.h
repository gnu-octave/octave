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

#if !defined (octave_MArray2_h)
#define octave_MArray2_h 1

#include "Array2.h"

#if defined (LTGT)
#undef LTGT
#endif

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
#define LTGT
#else

#define LTGT <>

template <class T>
class MArray2;

template <typename T> MArray2<T>& 
operator += (MArray2<T>& a, const T& s);

template <typename T> MArray2<T>& 
operator -= (MArray2<T>& a, const T& s);

template <typename T> MArray2<T>& 
operator += (MArray2<T>& a, const MArray2<T>& b);

template <typename T> MArray2<T>& 
operator -= (MArray2<T>& a, const MArray2<T>& b);

template <typename T> MArray2<T> 
operator + (const MArray2<T>& a, const T& s);

template <typename T> MArray2<T> 
operator - (const MArray2<T>& a, const T& s);

template <typename T> MArray2<T> 
operator * (const MArray2<T>& a, const T& s);

template <typename T> MArray2<T> 
operator / (const MArray2<T>& a, const T& s);

template <typename T> MArray2<T> 
operator + (const T& s, const MArray2<T>& a);

template <typename T> MArray2<T> 
operator - (const T& s, const MArray2<T>& a);

template <typename T> MArray2<T> 
operator * (const T& s, const MArray2<T>& a);

template <typename T> MArray2<T> 
operator / (const T& s, const MArray2<T>& a);

template <typename T> MArray2<T> 
operator + (const MArray2<T>& a, const MArray2<T>& b);

template <typename T> MArray2<T> 
operator - (const MArray2<T>& a, const MArray2<T>& b);

template <typename T> MArray2<T> 
product (const MArray2<T>& a, const MArray2<T>& b);

template <typename T> MArray2<T> 
quotient (const MArray2<T>& a, const MArray2<T>& b);

template <typename T> MArray2<T> 
operator - (const MArray2<T>& a);
#endif

// Two dimensional array with math ops.

template <class T>
class MArray2 : public Array2<T>
{
protected:

  MArray2 (T *d, int n, int m) : Array2<T> (d, n, m) { }

public:

  MArray2 (void) : Array2<T> () { }
  MArray2 (int n, int m) : Array2<T> (n, m) { }
  MArray2 (int n, int m, const T& val) : Array2<T> (n, m, val) { }
  MArray2 (const Array2<T>& a) : Array2<T> (a) { }
  MArray2 (const MArray2<T>& a) : Array2<T> (a) { }

  ~MArray2 (void) { }

  MArray2<T>& operator = (const MArray2<T>& a)
    {
      Array2<T>::operator = (a);
      return *this;
    }

  // element by element MArray2 by scalar ops

  friend MArray2<T>& operator += LTGT (MArray2<T>& a, const T& s);
  friend MArray2<T>& operator -= LTGT (MArray2<T>& a, const T& s);

  // element by element MArray2 by MArray2 ops

  friend MArray2<T>& operator += LTGT (MArray2<T>& a, const MArray2<T>& b);
  friend MArray2<T>& operator -= LTGT (MArray2<T>& a, const MArray2<T>& b);

  // element by element MArray2 by scalar ops

  friend MArray2<T> operator + LTGT (const MArray2<T>& a, const T& s);
  friend MArray2<T> operator - LTGT (const MArray2<T>& a, const T& s);
  friend MArray2<T> operator * LTGT (const MArray2<T>& a, const T& s);
  friend MArray2<T> operator / LTGT (const MArray2<T>& a, const T& s);

  // element by element scalar by MArray2 ops

  friend MArray2<T> operator + LTGT (const T& s, const MArray2<T>& a);
  friend MArray2<T> operator - LTGT (const T& s, const MArray2<T>& a);
  friend MArray2<T> operator * LTGT (const T& s, const MArray2<T>& a);
  friend MArray2<T> operator / LTGT (const T& s, const MArray2<T>& a);

  // element by element MArray2 by MArray2 ops

  friend MArray2<T> operator + LTGT (const MArray2<T>& a, const MArray2<T>& b);
  friend MArray2<T> operator - LTGT (const MArray2<T>& a, const MArray2<T>& b);

  friend MArray2<T> product LTGT (const MArray2<T>& a, const MArray2<T>& b);
  friend MArray2<T> quotient LTGT (const MArray2<T>& a, const MArray2<T>& b);

  friend MArray2<T> operator - LTGT (const MArray2<T>& a);
};

#undef LTGT

extern void
gripe_nonconformant (const char *op, int op1_nr, int op1_nc,
		     int op2_nr, int op2_nc);

#define INSTANTIATE_MARRAY2_FRIENDS(T) \
  template MArray2<T>& operator += (MArray2<T>& a, const T& s); \
  template MArray2<T>& operator -= (MArray2<T>& a, const T& s); \
  template MArray2<T>& operator += (MArray2<T>& a, const MArray2<T>& b); \
  template MArray2<T>& operator -= (MArray2<T>& a, const MArray2<T>& b); \
  template MArray2<T> operator + (const MArray2<T>& a, const T& s); \
  template MArray2<T> operator - (const MArray2<T>& a, const T& s); \
  template MArray2<T> operator * (const MArray2<T>& a, const T& s); \
  template MArray2<T> operator / (const MArray2<T>& a, const T& s); \
  template MArray2<T> operator + (const T& s, const MArray2<T>& a); \
  template MArray2<T> operator - (const T& s, const MArray2<T>& a); \
  template MArray2<T> operator * (const T& s, const MArray2<T>& a); \
  template MArray2<T> operator / (const T& s, const MArray2<T>& a); \
  template MArray2<T> operator + (const MArray2<T>& a, const MArray2<T>& b); \
  template MArray2<T> operator - (const MArray2<T>& a, const MArray2<T>& b); \
  template MArray2<T> product (const MArray2<T>& a, const MArray2<T>& b); \
  template MArray2<T> quotient (const MArray2<T>& a, const MArray2<T>& b); \
  template MArray2<T> operator - (const MArray2<T>& a);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
