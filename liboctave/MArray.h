// Template array classes with like-type math ops          -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

// Classes we declare.

template <class T> class MArray;
template <class T> class MArray2;

#ifndef NO_DIAG_ARRAY
template <class T> class MDiagArray;
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

#ifndef NO_DIAG_ARRAY
  MArray2 (const MDiagArray<T>& a);
#endif

  ~MArray2 (void) { }

  MArray2<T>& operator = (const MArray2<T>& a)
    {
      Array2<T>::operator = (a);
      return *this;
    }

  // element by element MArray2 by scalar ops

  friend MArray2<T>& operator += (MArray2<T>& a, const T& s);
  friend MArray2<T>& operator -= (MArray2<T>& a, const T& s);

  // element by element MArray2 by MArray2 ops

  friend MArray2<T>& operator += (MArray2<T>& a, const MArray2<T>& b);
  friend MArray2<T>& operator -= (MArray2<T>& a, const MArray2<T>& b);

  // element by element MArray2 by scalar ops

  friend MArray2<T> operator + (const MArray2<T>& a, const T& s);
  friend MArray2<T> operator - (const MArray2<T>& a, const T& s);
  friend MArray2<T> operator * (const MArray2<T>& a, const T& s);
  friend MArray2<T> operator / (const MArray2<T>& a, const T& s);

  // element by element scalar by MArray2 ops

  friend MArray2<T> operator + (const T& s, const MArray2<T>& a);
  friend MArray2<T> operator - (const T& s, const MArray2<T>& a);
  friend MArray2<T> operator * (const T& s, const MArray2<T>& a);
  friend MArray2<T> operator / (const T& s, const MArray2<T>& a);

  // element by element MArray2 by MArray2 ops

  friend MArray2<T> operator + (const MArray2<T>& a, const MArray2<T>& b);
  friend MArray2<T> operator - (const MArray2<T>& a, const MArray2<T>& b);

  friend MArray2<T> product (const MArray2<T>& a, const MArray2<T>& b);
  friend MArray2<T> quotient (const MArray2<T>& a, const MArray2<T>& b);

  friend MArray2<T> operator - (const MArray2<T>& a);
};

// Two dimensional diagonal array with math ops.

#ifndef NO_DIAG_ARRAY
template <class T>
class MDiagArray : public DiagArray<T>
{
protected:

  MDiagArray (T *d, int r, int c) : DiagArray<T> (d, r, c) { }

public:
  
  MDiagArray (void) : DiagArray<T> () { }
  MDiagArray (int n) : DiagArray<T> (n) { }
//  MDiagArray (int n, const T& val) : DiagArray<T> (n, val) { }
  MDiagArray (int r, int c) : DiagArray<T> (r, c) { }
  MDiagArray (int r, int c, const T& val) : DiagArray<T> (r, c, val) { }
  MDiagArray (const DiagArray<T>& a) : DiagArray<T> (a) { }
  MDiagArray (const MDiagArray<T>& a) : DiagArray<T> (a) { }
  MDiagArray (const MArray<T>& a) : DiagArray<T> (a) { }

  ~MDiagArray (void) { }

  MDiagArray<T>& operator = (const MDiagArray<T>& a)
    {
      DiagArray<T>::operator = (a);
      return *this;
    }

  // element by element MDiagArray by MDiagArray ops

  friend MDiagArray<T>& operator += (MDiagArray<T>& a, const MDiagArray<T>& b);
  friend MDiagArray<T>& operator -= (MDiagArray<T>& a, const MDiagArray<T>& b);

  // element by element MDiagArray by scalar ops

  friend MDiagArray<T> operator * (const MDiagArray<T>& a, const T& s);
  friend MDiagArray<T> operator / (const MDiagArray<T>& a, const T& s);

  // element by element scalar by MDiagArray ops

  friend MDiagArray<T> operator * (const T& s, const MDiagArray<T>& a);

  // element by element MDiagArray by MDiagArray ops

  friend MDiagArray<T> operator + (const MDiagArray<T>& a,
				   const MDiagArray<T>& b); 

  friend MDiagArray<T> operator - (const MDiagArray<T>& a,
				   const MDiagArray<T>& b);

  friend MDiagArray<T> product (const MDiagArray<T>& a,
				const MDiagArray<T>& b);

  friend MDiagArray<T> operator - (const MDiagArray<T>& a);
};
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
