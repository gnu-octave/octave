// Template array classes                              -*- C++ -*-
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

#if !defined (octave_Array_h)
#define octave_Array_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cassert>

#include "lo-error.h"

// Classes we declare.

template <class T> class ArrayRep;
template <class T> class Array;
template <class T> class Array2;
template <class T> class Array3;
template <class T> class DiagArray;

// The real representation of all arrays.

template <class T>
class ArrayRep
{
  // Rethink resize()?

  friend class Array<T>;
  friend class Array2<T>;
  friend class Array3<T>;
  friend class DiagArray<T>;

private:

  int count;
  int len;
  T *data;

protected:

  ArrayRep (T *d, int l)
    {
      len = l;
      data = d;
    }

public:

  ArrayRep (void)
    {
      len = 0;
      data = 0;
    }

  ArrayRep (int n);

  ArrayRep (const ArrayRep<T>& a);

  ~ArrayRep (void);

  int length (void) const { return len; }

  T& elem (int n);

  T elem (int n) const;

  void resize (int n);
};

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
class Array
{
protected:

  ArrayRep<T> *rep;

  Array (T *d, int l)
    {
      rep = new ArrayRep<T> (d, l);
      rep->count = 1;
    }

public:

  Array (void)
    {
      rep = new ArrayRep<T>;
      rep->count = 1;
    }

  Array (int n)
    {
      rep = new ArrayRep<T> (n);
      rep->count = 1;
    }

  Array (int n, const T& val);

  Array (const Array<T>& a)
    {
      rep = a.rep;
      rep->count++;
    }

  ~Array (void)
    {
      if (--rep->count <= 0)
	delete rep;
    }

  Array<T>& operator = (const Array<T>& a);

  int capacity (void) const { return rep->length (); }
  int length (void) const { return rep->length (); }

  T& elem (int n)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new ArrayRep<T> (*rep);
	  rep->count = 1;
	}
      return rep->elem (n);
    }

  T& checkelem (int n);
  T& operator () (int n) { return checkelem (n); }

  // No checking.

  T& xelem (int n) { return rep->elem (n); }

  T elem (int n) const;
  T checkelem (int n) const;
  T operator () (int n) const;

  void resize (int n);
  void resize (int n, const T& val);

  const T *data (void) const { return rep->data; }

  T *fortran_vec (void);
};

// Two dimensional array class.

template <class T>
class Array2 : public Array<T>
{
protected:

  int d1;
  int d2;

  Array2 (T *d, int n, int m) : Array<T> (d, n*m)
    {
      d1 = n;
      d2 = m;
    }

public:

  Array2 (void) : Array<T> ()
    {
      d1 = 0;
      d2 = 0;
    }

  Array2 (int n, int m) : Array<T> (n*m)
    {
      d1 = n;
      d2 = m;
    }

  Array2 (int n, int m, const T& val) : Array<T> (n*m, val)
    {
      d1 = n;
      d2 = m;
    }

  Array2 (const Array2<T>& a) : Array<T> (a)
    {
      d1 = a.d1;
      d2 = a.d2;
    }

  Array2 (const DiagArray<T>& a)  : Array<T> (a.rows () * a.cols (), T (0))
    {
      for (int i = 0; i < a.length (); i++)
	elem (i, i) = a.elem (i, i);
    }

  ~Array2 (void) { }

  Array2<T>& operator = (const Array2<T>& a)
    {
      if (this != &a)
	{
	  Array<T>::operator = (a);
	  d1 = a.d1;
	  d2 = a.d2;
	}

      return *this;
    }

  int dim1 (void) const { return d1; }
  int dim2 (void) const { return d2; }

  int rows (void) const { return d1; }
  int cols (void) const { return d2; }
  int columns (void) const { return d2; }

  T& elem (int i, int j) { return Array<T>::elem (d1*j+i); }
  T& checkelem (int i, int j);
  T& operator () (int i, int j) { return checkelem (i, j); }

  // No checking.

  T& xelem (int i, int j) { return Array<T>::xelem (d1*j+i); }

  T elem (int i, int j) const;
  T checkelem (int i, int j) const;
  T operator () (int i, int j) const;

  void resize (int n, int m);
  void resize (int n, int m, const T& val);
};

// Three dimensional array class.

template <class T>
class Array3 : public Array2<T>
{
protected:

  int d3;

  Array3 (T *d, int n, int m, int k) : Array2<T> (d, n, m*k)
    {
      d2 = m;
      d3 = k;
    }

public:

  Array3 (void) : Array2<T> ()
    {
      d2 = 0;
      d3 = 0;
    }

  Array3 (int n, int m, int k) : Array2<T> (n, m*k)
    {
      d2 = m;
      d3 = k;
    }

  Array3 (int n, int m, int k, const T& val) : Array2<T> (n, m*k, val)
    {
      d2 = m;
      d3 = k;
    }

  Array3 (const Array3<T>& a) : Array2<T> (a)
    {
      d2 = a.d2;
      d3 = a.d3;
    }

  ~Array3 (void) { }

  Array3<T>& operator = (const Array3<T>& a)
    {
      if (this != &a)
	{
	  Array<T>::operator = (a);
	  d1 = a.d1;
	  d2 = a.d2;
	  d3 = a.d3;
	}

      return *this;
    }

  int dim3 (void) const { return d3; }

  T& elem (int i, int j, int k) { return Array2<T>::elem (i, d2*k+j); }
  T& checkelem (int i, int j, int k);
  T& operator () (int i, int j, int k) { return checkelem (i, j, k); }

  // No checking.

  T& xelem (int i, int j, int k) { return Array2<T>::xelem (i, d2*k+j); }

  T elem (int i, int j, int k) const;
  T checkelem (int i, int j, int k) const;
  T operator () (int i, int j, int k) const;

  void resize (int n, int m, int k);
  void resize (int n, int m, int k, const T& val);
};

// A two-dimensional array with diagonal elements only.
//
// Idea and example code for Proxy class and functions from:
//
// From: kanze@us-es.sel.de (James Kanze)
// Subject: Re: How to overload [] to do READ/WRITE differently ?
// Message-ID: <KANZE.93Nov29151407@slsvhdt.us-es.sel.de>
// Sender: news@us-es.sel.de
// Date: 29 Nov 1993 14:14:07 GMT
// --
// James Kanze                             email: kanze@us-es.sel.de
// GABI Software, Sarl., 8 rue du Faisan, F-67000 Strasbourg, France

template <class T>
class DiagArray : public Array<T>
{
private:
  inline T get (int i) { return Array<T>::elem (i); }
  inline void set (const T& val, int i) { Array<T>::elem (i) = val; }

#if 0
#if ! (defined (_AIX) && defined (__GNUG__) && __GNUC__ > 1 && __GNUC_MINOR__ < 6)
  class Proxy
  {
  public:

    inline Proxy (DiagArray<T> *ref, int r, int c)
      : i (r), j (c), object (ref) { } 

    inline const Proxy& operator = (const T& val) const
    {
      if (i == j)
	{
	  if (object)
	    object->set (val, i);
	}
      else
	(*current_liboctave_error_handler)
	  ("assignment to off-diagonal element attempted for diagonal array");

      return *this;
    }

    inline operator T () const
    {
      if (object && i == j)
	return object->get (i);
      else
	{
	  static T foo (0);
	  return foo;
	}
    }

  private:

    // XXX FIXME XXX -- this is declared private to keep the user from
    // taking the address of a Proxy.  Maybe it should be implemented
    // by means of a companion function in the DiagArray class.

    inline T *operator& () const { assert (0); return (T *) 0; }

    int i;
    int j;

    DiagArray<T> *object;

  };

friend class Proxy;
#endif
#endif

protected:

  int nr;
  int nc;

  DiagArray (T *d, int r, int c) : Array<T> (d, r < c ? r : c)
    {
      nr = r;
      nc = c;
    }

public:

  DiagArray (void) : Array<T> ()
    {
      nr = 0;
      nc = 0;
    }

  DiagArray (int n) : Array<T> (n)
{
  nr = n;
  nc = n;
}

  DiagArray (int n, const T& val) : Array<T> (n, val)
{
  nr = n;
  nc = n;
}

  DiagArray (int r, int c) : Array<T> (r < c ? r : c)
{
  nr = r;
  nc = c;
}

  DiagArray (int r, int c, const T& val) : Array<T> (r < c ? r : c, val)
{
  nr = r;
  nc = c;
}

  DiagArray (const Array<T>& a) : Array<T> (a)
{
  nr = nc = a.length ();
}

  DiagArray (const DiagArray<T>& a) : Array<T> (a)
{
  nr = a.nr;
  nc = a.nc;
}

  ~DiagArray (void) { }

  DiagArray<T>& operator = (const DiagArray<T>& a)
{
  if (this != &a)
    {
      Array<T>::operator = (a);
      nr = a.nr;
      nc = a.nc;
    }

  return *this;
}

  int dim1 (void) const { return nr; }
  int dim2 (void) const { return nc; }

  int rows (void) const { return nr; }
  int cols (void) const { return nc; }
  int columns (void) const { return nc; }

#if 0
  inline Proxy elem (int r, int c)
  {
    return Proxy (this, r, c);
  }

  inline Proxy checkelem (int r, int c)
  {
    if (r < 0 || c < 0 || r >= nr || c >= nc)
      {
	(*current_liboctave_error_handler) ("range error");
	return Proxy (0, r, c);
      }
    else
      return Proxy (this, r, c);
  }

  inline Proxy operator () (int r, int c)
  {
    if (r < 0 || c < 0 || r >= nr || c >= nc)
      {
	(*current_liboctave_error_handler) ("range error");
	return Proxy (0, r, c);
      }
    else
      return Proxy (this, r, c);
  }
#else
  T& elem (int r, int c);
  T& checkelem (int r, int c);
  T& operator () (int r, int c);
#endif

  // No checking.

  T& xelem (int r, int c);

  T elem (int r, int c) const;
  T checkelem (int r, int c) const;
  T operator () (int r, int c) const;

  void resize (int n, int m);
  void resize (int n, int m, const T& val);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
