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

#define HEAVYWEIGHT_INDEXING 1

#include <cassert>

#include "lo-error.h"

class idx_vector;

// Classes we declare.

template <class T> class ArrayRep;
template <class T> class Array;
template <class T> class Array2;
template <class T> class Array3;

#ifndef NO_DIAG_ARRAY
template <class T> class DiagArray;
#endif

// The real representation of all arrays.

template <class T>
class ArrayRep
{
  // Rethink resize()?

  friend class Array<T>;
  friend class Array2<T>;
  friend class Array3<T>;

#ifndef NO_DIAG_ARRAY
  friend class DiagArray<T>;
#endif

private:

  T *data;
  int len;
  int count;

  ArrayRep<T>& operator = (const ArrayRep<T>& a);  

protected:

  ArrayRep (T *d, int l) : data (d), len (l), count (1) { }

public:

  ArrayRep (void) : data (0), len (0), count (1) { }

  ArrayRep (int n);

  ArrayRep (const ArrayRep<T>& a);

  ~ArrayRep (void);

  int length (void) const { return len; }

  T& elem (int n);

  T elem (int n) const;
};

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
class Array
{
private:

#ifdef HEAVYWEIGHT_INDEXING
  idx_vector *idx;
  int max_indices;
  int idx_count;
#endif

protected:

  ArrayRep<T> *rep;

  Array (T *d, int l)
    {
      rep = new ArrayRep<T> (d, l);

#ifdef HEAVYWEIGHT_INDEXING
      idx = 0;
      max_indices = 1;
      idx_count = 0;
#endif
    }

public:

  Array (void)
    {
      rep = new ArrayRep<T> ();

#ifdef HEAVYWEIGHT_INDEXING
      idx = 0;
      max_indices = 1;
      idx_count = 0;
#endif
    }

  Array (int n)
    {
      rep = new ArrayRep<T> (n);

#ifdef HEAVYWEIGHT_INDEXING
      idx = 0;
      max_indices = 1;
      idx_count = 0;
#endif
    }

  Array (int n, const T& val);

  Array (const Array<T>& a)
    {
      rep = a.rep;
      rep->count++;

#ifdef HEAVYWEIGHT_INDEXING
      max_indices = a.max_indices;
      idx_count = 0;
      idx = 0;
#endif
    }

  ~Array (void);

  Array<T>& operator = (const Array<T>& a);

  int capacity (void) const { return rep->length (); }
  int length (void) const { return rep->length (); }

  T& elem (int n)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new ArrayRep<T> (*rep);
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

#ifdef HEAVYWEIGHT_INDEXING
  void set_max_indices (int mi) { max_indices = mi; }

  void clear_index (void);

  void set_index (const idx_vector& i);

  int index_count (void) const { return idx_count; }

  idx_vector *get_idx (void) const { return idx; }

  void maybe_delete_elements (idx_vector& i);

  Array<T> value (void);
#endif
};

template <class LT, class RT>
int assign (Array<LT>& lhs, const Array<RT>& rhs);

// Two dimensional array class.

template <class T>
class Array2 : public Array<T>
{
protected:

  Array2 (T *d, int n, int m) : Array<T> (d, n*m)
    {
      d1 = n;
      d2 = m;
      set_max_indices (2);
    }

public:

  // These really need to be protected (and they will be in the
  // future, so don't depend on them being here!), but they can't be
  // until template friends work correctly in g++.

  int d1;
  int d2;

  Array2 (void) : Array<T> ()
    {
      d1 = 0;
      d2 = 0;
      set_max_indices (2);
    }

  Array2 (int n, int m) : Array<T> (n*m)
    {
      d1 = n;
      d2 = m;
      set_max_indices (2);
    }

  Array2 (int n, int m, const T& val) : Array<T> (n*m, val)
    {
      d1 = n;
      d2 = m;
      set_max_indices (2);
    }

  Array2 (const Array2<T>& a) : Array<T> (a)
    {
      d1 = a.d1;
      d2 = a.d2;
      set_max_indices (2);
    }

  Array2 (const Array<T>& a, int n, int m) : Array<T> (a)
    {
      d1 = n;
      d2 = m;
      set_max_indices (2);
    }

#ifndef NO_DIAG_ARRAY
  Array2 (const DiagArray<T>& a) : Array<T> (a.rows () * a.cols (), T (0))
    {
      for (int i = 0; i < a.length (); i++)
	elem (i, i) = a.elem (i, i);

      set_max_indices (2);
    }
#endif

  ~Array2 (void) { }

  Array2<T>& operator = (const Array2<T>& a)
    {
      if (this != &a && rep != a.rep)
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

  Array2<T>& insert (const Array2<T>& a, int r, int c);

#ifdef HEAVYWEIGHT_INDEXING
  void maybe_delete_elements (idx_vector& i, idx_vector& j);

  Array2<T> value (void);
#endif
};

template <class LT, class RT>
int assign (Array2<LT>& lhs, const Array2<RT>& rhs);

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
      set_max_indices (3);
    }

public:

  Array3 (void) : Array2<T> ()
    {
      d2 = 0;
      d3 = 0;
      set_max_indices (3);
    }

  Array3 (int n, int m, int k) : Array2<T> (n, m*k)
    {
      d2 = m;
      d3 = k;
      set_max_indices (3);
    }

  Array3 (int n, int m, int k, const T& val) : Array2<T> (n, m*k, val)
    {
      d2 = m;
      d3 = k;
      set_max_indices (3);
    }

  Array3 (const Array3<T>& a) : Array2<T> (a)
    {
      d2 = a.d2;
      d3 = a.d3;
      set_max_indices (3);
    }

  ~Array3 (void) { }

  Array3<T>& operator = (const Array3<T>& a)
    {
      if (this != &a && rep != a.rep)
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

#ifdef HEAVYWEIGHT_INDEXING
  void maybe_delete_elements (idx_vector& i, idx_vector& j, idx_vector& k);

  Array3<T> value (void);
#endif
};

template <class LT, class RT>
int assign (Array3<LT>& lhs, const Array3<RT>& rhs);

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

#ifndef NO_DIAG_ARRAY
template <class T>
class DiagArray : public Array<T>
{
private:
  T get (int i) { return Array<T>::elem (i); }
  void set (const T& val, int i) { Array<T>::elem (i) = val; }

  class Proxy
  {
  public:

    Proxy (DiagArray<T> *ref, int r, int c)
      : i (r), j (c), object (ref) { } 

    const Proxy& operator = (const T& val) const
      {
	if (i == j)
	  {
	    if (object)
	      object->set (val, i);
	  }
	else
	  (*current_liboctave_error_handler) ("invalid assignment to off-diagonal in diagonal array");

	return *this;
      }

    operator T () const
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

    T *operator& () const { assert (0); return (T *) 0; }

    int i;
    int j;

    DiagArray<T> *object;

  };

friend class Proxy;

protected:

  int nr;
  int nc;

  DiagArray (T *d, int r, int c) : Array<T> (d, r < c ? r : c)
    {
      nr = r;
      nc = c;
      set_max_indices (2);
    }

public:

  DiagArray (void) : Array<T> ()
    {
      nr = 0;
      nc = 0;
      set_max_indices (2);
    }

  DiagArray (int n) : Array<T> (n)
    {
      nr = n;
      nc = n;
      set_max_indices (2);
    }

  DiagArray (int n, const T& val) : Array<T> (n, val)
    {
      nr = n;
      nc = n;
      set_max_indices (2);
    }

  DiagArray (int r, int c) : Array<T> (r < c ? r : c)
    {
      nr = r;
      nc = c;
      set_max_indices (2);
    }

  DiagArray (int r, int c, const T& val) : Array<T> (r < c ? r : c, val)
    {
      nr = r;
      nc = c;
      set_max_indices (2);
    }

  DiagArray (const Array<T>& a) : Array<T> (a)
    {
      nr = nc = a.length ();
      set_max_indices (2);
    }

  DiagArray (const DiagArray<T>& a) : Array<T> (a)
    {
      nr = a.nr;
      nc = a.nc;
      set_max_indices (2);
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
  Proxy elem (int r, int c)
    {
      return Proxy (this, r, c);
    }

  Proxy checkelem (int r, int c)
    {
      if (r < 0 || c < 0 || r >= nr || c >= nc)
	{
	  (*current_liboctave_error_handler) ("range error");
	  return Proxy (0, r, c);
	}
      else
	return Proxy (this, r, c);
    }

  Proxy operator () (int r, int c)
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

  void maybe_delete_elements (idx_vector& i, idx_vector& j);
};
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
