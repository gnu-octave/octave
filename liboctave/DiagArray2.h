// Template array classes
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

#if !defined (octave_DiagArray2_h)
#define octave_DiagArray2_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cassert>
#include <cstdlib>

#include "Array2.h"
#include "lo-error.h"

class idx_vector;

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
class
DiagArray2 : public Array<T>
{
private:

  T get (int i) { return Array<T>::xelem (i); }

  void set (const T& val, int i) { Array<T>::xelem (i) = val; }

  class Proxy
  {
  public:

    Proxy (DiagArray2<T> *ref, int r, int c)
      : i (r), j (c), object (ref) { } 

    const Proxy& operator = (const T& val) const
      {
	if (i == j)
	  {
	    if (object)
	      object->set (val, i);
	  }
	else
	  (*current_liboctave_error_handler)
	    ("invalid assignment to off-diagonal in diagonal array");

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
    // by means of a companion function in the DiagArray2 class.

    T *operator& () const { assert (0); return (T *) 0; }

    int i;
    int j;

    DiagArray2<T> *object;

  };

friend class Proxy;

protected:

  int nr;
  int nc;

  DiagArray2 (T *d, int r, int c) : Array<T> (d, r < c ? r : c)
    {
      nr = r;
      nc = c;
      set_max_indices (2);
    }

public:

  DiagArray2 (void) : Array<T> ()
    {
      nr = 0;
      nc = 0;
      set_max_indices (2);
    }

  DiagArray2 (int r, int c) : Array<T> (r < c ? r : c)
    {
      nr = r;
      nc = c;
      set_max_indices (2);
    }

  DiagArray2 (int r, int c, const T& val) : Array<T> (r < c ? r : c, val)
    {
      nr = r;
      nc = c;
      set_max_indices (2);
    }

  DiagArray2 (const Array<T>& a) : Array<T> (a)
    {
      nr = nc = a.length ();
      set_max_indices (2);
    }

  DiagArray2 (const DiagArray2<T>& a) : Array<T> (a)
    {
      nr = a.nr;
      nc = a.nc;
      set_max_indices (2);
    }

  ~DiagArray2 (void) { }

  DiagArray2<T>& operator = (const DiagArray2<T>& a)
    {
      if (this != &a)
	{
	  Array<T>::operator = (a);
	  nr = a.nr;
	  nc = a.nc;
	}

      return *this;
    }

#if 0
  operator Array2<T> () const
    {
      Array2<T> retval (nr, nc,  T (0));

      int len = nr < nc ? nr : nc;

      for (int i = 0; i < len; i++)
	retval.xelem (i, i) = xelem (i, i);

      return retval;
    }
#endif

  int dim1 (void) const { return nr; }
  int dim2 (void) const { return nc; }

  int rows (void) const { return nr; }
  int cols (void) const { return nc; }
  int columns (void) const { return nc; }

#if 1
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

  T elem (int r, int c) const;
  T checkelem (int r, int c) const;
  T operator () (int r, int c) const;

  // No checking.

  T& xelem (int r, int c);
  T xelem (int r, int c) const;

  void resize (int n, int m);
  void resize (int n, int m, const T& val);

  void maybe_delete_elements (idx_vector& i, idx_vector& j);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
