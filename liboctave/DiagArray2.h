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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_DiagArray2_h)
#define octave_DiagArray2_h 1

#include <cassert>
#include <cstdlib>

#include "Array.h"
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

  T get (octave_idx_type i) { return Array<T>::xelem (i); }

  void set (const T& val, octave_idx_type i) { Array<T>::xelem (i) = val; }

  class Proxy
  {
  public:

    Proxy (DiagArray2<T> *ref, octave_idx_type r, octave_idx_type c)
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
	    static T foo;
	    return foo;
	  }
      }

  private:

    // XXX FIXME XXX -- this is declared private to keep the user from
    // taking the address of a Proxy.  Maybe it should be implemented
    // by means of a companion function in the DiagArray2 class.

    T *operator& () const { assert (0); return 0; }

    octave_idx_type i;
    octave_idx_type j;

    DiagArray2<T> *object;

  };

friend class Proxy;

protected:

  DiagArray2 (T *d, octave_idx_type r, octave_idx_type c) : Array<T> (d, r < c ? r : c)
    { Array<T>::dimensions = dim_vector (r, c); }

public:

  DiagArray2 (void) : Array<T> (dim_vector (0, 0)) { }

  DiagArray2 (octave_idx_type r, octave_idx_type c) : Array<T> (r < c ? r : c)
    { this->dimensions = dim_vector (r, c); }

  DiagArray2 (octave_idx_type r, octave_idx_type c, const T& val) : Array<T> (r < c ? r : c)
    {
      this->dimensions = dim_vector (r, c);

      Array<T>::fill (val);
    }

  DiagArray2 (const Array<T>& a) : Array<T> (a)
    { this->dimensions = dim_vector (a.length (), a.length ()); }

  DiagArray2 (const DiagArray2<T>& a) : Array<T> (a)
    { this->dimensions = a.dims (); }

  ~DiagArray2 (void) { }

  DiagArray2<T>& operator = (const DiagArray2<T>& a)
    {
      if (this != &a)
	Array<T>::operator = (a);

      return *this;
    }

  Proxy elem (octave_idx_type r, octave_idx_type c)
    {
      return Proxy (this, r, c);
    }

  Proxy checkelem (octave_idx_type r, octave_idx_type c)
    {
      if (r < 0 || c < 0 || r >= this->dim1 () || c >= this->dim2 ())
	{
	  (*current_liboctave_error_handler) ("range error in DiagArray2");
	  return Proxy (0, r, c);
	}
      else
	return Proxy (this, r, c);
    }

  Proxy operator () (octave_idx_type r, octave_idx_type c)
    {
      if (r < 0 || c < 0 || r >= this->dim1 () || c >= this->dim2 ())
	{
	  (*current_liboctave_error_handler) ("range error in DiagArray2");
	  return Proxy (0, r, c);
	}
      else
	return Proxy (this, r, c);
  }

  T elem (octave_idx_type r, octave_idx_type c) const;
  T checkelem (octave_idx_type r, octave_idx_type c) const;
  T operator () (octave_idx_type r, octave_idx_type c) const;

  // No checking.

  T& xelem (octave_idx_type r, octave_idx_type c);
  T xelem (octave_idx_type r, octave_idx_type c) const;

  void resize (octave_idx_type n, octave_idx_type m);
  void resize (octave_idx_type n, octave_idx_type m, const T& val);

  void maybe_delete_elements (idx_vector& i, idx_vector& j);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
