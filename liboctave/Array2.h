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

#if !defined (octave_Array2_h)
#define octave_Array2_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cassert>
#include <climits>
#include <cmath>
#include <cstdlib>

#include "Array.h"
#include "lo-error.h"

class idx_vector;

// Two dimensional array class.

template <class T>
class Array2 : public Array<T>
{
protected:

  int get_size (int r, int c) const;

  Array2 (T *d, int n, int m) : Array<T> (d, get_size (n, m))
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

  Array2 (int n, int m) : Array<T> (get_size (n, m))
    {
      d1 = n;
      d2 = m;
      set_max_indices (2);
    }

  Array2 (int n, int m, const T& val) : Array<T> (get_size (n, m), val)
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

  // No checking of any kind, ever.

  T& xelem (int i, int j) { return Array<T>::xelem (d1*j+i); }
  T xelem (int i, int j) const { return Array<T>::xelem (d1*j+i); }

  // Note that the following element selection methods don't use
  // xelem() because they need to make use of the code in
  // Array<T>::elem() that checks the reference count.

  T& checkelem (int i, int j)
    {
      if (i < 0 || j < 0 || i >= d1 || j >= d2)
	{
	  (*current_liboctave_error_handler)
	    ("T& Array2<T>::checkelem (%d, %d): range error", i, j);
	  static T foo;
	  return foo;
	}
      else
	return Array<T>::elem (d1*j+i);
    }

  T& elem (int i, int j) { return Array<T>::elem (d1*j+i); }

#if defined (BOUNDS_CHECKING)
  T& operator () (int i, int j) { return checkelem (i, j); }
#else
  T& operator () (int i, int j) { return elem (i, j); }
#endif

  T checkelem (int i, int j) const
    {
      if (i < 0 || j < 0 || i >= d1 || j >= d2)
	{
	  (*current_liboctave_error_handler)
	    ("T Array2<T>::checkelem (%d, %d): range error", i, j);
	  return T ();
	}
      else
	return Array<T>::elem (d1*j+i);
    }

  T elem (int i, int j) const { return Array<T>::elem (d1*j+i); }

#if defined (BOUNDS_CHECKING)
  T operator () (int i, int j) const { return checkelem (i, j); }
#else
  T operator () (int i, int j) const { return elem (i, j); }
#endif

  T range_error (const char *fcn, int i, int j) const;
  T& range_error (const char *fcn, int i, int j);

  void resize (int n, int m);
  void resize (int n, int m, const T& val);

  Array2<T>& insert (const Array2<T>& a, int r, int c);

  bool is_square (void) const { return (d1 == d2); }

  Array2<T> transpose (void) const;

#ifdef HEAVYWEIGHT_INDEXING
  void maybe_delete_elements (idx_vector& i);

  void maybe_delete_elements (idx_vector& i, idx_vector& j);

  Array2<T> value (void);

  Array2<T> index (idx_vector& i) const;

  Array2<T> index (idx_vector& i, idx_vector& j) const;
#endif
};

template <class LT, class RT>
int assign (Array2<LT>& lhs, const Array2<RT>& rhs);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
