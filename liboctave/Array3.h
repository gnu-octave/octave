// Template array classes
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

#if !defined (octave_Array3_h)
#define octave_Array3_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cassert>
#include <cstdlib>

#include "Array2.h"
#include "lo-error.h"

class idx_vector;

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

  // No checking of any kind, ever.

  T& xelem (int i, int j, int k) { return Array2<T>::xelem (i, d2*k+j); }
  T xelem (int i, int j, int k) const { return Array2<T>::xelem (i, d2*k+j); }

  // Note that the following element selection methods don't use
  // xelem() because they need to make use of the code in
  // Array<T>::elem() that checks the reference count.

  T& checkelem (int i, int j, int k)
    {
      if (i < 0 || j < 0 || k < 0 || i >= d1 || j >= d2 || k >= d3)
	{
	  (*current_liboctave_error_handler) ("range error");
	  static T foo;
	  return foo;
	}
      return Array2<T>::elem (i, d2*k+j);
    }

  T& elem (int i, int j, int k) { return Array2<T>::elem (i, d2*k+j); }

#if defined (BOUNDS_CHECKING)
  T& operator () (int i, int j, int k) { return checkelem (i, j, k); }
#else
  T& operator () (int i, int j, int k) { return elem (i, j, k); }
#endif

  T checkelem (int i, int j, int k) const
    {
      if (i < 0 || j < 0 || k < 0 || i >= d1 || j >= d2 || k >= d3)
	{
	  (*current_liboctave_error_handler) ("range error");
	  T foo;
	  static T *bar = &foo;
	  return foo;
	}
      return Array2<T>::elem (i, d1*k+j);
    }

  T elem (int i, int j, int k) const { return Array2<T>::elem (i, d2*k+j); }

#if defined (BOUNDS_CHECKING)
  T operator () (int i, int j, int k) const { return checkelem (i, j, k); }
#else
  T operator () (int i, int j, int k) const { return elem (i, j, k); }
#endif

  void resize (int n, int m, int k);
  void resize (int n, int m, int k, const T& val);

#ifdef HEAVYWEIGHT_INDEXING
  void maybe_delete_elements (idx_vector& i, idx_vector& j, idx_vector& k);

  Array3<T> value (void);
#endif
};

template <class LT, class RT>
int assign (Array3<LT>& lhs, const Array3<RT>& rhs);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
