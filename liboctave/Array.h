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

#if !defined (octave_Array_h)
#define octave_Array_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cassert>
#include <cstdlib>

#include "lo-error.h"

class idx_vector;

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
class Array
{
private:

// The real representation of all arrays.

  class ArrayRep
  {
  public:

    T *data;
    int len;
    int count;

    ArrayRep& operator = (const ArrayRep& a);

    ArrayRep (T *d, int l) : data (d), len (l), count (1) { }

    ArrayRep (void) : data (0), len (0), count (1) { }

    ArrayRep (int n) : data (new T [n]), len (n), count (1) { }

    ArrayRep (const ArrayRep& a)
      : data (new T [a.len]), len (a.len), count (1)
	{
	  for (int i = 0; i < len; i++)
	    data[i] = a.data[i];
	}

    ~ArrayRep (void) { delete [] data; }

    int length (void) const { return len; }

    T& elem (int n) { return data[n]; }

    T elem (int n) const { return data[n]; }

    void qsort (int (*compare) (const void *, const void *))
      {
	::qsort (data, len, sizeof (T), compare);
      }
  };

#ifdef HEAVYWEIGHT_INDEXING
  idx_vector *idx;
  int max_indices;
  int idx_count;
#endif

protected:

  ArrayRep *rep;

  Array (T *d, int l)
    {
      rep = new ArrayRep (d, l);

#ifdef HEAVYWEIGHT_INDEXING
      idx = 0;
      max_indices = 1;
      idx_count = 0;
#endif
    }

public:

  Array (void)
    {
      rep = new ArrayRep ();

#ifdef HEAVYWEIGHT_INDEXING
      idx = 0;
      max_indices = 1;
      idx_count = 0;
#endif
    }

  Array (int n)
    {
      rep = new ArrayRep (n);

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
	  rep = new ArrayRep (*rep);
	}
      return rep->elem (n);
    }

  T& checkelem (int n);
  T& operator () (int n) { return checkelem (n); }

  T elem (int n) const;
  T checkelem (int n) const;
  T operator () (int n) const;

  // No checking.

  T& xelem (int n) { return rep->elem (n); }
  T xelem (int n) const { return rep->elem (n); }

  void resize (int n);
  void resize (int n, const T& val);

  const T *data (void) const { return rep->data; }

  T *fortran_vec (void);

  Array<T>& qsort (int (*compare) (const void *, const void *))
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new ArrayRep (*rep);
	}

      rep->qsort (compare);

      return *this;
    }

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

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
