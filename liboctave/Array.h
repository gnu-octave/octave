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

#if !defined (octave_Array_h)
#define octave_Array_h 1

#include <cassert>
#include <cstddef>

#include <iostream>

#include "dim-vector.h"
#include "lo-utils.h"

class idx_vector;

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
T
resize_fill_value (const T& x)
{
  return x;
}

template <class T>
class
Array
{
protected:

  //--------------------------------------------------------------------
  // The real representation of all arrays.
  //--------------------------------------------------------------------

  class ArrayRep
  {
  public:

    T *data;
    int len;
    int count;

    ArrayRep (T *d, int l) : data (d), len (l), count (1) { }

    ArrayRep (void) : data (0), len (0), count (1) { }

    explicit ArrayRep (int n) : data (new T [n]), len (n), count (1) { }

    explicit ArrayRep (int n, const T& val)
      : data (new T [n]), len (n), count (1)
      {
	fill (val);
      }

    ArrayRep (const ArrayRep& a)
      : data (new T [a.len]), len (a.len), count (1)
      {
        for (int i = 0; i < len; i++)
	  data[i] = a.data[i];
      }
 
    ~ArrayRep (void) { delete [] data; }

    int length (void) const { return len; }

    void fill (const T& val)
      {
	for (int i = 0; i < len; i++)
	  data[i] = val;
      }

    T& elem (int n) { return data[n]; }

    T elem (int n) const { return data[n]; }

    void qsort (int (*compare) (const void *, const void *))
      {
	octave_qsort (data, static_cast<size_t> (len), sizeof (T), compare);
      }

  private:

    // No assignment!

    ArrayRep& operator = (const ArrayRep& a);
  };

  //--------------------------------------------------------------------

  void make_unique (void)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new ArrayRep (*rep);
	}
    }

  void make_unique (const T& val)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new ArrayRep (rep->length (), val);
	}
      else
	rep->fill (val);
    }

public:

  // !!! WARNING !!! -- these should be protected, not public.  You
  // should not access these data members directly!

  typename Array<T>::ArrayRep *rep;

  dim_vector dimensions;

protected:

  idx_vector *idx;
  int idx_count;

  Array (T *d, int n)
    : rep (new typename Array<T>::ArrayRep (d, n)), dimensions (n),
      idx (0), idx_count (0) { }

  Array (T *d, const dim_vector& dv)
    : rep (new typename Array<T>::ArrayRep (d, get_size (dv))),
      dimensions (dv), idx (0), idx_count (0) { }

private:

  typename Array<T>::ArrayRep *nil_rep (void) const
    {
      static typename Array<T>::ArrayRep *nr
	= new typename Array<T>::ArrayRep ();

      return nr;
    }

  template <class U>
  T *
  coerce (const U *a, int len)
  {
    T *retval = new T [len];

    for (int i = 0; i < len; i++)
      retval[i] = T (a[i]);

    return retval;
  }

public:

  Array (void)
    : rep (nil_rep ()), dimensions (),
      idx (0), idx_count (0) { rep->count++; }

  explicit Array (int n)
    : rep (new typename Array<T>::ArrayRep (n)), dimensions (n),
      idx (0), idx_count (0) { }

  explicit Array (int n, const T& val)
    : rep (new typename Array<T>::ArrayRep (n)), dimensions (n),
      idx (0), idx_count (0)
    {
      fill (val);
    }

  // Type conversion case.
  template <class U>
  Array (const Array<U>& a)
    : rep (new typename Array<T>::ArrayRep (coerce (a.data (), a.length ()), a.length ())),
      dimensions (a.dimensions), idx (0), idx_count (0)
    {
    }

  // No type conversion case.
  Array (const Array<T>& a)
    : rep (a.rep), dimensions (a.dimensions), idx (0), idx_count (0)
    {
      rep->count++;
    }

public:

  Array (const dim_vector& dv)
    : rep (new typename Array<T>::ArrayRep (get_size (dv))),
      dimensions (dv), idx (0), idx_count (0) { }

  Array (const dim_vector& dv, const T& val)
    : rep (new typename Array<T>::ArrayRep (get_size (dv))),
      dimensions (dv), idx (0), idx_count (0)
    {
      fill (val);
    }

  Array (const Array<T>& a, const dim_vector& dv);

  virtual ~Array (void);

  Array<T>& operator = (const Array<T>& a)
    {
      if (this != &a)
	{
	  if (--rep->count <= 0)
	    delete rep;

	  rep = a.rep;
	  rep->count++;

	  dimensions = a.dimensions;
	}

      idx_count = 0;
      idx = 0;

      return *this;
    }

  void fill (const T& val) { make_unique (val); }

  int capacity (void) const { return rep->length (); }
  int length (void) const { return capacity (); }
  int nelem (void) const { return capacity (); }
  int numel (void) const { return nelem (); }

  int dim1 (void) const { return dimensions(0); }
  int dim2 (void) const { return dimensions(1); }
  int dim3 (void) const { return dimensions(2); }

  int rows (void) const { return dim1 (); }
  int cols (void) const { return dim2 (); }
  int columns (void) const { return dim2 (); }
  int pages (void) const { return dim3 (); }

  size_t byte_size (void) const { return numel () * sizeof (T); }

  dim_vector dims (void) const { return dimensions; }

  Array<T> squeeze (void) const;
  
  void chop_trailing_singletons (void) 
  { dimensions.chop_trailing_singletons (); }
  
  static int get_size (int r, int c);
  static int get_size (int r, int c, int p);
  static int get_size (const dim_vector& dv);

  int compute_index (const Array<int>& ra_idx) const;

  T range_error (const char *fcn, int n) const;
  T& range_error (const char *fcn, int n);

  T range_error (const char *fcn, int i, int j) const;
  T& range_error (const char *fcn, int i, int j);

  T range_error (const char *fcn, int i, int j, int k) const;
  T& range_error (const char *fcn, int i, int j, int k);

  T range_error (const char *fcn, const Array<int>& ra_idx) const;
  T& range_error (const char *fcn, const Array<int>& ra_idx);

  // No checking, even for multiple references, ever.

  T& xelem (int n) { return rep->elem (n); }
  T xelem (int n) const { return rep->elem (n); }

  T& xelem (int i, int j) { return xelem (dim1()*j+i); }
  T xelem (int i, int j) const { return xelem (dim1()*j+i); }

  T& xelem (int i, int j, int k) { return xelem (i, dim2()*k+j); }
  T xelem (int i, int j, int k) const { return xelem (i, dim2()*k+j); }

  T& xelem (const Array<int>& ra_idx)
    { return xelem (compute_index (ra_idx)); }

  T xelem (const Array<int>& ra_idx) const
    { return xelem (compute_index (ra_idx)); }

  // XXX FIXME XXX -- would be nice to fix this so that we don't
  // unnecessarily force a copy, but that is not so easy, and I see no
  // clean way to do it.

  T& checkelem (int n)
    {
      if (n < 0 || n >= rep->length ())
	return range_error ("T& Array<T>::checkelem", n);
      else
	{
	  make_unique ();
	  return xelem (n);
	}
    }

  T& checkelem (int i, int j)
    {
      if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
	return range_error ("T& Array<T>::checkelem", i, j);
      else
	return elem (dim1()*j+i);
    }

  T& checkelem (int i, int j, int k)
    {
      if (i < 0 || j < 0 || k < 0 || i >= dim1 () || j >= dim2 () || k >= dim3 ())
	return range_error ("T& Array<T>::checkelem", i, j, k);
      else
	return elem (i, dim2()*k+j);
    }

  T& checkelem (const Array<int>& ra_idx)
    {
      int i = compute_index (ra_idx);

      if (i < 0)
	return range_error ("T& Array<T>::checkelem", ra_idx);
      else
	return elem (i);
    }

  T& elem (int n)
    {
      make_unique ();
      return xelem (n);
    }

  T& elem (int i, int j) { return elem (dim1()*j+i); }

  T& elem (int i, int j, int k) { return elem (i, dim2()*k+j); }

  T& elem (const Array<int>& ra_idx)
    { return Array<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T& operator () (int n) { return checkelem (n); }
  T& operator () (int i, int j) { return checkelem (i, j); }
  T& operator () (int i, int j, int k) { return checkelem (i, j, k); }
  T& operator () (const Array<int>& ra_idx) { return checkelem (ra_idx); }
#else
  T& operator () (int n) { return elem (n); }
  T& operator () (int i, int j) { return elem (i, j); }
  T& operator () (int i, int j, int k) { return elem (i, j, k); }
  T& operator () (const Array<int>& ra_idx) { return elem (ra_idx); }
#endif

  T checkelem (int n) const
    {
      if (n < 0 || n >= rep->length ())
	return range_error ("T Array<T>::checkelem", n);
      else
	return xelem (n);
    }

  T checkelem (int i, int j) const
    {
      if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
	return range_error ("T Array<T>::checkelem", i, j);
      else
	return elem (dim1()*j+i);
    }

  T checkelem (int i, int j, int k) const
    {
      if (i < 0 || j < 0 || k < 0 || i >= dim1 () || j >= dim2 () || k >= dim3 ())
	return range_error ("T Array<T>::checkelem", i, j, k);
      else
	return Array<T>::elem (i, Array<T>::dim1()*k+j);
    }

  T checkelem (const Array<int>& ra_idx) const
    {
      int i = compute_index (ra_idx);

      if (i < 0)
	return range_error ("T Array<T>::checkelem", ra_idx);
      else
	return Array<T>::elem (i);
    }

  T elem (int n) const { return xelem (n); }

  T elem (int i, int j) const { return elem (dim1()*j+i); }

  T elem (int i, int j, int k) const { return elem (i, dim2()*k+j); }

  T elem (const Array<int>& ra_idx) const
    { return Array<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T operator () (int n) const { return checkelem (n); }
  T operator () (int i, int j) const { return checkelem (i, j); }
  T operator () (int i, int j, int k) const { return checkelem (i, j, k); }
  T operator () (const Array<int>& ra_idx) const { return checkelem (ra_idx); }
#else
  T operator () (int n) const { return elem (n); }
  T operator () (int i, int j) const { return elem (i, j); }
  T operator () (int i, int j, int k) const { return elem (i, j, k); }
  T operator () (const Array<int>& ra_idx) const { return elem (ra_idx); }
#endif

  Array<T> reshape (const dim_vector& new_dims) const;

  Array<T> permute (const Array<int>& vec, bool inv = false) const;
  Array<T> ipermute (const Array<int>& vec) const
    { return permute (vec, true); }

  void resize_no_fill (int n);
  void resize_and_fill (int n, const T& val);

  // !!! WARNING !!! -- the following resize_no_fill and
  // resize_and_fill functions are public because template friends
  // don't work properly with versions of gcc earlier than 3.3.  You
  // should use these functions only in classes that are derived
  // from Array<T>.

  // protected:

  void resize_no_fill (int r, int c);
  void resize_and_fill (int r, int c, const T& val);

  void resize_no_fill (int r, int c, int p);
  void resize_and_fill (int r, int c, int p, const T& val);

  void resize_no_fill (const dim_vector& dv);
  void resize_and_fill (const dim_vector& dv, const T& val);

public:

  void resize (int n) { resize_no_fill (n); }

  void resize (int n, const T& val) { resize_and_fill (n, val); }

  void resize (const dim_vector& dv) { resize_no_fill (dv); }

  void resize (const dim_vector& dv, const T& val)
    { resize_and_fill (dv, val); }

  Array<T>& insert (const Array<T>& a, int r, int c);
  Array<T>& insert2 (const Array<T>& a, int r, int c);
  Array<T>& insertN (const Array<T>& a, int r, int c);

  Array<T>& insert (const Array<T>& a, const Array<int>& idx);

  bool is_square (void) const { return (dim1 () == dim2 ()); }

  bool is_empty (void) const { return numel () == 0; }

  Array<T> transpose (void) const;

  const T *data (void) const { return rep->data; }

  const T *fortran_vec (void) const { return data (); }

  T *fortran_vec (void);

  Array<T>& qsort (int (*compare) (const void *, const void *))
    {
      make_unique ();

      rep->qsort (compare);

      return *this;
    }

  int ndims (void) const { return dimensions.length (); }

  void maybe_delete_dims (void);

  void clear_index (void);

  void set_index (const idx_vector& i);

  int index_count (void) const { return idx_count; }

  idx_vector *get_idx (void) const { return idx; }

  void maybe_delete_elements (idx_vector& i);

  void maybe_delete_elements_1 (idx_vector& i);

  void maybe_delete_elements_2 (idx_vector& i);

  void maybe_delete_elements (idx_vector& i, idx_vector& j);

  void maybe_delete_elements (idx_vector& i, idx_vector& j, idx_vector& k);

  void maybe_delete_elements (Array<idx_vector>& ra_idx, const T& rfv);

  Array<T> value (void);

  Array<T> index (idx_vector& i, int resize_ok = 0,
		  const T& rfv = resize_fill_value (T ())) const;

  Array<T> index1 (idx_vector& i, int resize_ok = 0,
		   const T& rfv = resize_fill_value (T ())) const;

  Array<T> index2 (idx_vector& i, int resize_ok = 0,
		   const T& rfv = resize_fill_value (T ())) const;

  Array<T> indexN (idx_vector& i, int resize_ok = 0,
		   const T& rfv = resize_fill_value (T ())) const;

  Array<T> index (idx_vector& i, idx_vector& j, int resize_ok = 0,
		  const T& rfv = resize_fill_value (T ())) const;

  Array<T> index (Array<idx_vector>& ra_idx, int resize_ok = 0,
		  const T& rfv = resize_fill_value (T ())) const;

  //  static T resize_fill_value (void) { return T (); }

  void print_info (std::ostream& os, const std::string& prefix) const;
};

// NOTE: these functions should be friends of the Array<T> class and
// Array<T>::dimensions should be protected, not public, but we can't
// do that because of bugs in gcc prior to 3.3.

template <class LT, class RT>
/* friend */ int
assign (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv);

template <class LT, class RT>
/* friend */ int
assign1 (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv);

template <class LT, class RT>
/* friend */ int
assign2 (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv);

template <class LT, class RT>
/* friend */ int
assignN (Array<LT>& lhs, const Array<RT>& rhs, const LT& rfv);

template <class LT, class RT>
int
assign (Array<LT>& lhs, const Array<RT>& rhs)
{
  return assign (lhs, rhs, resize_fill_value (LT ()));
}

#define INSTANTIATE_ARRAY_ASSIGN(LT, RT) \
  template int assign (Array<LT>&, const Array<RT>&, const LT&); \
  template int assign1 (Array<LT>&, const Array<RT>&, const LT&); \
  template int assign2 (Array<LT>&, const Array<RT>&, const LT&); \
  template int assignN (Array<LT>&, const Array<RT>&, const LT&); \
  template int assign (Array<LT>&, const Array<RT>&)


#define INSTANTIATE_ARRAY(T) \
  template class Array<T>; \
  template T resize_fill_value (const T&); \

#define INSTANTIATE_ARRAY_AND_ASSIGN(T) \
  INSTANTIATE_ARRAY (T); \
  INSTANTIATE_ARRAY_ASSIGN (T, T)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
