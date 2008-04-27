// Template array classes
/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2001, 2002, 2003,
              2004, 2005, 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_Array_h)
#define octave_Array_h 1

#include <cassert>
#include <cstddef>

#include <iostream>

#include "dim-vector.h"
#include "lo-utils.h"
#include "oct-sort.h"
#include "quit.h"

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
    octave_idx_type len;
    int count;

    ArrayRep (T *d, octave_idx_type l) : data (d), len (l), count (1) { }

    ArrayRep (void) : data (0), len (0), count (1) { }

    explicit ArrayRep (octave_idx_type n) : data (new T [n]), len (n), count (1) { }

    explicit ArrayRep (octave_idx_type n, const T& val)
      : data (new T [n]), len (n), count (1)
      {
	fill (val);
      }

    ArrayRep (const ArrayRep& a)
      : data (new T [a.len]), len (a.len), count (1)
      {
        for (octave_idx_type i = 0; i < len; i++)
	  data[i] = a.data[i];
      }
 
    ~ArrayRep (void) { delete [] data; }

    octave_idx_type length (void) const { return len; }

    void fill (const T& val)
      {
	for (octave_idx_type i = 0; i < len; i++)
	  data[i] = val;
      }

    T& elem (octave_idx_type n) { return data[n]; }

    T elem (octave_idx_type n) const { return data[n]; }

    void qsort (int (*compare) (const void *, const void *))
      {
	octave_qsort (data, static_cast<size_t> (len), sizeof (T), compare);
      }

  private:

    // No assignment!

    ArrayRep& operator = (const ArrayRep& a);
  };

  //--------------------------------------------------------------------

public:

  // !!! WARNING !!! -- these should be protected, not public.  You
  // should not access these methods directly!

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

  typedef T element_type;

  // !!! WARNING !!! -- these should be protected, not public.  You
  // should not access these data members directly!

  typename Array<T>::ArrayRep *rep;

  dim_vector dimensions;

protected:

  mutable idx_vector *idx;
  mutable int idx_count;

  Array (T *d, octave_idx_type n)
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

  explicit Array (octave_idx_type n)
    : rep (new typename Array<T>::ArrayRep (n)), dimensions (n),
      idx (0), idx_count (0) { }

  explicit Array (octave_idx_type n, const T& val)
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

  Array<T>& operator = (const Array<T>& a);

  void fill (const T& val) { make_unique (val); }

  octave_idx_type capacity (void) const { return rep->length (); }
  octave_idx_type length (void) const { return capacity (); }
  octave_idx_type nelem (void) const { return capacity (); }
  octave_idx_type numel (void) const { return nelem (); }

  octave_idx_type dim1 (void) const { return dimensions(0); }
  octave_idx_type dim2 (void) const { return dimensions(1); }
  octave_idx_type dim3 (void) const { return dimensions(2); }

  octave_idx_type rows (void) const { return dim1 (); }
  octave_idx_type cols (void) const { return dim2 (); }
  octave_idx_type columns (void) const { return dim2 (); }
  octave_idx_type pages (void) const { return dim3 (); }

  size_t byte_size (void) const { return numel () * sizeof (T); }

  dim_vector dims (void) const { return dimensions; }

  Array<T> squeeze (void) const;
  
  void chop_trailing_singletons (void) 
  { dimensions.chop_trailing_singletons (); }
  
  static octave_idx_type get_size (octave_idx_type r, octave_idx_type c);
  static octave_idx_type get_size (octave_idx_type r, octave_idx_type c, octave_idx_type p);
  static octave_idx_type get_size (const dim_vector& dv);

  octave_idx_type compute_index (const Array<octave_idx_type>& ra_idx) const;

  T range_error (const char *fcn, octave_idx_type n) const;
  T& range_error (const char *fcn, octave_idx_type n);

  T range_error (const char *fcn, octave_idx_type i, octave_idx_type j) const;
  T& range_error (const char *fcn, octave_idx_type i, octave_idx_type j);

  T range_error (const char *fcn, octave_idx_type i, octave_idx_type j, octave_idx_type k) const;
  T& range_error (const char *fcn, octave_idx_type i, octave_idx_type j, octave_idx_type k);

  T range_error (const char *fcn, const Array<octave_idx_type>& ra_idx) const;
  T& range_error (const char *fcn, const Array<octave_idx_type>& ra_idx);

  // No checking, even for multiple references, ever.

  T& xelem (octave_idx_type n) { return rep->elem (n); }
  T xelem (octave_idx_type n) const { return rep->elem (n); }

  T& xelem (octave_idx_type i, octave_idx_type j) { return xelem (dim1()*j+i); }
  T xelem (octave_idx_type i, octave_idx_type j) const { return xelem (dim1()*j+i); }

  T& xelem (octave_idx_type i, octave_idx_type j, octave_idx_type k) { return xelem (i, dim2()*k+j); }
  T xelem (octave_idx_type i, octave_idx_type j, octave_idx_type k) const { return xelem (i, dim2()*k+j); }

  T& xelem (const Array<octave_idx_type>& ra_idx)
    { return xelem (compute_index (ra_idx)); }

  T xelem (const Array<octave_idx_type>& ra_idx) const
    { return xelem (compute_index (ra_idx)); }

  // FIXME -- would be nice to fix this so that we don't
  // unnecessarily force a copy, but that is not so easy, and I see no
  // clean way to do it.

  T& checkelem (octave_idx_type n)
    {
      if (n < 0 || n >= rep->length ())
	return range_error ("T& Array<T>::checkelem", n);
      else
	{
	  make_unique ();
	  return xelem (n);
	}
    }

  T& checkelem (octave_idx_type i, octave_idx_type j)
    {
      if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
	return range_error ("T& Array<T>::checkelem", i, j);
      else
	return elem (dim1()*j+i);
    }

  T& checkelem (octave_idx_type i, octave_idx_type j, octave_idx_type k)
    {
      if (i < 0 || j < 0 || k < 0 || i >= dim1 () || j >= dim2 () || k >= dim3 ())
	return range_error ("T& Array<T>::checkelem", i, j, k);
      else
	return elem (i, dim2()*k+j);
    }

  T& checkelem (const Array<octave_idx_type>& ra_idx)
    {
      octave_idx_type i = compute_index (ra_idx);

      if (i < 0)
	return range_error ("T& Array<T>::checkelem", ra_idx);
      else
	return elem (i);
    }

  T& elem (octave_idx_type n)
    {
      make_unique ();
      return xelem (n);
    }

  T& elem (octave_idx_type i, octave_idx_type j) { return elem (dim1()*j+i); }

  T& elem (octave_idx_type i, octave_idx_type j, octave_idx_type k) { return elem (i, dim2()*k+j); }

  T& elem (const Array<octave_idx_type>& ra_idx)
    { return Array<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T& operator () (octave_idx_type n) { return checkelem (n); }
  T& operator () (octave_idx_type i, octave_idx_type j) { return checkelem (i, j); }
  T& operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k) { return checkelem (i, j, k); }
  T& operator () (const Array<octave_idx_type>& ra_idx) { return checkelem (ra_idx); }
#else
  T& operator () (octave_idx_type n) { return elem (n); }
  T& operator () (octave_idx_type i, octave_idx_type j) { return elem (i, j); }
  T& operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k) { return elem (i, j, k); }
  T& operator () (const Array<octave_idx_type>& ra_idx) { return elem (ra_idx); }
#endif

  T checkelem (octave_idx_type n) const
    {
      if (n < 0 || n >= rep->length ())
	return range_error ("T Array<T>::checkelem", n);
      else
	return xelem (n);
    }

  T checkelem (octave_idx_type i, octave_idx_type j) const
    {
      if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
	return range_error ("T Array<T>::checkelem", i, j);
      else
	return elem (dim1()*j+i);
    }

  T checkelem (octave_idx_type i, octave_idx_type j, octave_idx_type k) const
    {
      if (i < 0 || j < 0 || k < 0 || i >= dim1 () || j >= dim2 () || k >= dim3 ())
	return range_error ("T Array<T>::checkelem", i, j, k);
      else
	return Array<T>::elem (i, Array<T>::dim1()*k+j);
    }

  T checkelem (const Array<octave_idx_type>& ra_idx) const
    {
      octave_idx_type i = compute_index (ra_idx);

      if (i < 0)
	return range_error ("T Array<T>::checkelem", ra_idx);
      else
	return Array<T>::elem (i);
    }

  T elem (octave_idx_type n) const { return xelem (n); }

  T elem (octave_idx_type i, octave_idx_type j) const { return elem (dim1()*j+i); }

  T elem (octave_idx_type i, octave_idx_type j, octave_idx_type k) const { return elem (i, dim2()*k+j); }

  T elem (const Array<octave_idx_type>& ra_idx) const
    { return Array<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T operator () (octave_idx_type n) const { return checkelem (n); }
  T operator () (octave_idx_type i, octave_idx_type j) const { return checkelem (i, j); }
  T operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k) const { return checkelem (i, j, k); }
  T operator () (const Array<octave_idx_type>& ra_idx) const { return checkelem (ra_idx); }
#else
  T operator () (octave_idx_type n) const { return elem (n); }
  T operator () (octave_idx_type i, octave_idx_type j) const { return elem (i, j); }
  T operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k) const { return elem (i, j, k); }
  T operator () (const Array<octave_idx_type>& ra_idx) const { return elem (ra_idx); }
#endif

  Array<T> reshape (const dim_vector& new_dims) const;

  Array<T> permute (const Array<octave_idx_type>& vec, bool inv = false) const;
  Array<T> ipermute (const Array<octave_idx_type>& vec) const
    { return permute (vec, true); }

  void resize_no_fill (octave_idx_type n);
  void resize_and_fill (octave_idx_type n, const T& val);

  // !!! WARNING !!! -- the following resize_no_fill and
  // resize_and_fill functions are public because template friends
  // don't work properly with versions of gcc earlier than 3.3.  You
  // should use these functions only in classes that are derived
  // from Array<T>.

  // protected:

  void resize_no_fill (octave_idx_type r, octave_idx_type c);
  void resize_and_fill (octave_idx_type r, octave_idx_type c, const T& val);

  void resize_no_fill (octave_idx_type r, octave_idx_type c, octave_idx_type p);
  void resize_and_fill (octave_idx_type r, octave_idx_type c, octave_idx_type p, const T& val);

  void resize_no_fill (const dim_vector& dv);
  void resize_and_fill (const dim_vector& dv, const T& val);

public:

  void resize (octave_idx_type n) { resize_no_fill (n); }

  void resize (octave_idx_type n, const T& val) { resize_and_fill (n, val); }

  void resize (const dim_vector& dv) { resize_no_fill (dv); }

  void resize (const dim_vector& dv, const T& val)
    { resize_and_fill (dv, val); }

  Array<T>& insert (const Array<T>& a, octave_idx_type r, octave_idx_type c);
  Array<T>& insert2 (const Array<T>& a, octave_idx_type r, octave_idx_type c);
  Array<T>& insertN (const Array<T>& a, octave_idx_type r, octave_idx_type c);

  Array<T>& insert (const Array<T>& a, const Array<octave_idx_type>& idx);

  bool is_square (void) const { return (dim1 () == dim2 ()); }

  bool is_empty (void) const { return numel () == 0; }

  Array<T> transpose (void) const;
  Array<T> hermitian (T (*fcn) (const T&) = 0) const;

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

  void clear_index (void) const;

  void set_index (const idx_vector& i) const;

  int index_count (void) const { return idx_count; }

  idx_vector *get_idx (void) const { return idx; }

  void maybe_delete_elements (idx_vector& i);

  void maybe_delete_elements_1 (idx_vector& i);

  void maybe_delete_elements_2 (idx_vector& i);

  void maybe_delete_elements (idx_vector& i, idx_vector& j);

  void maybe_delete_elements (idx_vector& i, idx_vector& j, idx_vector& k);

  void maybe_delete_elements (Array<idx_vector>& ra_idx, const T& rfv);

  Array<T> value (void) const;

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

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return const_cast<T *> (data ()); }

  Array<T> sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const;
  Array<T> sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
		 sortmode mode = ASCENDING) const;

  Array<T> diag (octave_idx_type k = 0) const;

  template <class U, class F>
  Array<U>
  map (F fcn) const
  {
    octave_idx_type len = length ();

    const T *m = data ();

    Array<U> result (dims ());
    U *p = result.fortran_vec ();

    for (octave_idx_type i = 0; i < len; i++)
      {
	OCTAVE_QUIT;

	p[i] = fcn (m[i]);
      }

    return result;
  }
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

#define INSTANTIATE_ARRAY_ASSIGN(LT, RT, API) \
  template API int assign (Array<LT>&, const Array<RT>&, const LT&); \
  template API int assign1 (Array<LT>&, const Array<RT>&, const LT&); \
  template API int assign2 (Array<LT>&, const Array<RT>&, const LT&); \
  template API int assignN (Array<LT>&, const Array<RT>&, const LT&); \
  template API int assign (Array<LT>&, const Array<RT>&)


#define INSTANTIATE_ARRAY(T, API) \
  template class API Array<T>; \
  template API T resize_fill_value (const T&); \

#define INSTANTIATE_ARRAY_AND_ASSIGN(T, API) \
  INSTANTIATE_ARRAY (T, API); \
  INSTANTIATE_ARRAY_ASSIGN (T, T, API)

#define INSTANTIATE_ARRAY_SORT(T) \
  template class octave_sort<T>; \
  template class vec_index<T>; \
  template class octave_sort<vec_index<T> *>;

#define NO_INSTANTIATE_ARRAY_SORT(T) \
  template class vec_index<T>; \
  template <> bool ascending_compare (T, T) { return true; } \
  template <> bool ascending_compare (vec_index<T> *, vec_index<T> *) \
    { return true; } \
  template <> bool descending_compare (T, T) { return true; } \
  template <> bool descending_compare (vec_index<T> *, vec_index<T> *) \
    { return true; } \
  template <> Array<T> Array<T>::sort \
    (octave_idx_type, sortmode) const { return *this; } \
  template <> Array<T> Array<T>::sort (Array<octave_idx_type> &sidx, \
    octave_idx_type, sortmode) const \
    { sidx = Array<octave_idx_type> (); return *this; }

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
