// Template sparse classes
/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_Sparse_h)
#define octave_Sparse_h 1

#include <cassert>
#include <cstddef>

#include <iostream>

#include "Array.h"
#include "Array2.h"
#include "dim-vector.h"
#include "lo-utils.h"

class idx_vector;

// Two dimensional sparse class.  Handles the reference counting for
// all the derived classes.

template <class T>
class
Sparse
{
protected:
  //--------------------------------------------------------------------
  // The real representation of all Sparse arrays.
  //--------------------------------------------------------------------

  class SparseRep
  {
  public:

    T *d;
    int *r;
    int *c;
    int nnz;
    int nrows;
    int ncols;
    int count;

    SparseRep (void) : d (0), r (0), c (new int [1]), nnz (0), nrows (0),
		       ncols (0), count (1) { c[0] = 0; }

    SparseRep (int n) : d (0), r (0), c (new int [n+1]), nnz (0), nrows (n),
      ncols (n), count (1)
      { 
	for (int i = 0; i < n + 1; i++)
	  c[i] = 0;
      }

    SparseRep (int nr, int nc) : d (0), r (0), c (new int [nc+1]), nnz (0), 
      nrows (nr), ncols (nc), count (1)
      { 
	for (int i = 0; i < nc + 1; i++)
	  c[i] = 0;
      }

    SparseRep (int nr, int nc, int nz) : d (new T [nz]), 
      r (new int [nz]), c (new int [nc+1]), nnz (nz), nrows (nr), 
      ncols (nc), count (1)
      { 
	for (int i = 0; i < nc + 1; i++)
	  c[i] = 0;
      }

    SparseRep (const SparseRep& a)
      : d (new T [a.nnz]), r (new int [a.nnz]), c (new int [a.ncols + 1]), 
      nnz (a.nnz), nrows (a.nrows), ncols (a.ncols), count (1)
      {
	for (int i = 0; i < nnz; i++)
	  {
	    d[i] = a.d[i];
	    r[i] = a.r[i];
	  }
	for (int i = 0; i < ncols + 1; i++)
	  c[i] = a.c[i];
      }
 
    ~SparseRep (void) { delete [] d; delete [] r; delete [] c; }

    int length (void) const { return nnz; }

    int nonzero (void) const { return c [ncols]; }

    T& elem (int _r, int _c);

    T celem (int _r, int _c) const;

    T& data (int i) { return d[i]; }

    T cdata (int i) const { return d[i]; }

    int& ridx (int i) { return r[i]; }

    int cridx (int i) const { return r[i]; }

    int& cidx (int i) { return c[i]; }

    int ccidx (int i) const { return c[i]; }

    void maybe_compress (bool remove_zeros);

    void change_length (int nz);

  private:

    // No assignment!

    SparseRep& operator = (const SparseRep& a);
  };

  //--------------------------------------------------------------------

  void make_unique (void)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = new SparseRep (*rep);
	}
    }

public:

  // !!! WARNING !!! -- these should be protected, not public.  You
  // should not access these data members directly!

  typename Sparse<T>::SparseRep *rep;

  dim_vector dimensions;

protected:
  idx_vector *idx;
  int idx_count;

private:

  typename Sparse<T>::SparseRep *nil_rep (void) const
    {
      static typename Sparse<T>::SparseRep *nr
	= new typename Sparse<T>::SparseRep ();

      nr->count++;

      return nr;
    }

public:

  Sparse (void)
    : rep (nil_rep ()), dimensions (dim_vector(0,0)),
      idx (0), idx_count (0) { }

  explicit Sparse (int n)
    : rep (new typename Sparse<T>::SparseRep (n)), 
      dimensions (dim_vector (n, n)), idx (0), idx_count (0) { }

  explicit Sparse (int nr, int nc)
    : rep (new typename Sparse<T>::SparseRep (nr, nc)), 
      dimensions (dim_vector (nr, nc)), idx (0), idx_count (0) { }

  explicit Sparse (int nr, int nc, T val);

  Sparse (const dim_vector& dv, int nz)
    : rep (new typename Sparse<T>::SparseRep (dv(0), dv(1), nz)),
    dimensions (dv), idx (0), idx_count (0) { }

  Sparse (int nr, int nc, int nz)
    : rep (new typename Sparse<T>::SparseRep (nr, nc, nz)),
      dimensions (dim_vector (nr, nc)), idx (0), idx_count (0) { }

  // Type conversion case.
  template <class U> Sparse (const Sparse<U>& a);

  // No type conversion case.
  Sparse (const Sparse<T>& a)
    : rep (a.rep), dimensions (a.dimensions), idx (0), idx_count (0)
    {
      rep->count++;
    }

public:

  Sparse (const dim_vector& dv);

  Sparse (const Sparse<T>& a, const dim_vector& dv);

  Sparse (const Array<T>& a, const Array<int>& r, const Array<int>& c,
	  int nr, int nc, bool sum_terms);

  Sparse (const Array<T>& a, const Array<double>& r, const Array<double>& c,
	  int nr, int nc, bool sum_terms);

  // Sparsify a normal matrix
  Sparse (const Array2<T>& a);
  Sparse (const Array<T>& a);

  virtual ~Sparse (void);

  Sparse<T>& operator = (const Sparse<T>& a)
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

  // Note that capacity and nnz are the amount of storage for non-zero
  // elements, while nonzero is the actual number of non-zero terms
  int capacity (void) const { return rep->length (); }
  int nnz (void) const { return capacity (); }
  int nonzero (void) const { return rep->nonzero (); }

  // Paranoid number of elements test for case of dims = (-1,-1)
  int numel (void) const 
    { 
      if (dim1() < 0 || dim2() < 0)
        return 0;
      else
        return dimensions.numel (); 
    }

  int nelem (void) const { return capacity (); }
  int length (void) const { return numel (); }

  int dim1 (void) const { return dimensions(0); }
  int dim2 (void) const { return dimensions(1); }

  int rows (void) const { return dim1 (); }
  int cols (void) const { return dim2 (); }
  int columns (void) const { return dim2 (); }

  int get_row_index (int k) { return ridx (k); }
  int get_col_index (int k)
    {
      int ret = 0;
      while (cidx(ret+1) < k)
        ret++;
      return ret;
    }
  size_t byte_size (void) const { return (cols () + 1) * sizeof (int) +
      capacity () * (sizeof (T) + sizeof (int)); }

  dim_vector dims (void) const { return dimensions; }

  Sparse<T> squeeze (void) const { return *this; }
  
  int compute_index (const Array<int>& ra_idx) const;

  T range_error (const char *fcn, int n) const;
  T& range_error (const char *fcn, int n);

  T range_error (const char *fcn, int i, int j) const;
  T& range_error (const char *fcn, int i, int j);

  T range_error (const char *fcn, const Array<int>& ra_idx) const;
  T& range_error (const char *fcn, const Array<int>& ra_idx);

  // No checking, even for multiple references, ever.

  T& xelem (int n) 
    { 
      int i = n % rows (), j = n / rows(); 
      return xelem (i, j); 
    }

  T xelem (int n) const 
    { 
      int i = n % rows (), j = n / rows(); 
      return xelem (i, j); 
    }
  
  T& xelem (int i, int j) { return rep->elem (i, j); }
  T xelem (int i, int j) const { return rep->celem (i, j); }

  T& xelem (const Array<int>& ra_idx)
    { return xelem (compute_index (ra_idx)); }

  T xelem (const Array<int>& ra_idx) const
    { return xelem (compute_index (ra_idx)); }

  // XXX FIXME XXX -- would be nice to fix this so that we don't
  // unnecessarily force a copy, but that is not so easy, and I see no
  // clean way to do it.

  T& checkelem (int n)
    {
      if (n < 0 || n >= numel ())
	return range_error ("T& Sparse<T>::checkelem", n);
      else
	{
	  make_unique ();
	  return xelem (n);
	}
    }

  T& checkelem (int i, int j)
    {
      if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
	return range_error ("T& Sparse<T>::checkelem", i, j);
      else
	{
	  make_unique ();
	  return xelem (i, j);
	}
    }

  T& checkelem (const Array<int>& ra_idx)
    {
      int i = compute_index (ra_idx);

      if (i < 0)
	return range_error ("T& Sparse<T>::checkelem", ra_idx);
      else
	return elem (i);
    }

  T& elem (int n)
    {
      make_unique ();
      return xelem (n);
    }

  T& elem (int i, int j) 
    { 
      make_unique ();
      return xelem (i, j); 
    }

  T& elem (const Array<int>& ra_idx)
    { return Sparse<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T& operator () (int n) { return checkelem (n); }
  T& operator () (int i, int j) { return checkelem (i, j); }
  T& operator () (const Array<int>& ra_idx) { return checkelem (ra_idx); }
#else
  T& operator () (int n) { return elem (n); }
  T& operator () (int i, int j) { return elem (i, j); }
  T& operator () (const Array<int>& ra_idx) { return elem (ra_idx); }
#endif

  T checkelem (int n) const
    {
      if (n < 0 || n >= numel ())
	return range_error ("T Sparse<T>::checkelem", n);
      else
	return xelem (n);
    }

  T checkelem (int i, int j) const
    {
      if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
	return range_error ("T Sparse<T>::checkelem", i, j);
      else
	return xelem (i, j);
    }

  T checkelem (const Array<int>& ra_idx) const
    {
      int i = compute_index (ra_idx);

      if (i < 0)
	return range_error ("T Sparse<T>::checkelem", ra_idx);
      else
	return Sparse<T>::elem (i);
    }

  T elem (int n) const { return xelem (n); }

  T elem (int i, int j) const { return xelem (i, j); }

  T elem (const Array<int>& ra_idx) const
    { return Sparse<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T operator () (int n) const { return checkelem (n); }
  T operator () (int i, int j) const { return checkelem (i, j); }
  T operator () (const Array<int>& ra_idx) const { return checkelem (ra_idx); }
#else
  T operator () (int n) const { return elem (n); }
  T operator () (int i, int j) const { return elem (i, j); }
  T operator () (const Array<int>& ra_idx) const { return elem (ra_idx); }
#endif

  Sparse<T> maybe_compress (bool remove_zeros = false) 
  { rep->maybe_compress (remove_zeros); return (*this); }

  Sparse<T> reshape (const dim_vector& new_dims) const;

  // !!! WARNING !!! -- the following resize_no_fill functions are 
  // public because template friends don't work properly with versions
  // of gcc earlier than 3.3.  You should use these functions only in 
  // classes that are derived from Sparse<T>.

  // protected:

  void resize_no_fill (int r, int c);

  void resize_no_fill (const dim_vector& dv);

public:
  Sparse<T> permute (const Array<int>& vec, bool inv = false) const;

  Sparse<T> ipermute (const Array<int>& vec) const
    { return permute (vec, true); }

  void resize (int r, int c) { resize_no_fill (r, c); }

  void resize (const dim_vector& dv) { resize_no_fill (dv); }

  void change_capacity (int nz) { rep->change_length (nz); }

  Sparse<T>& insert (const Sparse<T>& a, int r, int c);
  Sparse<T>& insert (const Sparse<T>& a, const Array<int>& idx);

  bool is_square (void) const { return (dim1 () == dim2 ()); }

  bool is_empty (void) const { return (rows () < 1 && cols () < 1); }

  Sparse<T> transpose (void) const;

  T* data (void) { make_unique (); return rep->d; }
  T& data (int i) { make_unique (); return rep->data (i); }
  T* xdata (void) { return rep->d; }
  T& xdata (int i) { return rep->data (i); }

  T data (int i) const { return rep->data (i); }
  T* data (void) const { return rep->d; }

  int* ridx (void) { make_unique (); return rep->r; }
  int& ridx (int i) { make_unique (); return rep->ridx (i); }
  int* xridx (void) { return rep->r; }
  int& xridx (int i) { return rep->ridx (i); }

  int ridx (int i) const { return rep->cridx (i); }
  int* ridx (void) const { return rep->r; }

  int* cidx (void) { make_unique (); return rep->c; }
  int& cidx (int i) { make_unique (); return rep->cidx (i); }
  int* xcidx (void) { return rep->c; }
  int& xcidx (int i) { return rep->cidx (i); }

  int cidx (int i) const { return rep->ccidx (i); }
  int* cidx (void) const { return rep->c; }

  int ndims (void) const { return dimensions.length (); }

  void clear_index (void);

  void set_index (const idx_vector& i);

  int index_count (void) const { return idx_count; }

  idx_vector *get_idx (void) const { return idx; }

  void maybe_delete_elements (idx_vector& i);

  void maybe_delete_elements (idx_vector& i, idx_vector& j);

  void maybe_delete_elements (Array<idx_vector>& ra_idx);

  Sparse<T> value (void);

  Sparse<T> index (idx_vector& i, int resize_ok = 0) const;

  Sparse<T> index (idx_vector& i, idx_vector& j, int resize_ok = 0) const;

  Sparse<T> index (Array<idx_vector>& ra_idx, int resize_ok = 0) const;

  void print_info (std::ostream& os, const std::string& prefix) const;

};

// NOTE: these functions should be friends of the Sparse<T> class and
// Sparse<T>::dimensions should be protected, not public, but we can't
// do that because of bugs in gcc prior to 3.3.

template <class LT, class RT>
/* friend */ int
assign (Sparse<LT>& lhs, const Sparse<RT>& rhs);

template <class LT, class RT>
/* friend */ int
assign1 (Sparse<LT>& lhs, const Sparse<RT>& rhs);

#define INSTANTIATE_SPARSE_ASSIGN(LT, RT) \
  template int assign (Sparse<LT>&, const Sparse<RT>&); \
  template int assign1 (Sparse<LT>&, const Sparse<RT>&);

#define INSTANTIATE_SPARSE(T) \
  template class Sparse<T>;

#define INSTANTIATE_SPARSE_AND_ASSIGN(T) \
  INSTANTIATE_SPARSE (T); \
  INSTANTIATE_SPARSE_ASSIGN (T, T)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
