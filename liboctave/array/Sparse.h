////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_Sparse_h)
#define octave_Sparse_h 1

#include "octave-config.h"

#include <cassert>
#include <cstddef>

#include <algorithm>
#include <iosfwd>
#include <string>

#include "Array-fwd.h"
#include "Sparse-fwd.h"
#include "mx-fwd.h"

// Two dimensional sparse class.  Handles the reference counting for
// all the derived classes.

template <typename T, typename Alloc>
class
OCTAVE_API
Sparse
{
public:

  typedef T element_type;

protected:
  //--------------------------------------------------------------------
  // The real representation of all Sparse arrays.
  //--------------------------------------------------------------------

class SparseRep : public Alloc
  {
  public:

    typedef std::allocator_traits<Alloc> Alloc_traits;

    typedef typename Alloc_traits::template rebind_traits<T> T_Alloc_traits;
    typedef typename T_Alloc_traits::pointer T_pointer;

    typedef typename Alloc_traits::template rebind_traits<octave_idx_type> idx_type_Alloc_traits;
    typedef typename idx_type_Alloc_traits::pointer idx_type_pointer;

    T_pointer m_data;
    idx_type_pointer m_ridx;
    idx_type_pointer m_cidx;
    octave_idx_type m_nzmax;
    octave_idx_type m_nrows;
    octave_idx_type m_ncols;
    octave::refcount<octave_idx_type> m_count;

    SparseRep (void)
      : Alloc (), m_data (T_allocate (1)), m_ridx (idx_type_allocate (1)),
        m_cidx (idx_type_allocate (1)),
        m_nzmax (1), m_nrows (0), m_ncols (0), m_count (1)
    { }

    SparseRep (octave_idx_type n)
      : Alloc (), m_data (T_allocate (1)), m_ridx (idx_type_allocate (1)),
        m_cidx (idx_type_allocate (n+1)),
        m_nzmax (1), m_nrows (n), m_ncols (n), m_count (1)
    { }

    SparseRep (octave_idx_type nr, octave_idx_type nc, octave_idx_type nz = 1)
      : Alloc (), m_data (T_allocate (nz > 0 ? nz : 1)),
        m_ridx (idx_type_allocate (nz > 0 ? nz : 1)),
        m_cidx (idx_type_allocate (nc+1)),
        m_nzmax (nz > 0 ? nz : 1), m_nrows (nr), m_ncols (nc), m_count (1)
    { }

    SparseRep (octave_idx_type nr, octave_idx_type nc, octave_idx_type nz,
               const T *d, const octave_idx_type *r, const octave_idx_type *c)
      : Alloc (), m_data (T_allocate (nz)),
        m_ridx (idx_type_allocate (nz)),
        m_cidx (idx_type_allocate (nc+1)),
        m_nzmax (nz), m_nrows (nr), m_ncols (nc), m_count (1)
    {
      std::copy_n (d, nz, m_data);
      std::copy_n (r, nz, m_ridx);
      std::copy_n (c, m_ncols + 1, m_cidx);
    }

    template <typename U>
    SparseRep (octave_idx_type nr, octave_idx_type nc, octave_idx_type nz,
               const U *d, const octave_idx_type *r, const octave_idx_type *c)
      : Alloc (), m_data (T_allocate (nz)),
        m_ridx (idx_type_allocate (nz)),
        m_cidx (idx_type_allocate (nc+1)),
        m_nzmax (nz), m_nrows (nr), m_ncols (nc), m_count (1)
    {
      std::copy_n (d, nz, m_data);
      std::copy_n (r, nz, m_ridx);
      std::copy_n (c, nc + 1, m_cidx);
    }

    template <typename U>
    SparseRep (const dim_vector& dv, octave_idx_type nz,
               U *d, octave_idx_type *r, octave_idx_type *c,
               const Alloc& xallocator = Alloc ())
      : Alloc (xallocator), m_data (d), m_ridx (r), m_cidx (c),
        m_nzmax (nz), m_nrows (dv(0)), m_ncols (dv(1)), m_count (1)
    { }

    SparseRep (const SparseRep& a)
      : Alloc (), m_data (T_allocate (a.m_nzmax)),
        m_ridx (idx_type_allocate (a.m_nzmax)),
        m_cidx (idx_type_allocate (a.m_ncols + 1)),
        m_nzmax (a.m_nzmax), m_nrows (a.m_nrows), m_ncols (a.m_ncols),
        m_count (1)
    {
      octave_idx_type nz = a.nnz ();
      std::copy_n (a.m_data, nz, m_data);
      std::copy_n (a.m_ridx, nz, m_ridx);
      std::copy_n (a.m_cidx, m_ncols + 1, m_cidx);
    }

    ~SparseRep (void)
    {
      T_deallocate (m_data, m_nzmax);
      idx_type_deallocate (m_ridx, m_nzmax);
      idx_type_deallocate (m_cidx, m_ncols + 1);
    }

    octave_idx_type nzmax (void) const { return m_nzmax; }
    octave_idx_type nnz (void) const { return m_cidx[m_ncols]; }

    octave_idx_type rows (void) const { return m_nrows; }
    octave_idx_type cols (void) const { return m_ncols; }
    octave_idx_type columns (void) const { return m_ncols; }

    OCTAVE_API T& elem (octave_idx_type r, octave_idx_type c);

    OCTAVE_API T celem (octave_idx_type r, octave_idx_type c) const;

    T& data (octave_idx_type i) { return m_data[i]; }

    T cdata (octave_idx_type i) const { return m_data[i]; }

    octave_idx_type& ridx (octave_idx_type i) { return m_ridx[i]; }

    octave_idx_type cridx (octave_idx_type i) const { return m_ridx[i]; }

    octave_idx_type& cidx (octave_idx_type i) { return m_cidx[i]; }

    octave_idx_type ccidx (octave_idx_type i) const { return m_cidx[i]; }

    OCTAVE_API void maybe_compress (bool remove_zeros);

    OCTAVE_API void change_length (octave_idx_type nz);

    OCTAVE_API bool indices_ok (void) const;

    OCTAVE_API bool any_element_is_nan (void) const;

    // Prefer nzmax.
    octave_idx_type length (void) const { return m_nzmax; }

    template <typename U, typename A> friend class Sparse;

    // No assignment!

    SparseRep& operator = (const SparseRep&) = delete;

    T_pointer T_allocate (size_t len)
    {
      typename T_Alloc_traits::allocator_type& alloc = *this;

      T_pointer data = T_Alloc_traits::allocate (alloc, len);
      for (size_t i = 0; i < len; i++)
        T_Alloc_traits::construct (alloc, data+i);

      return data;
    }

    void T_deallocate (T_pointer data, size_t len)
    {
      typename T_Alloc_traits::allocator_type& alloc = *this;

      for (size_t i = 0; i < len; i++)
        T_Alloc_traits::destroy (alloc, data+i);
      T_Alloc_traits::deallocate (alloc, data, len);
    }

    idx_type_pointer idx_type_allocate (size_t len)
    {
      typename idx_type_Alloc_traits::allocator_type alloc = *this;

      idx_type_pointer idx = idx_type_Alloc_traits::allocate (alloc, len);
      for (size_t i = 0; i < len; i++)
        idx_type_Alloc_traits::construct (alloc, idx+i);

      return idx;
    }

    void idx_type_deallocate (idx_type_pointer idx, size_t len)
    {
      typename idx_type_Alloc_traits::allocator_type alloc = *this;

      for (size_t i = 0; i < len; i++)
        idx_type_Alloc_traits::destroy (alloc, idx+i);
      idx_type_Alloc_traits::deallocate (alloc, idx, len);
    }
  };

  //--------------------------------------------------------------------

  void make_unique (void)
  {
    if (m_rep->m_count > 1)
      {
        SparseRep *r = new SparseRep (*m_rep);

        if (--m_rep->m_count == 0)
          delete m_rep;

        m_rep = r;
      }
  }

protected:

  typename Sparse<T, Alloc>::SparseRep *m_rep;

  dim_vector m_dimensions;

private:

  static OCTAVE_API typename Sparse<T, Alloc>::SparseRep * nil_rep (void);

public:

  Sparse (void)
    : m_rep (nil_rep ()), m_dimensions (dim_vector (0, 0))
  {
    m_rep->m_count++;
  }

  explicit Sparse (octave_idx_type n)
    : m_rep (new typename Sparse<T, Alloc>::SparseRep (n)),
      m_dimensions (dim_vector (n, n)) { }

  explicit Sparse (octave_idx_type nr, octave_idx_type nc)
    : m_rep (new typename Sparse<T, Alloc>::SparseRep (nr, nc)),
      m_dimensions (dim_vector (nr, nc)) { }

  explicit OCTAVE_API Sparse (octave_idx_type nr, octave_idx_type nc, T val);

  Sparse (const dim_vector& dv, octave_idx_type nz)
    : m_rep (new typename Sparse<T, Alloc>::SparseRep (dv(0), dv(1), nz)),
      m_dimensions (dv) { }

  Sparse (octave_idx_type nr, octave_idx_type nc, octave_idx_type nz)
    : m_rep (new typename Sparse<T, Alloc>::SparseRep (nr, nc, nz)),
      m_dimensions (dim_vector (nr, nc)) { }

  // Construct a Sparse array from pointers to externally allocated
  // arrays of values and indices.  PTR, RIDX, and CIDX must be
  // allocated with operator new.  The Sparse object takes ownership of
  // these arrays and will delete them when the Sparse object is
  // deleted.  The dimension vector DV must be consistent with the sizes
  // of the allocated PTR, CIDX, and RIDX arrays.

  Sparse (const dim_vector& dv, octave_idx_type nz,
          T *ptr, octave_idx_type *ridx, octave_idx_type *cidx,
          const Alloc& xallocator = Alloc ())
  : m_rep (new typename Sparse<T, Alloc>::SparseRep (dv, nz, ptr, ridx, cidx, xallocator)),
    m_dimensions (dv)
  { }

  // Both SparseMatrix and SparseBoolMatrix need this ctor, and this
  // is their only common ancestor.
  explicit OCTAVE_API Sparse (const PermMatrix& a);

  // Type conversion case.  Preserves nzmax.
  template <typename U>
  Sparse (const Sparse<U>& a)
    : m_rep (new typename Sparse<T, Alloc>::SparseRep (a.rows (), a.cols (),
                                                       a.nzmax (), a.data (),
                                                       a.ridx (), a.cidx ())),
      m_dimensions (a.dims ()) { }

  // No type conversion case.
  Sparse (const Sparse<T, Alloc>& a)
    : m_rep (a.m_rep), m_dimensions (a.m_dimensions)
  {
    m_rep->m_count++;
  }

public:

  OCTAVE_API Sparse (const dim_vector& dv);

  OCTAVE_API Sparse (const Sparse<T, Alloc>& a, const dim_vector& dv);

  OCTAVE_API
  Sparse (const Array<T>& a, const octave::idx_vector& r, const octave::idx_vector& c,
          octave_idx_type nr = -1, octave_idx_type nc = -1,
          bool sum_terms = true, octave_idx_type nzm = -1);

  // Sparsify a normal matrix
  OCTAVE_API Sparse (const Array<T>& a);

  virtual ~Sparse (void);

  OCTAVE_API Sparse<T, Alloc>& operator = (const Sparse<T, Alloc>& a);

  //! Amount of storage for nonzero elements.
  //! This may differ from the actual number of elements, see nnz().
  octave_idx_type nzmax (void) const { return m_rep->nzmax (); }

  //! Actual number of nonzero terms.
  octave_idx_type nnz (void) const { return m_rep->nnz (); }

  // Querying the number of elements (incl. zeros) may overflow the index type,
  // so don't do it unless you really need it.
  octave_idx_type numel (void) const
  {
    return m_dimensions.safe_numel ();
  }

  octave_idx_type dim1 (void) const { return m_dimensions(0); }
  octave_idx_type dim2 (void) const { return m_dimensions(1); }

  octave_idx_type rows (void) const { return dim1 (); }
  octave_idx_type cols (void) const { return dim2 (); }
  octave_idx_type columns (void) const { return dim2 (); }

  octave_idx_type get_row_index (octave_idx_type k) { return ridx (k); }
  octave_idx_type get_col_index (octave_idx_type k)
  {
    octave_idx_type ret = 0;
    while (cidx (ret+1) < k)
      ret++;
    return ret;
  }

  std::size_t byte_size (void) const
  {
    return (static_cast<std::size_t> (cols () + 1) * sizeof (octave_idx_type)
            + static_cast<std::size_t> (nzmax ())
            * (sizeof (T) + sizeof (octave_idx_type)));
  }

  dim_vector dims (void) const { return m_dimensions; }

  Sparse<T, Alloc> squeeze (void) const { return *this; }

  OCTAVE_API octave_idx_type
  compute_index (const Array<octave_idx_type>& ra_idx) const;

  OCTAVE_NORETURN OCTAVE_API T
  range_error (const char *fcn, octave_idx_type n) const;
  OCTAVE_NORETURN OCTAVE_API T&
  range_error (const char *fcn, octave_idx_type n);

  OCTAVE_NORETURN OCTAVE_API T
  range_error (const char *fcn, octave_idx_type i, octave_idx_type j) const;
  OCTAVE_NORETURN OCTAVE_API T&
  range_error (const char *fcn, octave_idx_type i, octave_idx_type j);

  OCTAVE_NORETURN OCTAVE_API T
  range_error (const char *fcn, const Array<octave_idx_type>& ra_idx) const;
  OCTAVE_NORETURN OCTAVE_API T&
  range_error (const char *fcn, const Array<octave_idx_type>& ra_idx);

  // No checking, even for multiple references, ever.

  T& xelem (octave_idx_type n)
  {
    octave_idx_type i = n % rows ();
    octave_idx_type j = n / rows ();
    return xelem (i, j);
  }

  T xelem (octave_idx_type n) const
  {
    octave_idx_type i = n % rows ();
    octave_idx_type j = n / rows ();
    return xelem (i, j);
  }

  T& xelem (octave_idx_type i, octave_idx_type j) { return m_rep->elem (i, j); }
  T xelem (octave_idx_type i, octave_idx_type j) const
  {
    return m_rep->celem (i, j);
  }

  T& xelem (const Array<octave_idx_type>& ra_idx)
  { return xelem (compute_index (ra_idx)); }

  T xelem (const Array<octave_idx_type>& ra_idx) const
  { return xelem (compute_index (ra_idx)); }

  // FIXME: would be nice to fix this so that we don't unnecessarily force a
  // copy, but that is not so easy, and I see no clean way to do it.

  T& checkelem (octave_idx_type n)
  {
    if (n < 0 || n >= numel ())
      range_error ("T& Sparse<T>::checkelem", n);
    else
      {
        make_unique ();
        return xelem (n);
      }
  }

  T& checkelem (octave_idx_type i, octave_idx_type j)
  {
    if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
      range_error ("T& Sparse<T>::checkelem", i, j);
    else
      {
        make_unique ();
        return xelem (i, j);
      }
  }

  T& checkelem (const Array<octave_idx_type>& ra_idx)
  {
    octave_idx_type i = compute_index (ra_idx);

    if (i < 0)
      range_error ("T& Sparse<T>::checkelem", ra_idx);
    else
      return elem (i);
  }

  T& elem (octave_idx_type n)
  {
    make_unique ();
    return xelem (n);
  }

  T& elem (octave_idx_type i, octave_idx_type j)
  {
    make_unique ();
    return xelem (i, j);
  }

  T& elem (const Array<octave_idx_type>& ra_idx)
  { return Sparse<T, Alloc>::elem (compute_index (ra_idx)); }

  T& operator () (octave_idx_type n)
  {
    return elem (n);
  }

  T& operator () (octave_idx_type i, octave_idx_type j)
  {
    return elem (i, j);
  }

  T& operator () (const Array<octave_idx_type>& ra_idx)
  {
    return elem (ra_idx);
  }

  T checkelem (octave_idx_type n) const
  {
    if (n < 0 || n >= numel ())
      range_error ("T Sparse<T>::checkelem", n);
    else
      return xelem (n);
  }

  T checkelem (octave_idx_type i, octave_idx_type j) const
  {
    if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
      range_error ("T Sparse<T>::checkelem", i, j);
    else
      return xelem (i, j);
  }

  T checkelem (const Array<octave_idx_type>& ra_idx) const
  {
    octave_idx_type i = compute_index (ra_idx);

    if (i < 0)
      range_error ("T Sparse<T>::checkelem", ra_idx);
    else
      return Sparse<T, Alloc>::elem (i);
  }

  T elem (octave_idx_type n) const { return xelem (n); }

  T elem (octave_idx_type i, octave_idx_type j) const { return xelem (i, j); }

  T elem (const Array<octave_idx_type>& ra_idx) const
  { return Sparse<T, Alloc>::elem (compute_index (ra_idx)); }

  T operator () (octave_idx_type n) const { return elem (n); }

  T operator () (octave_idx_type i, octave_idx_type j) const
  {
    return elem (i, j);
  }

  T operator () (const Array<octave_idx_type>& ra_idx) const
  {
    return elem (ra_idx);
  }

  Sparse<T, Alloc> maybe_compress (bool remove_zeros = false)
  {
    if (remove_zeros)
      make_unique ();  // Need to unshare because elements are removed.

    m_rep->maybe_compress (remove_zeros);
    return (*this);
  }

  OCTAVE_API Sparse<T, Alloc> reshape (const dim_vector& new_dims) const;

  OCTAVE_API Sparse<T, Alloc>
  permute (const Array<octave_idx_type>& vec, bool inv = false) const;

  Sparse<T, Alloc> ipermute (const Array<octave_idx_type>& vec) const
  {
    return permute (vec, true);
  }

  OCTAVE_API void resize1 (octave_idx_type n);

  OCTAVE_API void resize (octave_idx_type r, octave_idx_type c);

  OCTAVE_API void resize (const dim_vector& dv);

  void change_capacity (octave_idx_type nz)
  {
    if (nz < nnz ())
      make_unique ();  // Unshare now because elements will be truncated.
    m_rep->change_length (nz);
  }

  OCTAVE_API Sparse<T, Alloc>&
  insert (const Sparse<T, Alloc>& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API Sparse<T, Alloc>&
  insert (const Sparse<T, Alloc>& a, const Array<octave_idx_type>& idx);

  bool issquare (void) const { return (dim1 () == dim2 ()); }

  bool isempty (void) const { return (rows () < 1 || cols () < 1); }

  OCTAVE_API Sparse<T, Alloc> transpose (void) const;

  T * data (void) { make_unique (); return m_rep->m_data; }
  T& data (octave_idx_type i) { make_unique (); return m_rep->data (i); }
  T * xdata (void) { return m_rep->m_data; }
  T& xdata (octave_idx_type i) { return m_rep->data (i); }

  T data (octave_idx_type i) const { return m_rep->data (i); }
  // FIXME: shouldn't this be returning const T*?
  T * data (void) const { return m_rep->m_data; }

  octave_idx_type * ridx (void) { make_unique (); return m_rep->m_ridx; }
  octave_idx_type& ridx (octave_idx_type i)
  {
    make_unique (); return m_rep->ridx (i);
  }

  octave_idx_type * xridx (void) { return m_rep->m_ridx; }
  octave_idx_type& xridx (octave_idx_type i) { return m_rep->ridx (i); }

  octave_idx_type ridx (octave_idx_type i) const { return m_rep->cridx (i); }
  // FIXME: shouldn't this be returning const octave_idx_type*?
  octave_idx_type * ridx (void) const { return m_rep->m_ridx; }

  octave_idx_type * cidx (void) { make_unique (); return m_rep->m_cidx; }
  octave_idx_type& cidx (octave_idx_type i)
  {
    make_unique (); return m_rep->cidx (i);
  }

  octave_idx_type * xcidx (void) { return m_rep->m_cidx; }
  octave_idx_type& xcidx (octave_idx_type i) { return m_rep->cidx (i); }

  octave_idx_type cidx (octave_idx_type i) const { return m_rep->ccidx (i); }
  // FIXME: shouldn't this be returning const octave_idx_type*?
  octave_idx_type * cidx (void) const { return m_rep->m_cidx; }

  octave_idx_type ndims (void) const { return m_dimensions.ndims (); }

  OCTAVE_API void delete_elements (const octave::idx_vector& i);

  OCTAVE_API void delete_elements (int dim, const octave::idx_vector& i);

  OCTAVE_API void delete_elements (const octave::idx_vector& i, const octave::idx_vector& j);

  OCTAVE_API Sparse<T, Alloc>
  index (const octave::idx_vector& i, bool resize_ok = false) const;

  OCTAVE_API Sparse<T, Alloc>
  index (const octave::idx_vector& i, const octave::idx_vector& j,
         bool resize_ok = false) const;

  OCTAVE_API void assign (const octave::idx_vector& i,
                          const Sparse<T, Alloc>& rhs);

  OCTAVE_API void assign (const octave::idx_vector& i, const T& rhs);

  OCTAVE_API void
  assign (const octave::idx_vector& i, const octave::idx_vector& j,
          const Sparse<T, Alloc>& rhs);

  OCTAVE_API void
  assign (const octave::idx_vector& i, const octave::idx_vector& j,
          const T& rhs);

  OCTAVE_API void
  print_info (std::ostream& os, const std::string& prefix) const;

  OCTAVE_API Sparse<T, Alloc>
  sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const;
  OCTAVE_API Sparse<T, Alloc>
  sort (Array<octave_idx_type>& sidx, octave_idx_type dim = 0,
        sortmode mode = ASCENDING) const;

  OCTAVE_API Sparse<T, Alloc> diag (octave_idx_type k = 0) const;

  // dim = -1 and dim = -2 are special; see Array<T>::cat description.
  static OCTAVE_API Sparse<T, Alloc>
  cat (int dim, octave_idx_type n, const Sparse<T, Alloc> *sparse_list);

  OCTAVE_API Array<T> array_value (void) const;

  // Generic any/all test functionality with arbitrary predicate.
  template <typename F, bool zero>
  bool test (F fcn) const
  {
    return octave::any_all_test<F, T, zero> (fcn, data (), nnz ());
  }

  // Simpler calls.
  template <typename F>
  bool test_any (F fcn) const
  { return test<F, false> (fcn); }

  template <typename F>
  bool test_all (F fcn) const
  { return test<F, true> (fcn); }

  // Overloads for function references.
  bool test_any (bool (&fcn) (T)) const
  { return test<bool (&) (T), false> (fcn); }

  bool test_any (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), false> (fcn); }

  bool test_all (bool (&fcn) (T)) const
  { return test<bool (&) (T), true> (fcn); }

  bool test_all (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), true> (fcn); }

  template <typename U, typename F>
  Sparse<U>
  map (F fcn) const
  {
    Sparse<U> result;
    U f_zero = fcn (0.0);

    if (f_zero != 0.0)
      {
        octave_idx_type nr = rows ();
        octave_idx_type nc = cols ();

        result = Sparse<U> (nr, nc, f_zero);

        for (octave_idx_type j = 0; j < nc; j++)
          for (octave_idx_type i = cidx (j); i < cidx (j+1); i++)
            {
              octave_quit ();
              /* Use data instead of elem for better performance. */
              result.data (ridx (i) + j * nr) = fcn (data (i));
            }

        result.maybe_compress (true);
      }
    else
      {
        octave_idx_type nz = nnz ();
        octave_idx_type nr = rows ();
        octave_idx_type nc = cols ();

        result = Sparse<U> (nr, nc, nz);
        octave_idx_type ii = 0;
        result.cidx (ii) = 0;

        for (octave_idx_type j = 0; j < nc; j++)
          {
            for (octave_idx_type i = cidx (j); i < cidx (j+1); i++)
              {
                U val = fcn (data (i));
                if (val != 0.0)
                  {
                    result.data (ii) = val;
                    result.ridx (ii++) = ridx (i);
                  }
                octave_quit ();
              }
            result.cidx (j+1) = ii;
          }

        result.maybe_compress (false);
      }

    return result;
  }

  // Overloads for function references.
  template <typename U>
  Sparse<U>
  map (U (&fcn) (T)) const
  { return map<U, U (&) (T)> (fcn); }

  template <typename U>
  Sparse<U>
  map (U (&fcn) (const T&)) const
  { return map<U, U (&) (const T&)> (fcn); }

  bool indices_ok (void) const { return m_rep->indices_ok (); }

  bool any_element_is_nan (void) const
  { return m_rep->any_element_is_nan (); }
};

template <typename T>
OCTAVE_API
std::istream&
read_sparse_matrix (std::istream& is, Sparse<T>& a,
                    T (*read_fcn) (std::istream&));

#endif
