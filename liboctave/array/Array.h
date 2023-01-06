////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_Array_h)
#define octave_Array_h 1

#include "octave-config.h"

#include <cassert>
#include <cstddef>

#include <algorithm>
#include <iosfwd>
#include <string>

#include "Array-fwd.h"
#include "dim-vector.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "lo-traits.h"
#include "lo-utils.h"
#include "oct-refcount.h"
#include "oct-sort.h"
#include "quit.h"

//! N Dimensional Array with copy-on-write semantics.
//!
//! The Array class is at the root of Octave.  It provides a container
//! with an arbitrary number of dimensions.  The operator () provides
//! access to individual elements via subscript and linear indexing.
//! Indexing starts at 0.  Arrays are column-major order as in Fortran.
//!
//! @code{.cc}
//! // 3 D Array with 10 rows, 20 columns, and 5 pages, filled with 7.0
//! Array<double> A (dim_vector (10, 20, 5), 7.0);
//!
//! // set value for row 0, column 10, and page 3
//! A(0, 10, 3) = 2.5;
//!
//! // get value for row 1, column 2, and page 0
//! double v = A(1, 2, 0);
//!
//! // get value for 25th element (row 4, column 3, page 1)
//! double v = A(24);
//! @endcode
//!
//! ## Notes on STL compatibility
//!
//! ### size() and length()
//!
//! To access the total number of elements in an Array, use numel()
//! which is short for number of elements and is equivalent to the
//! Octave function with same name.
//!
//! @code{.cc}
//! Array<int> A (dim_vector (10, 20, 4), 1);
//!
//! octave_idx_type n = A.numel (); // returns 800 (10x20x4)
//!
//! octave_idx_type nr = A.size (0); // returns 10 (number of rows/dimension 0)
//! octave_idx_type nc = A.size (1); // returns 20 (number of columns)
//! octave_idx_type nc = A.size (2); // returns 4 (size of dimension 3)
//! octave_idx_type l6 = A.size (6); // returns 1 (implicit singleton dimension)
//!
//! // Alternatively, get a dim_vector which represents the dimensions.
//! dim_vector dims = A.dims ();
//! @endcode
//!
//! The methods size() and length() as they exist in the STL cause
//! confusion in the context of a N dimensional array.
//!
//! The size() of an array is the length of all dimensions.  In Octave,
//! the size() function returns a row vector with the length of each
//! dimension, or the size of a specific dimension.  Only the latter is
//! present in liboctave.
//!
//! Since there is more than 1 dimension, length() would not make sense
//! without expliciting which dimension.  If the function existed, which
//! length should it return?  Octave length() function returns the length
//! of the longest dimension which is an odd definition, only useful for
//! vectors and square matrices.  The alternatives numel(), rows(),
//! columns(), and size(d) are more explicit and recommended.
//!
//! ### size_type
//!
//! Array::size_type is 'octave_idx_type' which is a typedef for 'int'
//! or 'long int', depending whether Octave was configured for 64-bit
//! indexing.
//!
//! This is a signed integer which may cause problems when mixed with
//! STL containers.  The reason is that Octave interacts with Fortran
//! routines, providing an interface many Fortran numeric libraries.
//!
//! ## Subclasses
//!
//! The following subclasses specializations, will be of most use:
//!   - Matrix: Array<double> with only 2 dimensions
//!   - ComplexMatrix: Array<std::complex<double>> with only 2 dimensions
//!   - boolNDArray: N dimensional Array<bool>
//!   - ColumnVector: Array<double> with 1 column
//!   - string_vector: Array<std::string> with 1 column
//!   - Cell: Array<octave_value>, equivalent to an Octave cell.

template <typename T, typename Alloc>
class
OCTARRAY_TEMPLATE_API
Array
{
protected:

  //! The real representation of all arrays.
  class ArrayRep : public Alloc
  {
  public:

    typedef std::allocator_traits<Alloc> Alloc_traits;

    typedef typename Alloc_traits::template rebind_traits<T> T_Alloc_traits;
    typedef typename T_Alloc_traits::pointer pointer;

    pointer m_data;
    octave_idx_type m_len;
    octave::refcount<octave_idx_type> m_count;

    ArrayRep (pointer d, octave_idx_type len)
      : Alloc (), m_data (allocate (len)), m_len (len), m_count (1)
    {
      std::copy_n (d, len, m_data);
    }

    template <typename U>
    ArrayRep (U *d, octave_idx_type len)
      : Alloc (), m_data (allocate (len)), m_len (len), m_count (1)
    {
      std::copy_n (d, len, m_data);
    }

    // Use new instead of setting data to 0 so that fortran_vec and
    // data always return valid addresses, even for zero-size arrays.

    ArrayRep (void)
      : Alloc (), m_data (allocate (0)), m_len (0), m_count (1) { }

    explicit ArrayRep (octave_idx_type len)
      : Alloc (), m_data (allocate (len)), m_len (len), m_count (1) { }

    explicit ArrayRep (octave_idx_type len, const T& val)
      : Alloc (), m_data (allocate (len)), m_len (len), m_count (1)
    {
      std::fill_n (m_data, len, val);
    }

    explicit ArrayRep (pointer ptr, const dim_vector& dv,
                       const Alloc& xallocator = Alloc ())
      : Alloc (xallocator), m_data (ptr), m_len (dv.safe_numel ()), m_count (1)
    { }

    // FIXME: Should the allocator be copied or created with the default?
    ArrayRep (const ArrayRep& a)
      : Alloc (), m_data (allocate (a.m_len)), m_len (a.m_len),
        m_count (1)
    {
      std::copy_n (a.m_data, a.m_len, m_data);
    }

    ~ArrayRep (void) { deallocate (m_data, m_len); }

    octave_idx_type numel (void) const { return m_len; }

    // No assignment!

    ArrayRep& operator = (const ArrayRep&) = delete;

    pointer allocate (size_t len)
    {
      pointer data = Alloc_traits::allocate (*this, len);
      for (size_t i = 0; i < len; i++)
        T_Alloc_traits::construct (*this, data+i);
      return data;
    }

    void deallocate (pointer data, size_t len)
    {
      for (size_t i = 0; i < len; i++)
        T_Alloc_traits::destroy (*this, data+i);
      Alloc_traits::deallocate (*this, data, len);
    }
  };

  //--------------------------------------------------------------------

public:

  OCTARRAY_OVERRIDABLE_FUNC_API void make_unique (void)
  {
    if (m_rep->m_count > 1)
      {
        ArrayRep *r = new ArrayRep (m_slice_data, m_slice_len);

        if (--m_rep->m_count == 0)
          delete m_rep;

        m_rep = r;
        m_slice_data = m_rep->m_data;
      }
  }

  typedef T element_type;

  typedef T value_type;

  //! Used for operator(), and returned by numel() and size()
  //! (beware: signed integer)
  typedef octave_idx_type size_type;

  typedef typename ref_param<T>::type crefT;

  typedef bool (*compare_fcn_type) (typename ref_param<T>::type,
                                    typename ref_param<T>::type);

protected:

  dim_vector m_dimensions;

  typename Array<T, Alloc>::ArrayRep *m_rep;

  // Rationale:
  // m_slice_data is a pointer to m_rep->m_data, denoting together with m_slice_len the
  // actual portion of the data referenced by this Array<T> object.  This
  // allows to make shallow copies not only of a whole array, but also of
  // contiguous subranges.  Every time m_rep is directly manipulated, m_slice_data
  // and m_slice_len need to be properly updated.

  T *m_slice_data;
  octave_idx_type m_slice_len;

  //! slice constructor
  Array (const Array<T, Alloc>& a, const dim_vector& dv,
         octave_idx_type l, octave_idx_type u)
    : m_dimensions (dv), m_rep(a.m_rep), m_slice_data (a.m_slice_data+l), m_slice_len (u-l)
  {
    m_rep->m_count++;
    m_dimensions.chop_trailing_singletons ();
  }

private:

  static OCTARRAY_API typename Array<T, Alloc>::ArrayRep *nil_rep (void);

public:

  //! Empty ctor (0 by 0).
  Array (void)
    : m_dimensions (), m_rep (nil_rep ()), m_slice_data (m_rep->m_data),
      m_slice_len (m_rep->m_len)
  {
    m_rep->m_count++;
  }

  //! nD uninitialized ctor.
  explicit Array (const dim_vector& dv)
    : m_dimensions (dv),
      m_rep (new typename Array<T, Alloc>::ArrayRep (dv.safe_numel ())),
      m_slice_data (m_rep->m_data), m_slice_len (m_rep->m_len)
  {
    m_dimensions.chop_trailing_singletons ();
  }

  //! nD initialized ctor.
  explicit Array (const dim_vector& dv, const T& val)
    : m_dimensions (dv),
      m_rep (new typename Array<T, Alloc>::ArrayRep (dv.safe_numel ())),
      m_slice_data (m_rep->m_data), m_slice_len (m_rep->m_len)
  {
    fill (val);
    m_dimensions.chop_trailing_singletons ();
  }

  // Construct an Array from a pointer to an externally allocated array
  // of values.  PTR must be allocated with operator new.  The Array
  // object takes ownership of PTR and will delete it when the Array
  // object is deleted.  The dimension vector DV must be consistent with
  // the size of the allocated PTR array.

  OCTARRAY_OVERRIDABLE_FUNC_API
  explicit Array (T *ptr, const dim_vector& dv,
                  const Alloc& xallocator = Alloc ())
    : m_dimensions (dv),
      m_rep (new typename Array<T, Alloc>::ArrayRep (ptr, dv, xallocator)),
      m_slice_data (m_rep->m_data), m_slice_len (m_rep->m_len)
  {
    m_dimensions.chop_trailing_singletons ();
  }

  //! Reshape constructor.
  OCTARRAY_API Array (const Array<T, Alloc>& a, const dim_vector& dv);

  //! Constructor from standard library sequence containers.
  template<template <typename...> class Container>
  Array (const Container<T>& a, const dim_vector& dv);

  //! Type conversion case.
  template <typename U, typename A = Alloc>
  Array (const Array<U, A>& a)
    : m_dimensions (a.dims ()),
      m_rep (new typename Array<T, Alloc>::ArrayRep (a.data (), a.numel ())),
      m_slice_data (m_rep->m_data), m_slice_len (m_rep->m_len)
  { }

  //! No type conversion case.
  Array (const Array<T, Alloc>& a)
    : m_dimensions (a.m_dimensions), m_rep (a.m_rep), m_slice_data (a.m_slice_data),
      m_slice_len (a.m_slice_len)
  {
    m_rep->m_count++;
  }

  Array (Array<T, Alloc>&& a)
    : m_dimensions (std::move (a.m_dimensions)), m_rep (a.m_rep),
      m_slice_data (a.m_slice_data), m_slice_len (a.m_slice_len)
  {
    a.m_rep = nullptr;
    a.m_slice_data = nullptr;
    a.m_slice_len = 0;
  }

public:

  virtual ~Array (void)
  {
    // Because we define a move constructor and a move assignment
    // operator, m_rep may be a nullptr here.  We should only need to
    // protect the move assignment operator in a similar way.

    if (m_rep && --m_rep->m_count == 0)
      delete m_rep;
  }

  Array<T, Alloc>& operator = (const Array<T, Alloc>& a)
  {
    if (this != &a)
      {
        if (--m_rep->m_count == 0)
          delete m_rep;

        m_rep = a.m_rep;
        m_rep->m_count++;

        m_dimensions = a.m_dimensions;
        m_slice_data = a.m_slice_data;
        m_slice_len = a.m_slice_len;
      }

    return *this;
  }

  Array<T, Alloc>& operator = (Array<T, Alloc>&& a)
  {
    if (this != &a)
      {
        m_dimensions = std::move (a.m_dimensions);

        // Because we define a move constructor and a move assignment
        // operator, m_rep may be a nullptr here.  We should only need to
        // protect the destructor in a similar way.

        if (m_rep && --m_rep->m_count == 0)
          delete m_rep;

        m_rep = a.m_rep;
        m_slice_data = a.m_slice_data;
        m_slice_len = a.m_slice_len;

        a.m_rep = nullptr;
        a.m_slice_data = nullptr;
        a.m_slice_len = 0;
      }

    return *this;
  }

  OCTARRAY_API void fill (const T& val);

  OCTARRAY_API void clear (void);
  OCTARRAY_API void clear (const dim_vector& dv);

  void clear (octave_idx_type r, octave_idx_type c)
  { clear (dim_vector (r, c)); }

  //! Number of elements in the array.
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type
  numel (void) const
  { return m_slice_len; }
  //@}

  //! Return the array as a column vector.
  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  as_column (void) const
  {
    Array<T, Alloc> retval (*this);
    if (m_dimensions.ndims () != 2 || m_dimensions(1) != 1)
      retval.m_dimensions = dim_vector (numel (), 1);

    return retval;
  }

  //! Return the array as a row vector.
  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  as_row (void) const
  {
    Array<T, Alloc> retval (*this);
    if (m_dimensions.ndims () != 2 || m_dimensions(0) != 1)
      retval.m_dimensions = dim_vector (1, numel ());

    return retval;
  }

  //! Return the array as a matrix.
  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  as_matrix (void) const
  {
    Array<T, Alloc> retval (*this);
    if (m_dimensions.ndims () != 2)
      retval.m_dimensions = m_dimensions.redim (2);

    return retval;
  }

  //! @name First dimension
  //!
  //! Get the first dimension of the array (number of rows)
  //@{
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type
  dim1 (void) const
  { return m_dimensions(0); }
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type
  rows (void) const
  { return dim1 (); }
  //@}

  //! @name Second dimension
  //!
  //! Get the second dimension of the array (number of columns)
  //@{
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type dim2 (void) const
  { return m_dimensions(1); }
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type cols (void) const
  { return dim2 (); }
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type columns (void) const
  { return dim2 (); }
  //@}

  //! @name Third dimension
  //!
  //! Get the third dimension of the array (number of pages)
  //@{
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type dim3 (void) const
  { return m_dimensions.ndims () >= 3 ? m_dimensions(2) : 1; }
  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type pages (void) const
  { return dim3 (); }
  //@}

  //! Size of the specified dimension.
  //!
  //! Dimensions beyond the Array number of dimensions return 1 as
  //! those are implicit singleton dimensions.
  //!
  //! Equivalent to Octave's 'size (A, DIM)'

  OCTARRAY_OVERRIDABLE_FUNC_API size_type size (const size_type d) const
  {
    // Should we throw for negative values?
    // Should >= ndims () be handled by dim_vector operator() instead ?
    return d >= ndims () ? 1 : m_dimensions(d);
  }

  OCTARRAY_OVERRIDABLE_FUNC_API std::size_t byte_size (void) const
  { return static_cast<std::size_t> (numel ()) * sizeof (T); }

  //! Return a const-reference so that dims ()(i) works efficiently.
  OCTARRAY_OVERRIDABLE_FUNC_API const dim_vector& dims (void) const
  { return m_dimensions; }

  //! Chop off leading singleton dimensions
  OCTARRAY_API Array<T, Alloc> squeeze (void) const;

  OCTARRAY_API octave_idx_type
  compute_index (octave_idx_type i, octave_idx_type j) const;
  OCTARRAY_API octave_idx_type
  compute_index (octave_idx_type i, octave_idx_type j, octave_idx_type k) const;
  OCTARRAY_API octave_idx_type
  compute_index (const Array<octave_idx_type>& ra_idx) const;

  OCTARRAY_OVERRIDABLE_FUNC_API octave_idx_type
  compute_index_unchecked (const Array<octave_idx_type>& ra_idx) const
  {
    return m_dimensions.compute_index (ra_idx.data (), ra_idx.numel ());
  }

  // No checking, even for multiple references, ever.

  OCTARRAY_OVERRIDABLE_FUNC_API T& xelem (octave_idx_type n)
  { return m_slice_data[n]; }
  OCTARRAY_OVERRIDABLE_FUNC_API crefT xelem (octave_idx_type n) const
  { return m_slice_data[n]; }

  OCTARRAY_OVERRIDABLE_FUNC_API T&
  xelem (octave_idx_type i, octave_idx_type j)
  { return xelem (dim1 ()*j+i); }
  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  xelem (octave_idx_type i, octave_idx_type j) const
  { return xelem (dim1 ()*j+i); }

  OCTARRAY_OVERRIDABLE_FUNC_API T&
  xelem (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return xelem (i, dim2 ()*k+j); }
  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  xelem (octave_idx_type i, octave_idx_type j, octave_idx_type k) const
  { return xelem (i, dim2 ()*k+j); }

  OCTARRAY_OVERRIDABLE_FUNC_API T&
  xelem (const Array<octave_idx_type>& ra_idx)
  { return xelem (compute_index_unchecked (ra_idx)); }

  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  xelem (const Array<octave_idx_type>& ra_idx) const
  { return xelem (compute_index_unchecked (ra_idx)); }

  // FIXME: would be nice to fix this so that we don't unnecessarily force
  //        a copy, but that is not so easy, and I see no clean way to do it.

  OCTARRAY_API T& checkelem (octave_idx_type n);

  OCTARRAY_API T& checkelem (octave_idx_type i, octave_idx_type j);

  OCTARRAY_API T& checkelem (octave_idx_type i, octave_idx_type j, octave_idx_type k);

  OCTARRAY_API T& checkelem (const Array<octave_idx_type>& ra_idx);

  OCTARRAY_OVERRIDABLE_FUNC_API T& elem (octave_idx_type n)
  {
    make_unique ();
    return xelem (n);
  }

  OCTARRAY_OVERRIDABLE_FUNC_API T& elem (octave_idx_type i, octave_idx_type j)
  { return elem (dim1 ()*j+i); }

  OCTARRAY_OVERRIDABLE_FUNC_API T& elem (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return elem (i, dim2 ()*k+j); }

  OCTARRAY_OVERRIDABLE_FUNC_API T& elem (const Array<octave_idx_type>& ra_idx)
  { return Array<T, Alloc>::elem (compute_index_unchecked (ra_idx)); }

  OCTARRAY_OVERRIDABLE_FUNC_API T& operator () (octave_idx_type n)
  { return elem (n); }
  OCTARRAY_OVERRIDABLE_FUNC_API T& operator () (octave_idx_type i, octave_idx_type j)
  { return elem (i, j); }
  OCTARRAY_OVERRIDABLE_FUNC_API T& operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return elem (i, j, k); }
  OCTARRAY_OVERRIDABLE_FUNC_API T& operator () (const Array<octave_idx_type>& ra_idx)
  { return elem (ra_idx); }

  OCTARRAY_API crefT checkelem (octave_idx_type n) const;

  OCTARRAY_API crefT checkelem (octave_idx_type i, octave_idx_type j) const;

  OCTARRAY_API crefT checkelem (octave_idx_type i, octave_idx_type j,
                                octave_idx_type k) const;

  OCTARRAY_API crefT checkelem (const Array<octave_idx_type>& ra_idx) const;

  OCTARRAY_OVERRIDABLE_FUNC_API crefT elem (octave_idx_type n) const
  { return xelem (n); }

  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  elem (octave_idx_type i, octave_idx_type j) const
  { return xelem (i, j); }

  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  elem (octave_idx_type i, octave_idx_type j, octave_idx_type k) const
  { return xelem (i, j, k); }

  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  elem (const Array<octave_idx_type>& ra_idx) const
  { return Array<T, Alloc>::xelem (compute_index_unchecked (ra_idx)); }

  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  operator () (octave_idx_type n) const { return elem (n); }
  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  operator () (octave_idx_type i, octave_idx_type j) const
  { return elem (i, j); }
  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k) const
  { return elem (i, j, k); }
  OCTARRAY_OVERRIDABLE_FUNC_API crefT
  operator () (const Array<octave_idx_type>& ra_idx) const
  { return elem (ra_idx); }

  // Fast extractors.  All of these produce shallow copies.

  //! Extract column: A(:,k+1).
  OCTARRAY_API Array<T, Alloc> column (octave_idx_type k) const;
  //! Extract page: A(:,:,k+1).
  OCTARRAY_API Array<T, Alloc> page (octave_idx_type k) const;

  //! Extract a slice from this array as a column vector: A(:)(lo+1:up).
  //! Must be 0 <= lo && up <= numel.  May be up < lo.
  OCTARRAY_API Array<T, Alloc>
  linear_slice (octave_idx_type lo, octave_idx_type up) const;

  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  reshape (octave_idx_type nr, octave_idx_type nc) const
  { return Array<T, Alloc> (*this, dim_vector (nr, nc)); }

  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  reshape (const dim_vector& new_dims) const
  { return Array<T, Alloc> (*this, new_dims); }

  OCTARRAY_API Array<T, Alloc>
  permute (const Array<octave_idx_type>& vec, bool inv = false) const;
  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  ipermute (const Array<octave_idx_type>& vec) const
  { return permute (vec, true); }

  OCTARRAY_OVERRIDABLE_FUNC_API bool issquare (void) const
  { return (dim1 () == dim2 ()); }

  OCTARRAY_OVERRIDABLE_FUNC_API bool isempty (void) const
  { return numel () == 0; }

  OCTARRAY_OVERRIDABLE_FUNC_API bool isvector (void) const
  { return m_dimensions.isvector (); }

  OCTARRAY_OVERRIDABLE_FUNC_API bool is_nd_vector (void) const
  { return m_dimensions.is_nd_vector (); }

  OCTARRAY_API Array<T, Alloc> transpose (void) const;
  OCTARRAY_API Array<T, Alloc> hermitian (T (*fcn) (const T&) = nullptr) const;

  OCTARRAY_OVERRIDABLE_FUNC_API const T * data (void) const
  { return m_slice_data; }

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "for read-only access, use 'data' method instead")
  OCTARRAY_OVERRIDABLE_FUNC_API const T * fortran_vec (void) const
  { return data (); }
#endif

  OCTARRAY_API T * fortran_vec (void);

  OCTARRAY_OVERRIDABLE_FUNC_API bool is_shared (void) const
  { return m_rep->m_count > 1; }

  OCTARRAY_OVERRIDABLE_FUNC_API int ndims (void) const
  { return m_dimensions.ndims (); }

  //@{
  //! Indexing without resizing.
  OCTARRAY_API Array<T, Alloc> index (const octave::idx_vector& i) const;

  OCTARRAY_API Array<T, Alloc> index (const octave::idx_vector& i, const octave::idx_vector& j) const;

  OCTARRAY_API Array<T, Alloc> index (const Array<octave::idx_vector>& ia) const;
  //@}

  virtual OCTARRAY_API T resize_fill_value (void) const;

  //@{
  //! Resizing (with fill).
  OCTARRAY_API void resize2 (octave_idx_type nr, octave_idx_type nc, const T& rfv);
  OCTARRAY_OVERRIDABLE_FUNC_API void resize2 (octave_idx_type nr, octave_idx_type nc)
  {
    resize2 (nr, nc, resize_fill_value ());
  }

  OCTARRAY_API void resize1 (octave_idx_type n, const T& rfv);
  OCTARRAY_OVERRIDABLE_FUNC_API void resize1 (octave_idx_type n)
  { resize1 (n, resize_fill_value ()); }

  OCTARRAY_API void resize (const dim_vector& dv, const T& rfv);
  OCTARRAY_OVERRIDABLE_FUNC_API void resize (const dim_vector& dv)
  { resize (dv, resize_fill_value ()); }
  //@}

  //@{
  //! Indexing with possible resizing and fill

  // FIXME: this is really a corner case, that should better be
  // handled directly in liboctinterp.

  OCTARRAY_API Array<T, Alloc>
  index (const octave::idx_vector& i, bool resize_ok, const T& rfv) const;
  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  index (const octave::idx_vector& i, bool resize_ok) const
  {
    return index (i, resize_ok, resize_fill_value ());
  }

  OCTARRAY_API Array<T, Alloc>
  index (const octave::idx_vector& i, const octave::idx_vector& j,
         bool resize_ok, const T& rfv) const;
  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  index (const octave::idx_vector& i, const octave::idx_vector& j,
         bool resize_ok) const
  {
    return index (i, j, resize_ok, resize_fill_value ());
  }

  OCTARRAY_API Array<T, Alloc>
  index (const Array<octave::idx_vector>& ia, bool resize_ok,
         const T& rfv) const;
  OCTARRAY_OVERRIDABLE_FUNC_API Array<T, Alloc>
  index (const Array<octave::idx_vector>& ia, bool resize_ok) const
  {
    return index (ia, resize_ok, resize_fill_value ());
  }
  //@}

  //@{
  //! Indexed assignment (always with resize & fill).
  OCTARRAY_API void
  assign (const octave::idx_vector& i, const Array<T, Alloc>& rhs, const T& rfv);
  OCTARRAY_OVERRIDABLE_FUNC_API void
  assign (const octave::idx_vector& i, const Array<T, Alloc>& rhs)
  {
    assign (i, rhs, resize_fill_value ());
  }

  OCTARRAY_API void
  assign (const octave::idx_vector& i, const octave::idx_vector& j,
          const Array<T, Alloc>& rhs, const T& rfv);
  OCTARRAY_OVERRIDABLE_FUNC_API void
  assign (const octave::idx_vector& i, const octave::idx_vector& j,
          const Array<T, Alloc>& rhs)
  {
    assign (i, j, rhs, resize_fill_value ());
  }

  OCTARRAY_API void
  assign (const Array<octave::idx_vector>& ia, const Array<T, Alloc>& rhs, const T& rfv);
  OCTARRAY_OVERRIDABLE_FUNC_API void
  assign (const Array<octave::idx_vector>& ia, const Array<T, Alloc>& rhs)
  {
    assign (ia, rhs, resize_fill_value ());
  }
  //@}

  //@{
  //! Deleting elements.

  //! A(I) = [] (with a single subscript)
  OCTARRAY_API void delete_elements (const octave::idx_vector& i);

  //! A(:,...,I,...,:) = [] (>= 2 subscripts, one of them is non-colon)
  OCTARRAY_API void delete_elements (int dim, const octave::idx_vector& i);

  //! Dispatcher to the above two.
  OCTARRAY_API void delete_elements (const Array<octave::idx_vector>& ia);
  //@}

  //! Insert an array into another at a specified position.  If
  //! size (a) is [d1 d2 ... dN] and idx is [i1 i2 ... iN], this
  //! method is equivalent to x(i1:i1+d1-1, i2:i2+d2-1, ... ,
  //! iN:iN+dN-1) = a.
  OCTARRAY_API Array<T, Alloc>&
  insert (const Array<T, Alloc>& a, const Array<octave_idx_type>& idx);

  //! This is just a special case for idx = [r c 0 ...]
  OCTARRAY_API Array<T, Alloc>&
  insert (const Array<T, Alloc>& a, octave_idx_type r, octave_idx_type c);

  OCTARRAY_OVERRIDABLE_FUNC_API void maybe_economize (void)
  {
    if (m_rep->m_count == 1 && m_slice_len != m_rep->m_len)
      {
        ArrayRep *new_rep = new ArrayRep (m_slice_data, m_slice_len);
        delete m_rep;
        m_rep = new_rep;
        m_slice_data = m_rep->m_data;
      }
  }

  OCTARRAY_API void print_info (std::ostream& os, const std::string& prefix) const;

  OCTARRAY_API Array<T, Alloc> sort (int dim = 0, sortmode mode = ASCENDING) const;
  OCTARRAY_API Array<T, Alloc> sort (Array<octave_idx_type>& sidx, int dim = 0,
                                     sortmode mode = ASCENDING) const;

  //! Ordering is auto-detected or can be specified.
  OCTARRAY_API sortmode issorted (sortmode mode = UNSORTED) const;

  //! Sort by rows returns only indices.
  OCTARRAY_API Array<octave_idx_type> sort_rows_idx (sortmode mode = ASCENDING) const;

  //! Ordering is auto-detected or can be specified.
  OCTARRAY_API sortmode is_sorted_rows (sortmode mode = UNSORTED) const;

  //! Do a binary lookup in a sorted array.  Must not contain NaNs.
  //! Mode can be specified or is auto-detected by comparing 1st and last element.
  OCTARRAY_API octave_idx_type lookup (const T& value, sortmode mode = UNSORTED) const;

  //! Ditto, but for an array of values, specializing on the case when values
  //! are sorted.  NaNs get the value N.
  OCTARRAY_API Array<octave_idx_type> lookup (const Array<T, Alloc>& values,
                                              sortmode mode = UNSORTED) const;

  //! Count nonzero elements.
  OCTARRAY_API octave_idx_type nnz (void) const;

  //! Find indices of (at most n) nonzero elements.  If n is specified,
  //! backward specifies search from backward.
  OCTARRAY_API Array<octave_idx_type> find (octave_idx_type n = -1,
                                            bool backward = false) const;

  //! Returns the n-th element in increasing order, using the same
  //! ordering as used for sort.  n can either be a scalar index or a
  //! contiguous range.
  OCTARRAY_API Array<T, Alloc> nth_element (const octave::idx_vector& n, int dim = 0) const;

  //! Get the kth super or subdiagonal.  The zeroth diagonal is the
  //! ordinary diagonal.
  OCTARRAY_API Array<T, Alloc> diag (octave_idx_type k = 0) const;

  OCTARRAY_API Array<T, Alloc> diag (octave_idx_type m, octave_idx_type n) const;

  //! Concatenation along a specified (0-based) dimension, equivalent
  //! to cat().  dim = -1 corresponds to dim = 0 and dim = -2
  //! corresponds to dim = 1, but apply the looser matching rules of
  //! vertcat/horzcat.
  static OCTARRAY_API Array<T, Alloc>
  cat (int dim, octave_idx_type n, const Array<T, Alloc> *array_list);

  //! Apply function fcn to each element of the Array<T, Alloc>.  This function
  //! is optimized with a manually unrolled loop.
#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)
  template <typename U, typename F,
            typename A = std::pmr::polymorphic_allocator<U>>
#else
  template <typename U, typename F, typename A = std::allocator<U>>
#endif
  Array<U, A>
  map (F fcn) const
  {
    octave_idx_type len = numel ();

    const T *m = data ();

    Array<U, A> result (dims ());
    U *p = result.fortran_vec ();

    octave_idx_type i;
    for (i = 0; i < len - 3; i += 4)
      {
        octave_quit ();

        p[i] = fcn (m[i]);
        p[i+1] = fcn (m[i+1]);
        p[i+2] = fcn (m[i+2]);
        p[i+3] = fcn (m[i+3]);
      }

    octave_quit ();

    for (; i < len; i++)
      p[i] = fcn (m[i]);

    return result;
  }

  //@{
  //! Overloads for function references.
#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)
  template <typename U, typename A = std::pmr::polymorphic_allocator<U>>
#else
  template <typename U, typename A = std::allocator<U>>
#endif
  Array<U, A>
  map (U (&fcn) (T)) const
  { return map<U, U (&) (T), A> (fcn); }

#if defined (OCTAVE_HAVE_STD_PMR_POLYMORPHIC_ALLOCATOR)
  template <typename U, typename A = std::pmr::polymorphic_allocator<U>>
#else
  template <typename U, typename A = std::allocator<U>>
#endif
  Array<U, A>
  map (U (&fcn) (const T&)) const
  { return map<U, U (&) (const T&), A> (fcn); }
  //@}

  //! Generic any/all test functionality with arbitrary predicate.
  template <typename F, bool zero>
  bool test (F fcn) const
  {
    return octave::any_all_test<F, T, zero> (fcn, data (), numel ());
  }

  //@{
  //! Simpler calls.
  template <typename F>
  bool test_any (F fcn) const
  { return test<F, false> (fcn); }

  template <typename F>
  bool test_all (F fcn) const
  { return test<F, true> (fcn); }
  //@}

  //@{
  //! Overloads for function references.
  bool test_any (bool (&fcn) (T)) const
  { return test<bool (&) (T), false> (fcn); }

  bool test_any (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), false> (fcn); }

  bool test_all (bool (&fcn) (T)) const
  { return test<bool (&) (T), true> (fcn); }

  bool test_all (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), true> (fcn); }
  //@}

  template <typename U, typename A> friend class Array;

  //! Returns true if this->dims () == dv, and if so, replaces this->m_dimensions
  //! by a shallow copy of dv.  This is useful for maintaining several arrays
  //! with supposedly equal dimensions (e.g. structs in the interpreter).
  OCTARRAY_API bool optimize_dimensions (const dim_vector& dv);

private:
  OCTARRAY_API static void instantiation_guard ();
};

// We use a variadic template for template template parameter so that
// we don't have to specify all the template parameters and limit this
// to Container<T>. http://stackoverflow.com/a/20499809/1609556
template<typename T, typename Alloc>
template<template <typename...> class Container>
Array<T, Alloc>::Array (const Container<T>& a, const dim_vector& dv)
  : m_dimensions (dv), m_rep (new typename Array<T, Alloc>::ArrayRep (dv.safe_numel ())),
    m_slice_data (m_rep->m_data), m_slice_len (m_rep->m_len)
{
  if (m_dimensions.safe_numel () != octave_idx_type (a.size ()))
    {
      std::string new_dims_str = m_dimensions.str ();

      (*current_liboctave_error_handler)
        ("reshape: can't reshape %zi elements into %s array",
         a.size (), new_dims_str.c_str ());
    }

  octave_idx_type i = 0;
  for (const T& x : a)
    m_slice_data[i++] = x;

  m_dimensions.chop_trailing_singletons ();
}

template <typename T, typename Alloc>
OCTARRAY_API std::ostream&
operator << (std::ostream& os, const Array<T, Alloc>& a);

#endif
