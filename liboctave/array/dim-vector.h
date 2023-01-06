////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// or <https://octave.org/copyright/>.
//
// Copyirght (C) 2009, 2010 VZLU Prague
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

#if ! defined (octave_dim_vector_h)
#define octave_dim_vector_h 1

#include "octave-config.h"

#include <cassert>

#include <algorithm>
#include <initializer_list>
#include <string>

#include "Array-fwd.h"
#include "oct-atomic.h"
#include "oct-refcount.h"

//! Vector representing the dimensions (size) of an Array.
//!
//! A dim_vector is used to represent dimensions of an Array.  It is used
//! on its constructor to specify its size, or when reshaping it.
//!
//! @code{.cc}
//! // Matrix with 10 rows and 20 columns.
//! Matrix m Matrix (dim_vector (10, 20));
//!
//! // Change its size to 5 rows and 40 columns.
//! Matrix m2 = m.reshape (dim_vector (5, 40));
//!
//! // Five dimensional Array of length 10, 20, 3, 8, 7 on each dimension.
//! NDArray a (dim_vector (10, 20, 3, 8, 7));
//!
//! // Uninitialized array of same size as other.
//! NDArray b (a.dims ());
//! @endcode
//!
//! The main thing to understand about this class, is that methods such as
//! ndims() and numel(), return the value for an Array of these dimensions,
//! not the actual number of elements in the dim_vector.
//!
//! @code{.cc}
//! dim_vector d (10, 5, 3);
//! octave_idx_type n = d.numel (); // returns 150
//! octave_idx_type nd = d.ndims (); // returns 3
//! @endcode
//!
//! ## Implementation details ##
//!
//! This implementation is more tricky than Array, but the big plus is that
//! dim_vector requires only one allocation instead of two.  It is (slightly)
//! patterned after GCC's basic_string implementation.  rep is a pointer to an
//! array of memory, comprising count, length, and the data:
//!
//! @verbatim
//!        <count>
//!        <ndims>
//! rep --> <dims[0]>
//!        <dims[1]>
//!        ...
//! @endverbatim
//!
//! The inlines count(), ndims() recover this data from the rep.  Note
//! that rep points to the beginning of dims to grant faster access
//! (reinterpret_cast is assumed to be an inexpensive operation).

class
OCTAVE_API
dim_vector
{
private:

  octave_idx_type m_num_dims;

  octave_idx_type *m_dims;

public:

  //! Construct dim_vector for a N dimensional array.
  //!
  //! Each argument to constructor defines the length of an additional
  //! dimension.  A dim_vector always represents a minimum of 2 dimensions
  //! (just like an Array has at least 2 dimensions) and there is no
  //! upper limit on the number of dimensions.
  //!
  //! @code{.cc}
  //! dim_vector dv (7, 5);
  //! Matrix mat (dv);
  //! @endcode
  //!
  //! The constructed dim_vector @c dv will have two elements, @f$[7, 5]@f$,
  //! one for each dimension.  It can then be used to construct a Matrix
  //! with such dimensions, i.e., 7 rows and 5 columns.
  //!
  //! @code{.cc}
  //! NDArray x (dim_vector (7, 5, 10));
  //! @endcode
  //!
  //! This will construct a 3 dimensional NDArray of lengths 7, 5, and 10,
  //! on the first, second, and third dimension (rows, columns, and pages)
  //! respectively.
  //!
  //! Note that that there is no constructor that accepts only one
  //! dimension length to avoid confusion.  The source for such confusion
  //! is that constructor could mean:
  //!   - a column vector, i.e., assume @f$[N, 1]@f$;
  //!   - a square matrix, i.e., as is common in Octave interpreter;
  //!   - support for a 1 dimensional Array (does not exist);
  //!
  //! Using r, c, and lengths... as arguments, allow us to check at compile
  //! time that there's at least 2 dimensions specified, while maintaining
  //! type safety.

  template <typename... Ints>
  dim_vector (const octave_idx_type r, const octave_idx_type c,
              Ints... lengths)
    : m_num_dims (2 + sizeof... (Ints)), m_dims (new octave_idx_type [m_num_dims])
  {
    std::initializer_list<octave_idx_type> all_lengths = {r, c, lengths...};
    octave_idx_type *ptr = m_dims;
    for (const octave_idx_type l: all_lengths)
      *ptr++ = l;
  }

  // Fast access with absolutely no checking

  octave_idx_type& xelem (int i) { return m_dims[i]; }

  octave_idx_type xelem (int i) const { return m_dims[i]; }

  // Safe access to to elements

  octave_idx_type& elem (int i)
  {
    return xelem (i);
  }

  octave_idx_type elem (int i) const { return xelem (i); }

  void chop_trailing_singletons (void)
  {
    while (m_num_dims > 2 && xelem(m_num_dims-1) == 1)
      m_num_dims--;
  }

  OCTAVE_API void chop_all_singletons (void);

private:

  explicit dim_vector (octave_idx_type ndims)
    : m_num_dims (ndims < 2 ? 2 : ndims), m_dims (new octave_idx_type [m_num_dims])
  {
    std::fill_n (m_dims, m_num_dims, 0);
  }

public:

  static OCTAVE_API octave_idx_type dim_max (void);

  explicit dim_vector (void)
    : m_num_dims (2), m_dims (new octave_idx_type [m_num_dims])
  {
    std::fill_n (m_dims, m_num_dims, 0);
  }

  dim_vector (const dim_vector& dv)
    : m_num_dims (dv.m_num_dims), m_dims (new octave_idx_type [m_num_dims])
  {
    std::copy_n (dv.m_dims, m_num_dims, m_dims);
  }

  dim_vector (dim_vector&& dv)
    : m_num_dims (0), m_dims (nullptr)
  {
    *this = std::move (dv);
  }

  static dim_vector alloc (int n)
  {
    return dim_vector (n);
  }

  dim_vector& operator = (const dim_vector& dv)
  {
    if (&dv != this)
      {
        delete [] m_dims;

        m_num_dims = dv.m_num_dims;
        m_dims = new octave_idx_type [m_num_dims];

        std::copy_n (dv.m_dims, m_num_dims, m_dims);
      }

    return *this;
  }

  dim_vector& operator = (dim_vector&& dv)
  {
    if (&dv != this)
      {
        // Because we define a move constructor and a move assignment
        // operator, m_dims may be a nullptr here.  We should only need to
        // protect the destructor in a similar way.

        delete [] m_dims;

        m_num_dims = dv.m_num_dims;
        m_dims = dv.m_dims;

        dv.m_num_dims = 0;
        dv.m_dims = nullptr;
      }

    return *this;
  }

  ~dim_vector (void)
  {
    // Because we define a move constructor and a move assignment
    // operator, m_dims may be a nullptr here.  We should only need to
    // protect the move assignment operator in a similar way.

    delete [] m_dims;
  }

  //! Number of dimensions.
  //!
  //! Returns the number of dimensions of the dim_vector.  This is number of
  //! elements in the dim_vector including trailing singletons.  It is also
  //! the number of dimensions an Array with this dim_vector would have.

  octave_idx_type ndims (void) const { return m_num_dims; }

  //! Number of dimensions.
  //! Synonymous with ndims().
  //!
  //! While this method is not officially deprecated, consider using ndims()
  //! instead to avoid confusion.  Array does not have length because of its
  //! odd definition as length of the longest dimension.

  int length (void) const { return ndims (); }

  octave_idx_type& operator () (int i) { return elem (i); }

  octave_idx_type operator () (int i) const { return elem (i); }

  void resize (int n, int fill_value = 0)
  {
    if (n < 2)
      n = 2;

    if (n == m_num_dims)
      return;

    if (n < m_num_dims)
      {
        m_num_dims = n;
        return;
      }

    octave_idx_type *new_rep = new octave_idx_type [n];

    std::copy_n (m_dims, m_num_dims, new_rep);
    std::fill_n (new_rep + m_num_dims, n - m_num_dims, fill_value);

    delete [] m_dims;

    m_dims = new_rep;

    m_num_dims = n;
  }

  OCTAVE_API std::string str (char sep = 'x') const;

  bool all_zero (void) const
  {
    return std::all_of (m_dims, m_dims + ndims (),
                        [] (octave_idx_type dim) { return dim == 0; });
  }

  bool empty_2d (void) const
  {
    return ndims () == 2 && (xelem (0) == 0 || xelem (1) == 0);
  }

  bool zero_by_zero (void) const
  {
    return ndims () == 2 && xelem (0) == 0 && xelem (1) == 0;
  }

  bool any_zero (void) const
  {
    return std::any_of (m_dims, m_dims + ndims (),
                        [] (octave_idx_type dim) { return dim == 0; });
  }

  OCTAVE_API int num_ones (void) const;

  bool all_ones (void) const
  {
    return (num_ones () == ndims ());
  }

  //! Number of elements that a matrix with this dimensions would have.
  //!
  //! Return the number of elements that a matrix with this dimension
  //! vector would have, NOT the number of dimensions (elements in the
  //! dimension vector).

  octave_idx_type numel (int n = 0) const
  {
    int n_dims = ndims ();

    octave_idx_type retval = 1;

    for (int i = n; i < n_dims; i++)
      retval *= elem (i);

    return retval;
  }

  //! The following function will throw a std::bad_alloc ()
  //! exception if the requested size is larger than can be indexed by
  //! octave_idx_type.  This may be smaller than the actual amount of
  //! memory that can be safely allocated on a system.  However, if we
  //! don't fail here, we can end up with a mysterious crash inside a
  //! function that is iterating over an array using octave_idx_type
  //! indices.

  OCTAVE_API octave_idx_type safe_numel (void) const;

  bool any_neg (void) const
  {
    return std::any_of (m_dims, m_dims + ndims (),
                        [] (octave_idx_type dim) { return dim < 0; });
  }

  OCTAVE_API dim_vector squeeze (void) const;

  //! This corresponds to cat().
  OCTAVE_API bool concat (const dim_vector& dvb, int dim);

  //! This corresponds to [,] (horzcat, dim = 0) and [;] (vertcat, dim = 1).
  // The rules are more relaxed here.
  OCTAVE_API bool hvcat (const dim_vector& dvb, int dim);

  //! Force certain dimensionality, preserving numel ().  Missing
  //! dimensions are set to 1, redundant are folded into the trailing
  //! one.  If n = 1, the result is 2d and the second dim is 1
  //! (dim_vectors are always at least 2D).

  OCTAVE_API dim_vector redim (int n) const;

  dim_vector as_column (void) const
  {
    if (ndims () == 2 && xelem (1) == 1)
      return *this;
    else
      return dim_vector (numel (), 1);
  }

  dim_vector as_row (void) const
  {
    if (ndims () == 2 && xelem (0) == 1)
      return *this;
    else
      return dim_vector (1, numel ());
  }

  bool isvector (void) const
  {
    return (ndims () == 2 && (xelem (0) == 1 || xelem (1) == 1));
  }

  bool is_nd_vector (void) const
  {
    int num_non_one = 0;

    for (int i = 0; i < ndims (); i++)
      {
        if (xelem (i) != 1)
          {
            num_non_one++;

            if (num_non_one > 1)
              break;
          }
      }

    return num_non_one == 1;
  }

  // Create a vector with length N.  If this object is a vector,
  // preserve the orientation, otherwise, create a column vector.

  dim_vector make_nd_vector (octave_idx_type n) const
  {
    dim_vector orig_dims;

    if (is_nd_vector ())
      {
        orig_dims = *this;

        for (int i = 0; i < orig_dims.ndims (); i++)
          {
            if (orig_dims(i) != 1)
              {
                orig_dims(i) = n;
                break;
              }
          }
      }
    else
      orig_dims = dim_vector (n, 1);

    return orig_dims;
  }

  int first_non_singleton (int def = 0) const
  {
    for (int i = 0; i < ndims (); i++)
      {
        if (xelem (i) != 1)
          return i;
      }

    return def;
  }

  //! Linear index from an index tuple.
  octave_idx_type compute_index (const octave_idx_type *idx) const
  { return compute_index (idx, ndims ()); }

  //! Linear index from an incomplete index tuple (nidx < length ()).
  octave_idx_type compute_index (const octave_idx_type *idx, int nidx) const
  {
    octave_idx_type k = 0;
    for (int i = nidx - 1; i >= 0; i--)
      k = xelem(i) * k + idx[i];

    return k;
  }

  //! Increment a multi-dimensional index tuple, optionally starting
  //! from an offset position and return the index of the last index
  //! position that was changed, or length () if just cycled over.

  int increment_index (octave_idx_type *idx, int start = 0) const
  {
    int i;
    for (i = start; i < ndims (); i++)
      {
        if (++(*idx) == xelem(i))
          *idx++ = 0;
        else
          break;
      }
    return i;
  }

  //! Return cumulative dimensions.

  dim_vector cumulative (void) const
  {
    int nd = ndims ();
    dim_vector retval = alloc (nd);

    octave_idx_type k = 1;
    for (int i = 0; i < nd; i++)
      retval.xelem(i) = (k *= xelem(i));

    return retval;
  }

  //! Compute a linear index from an index tuple.  Dimensions are
  //! required to be cumulative.

  octave_idx_type cum_compute_index (const octave_idx_type *idx) const
  {
    octave_idx_type k = idx[0];

    for (int i = 1; i < ndims (); i++)
      k += xelem(i-1) * idx[i];

    return k;
  }

  friend OCTAVE_API bool
  operator == (const dim_vector& a, const dim_vector& b);

  OCTAVE_API Array<octave_idx_type> as_array (void) const;
};

inline bool
operator == (const dim_vector& a, const dim_vector& b)
{
  // Fast case.
  if (a.m_dims == b.m_dims)
    return true;

  int a_len = a.ndims ();
  int b_len = b.ndims ();

  if (a_len != b_len)
    return false;

  return std::equal (a.m_dims, a.m_dims + a_len, b.m_dims);
}

inline bool
operator != (const dim_vector& a, const dim_vector& b)
{
  return ! operator == (a, b);
}

#endif
