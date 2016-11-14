/*

Copyright (C) 2003-2016 John W. Eaton
Copyirght (C) 2009, 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_dim_vector_h)
#define octave_dim_vector_h 1

#include "octave-config.h"

#include <cassert>

#include <initializer_list>
#include <string>

#include "lo-error.h"
#include "lo-macros.h"
#include "oct-refcount.h"

//! Vector representing the dimensions (size) of an Array.
/*!
  A dim_vector is used to represent dimensions of an Array.  It is used
  on its constructor to specify its size, or when reshaping it.

  @code{.cc}
  // Matrix with 10 rows and 20 columns.
  Matrix m Matrix (dim_vector (10, 20));

  // Change its size to 5 rows and 40 columns.
  Matrix m2 = m.reshape (dim_vector (5, 40));

  // Five dimensional Array of length 10, 20, 3, 8, 7 on each dimension.
  NDArray a (dim_vector (10, 20, 3, 8, 7));

  // Uninitialized array of same size as other.
  NDArray b (a.dims ());
  @endcode

  The main thing to understand about this class, is that methods such as
  ndims() and numel(), return the value for an Array of these dimensions,
  not the actual number of elements in the dim_vector.

  @code{.cc}
  dim_vector d (10, 5, 3);
  octave_idx_type n = d.numel (); // returns 150
  octave_idx_type nd = d.ndims (); // returns 3
  @endcode

  ## Implementation details ##

  This implementation is more tricky than Array, but the big plus is that
  dim_vector requires only one allocation instead of two.  It is (slightly)
  patterned after GCC's basic_string implementation.  rep is a pointer to an
  array of memory, comprising count, length, and the data:

  @verbatim
          <count>
          <ndims>
  rep --> <dims[0]>
          <dims[1]>
          ...
  @endverbatim

  The inlines count(), ndims() recover this data from the rep.  Note
  that rep points to the beginning of dims to grant faster access
  (reinterpret_cast is assumed to be an inexpensive operation).
*/

class
OCTAVE_API
dim_vector
{
private:

  octave_idx_type *rep;

  octave_idx_type& count (void) const { return rep[-2]; }

  //! Construct a new rep with count = 1 and ndims given.

  static octave_idx_type *newrep (int ndims)
  {
    octave_idx_type *r = new octave_idx_type [ndims + 2];

    *r++ = 1;
    *r++ = ndims;

    return r;
  }

  //! Clone this->rep.

  octave_idx_type *clonerep (void)
  {
    int l = ndims ();

    octave_idx_type* r = newrep (l);

    for (int i = 0; i < l; i++)
      r[i] = rep[i];

    return r;
  }

  //! Clone and resize this->rep to length n, filling by given value.

  octave_idx_type *resizerep (int n, octave_idx_type fill_value)
  {
    int l = ndims ();

    if (n < 2)
      n = 2;

    octave_idx_type* r = newrep (n);

    if (l > n)
      l = n;

    int j = 0;
    for (; j < l; j++)
      r[j] = rep[j];
    for (; j < n; j++)
      r[j] = fill_value;

    return r;
  }

  //! Free the rep.

  void freerep (void)
  {
    assert (count () == 0);
    delete [] (rep - 2);
  }

  void make_unique (void)
  {
    if (count () > 1)
      {
        octave_idx_type *new_rep = clonerep ();

        if (OCTAVE_ATOMIC_DECREMENT (&(count ())) == 0)
          freerep ();

        rep = new_rep;
      }
  }

public:

  //! Construct dim_vector for a N dimensional array.
  /*!

    Each argument to constructor defines the length of an additional
    dimension.  A dim_vector always represents a minimum of 2 dimensions
    (just like an Array has at least 2 dimensions) and there is no
    upper limit on the number of dimensions.

    @code{.cc}
    dim_vector dv (7, 5);
    Matrix mat (dv);
    @endcode

    The constructed dim_vector @c dv will have two elements, @f$[7, 5]@f$,
    one for each dimension.  It can then be used to construct a Matrix
    with such dimensions, i.e., 7 rows and 5 columns.

    @code{.cc}
    NDArray x (dim_vector (7, 5, 10));
    @endcode

    This will construct a 3 dimensional NDArray of lengths 7, 5, and 10,
    on the first, second, and third dimension (rows, columns, and pages)
    respectively.

    Note that that there is no constructor that accepts only one
    dimension length to avoid confusion.  The source for such confusion
    is that constructor could mean:
      - a column vector, i.e., assume @f$[N, 1]@f$;
      - a square matrix, i.e., as is common in Octave interpreter;
      - support for a 1 dimensional Array (does not exist);

    Using r, c, and lengths... as arguments, allow us to check at compile
    time that there's at least 2 dimensions specified, while maintaining
    type safety.
  */
  template <typename... Ints>
  dim_vector (const octave_idx_type r, const octave_idx_type c,
              Ints... lengths) : rep (newrep (2 + sizeof... (Ints)))
  {
    std::initializer_list<octave_idx_type> all_lengths = {r, c, lengths...};
    for (const octave_idx_type l: all_lengths)
      *rep++ = l;
    rep -= all_lengths.size ();
  }

  octave_idx_type& elem (int i)
  {
#if defined (OCTAVE_ENABLE_BOUNDS_CHECK)
    assert (i >= 0 && i < ndims ());
#endif
    make_unique ();
    return rep[i];
  }

  octave_idx_type elem (int i) const
  {
#if defined (OCTAVE_ENABLE_BOUNDS_CHECK)
    assert (i >= 0 && i < ndims ());
#endif
    return rep[i];
  }

  void chop_trailing_singletons (void)
  {
    int l = ndims ();
    if (l > 2 && rep[l-1] == 1)
      {
        make_unique ();
        do
          l--;
        while (l > 2 && rep[l-1] == 1);
        rep[-1] = l;
      }
  }

  void chop_all_singletons (void);

  // WARNING: Only call by jit
  octave_idx_type *to_jit (void) const
  {
    return rep;
  }

private:

  static octave_idx_type *nil_rep (void);

public:

  static octave_idx_type dim_max (void);

  explicit dim_vector (void) : rep (nil_rep ())
  { OCTAVE_ATOMIC_INCREMENT (&(count ())); }

  dim_vector (const dim_vector& dv) : rep (dv.rep)
  { OCTAVE_ATOMIC_INCREMENT (&(count ())); }

  // FIXME: Should be private, but required by array constructor for jit
  explicit dim_vector (octave_idx_type *r) : rep (r) { }

  static dim_vector alloc (int n)
  {
    return dim_vector (newrep (n < 2 ? 2 : n));
  }

  dim_vector& operator = (const dim_vector& dv)
  {
    if (&dv != this)
      {
        if (OCTAVE_ATOMIC_DECREMENT (&(count ())) == 0)
          freerep ();

        rep = dv.rep;
        OCTAVE_ATOMIC_INCREMENT (&(count ()));
      }

    return *this;
  }

  ~dim_vector (void)
  {
    if (OCTAVE_ATOMIC_DECREMENT (&(count ())) == 0)
      freerep ();
  }

  //! Number of dimensions.
  /*!
      Returns the number of dimensions of the dim_vector.  This is number of
      elements in the dim_vector including trailing singetons.  It is also
      the number of dimensions an Array with this dim_vector would have.
  */
  octave_idx_type ndims (void) const { return rep[-1]; }

  //! Number of dimensions.
  //! Synonymous with ndims().
  /*!
    While this method is not officially deprecated, consider using ndims()
    instead to avoid confusion.  Array does not have length because of its
    odd definition as length of the longest dimension.
  */
  int length (void) const { return ndims (); }

  octave_idx_type& operator () (int i) { return elem (i); }

  octave_idx_type operator () (int i) const { return elem (i); }

  void resize (int n, int fill_value = 0)
  {
    int len = ndims ();

    if (n != len)
      {
        octave_idx_type *r = resizerep (n, fill_value);

        if (OCTAVE_ATOMIC_DECREMENT (&(count ())) == 0)
          freerep ();

        rep = r;
      }
  }

  std::string str (char sep = 'x') const;

  bool all_zero (void) const
  {
    bool retval = true;

    for (int i = 0; i < ndims (); i++)
      {
        if (elem (i) != 0)
          {
            retval = false;
            break;
          }
      }

    return retval;
  }

  bool empty_2d (void) const
  {
    return ndims () == 2 && (elem (0) == 0 || elem (1) == 0);
  }

  bool zero_by_zero (void) const
  {
    return ndims () == 2 && elem (0) == 0 && elem (1) == 0;
  }

  bool any_zero (void) const
  {
    bool retval = false;

    for (int i = 0; i < ndims (); i++)
      {
        if (elem (i) == 0)
          {
            retval = true;
            break;
          }
      }

    return retval;
  }

  int num_ones (void) const;

  bool all_ones (void) const
  {
    return (num_ones () == ndims ());
  }

  //! Number of elements that a matrix with this dimensions would have.
  /*!
     Return the number of elements that a matrix with this dimension
     vector would have, NOT the number of dimensions (elements in the
     dimension vector).
  */

  octave_idx_type numel (int n = 0) const
  {
    int n_dims = ndims ();

    octave_idx_type retval = 1;

    for (int i = n; i < n_dims; i++)
      retval *= elem (i);

    return retval;
  }

  /*!
     The following function will throw a std::bad_alloc ()
     exception if the requested size is larger than can be indexed by
     octave_idx_type.  This may be smaller than the actual amount of
     memory that can be safely allocated on a system.  However, if we
     don't fail here, we can end up with a mysterious crash inside a
     function that is iterating over an array using octave_idx_type
     indices.
  */

  octave_idx_type safe_numel (void) const;

  bool any_neg (void) const
  {
    int n_dims = ndims ();
    int i;

    for (i = 0; i < n_dims; i++)
      if (elem (i) < 0)
        break;

    return i < n_dims;
  }

  dim_vector squeeze (void) const;

  //! This corresponds to cat().
  bool concat (const dim_vector& dvb, int dim);

  //! This corresponds to [,] (horzcat, dim = 0) and [;] (vertcat, dim = 1).
  // The rules are more relaxed here.
  bool hvcat (const dim_vector& dvb, int dim);

  /*!
      Force certain dimensionality, preserving numel ().  Missing
      dimensions are set to 1, redundant are folded into the trailing
      one.  If n = 1, the result is 2d and the second dim is 1
      (dim_vectors are always at least 2D).
  */
  dim_vector redim (int n) const;

  dim_vector as_column (void) const
  {
    if (ndims () == 2 && elem (1) == 1)
      return *this;
    else
      return dim_vector (numel (), 1);
  }

  dim_vector as_row (void) const
  {
    if (ndims () == 2 && elem (0) == 1)
      return *this;
    else
      return dim_vector (1, numel ());
  }

  bool is_vector (void) const
  {
    return (ndims () == 2 && (elem (0) == 1 || elem (1) == 1));
  }

  int first_non_singleton (int def = 0) const
  {
    for (int i = 0; i < ndims (); i++)
      {
        if (elem (i) != 1)
          return i;
      }

    return def;
  }

  //! Linear index from an index tuple.
  octave_idx_type compute_index (const octave_idx_type* idx) const
  { return compute_index (idx, ndims ()); }

  //! Linear index from an incomplete index tuple (nidx < length ()).
  octave_idx_type compute_index (const octave_idx_type *idx, int nidx) const
  {
    octave_idx_type k = 0;
    for (int i = nidx - 1; i >= 0; i--)
      k = rep[i] * k + idx[i];

    return k;
  }

  /*/!
      Increment a multi-dimensional index tuple, optionally starting
      from an offset position and return the index of the last index
      position that was changed, or length () if just cycled over.
  */

  int increment_index (octave_idx_type *idx, int start = 0) const
  {
    int i;
    for (i = start; i < ndims (); i++)
      {
        if (++(*idx) == rep[i])
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
      retval.rep[i] = k *= rep[i];

    return retval;
  }

  //! Compute a linear index from an index tuple.  Dimensions are
  //! required to be cumulative.

  octave_idx_type cum_compute_index (const octave_idx_type *idx) const
  {
    octave_idx_type k = idx[0];

    for (int i = 1; i < ndims (); i++)
      k += rep[i-1] * idx[i];

    return k;
  }

  friend bool operator == (const dim_vector& a, const dim_vector& b);
};

inline bool
operator == (const dim_vector& a, const dim_vector& b)
{
  // Fast case.
  if (a.rep == b.rep)
    return true;

  bool retval = true;

  int a_len = a.ndims ();
  int b_len = b.ndims ();

  if (a_len != b_len)
    retval = false;
  else
    {
      for (int i = 0; i < a_len; i++)
        {
          if (a(i) != b(i))
            {
              retval = false;
              break;
            }
        }
    }

  return retval;
}

inline bool
operator != (const dim_vector& a, const dim_vector& b)
{
  return ! operator == (a, b);
}

#endif

