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

#if ! defined (octave_Range_h)
#define octave_Range_h 1

#include "octave-config.h"

#include <iosfwd>
#include <type_traits>

#include "Array-fwd.h"
#include "dMatrix.h"
#include "dim-vector.h"
#include "lo-error.h"
#include "oct-sort.h"
#include "range-fwd.h"

OCTAVE_BEGIN_NAMESPACE(octave)

  // For now, only define for floating point types.  However, we only
  // need range<float> as a temporary local variable in make_float_range
  // in ov.cc.

  template <typename T>
  class
  range<T, typename std::enable_if<std::is_floating_point<T>::value>::type>
  {
  public:

    range ()
      : m_base (0), m_increment (0), m_limit (0), m_final (0), m_numel (0),
        m_reverse (false)
    { }

    // LIMIT is an upper limit and may be outside the range of actual
    // values.  For floating point ranges, we perform a tolerant check
    // to attempt to capture limit in the set of values if it is "close"
    // to the value of base + a multiple of the increment.

    range (const T& base, const T& increment, const T& limit,
           bool reverse = false)
      : m_base (base), m_increment (increment), m_limit (limit),
        m_final (), m_numel (), m_reverse (reverse)
    {
      init ();
    }

    range (const T& base, const T& limit)
      : m_base (base), m_increment (1), m_limit (limit), m_final (), m_numel (),
        m_reverse (false)
    {
      init ();
    }

    // Allow conversion from (presumably) properly constructed Range
    // objects.  The values of base, limit, increment, and numel must be
    // consistent.

    // FIXME: Actually check that base, limit, increment, and numel are
    // consistent?

    range (const T& base, const T& increment, const T& limit,
           octave_idx_type numel, bool reverse = false)
      : m_base (base), m_increment (increment), m_limit (limit),
        m_final (limit), m_numel (numel), m_reverse (reverse)
    { }

    // We don't use a constructor for this because it will conflict with
    // range<T> (base, limit, increment) when T is octave_idx_type.

    static range<T> make_n_element_range (const T& base, const T& increment,
                                          octave_idx_type numel,
                                          bool reverse = false)
    {
      // We could just make this constructor public, but it allows
      // inconsistent ranges to be constructed.  And it is probably much
      // clearer to see "make_n_element_range" instead of puzzling over the
      // purpose of this strange constructor form.

      T final_val = (reverse ? base - (numel - 1) * increment
                             : base + (numel - 1) * increment);

      return range<T> (base, increment, final_val, numel, reverse);
    }

    range (const range<T>& r)
      : m_base (r.m_base), m_increment (r.m_increment),
        m_limit (r.m_limit), m_final (r.m_final),
        m_numel (r.m_numel), m_reverse (r.m_reverse)
    { }

    range<T>& operator = (const range<T>& r)
    {
      if (this != &r)
        {
          m_base = r.m_base;
          m_increment = r.m_increment;
          m_limit = r.m_limit;
          m_final = r.m_final;
          m_numel = r.m_numel;
          m_reverse = r.m_reverse;
        }

      return *this;
    }

    ~range () = default;

    T base () const { return m_base; }
    T increment () const { return m_increment; }
    T limit () const { return m_limit; }
    bool reverse () const { return m_reverse; }

    T final_value () const { return m_final; }

    T min () const
    {
      return (m_numel > 0
              ? ((m_reverse ? m_increment > T (0)
                            : m_increment > T (0)) ? base () : final_value ())
              : T (0));
    }

    T max () const
    {
      return (m_numel > 0
              ? ((m_reverse ? m_increment < T (0)
                            : m_increment > T (0)) ? final_value () : base ())
              : T (0));
    }

    octave_idx_type numel () const { return m_numel; }

    // To support things like "for i = 1:Inf; ...; end" that are
    // required for Matlab compatibility, creation of a range object
    // like 1:Inf is allowed with m_numel set to
    // numeric_limits<octave_idx_type>::max().  However, it is not
    // possible to store these ranges.  The following function allows
    // us to easily distinguish ranges with an infinite number of
    // elements.  There are specializations for double and float.

    bool is_storable () const { return true; }

    dim_vector dims () const { return dim_vector (1, m_numel); }

    octave_idx_type rows () const { return 1; }

    octave_idx_type cols () const { return numel (); }
    octave_idx_type columns () const { return numel (); }

    bool isempty () const { return numel () == 0; }

    bool all_elements_are_ints () const { return true; }

    sortmode issorted (sortmode mode = ASCENDING) const
    {
      if (m_numel > 1 && (m_reverse ? m_increment < T (0)
                                    : m_increment > T (0)))
        mode = ((mode == DESCENDING) ? UNSORTED : ASCENDING);
      else if (m_numel > 1 && (m_reverse ? m_increment > T (0)
                                         : m_increment < T (0)))
        mode = ((mode == ASCENDING) ? UNSORTED : DESCENDING);
      else
        mode = ((mode == UNSORTED) ? ASCENDING : mode);

      return mode;
    }

    OCTAVE_API octave_idx_type nnz () const;

    // Support for single-index subscripting, without generating matrix cache.

    T checkelem (octave_idx_type i) const
    {
      if (i < 0 || i >= m_numel)
        err_index_out_of_range (2, 2, i+1, m_numel, dims ());

      if (i == 0)
        // Required for proper NaN handling.
        return (m_numel == 1 ? final_value () : m_base);
      else if (i < m_numel - 1)
        return (m_reverse ? m_base + T (i) * m_increment
                          : m_base + T (i) * m_increment);
      else
        return final_value ();
    }

    T checkelem (octave_idx_type i, octave_idx_type j) const
    {
      // Ranges are *always* row vectors.
      if (i != 0)
        err_index_out_of_range (1, 1, i+1, m_numel, dims ());

      return checkelem (j);
    }

    T elem (octave_idx_type i) const
    {
      if (i == 0)
        // Required for proper NaN handling.
        return (m_numel == 1 ? final_value () : m_base);
      else if (i < m_numel - 1)
        return (m_reverse ? m_base - T (i) * m_increment
                          : m_base + T (i) * m_increment);
      else
        return final_value ();
    }

    T elem (octave_idx_type /* i */, octave_idx_type j) const
    {
      return elem (j);
    }

    T operator () (octave_idx_type i) const
    {
      return elem (i);
    }

    T operator () (octave_idx_type i, octave_idx_type j) const
    {
      return elem (i, j);
    }

    Array<T> index (const idx_vector& idx) const
    {
      Array<T> retval;

      octave_idx_type n = m_numel;

      if (idx.is_colon ())
        {
          retval = array_value ().reshape (dim_vector (m_numel, 1));
        }
      else
        {
          if (idx.extent (n) != n)
            err_index_out_of_range (1, 1, idx.extent (n), n, dims ());

          dim_vector idx_dims = idx.orig_dimensions ();
          octave_idx_type idx_len = idx.length (n);

          // taken from Array.cc.
          if (n != 1 && idx_dims.isvector ())
            idx_dims = dim_vector (1, idx_len);

          retval.clear (idx_dims);

          // Loop over all values in IDX, executing the lambda
          // expression for each index value.

          T *array = retval.fortran_vec ();

          idx.loop (n, [=, &array] (octave_idx_type i)
          {
            if (i == 0)
              // Required for proper NaN handling.
              *array++ = (m_numel == 0 ? m_final : m_base);
            else if (i < m_numel - 1)
              *array++ = (m_reverse ? m_base - T (i) * m_increment
                                    : m_base + T (i) * m_increment);
            else
              *array++ = m_final;
          });
        }

      return retval;
    }

    Array<T> diag (octave_idx_type k) const
    {
      return array_value ().diag (k);
    }

    Array<T> array_value () const
    {
      octave_idx_type nel = numel ();

      Array<T> retval (dim_vector (1, nel));

      if (nel == 1)
        // Required for proper NaN handling.
        retval(0) = final_value ();
      else if (nel > 1)
        {
          // The first element must always be *exactly* the base.
          // E.g, -0 would otherwise become +0 in the loop (-0 + 0*increment).
          retval(0) = m_base;

          if (m_reverse)
            for (octave_idx_type i = 1; i < nel - 1; i++)
              retval.xelem (i) = m_base - i * m_increment;
          else
            for (octave_idx_type i = 1; i < nel - 1; i++)
              retval.xelem (i) = m_base + i * m_increment;

          retval.xelem (nel - 1) = final_value ();
        }

      return retval;
    }

  private:

    T m_base;
    T m_increment;
    T m_limit;
    T m_final;
    octave_idx_type m_numel;
    bool m_reverse;

    // Setting the number of elements to zero when the increment is zero
    // is intentional and matches the behavior of Matlab's colon
    // operator.

    // These calculations are appropriate for integer ranges.  There are
    // specializations for double and float.

    void init ()
    {
      if (m_reverse)
        {
          m_numel = ((m_increment == T (0)
                      || (m_limit > m_base && m_increment > T (0))
                      || (m_limit < m_base && m_increment < T (0)))
                     ? T (0)
                     : (m_base - m_limit - m_increment) / m_increment);

          m_final = m_base - (m_numel - 1) * m_increment;
        }
      else
        {
          m_numel = ((m_increment == T (0)
                      || (m_limit > m_base && m_increment < T (0))
                      || (m_limit < m_base && m_increment > T (0)))
                     ? T (0)
                     : (m_limit - m_base + m_increment) / m_increment);

          m_final = m_base + (m_numel - 1) * m_increment;
        }
    }
  };

  // Specializations defined externally.

  template <> OCTAVE_API bool range<double>::all_elements_are_ints () const;
  template <> OCTAVE_API bool range<float>::all_elements_are_ints () const;

  template <> OCTAVE_API void range<double>::init ();
  template <> OCTAVE_API void range<float>::init ();

  // For now, only define for floating point types.  However, we only
  // need range<float> as a temporary local variable in make_float_range
  // in ov.cc.

#if 0

  template <> OCTAVE_API void range<octave_int8>::init ();
  template <> OCTAVE_API void range<octave_int16>::init ();
  template <> OCTAVE_API void range<octave_int32>::init ();
  template <> OCTAVE_API void range<octave_int64>::init ();
  template <> OCTAVE_API void range<octave_uint8>::init ();
  template <> OCTAVE_API void range<octave_uint16>::init ();
  template <> OCTAVE_API void range<octave_uint32>::init ();
  template <> OCTAVE_API void range<octave_uint64>::init ();

#endif

  template <> OCTAVE_API bool range<double>::is_storable () const;
  template <> OCTAVE_API bool range<float>::is_storable () const;

  template <> OCTAVE_API octave_idx_type range<double>::nnz () const;
  template <> OCTAVE_API octave_idx_type range<float>::nnz () const;

OCTAVE_END_NAMESPACE(octave)

#endif
