/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_pt_tm_const_h)
#define octave_pt_tm_const_h 1

#include "octave-config.h"

#include <string>

#include "Array.h"
#include "Sparse.h"
#include "base-list.h"

#include "data.h"
#include "dim-vector.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"
#include "pt-arg-list.h"

namespace octave
{
  class tree_evaluator;
  class tree_matrix;

  // General matrices.  This list type is much more work to handle than
  // constant matrices, but it allows us to construct matrices from
  // other matrices, variables, and functions.

  // But first, some internal classes that make our job much easier.

  class
  tm_row_const
  {
  private:

    class
    tm_row_const_rep : public base_list<octave_value>
    {
    public:

      tm_row_const_rep (void)
        : m_count (1), m_dv (0, 0), m_all_str (false),
          m_all_sq_str (false), m_all_dq_str (false),
          m_some_str (false), m_all_real (false), m_all_cmplx (false),
          m_all_mt (true), m_any_cell (false), m_any_sparse (false),
          m_any_class (false), m_all_1x1 (false),
          m_first_elem_is_struct (false), m_class_nm (), m_ok (false)
      { }

      tm_row_const_rep (const tree_argument_list& row, tree_evaluator& tw)
        : m_count (1), m_dv (0, 0), m_all_str (false), m_all_sq_str (false),
          m_some_str (false), m_all_real (false), m_all_cmplx (false),
          m_all_mt (true), m_any_cell (false), m_any_sparse (false),
          m_any_class (false), m_all_1x1 (! row.empty ()),
          m_first_elem_is_struct (false), m_class_nm (), m_ok (false)
      { init (row, tw); }

      ~tm_row_const_rep (void) = default;

      refcount<int> m_count;

      dim_vector m_dv;

      bool m_all_str;
      bool m_all_sq_str;
      bool m_all_dq_str;
      bool m_some_str;
      bool m_all_real;
      bool m_all_cmplx;
      bool m_all_mt;
      bool m_any_cell;
      bool m_any_sparse;
      bool m_any_class;
      bool m_all_1x1;
      bool m_first_elem_is_struct;

      std::string m_class_nm;

      bool m_ok;

      void do_init_element (const octave_value&, bool&);

      void init (const tree_argument_list&, tree_evaluator& tw);

      void cellify (void);

    private:

      tm_row_const_rep (const tm_row_const_rep&);

      tm_row_const_rep& operator = (const tm_row_const_rep&);

    };

  public:

    typedef tm_row_const_rep::iterator iterator;
    typedef tm_row_const_rep::const_iterator const_iterator;

    tm_row_const (void)
      : m_rep (nullptr) { }

    tm_row_const (const tree_argument_list& row, tree_evaluator& tw)
      : m_rep (new tm_row_const_rep (row, tw)) { }

    tm_row_const (const tm_row_const& x)
      : m_rep (x.m_rep)
    {
      if (m_rep)
        m_rep->m_count++;
    }

    tm_row_const& operator = (const tm_row_const& x)
    {
      if (this != &x && m_rep != x.m_rep)
        {
          if (m_rep && --m_rep->m_count == 0)
            delete m_rep;

          m_rep = x.m_rep;

          if (m_rep)
            m_rep->m_count++;
        }

      return *this;
    }

    ~tm_row_const (void)
    {
      if (m_rep && --m_rep->m_count == 0)
        delete m_rep;
    }

    octave_idx_type rows (void) { return m_rep->m_dv(0); }
    octave_idx_type cols (void) { return m_rep->m_dv(1); }

    bool empty (void) const { return m_rep->empty (); }

    size_t length (void) const { return m_rep->length (); }

    dim_vector dims (void) { return m_rep->m_dv; }

    bool all_strings_p (void) const { return m_rep->m_all_str; }
    bool all_sq_strings_p (void) const { return m_rep->m_all_sq_str; }
    bool all_dq_strings_p (void) const { return m_rep->m_all_dq_str; }
    bool some_strings_p (void) const { return m_rep->m_some_str; }
    bool all_real_p (void) const { return m_rep->m_all_real; }
    bool all_complex_p (void) const { return m_rep->m_all_cmplx; }
    bool all_empty_p (void) const { return m_rep->m_all_mt; }
    bool any_cell_p (void) const { return m_rep->m_any_cell; }
    bool any_sparse_p (void) const { return m_rep->m_any_sparse; }
    bool any_class_p (void) const { return m_rep->m_any_class; }
    bool all_1x1_p (void) const { return m_rep->m_all_1x1; }
    bool first_elem_struct_p (void) const { return m_rep->m_first_elem_is_struct; }

    std::string class_name (void) const { return m_rep->m_class_nm; }

    void cellify (void) { m_rep->cellify (); }

    operator bool () const { return (m_rep && m_rep->m_ok); }

    iterator begin (void) { return m_rep->begin (); }
    const_iterator begin (void) const { return m_rep->begin (); }

    iterator end (void) { return m_rep->end (); }
    const_iterator end (void) const { return m_rep->end (); }

  private:

    tm_row_const_rep *m_rep;
  };

  class
  tm_const : public base_list<tm_row_const>
  {
  public:

    tm_const (const tree_matrix& tm, tree_evaluator& tw)
      : m_dv (0, 0), m_all_str (false), m_all_sq_str (false),
        m_all_dq_str (false),
        m_some_str (false), m_all_real (false), m_all_cmplx (false),
        m_all_mt (true), m_any_cell (false), m_any_sparse (false),
        m_any_class (false), m_class_nm (), m_ok (false)
    { init (tm, tw); }

    ~tm_const (void) = default;

    octave_idx_type rows (void) const { return m_dv.elem (0); }
    octave_idx_type cols (void) const { return m_dv.elem (1); }

    dim_vector dims (void) const { return m_dv; }

    bool all_strings_p (void) const { return m_all_str; }
    bool all_sq_strings_p (void) const { return m_all_sq_str; }
    bool all_dq_strings_p (void) const { return m_all_dq_str; }
    bool some_strings_p (void) const { return m_some_str; }
    bool all_real_p (void) const { return m_all_real; }
    bool all_complex_p (void) const { return m_all_cmplx; }
    bool all_empty_p (void) const { return m_all_mt; }
    bool any_cell_p (void) const { return m_any_cell; }
    bool any_sparse_p (void) const { return m_any_sparse; }
    bool any_class_p (void) const { return m_any_class; }
    bool all_1x1_p (void) const { return m_all_1x1; }

    std::string class_name (void) const { return m_class_nm; }

    operator bool () const { return m_ok; }

  private:

    dim_vector m_dv;

    bool m_all_str;
    bool m_all_sq_str;
    bool m_all_dq_str;
    bool m_some_str;
    bool m_all_real;
    bool m_all_cmplx;
    bool m_all_mt;
    bool m_any_cell;
    bool m_any_sparse;
    bool m_any_class;
    bool m_all_1x1;

    std::string m_class_nm;

    bool m_ok;

    tm_const (void);

    tm_const (const tm_const&);

    tm_const& operator = (const tm_const&);

    void init (const tree_matrix& tm, tree_evaluator& tw);
  };

  template <typename TYPE, typename T>
  void
  single_type_concat (Array<T>& result, tm_const& tmp)
  {
    octave_idx_type r = 0;
    octave_idx_type c = 0;

    for (tm_row_const& row : tmp)
      {
        // Skip empty arrays to allow looser rules.
        if (row.dims ().any_zero ())
          continue;

        for (auto& elt : row)
          {
            octave_quit ();

            TYPE ra = octave_value_extract<TYPE> (elt);

            // Skip empty arrays to allow looser rules.

            if (! ra.isempty ())
              {
                result.insert (ra, r, c);

                c += ra.columns ();
              }
          }

        r += row.rows ();
        c = 0;
      }
  }

  template <typename TYPE, typename T>
  void
  single_type_concat (Array<T>& result, const dim_vector& dv,
                      tm_const& tmp)
  {
    if (dv.any_zero ())
      {
        result = Array<T> (dv);
        return;
      }

    if (tmp.length () == 1)
      {
        // If possible, forward the operation to liboctave.
        // Single row.
        tm_row_const& row = tmp.front ();
        if (! (equal_types<T, char>::value || equal_types<T, octave_value>::value)
            && row.all_1x1_p ())
          {
            // Optimize all scalars case.
            result.clear (dv);
            assert (static_cast<size_t> (result.numel ()) == row.length ());
            octave_idx_type i = 0;
            for (const auto& elt : row)
              result(i++) = octave_value_extract<T> (elt);

            return;
          }

        octave_idx_type ncols = row.length ();
        octave_idx_type i = 0;
        OCTAVE_LOCAL_BUFFER (Array<T>, array_list, ncols);

        for (const auto& elt : row)
          {
            octave_quit ();

            array_list[i++] = octave_value_extract<TYPE> (elt);
          }

        result = Array<T>::cat (-2, ncols, array_list);
      }
    else
      {
        result = Array<T> (dv);
        single_type_concat<TYPE> (result, tmp);
      }
  }

  template <typename TYPE, typename T>
  void
  single_type_concat (Sparse<T>& result, const dim_vector& dv,
                      tm_const& tmp)
  {
    if (dv.any_zero ())
      {
        result = Sparse<T> (dv);
        return;
      }

    // Sparse matrices require preallocation for efficient indexing; besides,
    // only horizontal concatenation can be efficiently handled by indexing.
    // So we just cat all rows through liboctave, then cat the final column.
    octave_idx_type nrows = tmp.length ();
    octave_idx_type j = 0;
    OCTAVE_LOCAL_BUFFER (Sparse<T>, sparse_row_list, nrows);
    for (tm_row_const& row : tmp)
      {
        octave_idx_type ncols = row.length ();
        octave_idx_type i = 0;
        OCTAVE_LOCAL_BUFFER (Sparse<T>, sparse_list, ncols);

        for (auto& elt : row)
          {
            octave_quit ();

            sparse_list[i] = octave_value_extract<TYPE> (elt);
            i++;
          }

        Sparse<T> stmp = Sparse<T>::cat (-2, ncols, sparse_list);
        sparse_row_list[j] = stmp;
        j++;
      }

    result = Sparse<T>::cat (-1, nrows, sparse_row_list);
  }

  template <typename MAP>
  void
  single_type_concat (octave_map& result, const dim_vector& dv,
                      tm_const& tmp)
  {
    if (dv.any_zero ())
      {
        result = octave_map (dv);
        return;
      }

    octave_idx_type nrows = tmp.length ();
    octave_idx_type j = 0;
    OCTAVE_LOCAL_BUFFER (octave_map, map_row_list, nrows);
    for (tm_row_const& row : tmp)
      {
        octave_idx_type ncols = row.length ();
        octave_idx_type i = 0;
        OCTAVE_LOCAL_BUFFER (MAP, map_list, ncols);

        for (auto& elt : row)
          {
            octave_quit ();

            map_list[i] = octave_value_extract<MAP> (elt);
            i++;
          }

        octave_map mtmp = octave_map::cat (-2, ncols, map_list);
        map_row_list[j] = mtmp;
        j++;
      }

    result = octave_map::cat (-1, nrows, map_row_list);
  }

  template <typename TYPE>
  octave_value
  do_single_type_concat (const dim_vector& dv, tm_const& tmp)
  {
    TYPE result;

    single_type_concat<TYPE> (result, dv, tmp);

    return result;
  }

  template <>
  octave_value
  do_single_type_concat<octave_map> (const dim_vector& dv,
                                     tm_const& tmp);

  extern octave_value do_class_concat (tm_const& tmc);
}

#endif
