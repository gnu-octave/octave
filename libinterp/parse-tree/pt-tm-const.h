/*

Copyright (C) 1996-2017 John W. Eaton

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

namespace octave
{
  class tree_evaluator;

  // General matrices.  This list type is much more work to handle than
  // constant matrices, but it allows us to construct matrices from
  // other matrices, variables, and functions.

  // But first, some internal classes that make our job much easier.

  class
  tm_row_const
  {
  private:

    class
    tm_row_const_rep : public octave::base_list<octave_value>
    {
    public:

      tm_row_const_rep (void)
        : count (1), dv (0, 0), all_str (false),
          all_sq_str (false), all_dq_str (false),
          some_str (false), all_real (false), all_cmplx (false),
          all_mt (true), any_cell (false), any_sparse (false),
          any_class (false), all_1x1 (false),
          first_elem_is_struct (false), class_nm (), ok (false)
      { }

      tm_row_const_rep (const tree_argument_list& row, tree_evaluator *tw)
        : count (1), dv (0, 0), all_str (false), all_sq_str (false),
          some_str (false), all_real (false), all_cmplx (false),
          all_mt (true), any_cell (false), any_sparse (false),
          any_class (false), all_1x1 (! row.empty ()),
          first_elem_is_struct (false), class_nm (), ok (false)
      { init (row, tw); }

      ~tm_row_const_rep (void) = default;

      octave::refcount<int> count;

      dim_vector dv;

      bool all_str;
      bool all_sq_str;
      bool all_dq_str;
      bool some_str;
      bool all_real;
      bool all_cmplx;
      bool all_mt;
      bool any_cell;
      bool any_sparse;
      bool any_class;
      bool all_1x1;
      bool first_elem_is_struct;

      std::string class_nm;

      bool ok;

      void do_init_element (const octave_value&, bool&);

      void init (const tree_argument_list&, tree_evaluator *tw);

      void cellify (void);

    private:

      tm_row_const_rep (const tm_row_const_rep&);

      tm_row_const_rep& operator = (const tm_row_const_rep&);

    };

  public:

    typedef tm_row_const_rep::iterator iterator;
    typedef tm_row_const_rep::const_iterator const_iterator;

    tm_row_const (void)
      : rep (0) { }

    tm_row_const (const tree_argument_list& row, tree_evaluator *tw)
      : rep (new tm_row_const_rep (row, tw)) { }

    tm_row_const (const tm_row_const& x)
      : rep (x.rep)
    {
      if (rep)
        rep->count++;
    }

    tm_row_const& operator = (const tm_row_const& x)
    {
      if (this != &x && rep != x.rep)
        {
          if (rep && --rep->count == 0)
            delete rep;

          rep = x.rep;

          if (rep)
            rep->count++;
        }

      return *this;
    }

    ~tm_row_const (void)
    {
      if (rep && --rep->count == 0)
        delete rep;
    }

    octave_idx_type rows (void) { return rep->dv(0); }
    octave_idx_type cols (void) { return rep->dv(1); }

    bool empty (void) const { return rep->empty (); }

    size_t length (void) const { return rep->length (); }

    dim_vector dims (void) { return rep->dv; }

    bool all_strings_p (void) const { return rep->all_str; }
    bool all_sq_strings_p (void) const { return rep->all_sq_str; }
    bool all_dq_strings_p (void) const { return rep->all_dq_str; }
    bool some_strings_p (void) const { return rep->some_str; }
    bool all_real_p (void) const { return rep->all_real; }
    bool all_complex_p (void) const { return rep->all_cmplx; }
    bool all_empty_p (void) const { return rep->all_mt; }
    bool any_cell_p (void) const { return rep->any_cell; }
    bool any_sparse_p (void) const { return rep->any_sparse; }
    bool any_class_p (void) const { return rep->any_class; }
    bool all_1x1_p (void) const { return rep->all_1x1; }
    bool first_elem_struct_p (void) const { return rep->first_elem_is_struct; }

    std::string class_name (void) const { return rep->class_nm; }

    void cellify (void) { rep->cellify (); }

    operator bool () const { return (rep && rep->ok); }

    iterator begin (void) { return rep->begin (); }
    const_iterator begin (void) const { return rep->begin (); }

    iterator end (void) { return rep->end (); }
    const_iterator end (void) const { return rep->end (); }

  private:

    tm_row_const_rep *rep;
  };

  class
  tm_const : public octave::base_list<tm_row_const>
  {
  public:

    tm_const (const tree_matrix& tm, tree_evaluator *tw = 0)
      : dv (0, 0), all_str (false), all_sq_str (false),
        all_dq_str (false),
        some_str (false), all_real (false), all_cmplx (false),
        all_mt (true), any_cell (false), any_sparse (false),
        any_class (false), class_nm (), ok (false)
    { init (tm, tw); }

    ~tm_const (void) = default;

    octave_idx_type rows (void) const { return dv.elem (0); }
    octave_idx_type cols (void) const { return dv.elem (1); }

    dim_vector dims (void) const { return dv; }

    bool all_strings_p (void) const { return all_str; }
    bool all_sq_strings_p (void) const { return all_sq_str; }
    bool all_dq_strings_p (void) const { return all_dq_str; }
    bool some_strings_p (void) const { return some_str; }
    bool all_real_p (void) const { return all_real; }
    bool all_complex_p (void) const { return all_cmplx; }
    bool all_empty_p (void) const { return all_mt; }
    bool any_cell_p (void) const { return any_cell; }
    bool any_sparse_p (void) const { return any_sparse; }
    bool any_class_p (void) const { return any_class; }
    bool all_1x1_p (void) const { return all_1x1; }

    std::string class_name (void) const { return class_nm; }

    operator bool () const { return ok; }

  private:

    dim_vector dv;

    bool all_str;
    bool all_sq_str;
    bool all_dq_str;
    bool some_str;
    bool all_real;
    bool all_cmplx;
    bool all_mt;
    bool any_cell;
    bool any_sparse;
    bool any_class;
    bool all_1x1;

    std::string class_nm;

    bool ok;

    tm_const (void);

    tm_const (const tm_const&);

    tm_const& operator = (const tm_const&);

    void init (const tree_matrix& tm, tree_evaluator *tw);
  };

  template <typename TYPE, typename T>
  void
  single_type_concat (Array<T>& result, octave::tm_const& tmp)
  {
    octave_idx_type r = 0;
    octave_idx_type c = 0;

    for (octave::tm_row_const& row : tmp)
      {
        // Skip empty arrays to allow looser rules.
        if (row.dims ().any_zero ())
          continue;

        for (auto& elt : row)
          {
            octave_quit ();

            TYPE ra = octave_value_extract<TYPE> (elt);

            // Skip empty arrays to allow looser rules.

            if (! ra.is_empty ())
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
                      octave::tm_const& tmp)
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
        octave::tm_row_const& row = tmp.front ();
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
                      octave::tm_const& tmp)
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
    for (octave::tm_row_const& row : tmp)
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
                      octave::tm_const& tmp)
  {
    if (dv.any_zero ())
      {
        result = octave_map (dv);
        return;
      }

    octave_idx_type nrows = tmp.length ();
    octave_idx_type j = 0;
    OCTAVE_LOCAL_BUFFER (octave_map, map_row_list, nrows);
    for (octave::tm_row_const& row : tmp)
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
  do_single_type_concat (const dim_vector& dv, octave::tm_const& tmp)
  {
    TYPE result;

    single_type_concat<TYPE> (result, dv, tmp);

    return result;
  }

  template <>
  octave_value
  do_single_type_concat<octave_map> (const dim_vector& dv,
                                     octave::tm_const& tmp);

  extern octave_value do_class_concat (octave::tm_const& tmc);
}

#endif
