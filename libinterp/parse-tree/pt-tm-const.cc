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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>

#include "oct-locbuf.h"
#include "quit.h"

#include "data.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "oct-map.h"
#include "ovl.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-eval.h"
#include "pt-exp.h"
#include "pt-mat.h"
#include "pt-tm-const.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

OCTAVE_NORETURN static
void
eval_error (const char *msg, const dim_vector& x, const dim_vector& y)
{
  error ("%s (%s vs %s)", msg, x.str ().c_str (), y.str ().c_str ());
}

namespace octave
{
  void
  tm_row_const::tm_row_const_rep::do_init_element (const octave_value& val,
                                                   bool& first_elem)
  {
    std::string this_elt_class_nm
      = val.is_object () ? std::string ("class") : val.class_name ();

    class_nm = get_concat_class (class_nm, this_elt_class_nm);

    dim_vector this_elt_dv = val.dims ();

    if (! this_elt_dv.zero_by_zero ())
      {
        all_mt = false;

        if (first_elem)
          {
            if (val.is_map ())
              first_elem_is_struct = true;

            first_elem = false;
          }
      }
    else if (val.iscell ())
      first_elem = false;

    append (val);

    if (all_str && ! val.is_string ())
      all_str = false;

    if (all_sq_str && ! val.is_sq_string ())
      all_sq_str = false;

    if (all_dq_str && ! val.is_dq_string ())
      all_dq_str = false;

    if (! some_str && val.is_string ())
      some_str = true;

    if (all_real && ! val.is_real_type ())
      all_real = false;

    if (all_cmplx && ! (val.is_complex_type () || val.is_real_type ()))
      all_cmplx = false;

    if (! any_cell && val.iscell ())
      any_cell = true;

    if (! any_sparse && val.is_sparse_type ())
      any_sparse = true;

    if (! any_class && val.is_object ())
      any_class = true;

    // Special treatment of sparse matrices to avoid out-of-memory error
    all_1x1 = all_1x1 && ! val.is_sparse_type () && val.numel () == 1;
  }

  void
  tm_row_const::tm_row_const_rep::init (const tree_argument_list& row,
                                        tree_evaluator *tw)
  {
    all_str = true;
    all_sq_str = true;
    all_dq_str = true;
    all_real = true;
    all_cmplx = true;
    any_cell = false;
    any_sparse = false;
    any_class = false;

    bool first_elem = true;

    for (tree_expression* elt : row)
      {
        octave_quit ();

        octave_value tmp = tw->evaluate (elt);

        if (tmp.is_undefined ())
          {
            ok = true;
            return;
          }
        else
          {
            if (tmp.is_cs_list ())
              {
                octave_value_list tlst = tmp.list_value ();

                for (octave_idx_type i = 0; i < tlst.length (); i++)
                  {
                    octave_quit ();

                    do_init_element (tlst(i), first_elem);
                  }
              }
            else
              do_init_element (tmp, first_elem);
          }
      }

    if (any_cell && ! any_class && ! first_elem_is_struct)
      cellify ();

    first_elem = true;

    for (const octave_value& val : *this)
      {
        octave_quit ();

        dim_vector this_elt_dv = val.dims ();

        if (! this_elt_dv.zero_by_zero ())
          {
            all_mt = false;

            if (first_elem)
              {
                first_elem = false;
                dv = this_elt_dv;
              }
            else if ((! any_class) && (! dv.hvcat (this_elt_dv, 1)))
              eval_error ("horizontal dimensions mismatch", dv, this_elt_dv);
          }
      }

    ok = true;
  }

  void
  tm_row_const::tm_row_const_rep::cellify (void)
  {
    bool elt_changed = false;

    for (auto& elt : *this)
      {
        octave_quit ();

        if (! elt.iscell ())
          {
            elt_changed = true;

            if (elt.is_empty ())
              elt = Cell ();
            else
              elt = Cell (elt);
          }
      }

    if (elt_changed)
      {
        bool first_elem = true;

        for (const octave_value& val : *this)
          {
            octave_quit ();

            dim_vector this_elt_dv = val.dims ();

            if (! this_elt_dv.zero_by_zero ())
              {
                if (first_elem)
                  {
                    first_elem = false;
                    dv = this_elt_dv;
                  }
                else if (! dv.hvcat (this_elt_dv, 1))
                  eval_error ("horizontal dimensions mismatch", dv, this_elt_dv);
              }
          }
      }
  }

  void
  tm_const::init (const tree_matrix& tm, tree_evaluator *tw)
  {
    all_str = true;
    all_sq_str = true;
    all_dq_str = true;
    all_real = true;
    all_cmplx = true;
    any_cell = false;
    any_sparse = false;
    any_class = false;
    all_1x1 = ! tm.empty ();

    bool first_elem = true;
    bool first_elem_is_struct = false;

    // Just eval and figure out if what we have is complex or all strings.
    // We can't check columns until we know that this is a numeric matrix --
    // collections of strings can have elements of different lengths.
    for (const tree_argument_list* elt : tm)
      {
        octave_quit ();

        tm_row_const tmp (*elt, tw);

        if (first_elem)
          {
            first_elem_is_struct = tmp.first_elem_struct_p ();

            first_elem = false;
          }

        if (tmp && ! tmp.empty ())
          {
            if (all_str && ! tmp.all_strings_p ())
              all_str = false;

            if (all_sq_str && ! tmp.all_sq_strings_p ())
              all_sq_str = false;

            if (all_dq_str && ! tmp.all_dq_strings_p ())
              all_dq_str = false;

            if (! some_str && tmp.some_strings_p ())
              some_str = true;

            if (all_real && ! tmp.all_real_p ())
              all_real = false;

            if (all_cmplx && ! tmp.all_complex_p ())
              all_cmplx = false;

            if (all_mt && ! tmp.all_empty_p ())
              all_mt = false;

            if (! any_cell && tmp.any_cell_p ())
              any_cell = true;

            if (! any_sparse && tmp.any_sparse_p ())
              any_sparse = true;

            if (! any_class && tmp.any_class_p ())
              any_class = true;

            all_1x1 = all_1x1 && tmp.all_1x1_p ();

            append (tmp);
          }
        else
          break;
      }

    if (any_cell && ! any_class && ! first_elem_is_struct)
      {
        for (auto& elt : *this)
          {
            octave_quit ();

            elt.cellify ();
          }
      }

    first_elem = true;

    for (tm_row_const& elt : *this)
      {
        octave_quit ();

        octave_idx_type this_elt_nr = elt.rows ();
        octave_idx_type this_elt_nc = elt.cols ();

        std::string this_elt_class_nm = elt.class_name ();
        class_nm = get_concat_class (class_nm, this_elt_class_nm);

        dim_vector this_elt_dv = elt.dims ();

        all_mt = false;

        if (first_elem)
          {
            first_elem = false;

            dv = this_elt_dv;
          }
        else if (all_str && dv.ndims () == 2
                 && this_elt_dv.ndims () == 2)
          {
            // FIXME: this is Octave's specialty.
            // Character matrices allow rows of unequal length.
            if (this_elt_nc > cols ())
              dv(1) = this_elt_nc;
            dv(0) += this_elt_nr;
          }
        else if ((! any_class) && (! dv.hvcat (this_elt_dv, 0)))
          eval_error ("vertical dimensions mismatch", dv, this_elt_dv);
      }

    ok = true;
  }

  template <>
  octave_value
  do_single_type_concat<octave_map> (const dim_vector& dv,
                                     octave::tm_const& tmp)
  {
    octave_map result;

    if (tmp.all_1x1_p ())
      single_type_concat<octave_scalar_map> (result, dv, tmp);
    else
      single_type_concat<octave_map> (result, dv, tmp);

    return result;
  }

  octave_value do_class_concat (octave::tm_const& tmc)
  {
    octave_value retval;

    octave_value_list rows (tmc.length (), octave_value ());

    octave_idx_type j = 0;
    for (octave::tm_row_const& tmrc : tmc)
      {
        octave_quit ();

        if (tmrc.length () == 1)
          rows(j++) = *(tmrc.begin ());
        else
          {
            octave_value_list row (tmrc.length (), octave_value ());

            octave_idx_type i = 0;
            for (auto& elt : tmrc)
              row(i++) = elt;

            rows(j++) = do_class_concat (row, "horzcat", 1);
          }
      }

    if (rows.length () == 1)
      retval = rows(0);
    else
      retval = do_class_concat (rows, "vertcat", 0);

    return retval;
  }
}
