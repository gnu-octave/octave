////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

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

OCTAVE_BEGIN_NAMESPACE(octave)

void tm_row_const::cellify (void)
{
  bool elt_changed = false;

  for (auto& elt : m_values)
    {
      octave_quit ();

      if (! elt.iscell ())
        {
          elt_changed = true;

          if (elt.isempty ())
            elt = Cell ();
          else
            elt = Cell (elt);
        }
    }

  if (! elt_changed)
    return;

  bool first_elem = true;

  for (const auto& val : m_values)
    {
      octave_quit ();

      dim_vector this_elt_dv = val.dims ();

      if (! this_elt_dv.zero_by_zero ())
        {
          if (first_elem)
            {
              first_elem = false;
              m_dv = this_elt_dv;
            }
          else if (! m_dv.hvcat (this_elt_dv, 1))
            eval_error ("horizontal dimensions mismatch", m_dv, this_elt_dv);
        }
    }
}

void tm_row_const::init_element (const octave_value& val, bool& first_elem)
{
  std::string this_elt_class_name
    = val.isobject () ? "class" : val.class_name ();

  m_class_name = get_concat_class (m_class_name, this_elt_class_name);

  dim_vector this_elt_dv = val.dims ();

  if (! this_elt_dv.zero_by_zero ())
    {
      m_all_empty = false;

      if (first_elem)
        {
          if (val.isstruct ())
            m_first_elem_is_struct = true;

          first_elem = false;
        }
    }
  else if (val.iscell ())
    first_elem = false;

  m_values.push_back (val);

  if (m_all_strings && ! val.is_string ())
    m_all_strings = false;

  if (m_all_sq_strings && ! val.is_sq_string ())
    m_all_sq_strings = false;

  if (m_all_dq_strings && ! val.is_dq_string ())
    m_all_dq_strings = false;

  if (! m_some_strings && val.is_string ())
    m_some_strings = true;

  if (m_all_real && ! val.isreal ())
    m_all_real = false;

  if (m_all_complex && ! (val.iscomplex () || val.isreal ()))
    m_all_complex = false;

  if (! m_any_cell && val.iscell ())
    m_any_cell = true;

  if (! m_any_sparse && val.issparse ())
    m_any_sparse = true;

  if (! m_any_class && val.isobject ())
    m_any_class = true;

  // Special treatment of sparse matrices to avoid out-of-memory error
  m_all_1x1 = m_all_1x1 && ! val.issparse () && val.numel () == 1;
}

void tm_row_const::init (const tree_argument_list& row, tree_evaluator& tw)
{
  bool first_elem = true;

  for (auto *elt : row)
    {
      octave_quit ();

      octave_value tmp = elt->evaluate (tw);

      if (tmp.is_undefined ())
        error ("undefined element in matrix list");

      if (tmp.is_cs_list ())
        {
          octave_value_list tlst = tmp.list_value ();

          for (octave_idx_type i = 0; i < tlst.length (); i++)
            {
              octave_quit ();

              init_element (tlst(i), first_elem);
            }
        }
      else
        init_element (tmp, first_elem);
    }

  if (m_any_cell && ! m_any_class && ! m_first_elem_is_struct)
    cellify ();

  first_elem = true;

  for (const auto& val : m_values)
    {
      octave_quit ();

      dim_vector this_elt_dv = val.dims ();

      if (! this_elt_dv.zero_by_zero ())
        {
          m_all_empty = false;

          if (first_elem)
            {
              first_elem = false;
              m_dv = this_elt_dv;
            }
          else if ((! m_any_class) && (! m_dv.hvcat (this_elt_dv, 1)))
            eval_error ("horizontal dimensions mismatch", m_dv, this_elt_dv);
        }
    }
}

octave_value tm_const::concat (char string_fill_char) const
{
  if (m_tm_rows.empty ())
    return Matrix ();

  // Try to speed up the common cases.

  std::string result_type = m_class_name;

  if (m_any_class)
    return class_concat ();
  else if (result_type == "double")
    {
      if (m_any_sparse)
        {
          if (m_all_real)
            return sparse_array_concat<SparseMatrix> ();
          else
            return sparse_array_concat<SparseComplexMatrix> ();
        }
      else
        {
          if (m_all_real)
            return array_concat<NDArray> ();
          else
            return array_concat<ComplexNDArray> ();
        }
    }
  else if (result_type == "single")
    {
      if (m_all_real)
        return array_concat<FloatNDArray> ();
      else
        return array_concat<FloatComplexNDArray> ();
    }
  else if (result_type == "char")
    {
      if (! m_all_strings)
        warn_implicit_conversion ("Octave:num-to-str",
                                  "numeric", result_type);
      else
        maybe_warn_string_concat (m_all_dq_strings, m_all_sq_strings);

      return char_array_concat (string_fill_char);
    }
  else if (result_type == "logical")
    {
      if (m_any_sparse)
        return sparse_array_concat<SparseBoolMatrix> ();
      else
        return array_concat<boolNDArray> ();
    }
  else if (result_type == "int8")
    return array_concat<int8NDArray> ();
  else if (result_type == "int16")
    return array_concat<int16NDArray> ();
  else if (result_type == "int32")
    return array_concat<int32NDArray> ();
  else if (result_type == "int64")
    return array_concat<int64NDArray> ();
  else if (result_type == "uint8")
    return array_concat<uint8NDArray> ();
  else if (result_type == "uint16")
    return array_concat<uint16NDArray> ();
  else if (result_type == "uint32")
    return array_concat<uint32NDArray> ();
  else if (result_type == "uint64")
    return array_concat<uint64NDArray> ();
  else if (result_type == "cell")
    return array_concat<Cell> ();
  else if (result_type == "struct")
    {
      if (m_all_1x1)
        return map_concat<octave_scalar_map> ();
      else
        return map_concat<octave_map> ();
    }
  else
    return generic_concat ();
}

void tm_const::init (const tree_matrix& tm)
{
  bool first_elem = true;
  bool first_elem_is_struct = false;

  // Just eval and figure out if what we have is complex or all strings.
  // We can't check columns until we know that this is a numeric matrix --
  // collections of strings can have elements of different lengths.

  for (const auto *elt : tm)
    {
      octave_quit ();

      tm_row_const row (*elt, m_evaluator);

      if (first_elem)
        {
          first_elem_is_struct = row.first_elem_struct_p ();

          first_elem = false;
        }

      if (row.empty ())
        continue;

      if (m_all_strings && ! row.all_strings_p ())
        m_all_strings = false;

      if (m_all_sq_strings && ! row.all_sq_strings_p ())
        m_all_sq_strings = false;

      if (m_all_dq_strings && ! row.all_dq_strings_p ())
        m_all_dq_strings = false;

      if (! m_some_strings && row.some_strings_p ())
        m_some_strings = true;

      if (m_all_real && ! row.all_real_p ())
        m_all_real = false;

      if (m_all_complex && ! row.all_complex_p ())
        m_all_complex = false;

      if (m_all_empty && ! row.all_empty_p ())
        m_all_empty = false;

      if (! m_any_cell && row.any_cell_p ())
        m_any_cell = true;

      if (! m_any_sparse && row.any_sparse_p ())
        m_any_sparse = true;

      if (! m_any_class && row.any_class_p ())
        m_any_class = true;

      m_all_1x1 = m_all_1x1 && row.all_1x1_p ();

      m_tm_rows.push_back (row);
    }

  if (m_any_cell && ! m_any_class && ! first_elem_is_struct)
    {
      for (auto& elt : m_tm_rows)
        {
          octave_quit ();

          elt.cellify ();
        }
    }

  first_elem = true;

  for (const auto& elt : m_tm_rows)
    {
      octave_quit ();

      octave_idx_type this_elt_nr = elt.rows ();
      octave_idx_type this_elt_nc = elt.cols ();

      std::string this_elt_class_name = elt.class_name ();
      m_class_name = get_concat_class (m_class_name, this_elt_class_name);

      dim_vector this_elt_dv = elt.dims ();

      m_all_empty = false;

      if (first_elem)
        {
          first_elem = false;

          m_dv = this_elt_dv;
        }
      else if (m_all_strings && m_dv.ndims () == 2
               && this_elt_dv.ndims () == 2)
        {
          // This is Octave's specialty.
          // Character matrices support rows of unequal length.
          if (m_dv.any_zero ())
            {
              // Empty existing element (bug #52542).
              // Replace empty element with non-empty one.
              m_dv = this_elt_dv;
            }
          else
            {
              if (this_elt_nc > cols ())
                m_dv(1) = this_elt_nc;
              m_dv(0) += this_elt_nr;
            }
        }
      else if ((! m_any_class) && (! m_dv.hvcat (this_elt_dv, 0)))
        eval_error ("vertical dimensions mismatch", m_dv, this_elt_dv);
    }
}

octave_value tm_const::char_array_concat (char string_fill_char) const
{
  char type = (m_all_dq_strings ? '"' : '\'');

  charNDArray result (m_dv, string_fill_char);

  array_concat_internal<charNDArray> (result);

  return octave_value (result, type);
}

octave_value tm_const::class_concat (void) const
{
  octave_value retval;

  octave_value_list rows (m_tm_rows.size (), octave_value ());

  octave_idx_type j = 0;
  for (const auto& tmrc : m_tm_rows)
    {
      octave_quit ();

      if (tmrc.length () == 1)
        rows(j++) = *(tmrc.begin ());
      else
        {
          octave_value_list row (tmrc.length (), octave_value ());

          octave_idx_type i = 0;
          for (const auto& elt : tmrc)
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

octave_value tm_const::generic_concat (void) const
{
  // The line below might seem crazy, since we take a copy of the
  // first argument, resize it to be empty and then resize it to be
  // full.  This is done since it means that there is no recopying of
  // data, as would happen if we used a single resize.  It should be
  // noted that resize operation is also significantly slower than the
  // cat_op function, so it makes sense to have an empty matrix and
  // copy all data.
  //
  // We might also start with a empty octave_value using
  //
  //   ctmp = type_info::lookup_type (tmp.begin() -> begin() -> type_name());
  //
  // and then directly resize.  However, for some types there might be
  // some additional setup needed, and so this should be avoided.

  octave_value ctmp;

  // Find the first non-empty object

  if (m_any_sparse)
    {
      // Start with sparse matrix to avoid issues memory issues with
      // things like [ones(1,4),sprandn(1e8,4,1e-4)]

      if (m_all_real)
        ctmp = octave_sparse_matrix ().resize (m_dv);
      else
        ctmp = octave_sparse_complex_matrix ().resize (m_dv);
    }
  else
    {
      for (const auto& row : m_tm_rows)
        {
          octave_quit ();

          for (const auto& elt : row)
            {
              octave_quit ();

              ctmp = elt;

              if (! ctmp.all_zero_dims ())
                goto found_non_empty;
            }
        }

      ctmp = (*(m_tm_rows.begin () -> begin ()));

    found_non_empty:

      if (! m_all_empty)
        ctmp = ctmp.resize (dim_vector (0, 0)).resize (m_dv);
    }

  // Now, extract the values from the individual elements and insert
  // them in the result matrix.

  interpreter& interp = m_evaluator.get_interpreter ();

  type_info& ti = interp.get_type_info ();

  int dv_len = m_dv.ndims ();
  octave_idx_type ntmp = (dv_len > 1 ? dv_len : 2);
  Array<octave_idx_type> ra_idx (dim_vector (ntmp, 1), 0);

  for (const auto& row : m_tm_rows)
    {
      octave_quit ();

      for (const auto& elt : row)
        {
          octave_quit ();

          if (elt.isempty ())
            continue;

          ctmp = cat_op (ti, ctmp, elt, ra_idx);

          ra_idx (1) += elt.columns ();
        }

      ra_idx (0) += row.rows ();
      ra_idx (1) = 0;
    }

  octave_value retval = ctmp;

  // If some elements are strings, force the result to be a string.

  if (m_some_strings && ! retval.is_string ())
    retval = retval.convert_to_str ();

  return retval;
}

// The result is passed as a parameter to this function so that the
// char_array_concat function can create the array externally.
// Otherwise, we would need a specialization of this function for
// character arrays just to handle string_fill_char.

template <typename TYPE>
void tm_const::array_concat_internal (TYPE& result) const
{
  octave_idx_type r = 0;
  octave_idx_type c = 0;

  for (const auto& row : m_tm_rows)
    {
      // Skip empty arrays to allow looser rules.
      if (row.dims ().any_zero ())
        continue;

      for (const auto& elt : row)
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

template <typename TYPE>
TYPE tm_const::array_concat (void) const
{
  typedef typename TYPE::element_type ELT_T;

  if (m_dv.any_zero ())
    return TYPE (m_dv);

  if (m_tm_rows.size () == 1)
    {
      // If possible, forward the operation to liboctave.
      // Single row.
      const tm_row_const& row = m_tm_rows.front ();
      if (! (equal_types<ELT_T, char>::value
             || equal_types<ELT_T, octave_value>::value)
          && row.all_1x1_p ())
        {
          // Optimize all scalars case.
          TYPE result (m_dv);
          panic_unless (static_cast<std::size_t> (result.numel ())
                        == row.length ());
          octave_idx_type i = 0;
          for (const auto& elt : row)
            result(i++) = octave_value_extract<ELT_T> (elt);

          return result;
        }

      octave_idx_type ncols = row.length ();
      octave_idx_type i = 0;
      OCTAVE_LOCAL_BUFFER (TYPE, array_list, ncols);

      for (const auto& elt : row)
        {
          octave_quit ();

          array_list[i++] = octave_value_extract<TYPE> (elt);
        }

      return TYPE::cat (-2, ncols, array_list);
    }
  else
    {
      TYPE result (m_dv);
      array_concat_internal<TYPE> (result);
      return result;
    }
}

template <typename TYPE>
TYPE tm_const::sparse_array_concat (void) const
{
  if (m_dv.any_zero ())
    return TYPE (m_dv);

  // Sparse matrices require preallocation for efficient indexing; besides,
  // only horizontal concatenation can be efficiently handled by indexing.
  // So we just cat all rows through liboctave, then cat the final column.
  octave_idx_type nrows = m_tm_rows.size ();
  octave_idx_type j = 0;
  OCTAVE_LOCAL_BUFFER (TYPE, sparse_row_list, nrows);
  for (const auto& row : m_tm_rows)
    {
      octave_idx_type ncols = row.length ();
      octave_idx_type i = 0;
      OCTAVE_LOCAL_BUFFER (TYPE, sparse_list, ncols);

      for (auto& elt : row)
        {
          octave_quit ();

          sparse_list[i] = octave_value_extract<TYPE> (elt);
          i++;
        }

      TYPE stmp = TYPE::cat (-2, ncols, sparse_list);
      sparse_row_list[j] = stmp;
      j++;
    }

  return TYPE::cat (-1, nrows, sparse_row_list);
}

template <typename MAP>
octave_map tm_const::map_concat (void) const
{
  if (m_dv.any_zero ())
    return octave_map (m_dv);

  octave_idx_type nrows = m_tm_rows.size ();
  octave_idx_type j = 0;
  OCTAVE_LOCAL_BUFFER (octave_map, map_row_list, nrows);
  for (const auto& row : m_tm_rows)
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

  return octave_map::cat (-1, nrows, map_row_list);
}

OCTAVE_END_NAMESPACE(octave)

/*
## test concatenation with all zero matrices
%!assert ([ "" 65*ones(1,10) ], "AAAAAAAAAA")
%!assert ([ 65*ones(1,10) "" ], "AAAAAAAAAA")

%!test
%! c = {"foo"; "bar"; "bazoloa"};
%! assert ([c; "a"; "bc"; "def"], {"foo"; "bar"; "bazoloa"; "a"; "bc"; "def"});

%!assert (class ([int64(1), int64(1)]), "int64")
%!assert (class ([int64(1), int32(1)]), "int64")
%!assert (class ([int64(1), int16(1)]), "int64")
%!assert (class ([int64(1), int8(1)]), "int64")
%!assert (class ([int64(1), uint64(1)]), "int64")
%!assert (class ([int64(1), uint32(1)]), "int64")
%!assert (class ([int64(1), uint16(1)]), "int64")
%!assert (class ([int64(1), uint8(1)]), "int64")
%!assert (class ([int64(1), single(1)]), "int64")
%!assert (class ([int64(1), double(1)]), "int64")
%!assert (class ([int64(1), cell(1)]), "cell")
%!assert (class ([int64(1), true]), "int64")
%!assert (class ([int64(1), "a"]), "char")

%!assert (class ([int32(1), int64(1)]), "int32")
%!assert (class ([int32(1), int32(1)]), "int32")
%!assert (class ([int32(1), int16(1)]), "int32")
%!assert (class ([int32(1), int8(1)]), "int32")
%!assert (class ([int32(1), uint64(1)]), "int32")
%!assert (class ([int32(1), uint32(1)]), "int32")
%!assert (class ([int32(1), uint16(1)]), "int32")
%!assert (class ([int32(1), uint8(1)]), "int32")
%!assert (class ([int32(1), single(1)]), "int32")
%!assert (class ([int32(1), double(1)]), "int32")
%!assert (class ([int32(1), cell(1)]), "cell")
%!assert (class ([int32(1), true]), "int32")
%!assert (class ([int32(1), "a"]), "char")

%!assert (class ([int16(1), int64(1)]), "int16")
%!assert (class ([int16(1), int32(1)]), "int16")
%!assert (class ([int16(1), int16(1)]), "int16")
%!assert (class ([int16(1), int8(1)]), "int16")
%!assert (class ([int16(1), uint64(1)]), "int16")
%!assert (class ([int16(1), uint32(1)]), "int16")
%!assert (class ([int16(1), uint16(1)]), "int16")
%!assert (class ([int16(1), uint8(1)]), "int16")
%!assert (class ([int16(1), single(1)]), "int16")
%!assert (class ([int16(1), double(1)]), "int16")
%!assert (class ([int16(1), cell(1)]), "cell")
%!assert (class ([int16(1), true]), "int16")
%!assert (class ([int16(1), "a"]), "char")

%!assert (class ([int8(1), int64(1)]), "int8")
%!assert (class ([int8(1), int32(1)]), "int8")
%!assert (class ([int8(1), int16(1)]), "int8")
%!assert (class ([int8(1), int8(1)]), "int8")
%!assert (class ([int8(1), uint64(1)]), "int8")
%!assert (class ([int8(1), uint32(1)]), "int8")
%!assert (class ([int8(1), uint16(1)]), "int8")
%!assert (class ([int8(1), uint8(1)]), "int8")
%!assert (class ([int8(1), single(1)]), "int8")
%!assert (class ([int8(1), double(1)]), "int8")
%!assert (class ([int8(1), cell(1)]), "cell")
%!assert (class ([int8(1), true]), "int8")
%!assert (class ([int8(1), "a"]), "char")

%!assert (class ([uint64(1), int64(1)]), "uint64")
%!assert (class ([uint64(1), int32(1)]), "uint64")
%!assert (class ([uint64(1), int16(1)]), "uint64")
%!assert (class ([uint64(1), int8(1)]), "uint64")
%!assert (class ([uint64(1), uint64(1)]), "uint64")
%!assert (class ([uint64(1), uint32(1)]), "uint64")
%!assert (class ([uint64(1), uint16(1)]), "uint64")
%!assert (class ([uint64(1), uint8(1)]), "uint64")
%!assert (class ([uint64(1), single(1)]), "uint64")
%!assert (class ([uint64(1), double(1)]), "uint64")
%!assert (class ([uint64(1), cell(1)]), "cell")
%!assert (class ([uint64(1), true]), "uint64")
%!assert (class ([uint64(1), "a"]), "char")

%!assert (class ([uint32(1), int64(1)]), "uint32")
%!assert (class ([uint32(1), int32(1)]), "uint32")
%!assert (class ([uint32(1), int16(1)]), "uint32")
%!assert (class ([uint32(1), int8(1)]), "uint32")
%!assert (class ([uint32(1), uint64(1)]), "uint32")
%!assert (class ([uint32(1), uint32(1)]), "uint32")
%!assert (class ([uint32(1), uint16(1)]), "uint32")
%!assert (class ([uint32(1), uint8(1)]), "uint32")
%!assert (class ([uint32(1), single(1)]), "uint32")
%!assert (class ([uint32(1), double(1)]), "uint32")
%!assert (class ([uint32(1), cell(1)]), "cell")
%!assert (class ([uint32(1), true]), "uint32")
%!assert (class ([uint32(1), "a"]), "char")

%!assert (class ([uint16(1), int64(1)]), "uint16")
%!assert (class ([uint16(1), int32(1)]), "uint16")
%!assert (class ([uint16(1), int16(1)]), "uint16")
%!assert (class ([uint16(1), int8(1)]), "uint16")
%!assert (class ([uint16(1), uint64(1)]), "uint16")
%!assert (class ([uint16(1), uint32(1)]), "uint16")
%!assert (class ([uint16(1), uint16(1)]), "uint16")
%!assert (class ([uint16(1), uint8(1)]), "uint16")
%!assert (class ([uint16(1), single(1)]), "uint16")
%!assert (class ([uint16(1), double(1)]), "uint16")
%!assert (class ([uint16(1), cell(1)]), "cell")
%!assert (class ([uint16(1), true]), "uint16")
%!assert (class ([uint16(1), "a"]), "char")

%!assert (class ([uint8(1), int64(1)]), "uint8")
%!assert (class ([uint8(1), int32(1)]), "uint8")
%!assert (class ([uint8(1), int16(1)]), "uint8")
%!assert (class ([uint8(1), int8(1)]), "uint8")
%!assert (class ([uint8(1), uint64(1)]), "uint8")
%!assert (class ([uint8(1), uint32(1)]), "uint8")
%!assert (class ([uint8(1), uint16(1)]), "uint8")
%!assert (class ([uint8(1), uint8(1)]), "uint8")
%!assert (class ([uint8(1), single(1)]), "uint8")
%!assert (class ([uint8(1), double(1)]), "uint8")
%!assert (class ([uint8(1), cell(1)]), "cell")
%!assert (class ([uint8(1), true]), "uint8")
%!assert (class ([uint8(1), "a"]), "char")

%!assert (class ([single(1), int64(1)]), "int64")
%!assert (class ([single(1), int32(1)]), "int32")
%!assert (class ([single(1), int16(1)]), "int16")
%!assert (class ([single(1), int8(1)]), "int8")
%!assert (class ([single(1), uint64(1)]), "uint64")
%!assert (class ([single(1), uint32(1)]), "uint32")
%!assert (class ([single(1), uint16(1)]), "uint16")
%!assert (class ([single(1), uint8(1)]), "uint8")
%!assert (class ([single(1), single(1)]), "single")
%!assert (class ([single(1), double(1)]), "single")
%!assert (class ([single(1), cell(1)]), "cell")
%!assert (class ([single(1), true]), "single")
%!assert (class ([single(1), "a"]), "char")

%!assert (class ([double(1), int64(1)]), "int64")
%!assert (class ([double(1), int32(1)]), "int32")
%!assert (class ([double(1), int16(1)]), "int16")
%!assert (class ([double(1), int8(1)]), "int8")
%!assert (class ([double(1), uint64(1)]), "uint64")
%!assert (class ([double(1), uint32(1)]), "uint32")
%!assert (class ([double(1), uint16(1)]), "uint16")
%!assert (class ([double(1), uint8(1)]), "uint8")
%!assert (class ([double(1), single(1)]), "single")
%!assert (class ([double(1), double(1)]), "double")
%!assert (class ([double(1), cell(1)]), "cell")
%!assert (class ([double(1), true]), "double")
%!assert (class ([double(1), "a"]), "char")

%!assert (class ([cell(1), int64(1)]), "cell")
%!assert (class ([cell(1), int32(1)]), "cell")
%!assert (class ([cell(1), int16(1)]), "cell")
%!assert (class ([cell(1), int8(1)]), "cell")
%!assert (class ([cell(1), uint64(1)]), "cell")
%!assert (class ([cell(1), uint32(1)]), "cell")
%!assert (class ([cell(1), uint16(1)]), "cell")
%!assert (class ([cell(1), uint8(1)]), "cell")
%!assert (class ([cell(1), single(1)]), "cell")
%!assert (class ([cell(1), double(1)]), "cell")
%!assert (class ([cell(1), cell(1)]), "cell")
%!assert (class ([cell(1), true]), "cell")
%!assert (class ([cell(1), "a"]), "cell")

%!assert (class ([true, int64(1)]), "int64")
%!assert (class ([true, int32(1)]), "int32")
%!assert (class ([true, int16(1)]), "int16")
%!assert (class ([true, int8(1)]), "int8")
%!assert (class ([true, uint64(1)]), "uint64")
%!assert (class ([true, uint32(1)]), "uint32")
%!assert (class ([true, uint16(1)]), "uint16")
%!assert (class ([true, uint8(1)]), "uint8")
%!assert (class ([true, single(1)]), "single")
%!assert (class ([true, double(1)]), "double")
%!assert (class ([true, cell(1)]), "cell")
%!assert (class ([true, true]), "logical")
%!assert (class ([true, "a"]), "char")

%!assert (class (["a", int64(1)]), "char")
%!assert (class (["a", int32(1)]), "char")
%!assert (class (["a", int16(1)]), "char")
%!assert (class (["a", int8(1)]), "char")
%!assert (class (["a", int64(1)]), "char")
%!assert (class (["a", int32(1)]), "char")
%!assert (class (["a", int16(1)]), "char")
%!assert (class (["a", int8(1)]), "char")
%!assert (class (["a", single(1)]), "char")
%!assert (class (["a", double(1)]), "char")
%!assert (class (["a", cell(1)]), "cell")
%!assert (class (["a", true]), "char")
%!assert (class (["a", "a"]), "char")

%!assert (class ([cell(1), struct("foo", "bar")]), "cell")
%!error [struct("foo", "bar"), cell(1)]

%!test <*39041> assert (class ([cell(0), struct()]), "cell")
%!test <51086> assert (class ([struct(), cell(0)]), "struct")

%!assert ([,1], 1)
%!assert ([1,], 1)
%!assert ([,1,], 1)
%!assert ([,1,;;], 1)
%!assert ([,1,;,;], 1)

%!assert ([1,1], ones (1, 2))
%!assert ([,1,1], ones (1, 2))
%!assert ([1,1,], ones (1, 2))
%!assert ([,1,1,], ones (1, 2))
%!assert ([,1,1,;;], ones (1, 2))
%!assert ([,1,1,;,;], ones (1, 2))
%!assert ([,;,1,1], ones (1, 2))

%!assert ([1;1], ones (2, 1))
%!assert ([1,;1], ones (2, 1))
%!assert ([1,;,;1], ones (2, 1))

%!error eval ("[,,]")
%!error eval ("[,,;,]")
%!error eval ("[,;,,;,]")

%!assert (isnull ([,]))
%!assert (isnull ([;]))
%!assert (isnull ([;;]))
%!assert (isnull ([;,;]))
%!assert (isnull ([,;,;,]))

## Undefined elements.
%!function my_undef ()
%!endfunction
%!
%!shared es
%! es = struct ("a", {});
%!
%!assert <*58695> ([1; es.a; 3], [1; 3])
%!test <*58695>
%! fail ("undefined element in matrix list", "[1; my_undef(), 3]");
%!
%!assert <*58695> ([es.a; es.a; 3], 3)
%!test <*58695>
%! fail ("undefined element in matrix list", "[my_undef(); my_undef(); 3]")
*/
