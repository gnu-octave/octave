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

#if ! defined (octave_pt_tm_const_h)
#define octave_pt_tm_const_h 1

#include "octave-config.h"

#include <list>
#include <memory>
#include <string>

#include "Array.h"
#include "Sparse.h"

#include "data.h"
#include "dim-vector.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"
#include "pt-arg-list.h"
#include "pt-mat.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;

// Evaluate tree_matrix objects and convert them to octave_value
// arrays (full and sparse numeric, char, cell, struct, class and
// anything else that works like an array).  Use a separate class
// (tm_const) and pass the evaluator object to it instead of doing
// all this work in tree_evaluator::visit_matrix because the job is
// fairly large and requires extra data (stored in the tm_info
// class) for each row and for the overall array.

// Evaluate all elements of the array, recording info about each
// row, then create summary info for the full array.  Compute the
// result type and dimension first before copying values.

// FIXME: Handle overloading of horzcat and vertcat for for built-in
// types.

// Summary info about the current row or matrix.

class tm_info
{
public:

  tm_info (bool obj_is_empty)
    : m_dv (0, 0), m_all_strings (true), m_all_sq_strings (true),
      m_all_dq_strings (true), m_some_strings (false),
      m_all_real (true), m_all_complex (true), m_all_empty (true),
      m_any_cell (false), m_any_sparse (false),
      m_any_class (false), m_all_1x1 (! obj_is_empty),
      m_first_elem_is_struct (false), m_class_name ()
  { }

  dim_vector dims (void) const { return m_dv; }

  octave_idx_type rows (void) const { return m_dv(0); }
  octave_idx_type cols (void) const { return m_dv(1); }

  bool all_strings_p (void) const { return m_all_strings; }
  bool all_sq_strings_p (void) const { return m_all_sq_strings; }
  bool all_dq_strings_p (void) const { return m_all_dq_strings; }
  bool some_strings_p (void) const { return m_some_strings; }
  bool all_real_p (void) const { return m_all_real; }
  bool all_complex_p (void) const { return m_all_complex; }
  bool all_empty_p (void) const { return m_all_empty; }
  bool any_cell_p (void) const { return m_any_cell; }
  bool any_sparse_p (void) const { return m_any_sparse; }
  bool any_class_p (void) const { return m_any_class; }
  bool all_1x1_p (void) const { return m_all_1x1; }
  bool first_elem_struct_p (void) const { return m_first_elem_is_struct; }

  std::string class_name (void) const { return m_class_name; }

protected:

  // Size of this row or matrix after evaluation.
  dim_vector m_dv;

  // Are all elements character strings?
  bool m_all_strings;

  // Are all elements double-quoted character strings?
  bool m_all_sq_strings;

  // Are all elements single-quoted character strings?
  bool m_all_dq_strings;

  // Are any elements character strings?
  bool m_some_strings;

  // Are all elements real valued?
  bool m_all_real;

  // Are all elements complex valued?
  bool m_all_complex;

  // Are all elements empty?
  bool m_all_empty;

  // Are any elements cells?
  bool m_any_cell;

  // Are any elements sparse arrays?
  bool m_any_sparse;

  // Are any elements sparse class objects?
  bool m_any_class;

  // Do all elements have dimensions 1x1?
  bool m_all_1x1;

  // Is the first element a struct?
  bool m_first_elem_is_struct;

  // Class name of result.
  std::string m_class_name;
};

class tm_row_const : public tm_info
{
public:

  typedef std::list<octave_value>::iterator iterator;
  typedef std::list<octave_value>::const_iterator const_iterator;

  tm_row_const (void) = delete;

  tm_row_const (const tree_argument_list& row, tree_evaluator& tw)
    : tm_info (row.empty ()), m_values ()
  {
    init (row, tw);
  }

  tm_row_const (const tm_row_const&) = default;

  tm_row_const& operator = (const tm_row_const&) = delete;

  ~tm_row_const (void) = default;

  iterator begin (void) { return m_values.begin (); }
  const_iterator begin (void) const { return m_values.begin (); }

  iterator end (void) { return m_values.end (); }
  const_iterator end (void) const { return m_values.end (); }

  bool empty (void) const { return m_values.empty (); }

  std::size_t length (void) const { return m_values.size (); }

  void cellify (void);

private:

  std::list<octave_value> m_values;

  void init_element (const octave_value&, bool&);

  void init (const tree_argument_list&, tree_evaluator& tw);
};

class tm_const : public tm_info
{
public:

  typedef std::list<tm_row_const>::iterator iterator;
  typedef std::list<tm_row_const>::const_iterator const_iterator;

  tm_const (void) = delete;

  tm_const (const tree_matrix& tm, tree_evaluator& tw)
    : tm_info (tm.empty ()), m_evaluator (tw), m_tm_rows ()
  {
    init (tm);
  }

  // No copying!

  tm_const (const tm_const&) = delete;

  tm_const& operator = (const tm_const&) = delete;

  ~tm_const (void) = default;

  octave_value concat (char string_fill_char) const;

private:

  tree_evaluator& m_evaluator;

  // The list of lists of octave_value objects that contain the
  // values of elements in each row of the tree_matrix object we are
  // evaluating.

  std::list<tm_row_const> m_tm_rows;

  void init (const tree_matrix& tm);

  octave_value char_array_concat (char string_fill_char) const;

  octave_value class_concat (void) const;

  octave_value generic_concat (void) const;

  template <typename TYPE>
  void array_concat_internal (TYPE& result) const;

  template <typename TYPE>
  TYPE array_concat (void) const;

  template <typename TYPE>
  TYPE sparse_array_concat (void) const;

  template <typename MAP>
  octave_map map_concat (void) const;
};

OCTAVE_END_NAMESPACE(octave)

#endif
