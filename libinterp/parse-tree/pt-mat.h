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

#if ! defined (octave_pt_mat_h)
#define octave_pt_mat_h 1

#include "octave-config.h"

#include <iosfwd>

class octave_value;
class octave_value_list;

#include "base-list.h"
#include "pt-array-list.h"
#include "pt-exp.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_argument_list;

// General matrices.  This allows us to construct matrices from
// other matrices, variables, and functions.

class tree_matrix : public tree_array_list
{
public:

  tree_matrix (tree_argument_list *row = nullptr, int l = -1, int c = -1)
    : tree_array_list (row, l, c)
  { }

  // No copying!

  tree_matrix (const tree_matrix&) = delete;

  tree_matrix& operator = (const tree_matrix&) = delete;

  ~tree_matrix (void) = default;

  bool is_matrix (void) const { return true; }

  bool rvalue_ok (void) const { return true; }

  tree_expression * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_matrix (*this);
  }
};

extern std::string
get_concat_class (const std::string& c1, const std::string& c2);

extern void
maybe_warn_string_concat (bool all_dq_strings_p, bool all_sq_strings_p);

OCTAVE_END_NAMESPACE(octave)

#endif
