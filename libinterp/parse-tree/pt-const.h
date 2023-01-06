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

#if ! defined (octave_pt_const_h)
#define octave_pt_const_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class octave_value_list;

#include "error.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_evaluator;

class tree_constant : public tree_expression
{
public:

  tree_constant (int l = -1, int c = -1)
    : tree_expression (l, c), m_value (), m_orig_text ()
  { }

  tree_constant (const octave_value& v, int l = -1, int c = -1)
    : tree_expression (l, c), m_value (v), m_orig_text ()
  { }

  tree_constant (const octave_value& v, const std::string& ot,
                 int l = -1, int c = -1)
    : tree_expression (l, c), m_value (v), m_orig_text (ot)
  { }

  // No copying!

  tree_constant (const tree_constant&) = delete;

  tree_constant& operator = (const tree_constant&) = delete;

  ~tree_constant (void) = default;

  // Type.  It would be nice to eliminate the need for this.

  bool is_constant (void) const { return true; }

  void maybe_mutate (void) { m_value.maybe_mutate (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false,
              bool pr_orig_txt = true);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false,
                  bool pr_orig_txt = true);

  bool rvalue_ok (void) const { return true; }

  octave_value value (void) { return m_value; }

  tree_expression * dup (symbol_scope& scope) const;

  void accept (tree_walker& tw)
  {
    tw.visit_constant (*this);
  }

  // Store the original text corresponding to this constant for later
  // pretty printing.

  void stash_original_text (const std::string& s) { m_orig_text = s; }

  std::string original_text (void) const { return m_orig_text; }

  octave_value evaluate (tree_evaluator&, int nargout = 1)
  {
    if (nargout > 1)
      error ("invalid number of output arguments for constant expression");

    return value ();
  }

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

private:

  // The actual value that this constant refers to.
  octave_value m_value;

  // The original text form of this constant.
  std::string m_orig_text;
};

OCTAVE_END_NAMESPACE(octave)

#endif
