////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2024 The Octave Project Developers
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
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_evaluator;

class tree_constant : public tree_expression
{
public:

  tree_constant (const octave_value& v, const token& tok)
    : m_value (v), m_token (tok)
  { }

  tree_constant (const octave_value& v, const std::string& ot, const token& tok)
    : m_value (v), m_orig_text (ot), m_token (tok)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_constant)

  ~tree_constant () = default;

  // Type.  It would be nice to eliminate the need for this.

  bool is_constant () const { return true; }

  filepos beg_pos () const { return m_token.beg_pos (); }
  filepos end_pos () const { return m_token.end_pos (); }

  void maybe_mutate () { m_value.maybe_mutate (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false,
              bool pr_orig_txt = true);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false,
                  bool pr_orig_txt = true);

  bool rvalue_ok () const { return true; }

  octave_value value () { return m_value; }

  tree_expression * dup (symbol_scope& scope) const;

  void accept (tree_walker& tw)
  {
    tw.visit_constant (*this);
  }

  // Store the original text corresponding to this constant for later
  // pretty printing.

  void stash_original_text (const std::string& s) { m_orig_text = s; }

  std::string original_text () const { return m_orig_text; }

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

  token m_token;
};

OCTAVE_END_NAMESPACE(octave)

#endif
