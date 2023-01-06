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

#if ! defined (octave_pt_id_h)
#define octave_pt_id_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;

#include "oct-lvalue.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-walk.h"
#include "symscope.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;

// Symbols from the symbol table.

class tree_identifier : public tree_expression
{
  friend class tree_index_expression;

public:

  tree_identifier (int l = -1, int c = -1)
    : tree_expression (l, c), m_sym () { }

  tree_identifier (const symbol_record& s,
                   int l = -1, int c = -1)
    : tree_expression (l, c), m_sym (s) { }

  // No copying!

  tree_identifier (const tree_identifier&) = delete;

  tree_identifier& operator = (const tree_identifier&) = delete;

  ~tree_identifier (void) = default;

  bool is_identifier (void) const { return true; }

  std::string name (void) const { return m_sym.name (); }

  virtual bool is_black_hole (void) const { return false; }

  void mark_as_formal_parameter (void) { m_sym.mark_formal (); }

  // We really need to know whether this symbol refers to a variable
  // or a function, but we may not know that yet.

  bool lvalue_ok (void) const { return true; }

  octave_lvalue lvalue (tree_evaluator& tw);

  void eval_undefined_error (void);

  void static_workspace_error (void)
  {
    error (R"(can not add variable "%s" to a static workspace)",
           name ().c_str ());
  }

  tree_identifier * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator& tw, int nargout = 1)
  {
    octave_value_list retval = evaluate_n (tw, nargout);

    return retval.length () > 0 ? retval(0) : octave_value ();
  }

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1);

  void accept (tree_walker& tw)
  {
    tw.visit_identifier (*this);
  }

  symbol_record symbol (void) const { return m_sym; }

protected:

  // The symbol record that this identifier references.
  symbol_record m_sym;
};

class tree_black_hole : public tree_identifier
{
public:

  tree_black_hole (int l = -1, int c = -1)
    : tree_identifier (l, c) { }

  std::string name (void) const { return "~"; }

  bool is_black_hole (void) const { return true; }

  tree_black_hole * dup (symbol_scope&) const
  {
    return new tree_black_hole;
  }

  octave_lvalue lvalue (tree_evaluator& tw);
};

OCTAVE_END_NAMESPACE(octave)

#endif
