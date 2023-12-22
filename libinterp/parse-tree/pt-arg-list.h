////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

#if ! defined (octave_pt_arg_list_h)
#define octave_pt_arg_list_h 1

#include "octave-config.h"

#include <list>

class octave_value;
class octave_value_list;

#include "str-vec.h"

#include "base-list.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_evaluator;
class tree_expression;

// Argument lists.  Used to hold the list of expressions that are the
// arguments in a function call or index expression.

class tree_argument_list : public base_list<tree_expression *>
{
public:

  typedef tree_expression *element_type;

  tree_argument_list ()
    : m_list_includes_magic_tilde (false), m_simple_assign_lhs (false)
  { }

  tree_argument_list (tree_expression *t)
    : m_list_includes_magic_tilde (false), m_simple_assign_lhs (false)
  { append (t); }

  OCTAVE_DISABLE_COPY_MOVE (tree_argument_list)

  ~tree_argument_list ();

  bool has_magic_tilde () const
  {
    return m_list_includes_magic_tilde;
  }

  bool includes_magic_tilde () const
  {
    return m_list_includes_magic_tilde;
  }

  tree_expression * remove_front ()
  {
    auto p = begin ();
    tree_expression *retval = *p;
    erase (p);
    return retval;
  }

  void append (const element_type& s);

  void mark_as_simple_assign_lhs () { m_simple_assign_lhs = true; }

  bool is_simple_assign_lhs () { return m_simple_assign_lhs; }

  bool all_elements_are_constant () const;

  bool is_valid_lvalue_list () const;

  string_vector get_arg_names () const;

  std::list<std::string> variable_names () const;

  tree_argument_list * dup (symbol_scope& scope) const;

  void accept (tree_walker& tw)
  {
    tw.visit_argument_list (*this);
  }

private:

  bool m_list_includes_magic_tilde;

  bool m_simple_assign_lhs;
};

OCTAVE_END_NAMESPACE(octave)

#endif
