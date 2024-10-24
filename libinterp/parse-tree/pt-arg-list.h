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

#include "pt-delimiter-list.h"
#include "pt-exp.h"
#include "pt-walk.h"
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_evaluator;
class tree_expression;

// Argument lists.  Used to hold the list of expressions that are the
// arguments in a function call or index expression.

class tree_argument_list : public std::list<tree_expression *>
{
public:

  typedef tree_expression *element_type;

  tree_argument_list () { }

  tree_argument_list (tree_expression *t) { push_back (t); }

  OCTAVE_DISABLE_COPY_MOVE (tree_argument_list)

  ~tree_argument_list ();

  tree_argument_list * mark_in_delims (const token& open_delim, const token& close_delim)
  {
    m_delims.push (open_delim, close_delim);
    return this;
  }

  filepos beg_pos () const
  {
    if (m_delims.empty ())
      {
        if (empty ())
          return filepos ();

        tree_expression *elt = front ();
        return elt->beg_pos ();
      }

    return m_delims.beg_pos ();
  }

  filepos end_pos () const
  {
    if (m_delims.empty ())
      {
        if (empty ())
          return filepos ();

        tree_expression *elt = back ();
        return elt->end_pos ();
      }

    return m_delims.end_pos ();
  }

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

  void push_back (const element_type& s);

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

  bool m_list_includes_magic_tilde {false};

  bool m_simple_assign_lhs {false};

  tree_delimiter_list m_delims;
};

OCTAVE_END_NAMESPACE(octave)

#endif
