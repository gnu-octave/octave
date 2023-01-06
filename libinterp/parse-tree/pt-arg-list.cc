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

#include <string>

#include "str-vec.h"

#include "ovl.h"
#include "ov.h"
#include "pt-arg-list.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-pr-code.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Argument lists.

tree_argument_list::~tree_argument_list (void)
{
  while (! empty ())
    {
      auto p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_argument_list::append (const element_type& s)
{
  base_list<tree_expression *>::append (s);

  if (! m_list_includes_magic_tilde && s && s->is_identifier ())
    {
      tree_identifier *id = dynamic_cast<tree_identifier *> (s);
      m_list_includes_magic_tilde = id && id->is_black_hole ();
    }
}

bool
tree_argument_list::all_elements_are_constant (void) const
{
  for (const tree_expression *elt : *this)
    {
      if (! elt->is_constant ())
        return false;
    }

  return true;
}

bool
tree_argument_list::is_valid_lvalue_list (void) const
{
  bool retval = true;

  for (const tree_expression *elt : *this)
    {
      // There is no need for a separate check for the magic "~" because it
      // is represented by tree_black_hole, and that is derived from
      // tree_identifier.
      if (! (elt->is_identifier () || elt->is_index_expression ()))
        {
          retval = false;
          break;
        }
    }

  return retval;
}

string_vector
tree_argument_list::get_arg_names (void) const
{
  int len = length ();

  string_vector retval (len);

  int k = 0;

  for (tree_expression *elt : *this)
    retval(k++) = elt->str_print_code ();

  return retval;
}

std::list<std::string>
tree_argument_list::variable_names (void) const
{
  std::list<std::string> retval;

  for (tree_expression *elt : *this)
    {
      if (elt->is_identifier ())
        {
          tree_identifier *id = dynamic_cast<tree_identifier *> (elt);

          retval.push_back (id->name ());
        }
      else if (elt->is_index_expression ())
        {
          tree_index_expression *idx_expr
            = dynamic_cast<tree_index_expression *> (elt);

          retval.push_back (idx_expr->name ());
        }
    }

  return retval;
}

tree_argument_list *
tree_argument_list::dup (symbol_scope& scope) const
{
  tree_argument_list *new_list = new tree_argument_list ();

  new_list->m_simple_assign_lhs = m_simple_assign_lhs;

  for (const tree_expression *elt : *this)
    new_list->append (elt ? elt->dup (scope) : nullptr);

  return new_list;
}

OCTAVE_END_NAMESPACE(octave)
