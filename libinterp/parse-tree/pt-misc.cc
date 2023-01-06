////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#include "pt-idx.h"
#include "pt-misc.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Parameter lists.

tree_parameter_list::~tree_parameter_list (void)
{
  while (! empty ())
    {
      auto p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_parameter_list::mark_as_formal_parameters (void)
{
  for (tree_decl_elt *elt : *this)
    elt->mark_as_formal_parameter ();
}

std::list<std::string>
tree_parameter_list::variable_names (void) const
{
  std::list<std::string> retval;

  for (tree_decl_elt *elt : *this)
    retval.push_back (elt->name ());

  if (m_marked_for_varargs)
    retval.push_back (varargs_symbol_name ());

  return retval;
}

tree_parameter_list *
tree_parameter_list::dup (symbol_scope& scope) const
{
  tree_parameter_list *new_list = new tree_parameter_list (m_in_or_out);

  new_list->m_marked_for_varargs = m_marked_for_varargs;

  for (const tree_decl_elt *elt : *this)
    new_list->append (elt->dup (scope));

  return new_list;
}

OCTAVE_END_NAMESPACE(octave)
