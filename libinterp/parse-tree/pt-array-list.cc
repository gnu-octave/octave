////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
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

#include "quit.h"

#include "error.h"
#include "pt-array-list.h"

OCTAVE_BEGIN_NAMESPACE(octave)

tree_array_list::~tree_array_list (void)
{
  while (! empty ())
    {
      auto p = begin ();
      delete *p;
      erase (p);
    }
}

bool
tree_array_list::all_elements_are_constant (void) const
{
  for (const tree_argument_list *elt : *this)
    {
      octave_quit ();

      if (! elt->all_elements_are_constant ())
        return false;
    }

  return true;
}

void
tree_array_list::copy_base (const tree_array_list& array_list)
{
  tree_expression::copy_base (array_list);
}

void
tree_array_list::copy_base (const tree_array_list& array_list,
                            symbol_scope& scope)
{
  for (const tree_argument_list *elt : array_list)
    append (elt ? elt->dup (scope) : nullptr);

  copy_base (*this);
}

tree_expression *
tree_array_list::dup (symbol_scope&) const
{
  panic_impossible ();
  return nullptr;
}

void
tree_array_list::accept (tree_walker&)
{
  panic_impossible ();
}

OCTAVE_END_NAMESPACE(octave)
