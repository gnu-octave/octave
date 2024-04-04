////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2024 The Octave Project Developers
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

#if ! defined (octave_pt_array_list_h)
#define octave_pt_array_list_h 1

#include "octave-config.h"

#include <list>

#include "pt-arg-list.h"
#include "pt-exp.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_walker;

// Base class for cell arrays and matrices.

class tree_array_list : public tree_expression,
  public std::list<tree_argument_list *>
{
public:

  typedef std::list<tree_argument_list *>::iterator iterator;
  typedef std::list<tree_argument_list *>::const_iterator const_iterator;

protected:

  tree_array_list (tree_argument_list *row = nullptr)
  {
    if (row)
      push_back (row);
  }

public:

  OCTAVE_DISABLE_COPY_MOVE (tree_array_list)

  ~tree_array_list ();

  // The delimiter list for a cell array should never be empty.  But
  // better safe than sorry, I guess.

  filepos beg_pos () const { return m_delims.empty () ? filepos () : m_delims.beg_pos (); }
  filepos end_pos () const { return m_delims.empty () ? filepos () : m_delims.end_pos (); }

  bool all_elements_are_constant () const;

  // FIXME: should we import the functions from the base class and
  // overload them here, or should we use a different name so we don't
  // have to do this?  Without the using declaration or a name change,
  // the base class functions will be hidden.  That may be OK, but it
  // can also cause some confusion.
  using tree_expression::copy_base;

  void copy_base (const tree_array_list& array_list);

  void copy_base (const tree_array_list& array_list,
                  symbol_scope& scope);

  tree_expression * dup (symbol_scope& scope) const;

  void accept (tree_walker& tw);
};

OCTAVE_END_NAMESPACE(octave)

#endif
