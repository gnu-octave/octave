////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024 The Octave Project Developers
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

#if ! defined (octave_tree_delimiter_list_h)
#define octave_tree_delimiter_list_h 1

#include "octave-config.h"

#include <stack>

#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_delimiter_list
{
public:

  typedef std::pair<token, token> element_type;

  OCTAVE_DEFAULT_CONSTRUCT_COPY_MOVE_DELETE (tree_delimiter_list)

    size_t count () const { return m_delimiters.size (); }

  void push (const token& open_delim, const token& close_delim)
  {
    m_delimiters.push (element_type (open_delim, close_delim));
  }

private:

  std::stack<element_type> m_delimiters;
};

OCTAVE_END_NAMESPACE(octave)

#endif
