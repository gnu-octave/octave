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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <ostream>

#include "error.h"
#include "ovl.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// We are likely to have a lot of tree_constant objects to allocate,
// so make the grow_size large.

void
tree_constant::print (std::ostream& os, bool pr_as_read_syntax,
                      bool pr_orig_text)
{
  if (pr_orig_text && ! m_orig_text.empty ())
    os << m_orig_text;
  else
    m_value.print (os, pr_as_read_syntax);
}

void
tree_constant::print_raw (std::ostream& os, bool pr_as_read_syntax,
                          bool pr_orig_text)
{
  if (pr_orig_text && ! m_orig_text.empty ())
    os << m_orig_text;
  else
    m_value.print_raw (os, pr_as_read_syntax);
}

tree_expression *
tree_constant::dup (symbol_scope&) const
{
  tree_constant *new_tc
    = new tree_constant (m_value, m_orig_text, line (), column ());

  new_tc->copy_base (*this);

  return new_tc;
}

OCTAVE_END_NAMESPACE(octave)
