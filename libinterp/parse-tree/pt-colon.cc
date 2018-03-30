/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "pt-colon.h"

namespace octave
{
  // Colon expressions.

  tree_expression *
  tree_colon_expression::dup (symbol_scope& scope) const
  {
    tree_colon_expression *new_ce
      = new tree_colon_expression (m_base ? m_base->dup (scope) : nullptr,
                                   m_limit ? m_limit->dup (scope) : nullptr,
                                   m_increment ? m_increment->dup (scope) : nullptr,
                                   line (), column ());

    new_ce->copy_base (*new_ce);

    return new_ce;
  }
}
