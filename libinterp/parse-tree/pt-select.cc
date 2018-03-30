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

#include "pt-select.h"
#include "pt-stmt.h"

namespace octave
{
  // If clauses.

  tree_if_clause::~tree_if_clause (void)
  {
    delete m_expr;
    delete m_list;
    delete m_lead_comm;
  }

  // If.

  tree_if_command::~tree_if_command (void)
  {
    delete m_list;
    delete m_lead_comm;
    delete m_trail_comm;
  }

  // Switch cases.

  tree_switch_case::~tree_switch_case (void)
  {
    delete m_label;
    delete m_list;
    delete m_lead_comm;
  }

  // Switch.

  tree_switch_command::~tree_switch_command (void)
  {
    delete m_expr;
    delete m_list;
    delete m_lead_comm;
    delete m_trail_comm;
  }
}
