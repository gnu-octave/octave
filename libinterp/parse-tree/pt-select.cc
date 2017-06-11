/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "error.h"
#include "ovl.h"
#include "ov.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-select.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "Cell.h"
#include "ov-typeinfo.h"

namespace octave
{
  // If clauses.

  tree_if_clause::~tree_if_clause (void)
  {
    delete expr;
    delete list;
    delete lead_comm;
  }

  // If.

  tree_if_command::~tree_if_command (void)
  {
    delete list;
    delete lead_comm;
    delete trail_comm;
  }

  // Switch cases.

  tree_switch_case::~tree_switch_case (void)
  {
    delete label;
    delete list;
    delete lead_comm;
  }

  // Switch.

  tree_switch_command::~tree_switch_command (void)
  {
    delete expr;
    delete list;
    delete lead_comm;
    delete trail_comm;
  }
}
