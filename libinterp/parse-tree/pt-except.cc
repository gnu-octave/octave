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

#include "quit.h"

#include "error.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-cmd.h"
#include "pt-except.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-jump.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "unwind-prot.h"
#include "variables.h"

namespace octave
{
  // Simple exception handling.

  tree_try_catch_command::~tree_try_catch_command (void)
  {
    delete expr_id;
    delete try_code;
    delete catch_code;
    delete lead_comm;
    delete mid_comm;
    delete trail_comm;
  }

  // Simple exception handling.

  tree_unwind_protect_command::~tree_unwind_protect_command (void)
  {
    delete unwind_protect_code;
    delete cleanup_code;
    delete lead_comm;
    delete mid_comm;
    delete trail_comm;
  }
}
