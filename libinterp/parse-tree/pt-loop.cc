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
#include "errwarn.h"
#include "oct-map.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-jit.h"
#include "pt-jump.h"
#include "pt-loop.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "unwind-prot.h"

namespace octave
{
  // While.

  tree_while_command::~tree_while_command (void)
  {
    delete expr;
    delete list;
    delete lead_comm;
    delete trail_comm;
#if defined (HAVE_LLVM)
    delete compiled;
#endif
  }

  // For.

  tree_simple_for_command::~tree_simple_for_command (void)
  {
    delete lhs;
    delete expr;
    delete maxproc;
    delete list;
    delete lead_comm;
    delete trail_comm;
#if defined (HAVE_LLVM)
    delete compiled;
#endif
  }

  tree_complex_for_command::~tree_complex_for_command (void)
  {
    delete lhs;
    delete expr;
    delete list;
    delete lead_comm;
    delete trail_comm;
  }
}
