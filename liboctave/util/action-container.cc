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

#include "action-container.h"
#include "cmd-edit.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void action_container::run (std::size_t num)
{
  if (num > size ())
    num = size ();

  for (std::size_t i = 0; i < num; i++)
    {
      run_first ();

      // If event_loop_interrupted is TRUE, a user callback event has
      // requested that we break out of the readline event handler to
      // process a command or other action.

      if (command_editor::event_loop_interrupted ())
        {
          command_editor::interrupt_event_loop (false);
          break;
        }
    }
}

OCTAVE_END_NAMESPACE(octave)
