////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#include "main-window.h"
#include "octave-qobject.h"
#include "qt-application.h"

#include "lo-utils.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "signal-wrappers.h"

#include "display.h"
#include "octave.h"
#include "sysdep.h"

OCTAVE_BEGIN_NAMESPACE(octave)

qt_application::qt_application (int argc, char **argv)
: application (argc, argv)
{
  // This should probably happen early.
  sysdep_init ();
}

bool qt_application::start_gui_p (void) const
{
  // Note: this function is not needed if using the experimental
  // terminal widget, so return a dummy value of false in that case.

  return experimental_terminal_widget () ? false : m_options.gui ();
}

int qt_application::execute (void)
{
  octave_block_interrupt_signal ();

  set_application_id ();

  // Create and show main window.

  // Note: the second argument is ignored if using the new terminal
  // widget.

  base_qobject qt_interface (*this, start_gui_p ());

  return qt_interface.exec ();
}

OCTAVE_END_NAMESPACE(octave)
