/*

Copyright (C) 2011-2018 Jacob Dawid

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

#include "lo-utils.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "signal-wrappers.h"

#include "builtin-defun-decls.h"
#include "display.h"
#include "octave.h"
#include "sysdep.h"

#include "main-window.h"
#include "octave-gui.h"

namespace octave
{
  gui_application::gui_application (int argc, char **argv)
    : application (argc, argv)
  {
    // This should probably happen early.
    sysdep_init ();
  }

  bool gui_application::start_gui_p (void) const
  {
    return m_options.gui ();
  }

  int gui_application::execute (void)
  {
    octave_block_interrupt_signal ();

    set_application_id ();

    // Create and show main window.

    octave_qt_app oct_qt_app (*this);

    return oct_qt_app.exec ();
  }
}
