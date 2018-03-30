/*

Copyright (C) 2012-2018 John W. Eaton

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

#if ! defined (octave_octave_gui_h)
#define octave_octave_gui_h 1

#include "octave.h"

namespace octave
{
  class OCTGUI_API gui_application : public application
  {
  public:

    gui_application (int argc, char **argv);

    // No copying, at least not yet.

    gui_application (const gui_application&) = delete;

    gui_application& operator = (const gui_application&) = delete;

    // Should we start the GUI or fall back to the CLI?
    bool start_gui_p (void) const;

    int execute (void);

    bool gui_running (void) const { return m_gui_running; }
    void gui_running (bool arg) { m_gui_running = arg; }

  private:

    int m_argc;
    char **m_argv;
    bool m_gui_running;
  };
}

#endif
