/*

Copyright (C) 2013-2019 John W. Eaton
Copyright (C) 2011-2019 Jacob Dawid

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

#include "interpreter-qobject.h"
#include "qt-application.h"

#include "input.h"
#include "interpreter.h"

namespace octave
{
  interpreter_qobject::interpreter_qobject (qt_application& app_context)
    : QObject (), m_app_context (app_context)
  { }

  void interpreter_qobject::execute (void)
  {
    // The application context owns the interpreter.

    interpreter& interp = m_app_context.create_interpreter ();

    int exit_status = 0;

    try
      {
        // Final initialization.

        interp.initialize ();

        if (m_app_context.start_gui_p ())
          {
            input_system& input_sys = interp.get_input_system ();

            input_sys.PS1 (">> ");
            input_sys.PS2 ("");
          }

        if (interp.initialized ())
          {
            // The interpreter should be completely ready at this point so let
            // the GUI know.

            emit octave_ready_signal ();

            // Start executing commands in the command window.

            exit_status = interp.execute ();
          }
      }
    catch (const exit_exception& ex)
      {
        exit_status = ex.exit_status ();
      }

    // Whether or not initialization succeeds we need to clean up the
    // interpreter once we are done with it.

    m_app_context.delete_interpreter ();

    emit octave_finished_signal (exit_status);
  }
}
