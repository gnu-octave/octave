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
#include "octave-qobject.h"
#include "qt-interpreter-events.h"
#include "qt-application.h"

#include "input.h"
#include "interpreter.h"

namespace octave
{
  interpreter_qobject::interpreter_qobject (base_qobject *oct_qobj)
    : QObject (), m_octave_qobject (oct_qobj),
      m_qt_link (new qt_interpreter_events ())
  { }

  void interpreter_qobject::execute (void)
  {
    // The Octave application context owns the interpreter.

    qt_application& app_context = m_octave_qobject->app_context ();

    interpreter& interp = app_context.create_interpreter ();

    event_manager& evmgr = interp.get_event_manager ();

    evmgr.connect_link (m_qt_link);
    evmgr.enable ();

    connect (qt_link (), SIGNAL (confirm_shutdown_signal (void)),
             m_octave_qobject, SLOT (confirm_shutdown_octave (void)));

    connect (qt_link (),
             SIGNAL (copy_image_to_clipboard_signal (const QString&, bool)),
             m_octave_qobject,
             SLOT (copy_image_to_clipboard (const QString&, bool)));

    int exit_status = 0;

    try
      {
        // Final initialization.

        interp.initialize ();

        if (app_context.start_gui_p ())
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

    app_context.delete_interpreter ();

    emit octave_finished_signal (exit_status);
  }

  void interpreter_qobject::confirm_shutdown (bool closenow)
  {
    // Wait for link thread to go to sleep state.
    m_qt_link->lock ();

    m_qt_link->shutdown_confirmation (closenow);

    m_qt_link->unlock ();

    // Awake the worker thread so that it continues shutting down (or not).
    m_qt_link->wake_all ();
  }
}
