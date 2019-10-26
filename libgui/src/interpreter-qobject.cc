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
#include "qt-application.h"
#include "qt-interpreter-events.h"

#include "input.h"
#include "interpreter.h"

namespace octave
{
  interpreter_qobject::interpreter_qobject (base_qobject& oct_qobj)
    : QObject (), m_octave_qobj (oct_qobj), m_interpreter (nullptr)
  { }

  void interpreter_qobject::execute (void)
  {
    // The Octave application context owns the interpreter.

    qt_application& app_context = m_octave_qobj.app_context ();

    interpreter& interp = app_context.create_interpreter ();

    event_manager& evmgr = interp.get_event_manager ();

    evmgr.connect_link (m_octave_qobj.get_qt_interpreter_events ());
    evmgr.enable ();

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

            m_interpreter = &interp;

            emit octave_ready_signal ();

            // Start executing commands in the command window.

            exit_status = interp.execute ();
          }
      }
    catch (const exit_exception& ex)
      {
        exit_status = ex.exit_status ();
      }

    // Disable events from being passed from the GUI to the interpreter.

    m_interpreter = nullptr;

    // Whether or not initialization succeeds we need to clean up the
    // interpreter once we are done with it.

    app_context.delete_interpreter ();

    emit octave_finished_signal (exit_status);
  }

  void interpreter_qobject::interpreter_event (const fcn_callback& fcn)
  {
    if (! m_interpreter)
      return;

    event_manager& evmgr = m_interpreter->get_event_manager ();

    evmgr.post_event (fcn);
  }

  void interpreter_qobject::interpreter_event (const meth_callback& meth)
  {
    if (! m_interpreter)
      return;

    event_manager& evmgr = m_interpreter->get_event_manager ();

    evmgr.post_event (meth);
  }

  qt_interpreter_events * interpreter_qobject::qt_link (void)
  {
    return m_octave_qobj.qt_link ();
  }
}
