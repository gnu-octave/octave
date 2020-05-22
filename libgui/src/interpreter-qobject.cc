////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2020 The Octave Project Developers
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

#include "interpreter-qobject.h"
#include "octave-qobject.h"
#include "qt-application.h"
#include "qt-interpreter-events.h"

#include "graphics-init.h"
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

            emit ready ();

            graphics_init (interp, m_octave_qobj);

            // Start executing commands in the command window.

            exit_status = interp.execute ();
          }
      }
    catch (const exit_exception& ex)
      {
        exit_status = ex.exit_status ();
      }

    // Signal that the interpreter is done executing code in the main
    // REPL, from script files, or command line eval arguments.  By
    // using a signal here, we give the GUI a chance to process any
    // pending events, then signal that it is safe to shutdown the
    // interpreter.  Our notification here allows the GUI to insert the
    // request to shutdown the interpreter in the event queue after any
    // other pending signals.  The application context owns the
    // interpreter and will be responsible for deleting it later, when
    // the application object destructor is executed.

    emit execution_finished (exit_status);
  }

  // This function is expected to be executed when the GUI signals that
  // it is finished processing events and ready for the interpreter to
  // perform shutdown actions.

  void interpreter_qobject::shutdown (int exit_status)
  {
    if (m_interpreter)
      m_interpreter->shutdown ();

    // Signal that the interpreter has executed shutdown actions.

    emit shutdown_finished (exit_status);
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
