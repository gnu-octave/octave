/*

Copyright (C) 2013-2016 John W. Eaton
Copyright (C) 2011-2016 Jacob Dawid

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

#include <QApplication>

#include "octave.h"

#include "octave-interpreter.h"

octave_interpreter::octave_interpreter (octave::application *app_context)
  : QObject (), thread_manager (), m_app_context (app_context)
{ }

void
octave_interpreter::execute (void)
{
  thread_manager.register_current_thread ();

  octave_thread_manager::unblock_interrupt_signal ();

  // The application context owns the interpreter.

  m_app_context->create_interpreter ();

  int exit_status = 0;

  try
    {
      // Final initialization including executing startup files.  If
      // initialization fails, return the last available status from
      // that process.

      exit_status = m_app_context->initialize_interpreter ();

      if (m_app_context->interpreter_initialized ())
        {
          // The interpreter should be completely ready at this point so let
          // the GUI know.

          emit octave_ready_signal ();

          // Start executing commands in the command window.

          exit_status = m_app_context->execute_interpreter ();
        }
    }
  catch (const octave::exit_exception& ex)
    {
      exit_status = ex.exit_status ();
    }

  // Whether or not initialization succeeds we need to clean up the
  // interpreter once we are done with it.

  m_app_context->delete_interpreter ();

  qApp->exit (exit_status);
}

void
octave_interpreter::interrupt (void)
{
  thread_manager.interrupt ();
}
