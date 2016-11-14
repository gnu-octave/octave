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

#include "octave.h"

#include "octave-interpreter.h"

octave_interpreter::octave_interpreter (octave::application *app_context)
  : QObject (), thread_manager (), m_app_context (app_context),
    m_exit_status (0)
{ }

void
octave_interpreter::execute (void)
{
  thread_manager.register_current_thread ();

  octave_thread_manager::unblock_interrupt_signal ();

  // The application context owns the interpreter.

  m_app_context->create_interpreter ();

  emit octave_ready_signal ();

  m_exit_status = m_app_context->execute_interpreter ();
}

void
octave_interpreter::interrupt (void)
{
  thread_manager.interrupt ();
}

