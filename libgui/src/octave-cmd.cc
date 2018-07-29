/*

Copyright (C) 2014-2018 Torsten

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

// Author: Torsten <ttl@justmail.de>

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "octave-cmd.h"

#include "builtin-defun-decls.h"
#include "cmd-edit.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "octave-qt-link.h"
#include "utils.h"

namespace octave
{
  void octave_cmd_exec::execute (interpreter&)
  {
    std::string pending_input = command_editor::get_current_line ();

    command_editor::set_initial_input (pending_input);
    command_editor::replace_line (m_cmd.toStdString ());
    command_editor::redisplay ();
    command_editor::accept_line ();
  }

  void octave_cmd_eval::execute (interpreter&)
  {
    QString function_name = m_info.fileName ();
    function_name.chop (m_info.suffix ().length () + 1);
    std::string file_path = m_info.absoluteFilePath ().toStdString ();

    std::string pending_input = command_editor::get_current_line ();

    if (valid_identifier (function_name.toStdString ()))
      {
        // valid identifier: call as function with possibility to debug
        std::string path = m_info.absolutePath ().toStdString ();
        if (octave_qt_link::file_in_path (file_path, path))
          command_editor::replace_line (function_name.toStdString ());
      }
    else
      {
        // no valid identifier: use Fsource (), no debug possible
        Fsource (ovl (file_path));
        command_editor::replace_line ("");
      }

    command_editor::set_initial_input (pending_input);
    command_editor::redisplay ();

    command_editor::accept_line ();
  }

  void octave_cmd_builtin::execute (interpreter& interp)
  {
    m_callback_f (interp, m_argin, m_nargin);
  }

  void octave_cmd_debug::execute (interpreter& interp)
  {
    if (m_cmd == "step")
      {
        F__db_next_breakpoint_quiet__ (interp, ovl (m_suppress_dbg_location));
        Fdbstep (interp);
      }
    else if (m_cmd == "cont")
      {
        F__db_next_breakpoint_quiet__ (interp, ovl (m_suppress_dbg_location));
        Fdbcont (interp);
      }
    else if (m_cmd == "quit")
      Fdbquit (interp);
    else
      {
        F__db_next_breakpoint_quiet__ (interp, ovl (m_suppress_dbg_location));
        Fdbstep (interp, ovl (m_cmd.toStdString ()));
      }

    command_editor::interrupt (true);
  }

  // add a command to the queue

  void octave_command_queue::add_cmd (octave_cmd *cmd)
  {
    m_queue_mutex.lock ();
    m_queue.append (cmd);
    m_queue_mutex.unlock ();

    if (m_processing.tryAcquire ())  // if callback not processing, post event
      octave_link::post_event (this, &octave_command_queue::execute_command_callback);
  }

  // callback for executing the command by the worker thread

  void octave_command_queue::execute_command_callback (void)
  {
    bool repost = false;          // flag for reposting event for this callback

    if (! m_queue.isEmpty ())  // list can not be empty here, just to make sure
      {
        m_queue_mutex.lock ();     // critical path

        octave_cmd *cmd = m_queue.takeFirst ();

        if (m_queue.isEmpty ())
          m_processing.release (); // cmd queue empty, processing will stop
        else
          repost = true;          // not empty, repost at end
        m_queue_mutex.unlock ();

        // FIXME: Could we store a reference to the interpreter in the
        // octave_command_queue object?  If so, where is the proper
        // place to initialize that?

        interpreter& interp = __get_interpreter__ ("octave_command_queue::execute_command_callback");

        cmd->execute (interp);

        delete cmd;
      }

    if (repost)  // queue not empty, so repost event for further processing
      octave_link::post_event (this, &octave_command_queue::execute_command_callback);
  }
}
