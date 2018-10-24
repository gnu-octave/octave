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
    octave_value_list argout;
    if (m_callback_fi)
      argout = m_callback_fi (interp, m_argin, m_nargout);
    else if (m_callback_f)
      argout = m_callback_f (m_argin, m_nargout);

    switch (m_update)
      {
        case CMD_UPD_WORKSPACE:
          {
            symbol_scope scope
                = __get_current_scope__ ("octave_cmd_builtin::execute");
            if (scope)
              octave_link::set_workspace (true, scope);
            break;
          }

        default:
          break;
      }

    if (m_nargout)    // Return value expected: connect the related value
      emit argout_signal (argout);
  }

  void octave_cmd_builtin::init_cmd_retval ()
  {
    if (m_nargout)
      connect (this, SIGNAL (argout_signal (const octave_value_list&)),
               m_argout_receiver, m_argout_handler, Qt::QueuedConnection);
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
    // Get a guarded pointer from the pointer to the command object
    QPointer<octave_cmd> cmd_gp (cmd);

    // And add it to the command queue
    m_queue_mutex.lock ();
    m_queue.append (cmd_gp);
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

        QPointer<octave_cmd> cmd_gp = m_queue.takeFirst ();

        if (m_queue.isEmpty ())
          m_processing.release (); // cmd queue empty, processing will stop
        else
          repost = true;          // not empty, repost at end
        m_queue_mutex.unlock ();

        if (! cmd_gp.isNull ())
          {
            // The pointer to the command object is still valid

            // FIXME: Could we store a reference to the interpreter in the
            // octave_command_queue object?  If so, where is the proper
            // place to initialize that?
            interpreter& interp = __get_interpreter__ ("octave_command_queue::execute_command_callback");

            cmd_gp->execute (interp);
          }

        delete cmd_gp;    // destroy the referred octave_cmd object
      }

    if (repost)  // queue not empty, so repost event for further processing
      octave_link::post_event (this, &octave_command_queue::execute_command_callback);
  }
}
