/*

Copyright (C) 2014-2017 Torsten

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

// Author: Torsten <ttl@justmail.de>

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "octave-cmd.h"

#include "octave-qt-link.h"
#include "cmd-edit.h"
#include "builtin-defun-decls.h"
#include "utils.h"


// ---------------------------------------------------------------------
//  class octave_cmd_exec: executing a command

void
octave_cmd_exec::execute ()
{
  std::string pending_input = octave::command_editor::get_current_line ();

  octave::command_editor::set_initial_input (pending_input);
  octave::command_editor::replace_line (_cmd.toStdString ());
  octave::command_editor::redisplay ();
  octave::command_editor::accept_line ();
}

// ---------------------------------------------------------------------
//  class octave_cmd_eval: running a file

void
octave_cmd_eval::execute ()
{
  QString function_name = _info.fileName ();
  function_name.chop (_info.suffix ().length () + 1);
  std::string file_path = _info.absoluteFilePath ().toStdString ();

  std::string pending_input = octave::command_editor::get_current_line ();

  if (valid_identifier (function_name.toStdString ()))
    {
      // valid identifier: call as function with possibility to debug
      std::string path = _info.absolutePath ().toStdString ();
      if (octave_qt_link::file_in_path (file_path, path))
        octave::command_editor::replace_line (function_name.toStdString ());
    }
  else
    {
      // no valid identifier: use Fsource (), no debug possible
      Fsource (ovl (file_path));
      octave::command_editor::replace_line ("");
    }

  octave::command_editor::set_initial_input (pending_input);
  octave::command_editor::redisplay ();

  octave::command_editor::accept_line ();
}

// ---------------------------------------------------------------------
//  class octave_cmd_debug: executing a debugger command

void
octave_cmd_debug::execute ()
{
  if (_cmd == "step")
    {
      F__db_next_breakpoint_quiet__ (ovl (_suppress_dbg_location));
      Fdbstep ();
    }
  else if (_cmd == "cont")
    {
      F__db_next_breakpoint_quiet__ (ovl (_suppress_dbg_location));
      Fdbcont ();
    }
  else if (_cmd == "quit")
    Fdbquit ();
  else
    {
      F__db_next_breakpoint_quiet__ (ovl (_suppress_dbg_location));
      Fdbstep (ovl (_cmd.toStdString ()));
    }

  octave::command_editor::interrupt (true);
}

// ---------------------------------------------------------------------
//  class octave_command_queue: queue of octave commands

// add_cmd: add a command to the queue
void
octave_command_queue::add_cmd (octave_cmd *cmd)
{
  _queue_mutex.lock ();
  _queue.append (cmd);
  _queue_mutex.unlock ();

  if (_processing.tryAcquire ())  // if callback not processing, post event
    octave_link::post_event (this,
                             &octave_command_queue::execute_command_callback);
}

// callback for executing the command by the worker thread
void
octave_command_queue::execute_command_callback ()
{
  bool repost = false;          // flag for reposting event for this callback

  if (! _queue.isEmpty ())  // list can not be empty here, just to make sure
    {
      _queue_mutex.lock ();     // critical path

      octave_cmd *cmd = _queue.takeFirst ();

      if (_queue.isEmpty ())
        _processing.release (); // cmd queue empty, processing will stop
      else
        repost = true;          // not empty, repost at end
      _queue_mutex.unlock ();

      cmd->execute ();

      delete cmd;
    }

  if (repost)  // queue not empty, so repost event for further processing
    octave_link::post_event (this,
                             &octave_command_queue::execute_command_callback);

}
