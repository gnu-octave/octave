/*

Copyright (C) 2011-2012 Jacob Dawid
Copyright (C) 2011-2012 John P. Swensen

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "cmd-edit.h"
#include "oct-env.h"
#include "oct-mutex.h"
#include "singleton-cleanup.h"
#include "symtab.h"
#include "toplev.h"

#include "octave-link.h"

static int
octave_readline_hook (void)
{
  octave_link::entered_readline_hook ();
  octave_link::generate_events ();
  octave_link::process_events ();
  octave_link::finished_readline_hook ();

  return 0;
}

static void
octave_exit_hook (int)
{
  octave_link::about_to_exit ();
}

octave_link *octave_link::instance = 0;

octave_link::octave_link (void)
  : event_listener (0), event_queue_mutex (new octave_mutex ()),
    gui_event_queue (), last_cwd (), debugging (false)
{ }

void
octave_link::do_launch_octave (void)
{
  // Create both threads.
  main_thread = new octave_main_thread ();

  command_editor::add_event_hook (octave_readline_hook);

  octave_exit = octave_exit_hook;

  // Start the first one.
  main_thread->start ();
}

void
octave_link::do_register_event_listener (octave_event_listener *el)
{
  event_listener = el;
}

void
octave_link::do_generate_events (void)
{
  std::string current_working_directory = octave_env::get_current_directory ();

  if (current_working_directory != last_cwd)
    {
      last_cwd = current_working_directory;

      if (event_listener)
        event_listener->current_directory_has_changed (last_cwd);
    }

  if (debugging != Vdebugging)
    {
      debugging = Vdebugging;

      if (event_listener)
        {
          if (debugging)
            event_listener->entered_debug_mode ();
          else
            event_listener->quit_debug_mode ();
        }
    }
}

void
octave_link::do_process_events (void)
{
  event_queue_mutex->lock ();

  gui_event_queue.run ();

  event_queue_mutex->unlock ();
}

void
octave_link::do_about_to_exit (void)
{
  event_queue_mutex->lock ();

  gui_event_queue.discard ();

  event_queue_mutex->unlock ();

  if (event_listener)
    event_listener->about_to_exit ();
}

std::string
octave_link::do_last_working_directory (void)
{
  return last_cwd;
}

void
octave_link::do_update_workspace (void)
{
  if (event_listener)
    {
      event_listener->update_workspace ();

      do_process_events ();
    }
}

void
octave_link::do_update_history (void)
{
  if (event_listener)
    {
      event_listener->update_history ();

      do_process_events ();
    }
}

void
octave_link::do_insert_debugger_pointer (const octave_value_list& args)
{
  if (event_listener)
    {
      if (args.length () == 1)
        {
          octave_scalar_map m = args(0).scalar_map_value ();

          if (! error_state)
            {
              octave_value ov_file = m.getfield ("file");
              octave_value ov_line = m.getfield ("line");

              std::string file = ov_file.string_value ();
              int line = ov_line.int_value ();

              if (! error_state)
                {
                  event_listener->insert_debugger_pointer (file, line);

                  do_process_events ();
                }
              else
                ::error ("invalid struct in debug pointer callback");
            }
          else
            ::error ("expecting struct in debug pointer callback");
        }
      else
        ::error ("invalid call to debug pointer callback");
    }
}

void
octave_link::do_delete_debugger_pointer (const octave_value_list& args)
{
  if (event_listener)
    {
      if (args.length () == 1)
        {
          octave_scalar_map m = args(0).scalar_map_value ();

          if (! error_state)
            {
              octave_value ov_file = m.getfield ("file");
              octave_value ov_line = m.getfield ("line");

              std::string file = ov_file.string_value ();
              int line = ov_line.int_value ();

              if (! error_state)
                {
                  event_listener->delete_debugger_pointer (file, line);

                  do_process_events ();
                }
              else
                ::error ("invalid struct in debug pointer callback");
            }
          else
            ::error ("expecting struct in debug pointer callback");
        }
      else
        ::error ("invalid call to debug pointer callback");
    }
}

void
octave_link::do_pre_input_event_hook_fcn (void)
{
  do_update_workspace ();
}

void
octave_link::do_post_input_event_hook_fcn (void)
{
  do_update_history ();
}

void
octave_link::do_enter_debugger_event_hook_fcn (const octave_value_list& args)
{
  do_insert_debugger_pointer (args);
}

void
octave_link::do_exit_debugger_event_hook_fcn (const octave_value_list& args)
{
  do_delete_debugger_pointer (args);
}

void
octave_link::do_update_breakpoint_hook_fcn
  (bool insert, const octave_value_list& args)
{
  if (event_listener)
    {
      if (args.length () == 1)
        {
          octave_scalar_map m = args(0).scalar_map_value ();

          if (! error_state)
            {
              octave_value ov_file = m.getfield ("file");
              octave_value ov_line = m.getfield ("line");

              std::string file = ov_file.string_value ();
              int line = ov_line.int_value ();

              if (! error_state)
                {
                  event_listener->update_dbstop_marker (insert, file, line);

                  do_process_events ();
                }
              else
                ::error ("invalid struct in dbstop marker callback");
            }
          else
            ::error ("expecting struct in dbstop marker callback");
        }
      else
        ::error ("invalid call to dbstop marker callback");
    }
}

void
octave_link::do_edit_file (const octave_value_list& args)
{
  if (event_listener)
    {
      if (args.length () == 1)
        {
          std::string file = args(0).string_value ();

          if (! error_state)
            {
              event_listener->edit_file (file);
              do_process_events ();

            }
          else
            ::error ("expecting file name in edit file callback");
        }
      else
        ::error ("invalid call to edit file callback");
    }
}

bool
octave_link::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_link ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create octave_link object!");

      retval = false;
    }

  return retval;
}
