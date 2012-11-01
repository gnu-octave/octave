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
