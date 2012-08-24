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

#include <string>

#include "cmd-hist.h"
#include "oct-env.h"

#include "debug.h"
#include "oct-obj.h"
#include "parse.h"
#include "symtab.h"
#include "toplev.h"

#include "octave-event.h"

#include <readline/readline.h>

void
octave_event::call_octave_function (std::string name)
{
  call_octave_function (name, octave_value_list ());
}

void
octave_event::call_octave_function (std::string name,
                                    const octave_value_list& args,
                                    int nargout)
{
  try
    {
      feval (name, args, nargout);
    } catch (...) { } // Ignore exceptions. Crashes without that.
}

void
octave_event::finish_readline_event () const
{
  rl_line_buffer[0] = '\0';
  rl_point = rl_end = 0;
  rl_done = 1;
  //rl_forced_update_display ();
}

bool
octave_clear_history_event::perform ()
{
  int i;
  while ((i = command_history::length ()) > 0) {
    command_history::remove (i - 1);
  }
  return true;
}

bool
octave_debug_step_into_event::perform ()
{
  octave_value_list args;
  args.append (octave_value ("in"));
  call_octave_function ("dbstep", args);
  finish_readline_event ();
  return true;
}

bool
octave_debug_step_out_event::perform ()
{
  octave_value_list args;
  args.append (octave_value ("out"));
  call_octave_function ("dbstep", args);
  finish_readline_event ();
  return true;
}


bool
octave_exit_event::perform ()
{
  clean_up_and_exit (0);
  return true;
}

bool
octave_run_file_event::perform ()
{
  octave_value_list args;
  args.append (octave_value (_file));
  call_octave_function ("run", args);
  finish_readline_event ();
  return true;
}

bool
octave_change_directory_event::perform ()
{
  return octave_env::chdir (_directory);
}

bool
octave_load_workspace_event::perform ()
{
  octave_value_list args;
  args.append (octave_value (_file));
  call_octave_function ("load", args);
  return true;
}

bool
octave_save_workspace_event::perform ()
{
  octave_value_list args;
  args.append (octave_value (_file));
  call_octave_function ("save", args);
  return true;
}

bool
octave_add_breakpoint_event::perform ()
{
  bp_table::intmap intmap;
  intmap[0] = _line + 1;

  std::string previous_directory = octave_env::get_current_directory ();
  octave_env::chdir (_path);
  intmap = bp_table::add_breakpoint (_function_name, intmap);
  octave_env::chdir (previous_directory);
  return intmap.size () > 0;
}

bool
octave_remove_breakpoint_event::perform ()
{
  bp_table::intmap intmap;
  intmap[0] = _line;

  std::string previous_directory = octave_env::get_current_directory ();
  octave_env::chdir (_path);
  bp_table::remove_breakpoint (_function_name, intmap);
  octave_env::chdir (previous_directory);
  return true; // TODO: Check result.
}

bool
octave_remove_all_breakpoints_event::perform ()
{
  bp_table::intmap intmap;
  std::string previous_directory = octave_env::get_current_directory ();
  octave_env::chdir (_path);
  intmap = bp_table::remove_all_breakpoints_in_file (_function_name, true);
  octave_env::chdir (previous_directory);
  return intmap.size() > 0;
}
