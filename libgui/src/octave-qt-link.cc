/*

Copyright (C) 2013 John W. Eaton
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

#include "octave-qt-link.h"

octave_qt_link::octave_qt_link (void)
  : octave_link (), main_thread (new octave_main_thread)
{
  // Start the first one.
  main_thread->start ();
}

void
octave_qt_link::do_update_workspace (void)
{
  if (event_listener)
    {
      event_listener->update_workspace ();

      do_process_events ();
    }
}

void
octave_qt_link::do_update_history (void)
{
  if (event_listener)
    {
      event_listener->update_history ();

      do_process_events ();
    }
}

void
octave_qt_link::do_insert_debugger_pointer (const octave_value_list& args)
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
octave_qt_link::do_delete_debugger_pointer (const octave_value_list& args)
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
octave_qt_link::do_pre_input_event (void)
{
  do_update_workspace ();
}

void
octave_qt_link::do_post_input_event (void)
{
  do_update_history ();
}

void
octave_qt_link::do_enter_debugger_event (const octave_value_list& args)
{
  do_insert_debugger_pointer (args);
}

void
octave_qt_link::do_exit_debugger_event (const octave_value_list& args)
{
  do_delete_debugger_pointer (args);
}

void
octave_qt_link::do_update_breakpoint (bool insert,
                                      const octave_value_list& args)
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
octave_qt_link::do_edit_file (const octave_value_list& args)
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
