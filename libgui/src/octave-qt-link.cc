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

#include <QStringList>

#include "str-vec.h"

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
octave_qt_link::do_set_history (const string_vector& hist)
{
  QStringList qt_hist;

  for (octave_idx_type i = 0; i < hist.length (); i++)
    qt_hist.append (QString::fromStdString (hist[i]));

  emit set_history_signal (qt_hist);
}

void
octave_qt_link::do_append_history (const std::string& hist_entry)
{
  emit append_history_signal (QString::fromStdString (hist_entry));
}

void
octave_qt_link::do_clear_history (void)
{
  emit clear_history_signal ();
}

void
octave_qt_link::do_pre_input_event (void)
{
  do_update_workspace ();
}

void
octave_qt_link::do_post_input_event (void)
{
}

void
octave_qt_link::do_enter_debugger_event (const std::string& file, int line)
{
  do_insert_debugger_pointer (file, line);
}

void
octave_qt_link::do_exit_debugger_event (const std::string& file, int line)
{
  do_delete_debugger_pointer (file, line);
}

void
octave_qt_link::do_update_breakpoint (bool insert,
                                      const std::string& file, int line)
{
  emit update_dbstop_marker_signal (insert, QString::fromStdString (file), line);
}

bool
octave_qt_link::do_edit_file (const std::string& file)
{
  emit edit_file_signal (QString::fromStdString (file));

  return true;
}

void
octave_qt_link::do_insert_debugger_pointer (const std::string& file, int line)
{
  emit insert_debugger_pointer_signal (QString::fromStdString (file), line);
}

void
octave_qt_link::do_delete_debugger_pointer (const std::string& file, int line)
{
  emit delete_debugger_pointer_signal (QString::fromStdString (file), line);
}
