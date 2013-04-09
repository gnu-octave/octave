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

#include "workspace-element.h"

#include "octave-qt-link.h"

octave_qt_link::octave_qt_link (void)
  : octave_link (), main_thread (new octave_main_thread)
{
  connect (main_thread, SIGNAL (finished ()),
           this, SIGNAL (octave_thread_finished ()));
}

octave_qt_link::~octave_qt_link (void)
{
  delete main_thread;
}

void
octave_qt_link::execute_interpreter (void)
{
  main_thread->execute_interpreter ();
}

bool
octave_qt_link::do_edit_file (const std::string& file)
{
  emit edit_file_signal (QString::fromStdString (file));

  return true;
}

void
octave_qt_link::do_change_directory (const std::string& dir)
{
  emit change_directory_signal (QString::fromStdString (dir));
}

void
octave_qt_link::do_set_workspace (const std::list<workspace_element>& ws)
{
  QString scopes;
  QStringList symbols;
  QStringList class_names;
  QStringList dimensions;
  QStringList values;

  for (std::list<workspace_element>::const_iterator it = ws.begin ();
       it != ws.end (); it++)
    {
      scopes.append (it->scope ());
      symbols.append (QString::fromStdString (it->symbol ()));
      class_names.append (QString::fromStdString (it->class_name ()));
      dimensions.append (QString::fromStdString (it->dimension ()));
      values.append (QString::fromStdString (it->value ()));
    }

  emit set_workspace_signal (scopes, symbols, class_names, dimensions, values);
}

void
octave_qt_link::do_clear_workspace (void)
{
  emit clear_workspace_signal ();
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
}

void
octave_qt_link::do_post_input_event (void)
{
}

void
octave_qt_link::do_enter_debugger_event (const std::string& file, int line)
{
  do_insert_debugger_pointer (file, line);

  emit enter_debugger_signal ();
}

void
octave_qt_link::do_execute_in_debugger_event (const std::string& file, int line)
{
  do_delete_debugger_pointer (file, line);
}

void
octave_qt_link::do_exit_debugger_event (void)
{
  emit exit_debugger_signal ();
}

void
octave_qt_link::do_update_breakpoint (bool insert,
                                      const std::string& file, int line)
{
  emit update_breakpoint_marker_signal (insert, QString::fromStdString (file), line);
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
