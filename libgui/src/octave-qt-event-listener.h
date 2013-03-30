/*

Copyright (C) 2011-2012 Jacob Dawid

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

#ifndef OCTAVEQTEVENTLISTENER_H
#define OCTAVEQTEVENTLISTENER_H

#include <QObject>
#include <QString>
#include "octave-event-listener.h"

class octave_qt_event_listener
  : public QObject, public octave_event_listener
{
  Q_OBJECT
  public:
  octave_qt_event_listener (QObject *parent = 0);

  void current_directory_has_changed (const std::string& directory);
  void update_workspace (void);
  void update_history (void);
  void insert_debugger_pointer (const std::string& file, int line);
  void delete_debugger_pointer (const std::string& file, int line);
  void update_dbstop_marker (bool insert, const std::string& file, int line);
  void edit_file (const std::string& file);
  void about_to_exit ();

  void entered_debug_mode ();
  void quit_debug_mode ();

signals:
  void current_directory_has_changed_signal (const QString& directory);
  void update_workspace_signal (void);
  void update_history_signal (void);
  void insert_debugger_pointer_signal (const QString& file, int line);
  void delete_debugger_pointer_signal (const QString& file, int line);
  void update_dbstop_marker_signal (bool insert, const QString& file, int line);
  void edit_file_signal (const QString& file);
  void entered_debug_mode_signal ();
  void quit_debug_mode_signal ();
};

#endif // OCTAVEQTEVENTLISTENER_H
