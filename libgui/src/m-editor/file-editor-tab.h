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

#ifndef FILEEDITORTAB_H
#define FILEEDITORTAB_H

#include <Qsci/qsciscintilla.h>
#include <QWidget>
#include <QCloseEvent>
#include <QFileSystemWatcher>
#include "octave-event-observer.h"

class file_editor;
class file_editor_tab : public QWidget, public octave_event_observer
{
  Q_OBJECT
  public:
  file_editor_tab (file_editor *fileEditor);
  bool copy_available ();

  void event_accepted (octave_event *e);
  void event_reject (octave_event *e);

public slots:
  void update_window_title(bool modified);
  void handle_copy_available(bool enableCopy);
  void handle_margin_clicked (int line, int margin, Qt::KeyboardModifiers state);
  void comment_selected_text ();
  void uncomment_selected_text ();
  void find ();
  void remove_bookmark ();
  void toggle_bookmark ();
  void next_bookmark ();
  void previous_bookmark ();
  void remove_all_breakpoints ();
  void toggle_breakpoint ();
  void next_breakpoint ();
  void previous_breakpoint ();
  void cut ();
  void copy ();
  void paste ();
  void undo ();
  void redo ();
  void set_debugger_position (int line);

  void set_modified (bool modified = true);

  bool open_file (const QString& dir = QString ());
  bool load_file (const QString& fileName, bool silent = false);
  void new_file ();
  bool save_file ();
  bool save_file (const QString& saveFileName);
  bool save_file_as();
  void run_file ();

  void file_has_changed (const QString& fileName);
  QString get_file_name () const {return _file_name;}

  /** Tells the editor tab to react on changed settings. */
  void notice_settings ();

signals:
  void file_name_changed (const QString& fileName);
  void editor_state_changed ();
  void close_request ();

protected:
  void closeEvent (QCloseEvent *event);
  void set_file_name (const QString& fileName);

private:
  void update_lexer ();
  void request_add_breakpoint (int line);
  void request_remove_breakpoint (int line);

  void update_tracked_file ();
  int check_file_modified (const QString& msg, int cancelButton);
  void do_comment_selected_text (bool comment);

  file_editor *         _file_editor;
  QsciScintilla *       _edit_area;

  QString               _file_name;
  QString               _file_name_short;

  bool                  _long_title;
  bool                  _copy_available;

  QFileSystemWatcher    _file_system_watcher;
};

#endif // FILEEDITORTAB_H
