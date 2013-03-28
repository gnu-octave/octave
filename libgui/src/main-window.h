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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

// Qt includes
#include <QtGui/QMainWindow>
#include <QThread>
#include <QTabWidget>
#include <QMdiArea>
#include <QStatusBar>
#include <QToolBar>
#include <QQueue>
#include <QMdiSubWindow>
#include <QCloseEvent>
#include <QToolButton>
#include <QComboBox>

// Editor includes
#include "file-editor-interface.h"

// QTerminal includes
#include "QTerminal.h"

// Own includes
#include "resource-manager.h"
#include "workspace-view.h"
#include "history-dockwidget.h"
#include "files-dockwidget.h"
#include "terminal-dockwidget.h"
#include "documentation-dockwidget.h"
#include "octave-qt-event-listener.h"

/**
 * \class MainWindow
 *
 * Represents the main window.
 */
class main_window : public QMainWindow
{
  Q_OBJECT public:
  main_window (QWidget * parent = 0);
  ~main_window ();

  QTerminal *get_terminal_view () { return _terminal; }
  history_dock_widget *get_history_dock_widget ()
  {
    return _history_dock_widget;
  }
  files_dock_widget *get_files_dock_widget ()
  {
    return _files_dock_widget;
  }
  bool is_closing () { return _closing; }

signals:
  void settings_changed ();

public slots:
  void report_status_message (const QString& statusMessage);
  void handle_save_workspace_request ();
  void handle_load_workspace_request ();
  void handle_clear_workspace_request ();
  void handle_clear_history_request ();
  void handle_command_double_clicked (const QString& command);
  void new_file ();
  void open_file ();
  void open_file (const QString& file_name);
  void open_online_documentation_page ();
  void open_bug_tracker_page ();
  void open_octave_forge_page ();
  void open_agora_page ();
  void process_settings_dialog_request ();
  void show_about_octave ();
  void notice_settings ();
  void prepare_for_quit ();
  void reset_windows ();
  void current_working_directory_has_changed (const QString& directory);
  void update_workspace (void);
  void update_history (void);
  void change_current_working_directory ();
  void set_current_working_directory (const QString& directory);
  void current_working_directory_up ();
  void current_working_directory_entered ();

  void focus_command_window ();
  void focus_command_history ();
  void focus_current_directory ();
  void focus_workspace ();
  void focus_editor ();
  void focus_documentation ();
  void handle_command_window_visible (bool);
  void handle_command_history_visible (bool);
  void handle_current_directory_visible (bool);
  void handle_workspace_visible (bool);
  void handle_editor_visible (bool);
  void handle_documentation_visible (bool);

  void handle_entered_debug_mode ();
  void handle_quit_debug_mode ();
  void handle_update_debug_pointer_request (const QString& file, int line);
  void debug_continue ();
  void debug_step_into ();
  void debug_step_over ();
  void debug_step_out ();
  void debug_quit ();

  void read_settings ();
  void write_settings ();
  void connect_visibility_changed ();

protected:
  void closeEvent (QCloseEvent * closeEvent);

private:
  void construct ();
  void establish_octave_link ();

  void save_workspace_callback (const std::string& file);

  void load_workspace_callback (const std::string& file);

  void clear_workspace_callback (void);

  void clear_history_callback (void);

  void change_directory_callback (const std::string& directory);

  void debug_continue_callback (void);

  void debug_step_into_callback (void);

  void debug_step_over_callback (void);

  void debug_step_out_callback (void);

  void debug_quit_callback (void);

  void exit_callback (void);

  QTerminal *               _terminal;
#ifdef HAVE_QSCINTILLA
  file_editor_interface *   _file_editor;
#endif
  QMenu *                   _debug_menu;

  QAction *                 _debug_continue;
  QAction *                 _debug_step_into;
  QAction *                 _debug_step_over;
  QAction *                 _debug_step_out;
  QAction *                 _debug_quit;

  // Dock widgets.
  workspace_view *          _workspace_view;
  history_dock_widget *     _history_dock_widget;
  files_dock_widget *       _files_dock_widget;
  terminal_dock_widget *    _terminal_dock_widget;
  documentation_dock_widget*_documentation_dock_widget;

  // Toolbars.
  QStatusBar *              _status_bar;
  QComboBox *               _current_directory_combo_box;
  static const int        current_directory_width       = 300;
  static const int        current_directory_max_visible = 16;
  static const int        current_directory_max_count   = 16;
  QLineEdit *               _current_directory_line_edit;

  octave_qt_event_listener *_octave_qt_event_listener;

  // Flag for closing whole application
  bool                      _closing;
};

#endif // MAINWINDOW_H
