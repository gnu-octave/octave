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

#ifndef FILEEDITORMDISUBWINDOW_H
#define FILEEDITORMDISUBWINDOW_H

#include <QToolBar>
#include <QAction>
#include <QMenuBar>
#include <QStatusBar>
#include <QCloseEvent>
#include <QTabWidget>
#include <QSettings>

#include <map>

#include "file-editor-interface.h"
#include "file-editor-tab.h"

enum editor_markers
  {
    bookmark,
    breakpoint,
    debugger_position
  };

class file_editor : public file_editor_interface
{
  Q_OBJECT

public:

  typedef std::map<QString, QWidget *>::iterator editor_tab_map_iterator;
  typedef std::map<QString, QWidget *>::const_iterator editor_tab_map_const_iterator;

  file_editor (QWidget *p);
  ~file_editor ();

  void connect_visibility_changed (void);

  void loadFile (const QString& fileName);

  QMenu *           get_mru_menu ( ) { return _mru_file_menu; }
  QMenu *           debug_menu ();
  QToolBar *        toolbar ();

  void set_focus ();
  void handle_enter_debug_mode (void);
  void handle_exit_debug_mode (void);

signals:
  void fetab_settings_changed (const QSettings *settings);
  void fetab_close_request (const QWidget* ID);
  void fetab_change_request (const QWidget* ID);
  void fetab_file_name_query (const QWidget* ID);
  // Save is a ping-pong type of communication
  void fetab_save_file (const QWidget* ID, const QString& fileName, bool remove_on_success);
  // No fetab_open, functionality in editor
  // No fetab_new, functionality in editor
  void fetab_undo (const QWidget* ID);
  void fetab_redo (const QWidget* ID);
  void fetab_copy (const QWidget* ID);
  void fetab_cut (const QWidget* ID);
  void fetab_paste (const QWidget* ID);
  void fetab_save_file (const QWidget* ID);
  void fetab_save_file_as (const QWidget* ID);
  void fetab_print_file (const QWidget* ID);
  void fetab_run_file (const QWidget* ID);
  void fetab_toggle_bookmark (const QWidget* ID);
  void fetab_next_bookmark (const QWidget* ID);
  void fetab_previous_bookmark (const QWidget* ID);
  void fetab_remove_bookmark (const QWidget* ID);
  void fetab_toggle_breakpoint (const QWidget* ID);
  void fetab_next_breakpoint (const QWidget* ID);
  void fetab_previous_breakpoint (const QWidget* ID);
  void fetab_remove_all_breakpoints (const QWidget* ID);
  void fetab_comment_selected_text (const QWidget* ID);
  void fetab_uncomment_selected_text (const QWidget* ID);
  void fetab_find (const QWidget* ID);
  void fetab_goto_line (const QWidget* ID, int line = -1);
  void fetab_insert_debugger_pointer (const QWidget* ID, int line = -1);
  void fetab_delete_debugger_pointer (const QWidget* ID, int line = -1);
  void fetab_do_breakpoint_marker (bool insert, const QWidget* ID, int line = -1);
  void fetab_set_focus (const QWidget* ID);

public slots:
  void focus (void);
  void handle_visibility (bool visible);

  void request_new_file (const QString& commands);
  void request_open_file ();
  void request_mru_open_file ();
  void request_print_file ();

  void request_undo ();
  void request_redo ();
  void request_copy ();
  void request_cut ();
  void request_paste ();
  void request_save_file ();
  void request_save_file_as ();
  void request_run_file ();
  void request_toggle_bookmark ();
  void request_next_bookmark ();
  void request_previous_bookmark ();
  void request_remove_bookmark ();

  void request_toggle_breakpoint ();
  void request_next_breakpoint ();
  void request_previous_breakpoint ();
  void request_remove_breakpoint ();

  void request_comment_selected_text ();
  void request_uncomment_selected_text ();
  void request_find ();

  void request_goto_line ();

  void handle_file_name_changed (const QString& fileName, const QString& toolTip);
  void handle_tab_close_request (int index);
  void handle_tab_remove_request ();
  void handle_add_filename_to_list (const QString& fileName, QWidget *ID);
  void active_tab_changed (int index);
  void handle_editor_state_changed (bool enableCopy, const QString& fileName);
  void handle_mru_add_file (const QString& file_name);
  void check_conflict_save (const QString& fileName, bool remove_on_success);

  void handle_insert_debugger_pointer_request (const QString& file, int line);
  void handle_delete_debugger_pointer_request (const QString& file, int line);
  void handle_update_breakpoint_marker_request (bool insert, const QString& file,
                                            int line);
  void handle_edit_file_request (const QString& file);

  /** Tells the editor to react on changed settings. */
  void notice_settings (const QSettings *settings);

private slots:
  void request_open_file (const QString& fileName, int line = -1,
                          bool debug_pointer = false,
                          bool breakpoint_marker = false, bool insert = true);

private:
  void construct ();
  void add_file_editor_tab(file_editor_tab *f, const QString &fn);
  void save_file_as (QWidget *fetabID = 0);
  void mru_menu_update ();

  QWidget *find_tab_widget (const QString& openFileName) const;

  std::map<QString, QWidget *> editor_tab_map;

  QString ced;

  QMenuBar *        _menu_bar;
  QToolBar *        _tool_bar;
  QMenu *           _debug_menu;
  QAction *         _copy_action;
  QAction *         _cut_action;
  QAction *         _run_action;
  QTabWidget *      _tab_widget;
  int               _marker_breakpoint;

  enum { MaxMRUFiles = 10 };
  QMenu *_mru_file_menu;
  QAction *_mru_file_actions[MaxMRUFiles];
  QStringList _mru_files;

};

#endif // FILEEDITORMDISUBWINDOW_H
