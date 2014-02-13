/*

Copyright (C) 2011-2013 Jacob Dawid

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

#include <map>

#include "file-editor-interface.h"
#include "file-editor-tab.h"

class file_editor : public file_editor_interface
{
  Q_OBJECT

public:

  typedef std::map<QString, QWidget *>::iterator editor_tab_map_iterator;
  typedef std::map<QString, QWidget *>::const_iterator editor_tab_map_const_iterator;

  file_editor (QWidget *p);
  ~file_editor (void);

  void loadFile (const QString& fileName);

  QMenu *get_mru_menu (void) { return _mru_file_menu; }
  QMenu *debug_menu (void);
  QToolBar *toolbar (void);

  void set_focus (void);
  void handle_enter_debug_mode (void);
  void handle_exit_debug_mode (void);

  void check_actions (void);
  void empty_script (bool startup, bool visible);

signals:

  void fetab_settings_changed (const QSettings *settings);
  void fetab_close_request (const QWidget* ID, bool app_closing = false);
  void fetab_change_request (const QWidget* ID);
  void fetab_file_name_query (const QWidget* ID);
  // Save is a ping-pong type of communication
  void fetab_save_file (const QWidget* ID, const QString& fileName,
                        bool remove_on_success);
  // No fetab_open, functionality in editor
  // No fetab_new, functionality in editor
  void fetab_undo (const QWidget* ID);
  void fetab_redo (const QWidget* ID);
  void fetab_copy (const QWidget* ID);
  void fetab_cut (const QWidget* ID);
  void fetab_paste (const QWidget* ID);
  void fetab_selectall (const QWidget* ID);
  void fetab_context_help (const QWidget* ID, bool);
  void fetab_context_edit (const QWidget* ID);
  void fetab_save_file (const QWidget* ID);
  void fetab_save_file_as (const QWidget* ID);
  void fetab_print_file (const QWidget* ID);
  void fetab_run_file (const QWidget* ID);
  void fetab_context_run (const QWidget* ID);
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
  void fetab_indent_selected_text (const QWidget* ID);
  void fetab_unindent_selected_text (const QWidget* ID);
  void fetab_find (const QWidget* ID);
  void fetab_goto_line (const QWidget* ID, int line = -1);
  void fetab_completion (const QWidget*);
  void fetab_insert_debugger_pointer (const QWidget* ID, int line = -1);
  void fetab_delete_debugger_pointer (const QWidget* ID, int line = -1);
  void fetab_do_breakpoint_marker (bool insert, const QWidget* ID,
                                   int line = -1);
  void fetab_set_focus (const QWidget* ID);

  void fetab_zoom_in (const QWidget* ID);
  void fetab_zoom_out (const QWidget* ID);
  void fetab_zoom_normal (const QWidget* ID);

  void request_settings_dialog (const QString&);
  void execute_command_in_terminal_signal (const QString&);
  void file_loaded_signal ();

public slots:
  void focus (void);

  void request_new_file (const QString& commands);
  void request_new_script (const QString& commands);
  void request_new_function (bool triggered = true);
  void request_open_file (void);
  void request_close_file (bool);
  void request_close_all_files (bool);
  void request_close_other_files (bool);
  void request_mru_open_file (QAction *action);
  void request_print_file (void);

  void request_undo (void);
  void request_redo (void);
  void request_copy (void);
  void request_cut (void);
  void request_paste (void);
  void request_selectall (void);
  void request_context_help (bool);
  void request_context_doc (bool);
  void request_context_edit (bool);
  void request_save_file (void);
  void request_save_file_as (void);
  void request_run_file (void);
  void request_context_run (bool);
  void request_toggle_bookmark (void);
  void request_next_bookmark (void);
  void request_previous_bookmark (void);
  void request_remove_bookmark (void);

  void request_toggle_breakpoint (void);
  void request_next_breakpoint (void);
  void request_previous_breakpoint (void);
  void request_remove_breakpoint (void);

  void request_comment_selected_text (void);
  void request_uncomment_selected_text (void);

  void request_indent_selected_text (void);
  void request_unindent_selected_text (void);

  void request_find (void);

  void request_goto_line (void);
  void request_completion (void);

  void handle_file_name_changed (const QString& fileName,
                                 const QString& toolTip);
  void handle_tab_close_request (int index);
  void handle_tab_remove_request (void);
  void handle_add_filename_to_list (const QString& fileName, QWidget *ID);
  void active_tab_changed (int index);
  void handle_editor_state_changed (bool enableCopy, const QString& fileName);
  void handle_mru_add_file (const QString& file_name);
  void check_conflict_save (const QString& fileName, bool remove_on_success);

  void handle_insert_debugger_pointer_request (const QString& file, int line);
  void handle_delete_debugger_pointer_request (const QString& file, int line);
  void handle_update_breakpoint_marker_request (bool insert,
                                                const QString& file, int line);

  void handle_edit_file_request (const QString& file);

  // Tells the editor to react on changed settings.
  void notice_settings (const QSettings *settings);

  // Tells the ditor to dis- or enable some shortcuts
  void set_shortcuts (bool set_shortcuts);

  void handle_visibility (bool visible);


protected slots:
  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();

private slots:

  void request_open_files (const QStringList&);
  void request_open_file (const QString& fileName, int line = -1,
                          bool debug_pointer = false,
                          bool breakpoint_marker = false, bool insert = true);
  void request_preferences (bool);
  void request_styles_preferences (bool);
  void restore_create_file_setting ();

  void zoom_in (bool);
  void zoom_out (bool);
  void zoom_normal (bool);

private:

  bool is_editor_console_tabbed ();
  void construct (void);
  void add_file_editor_tab (file_editor_tab *f, const QString& fn);
  void save_file_as (QWidget *fetabID = 0);
  void mru_menu_update (void);
  bool call_custom_editor (const QString& file_name = QString (), int line = -1);

  QWidget *find_tab_widget (const QString& openFileName) const;

  std::map<QString, QWidget *> editor_tab_map;

  QString ced;

  QMenuBar *_menu_bar;
  QToolBar *_tool_bar;
  QMenu *_debug_menu;

  QAction *_comment_selection_action;
  QAction *_uncomment_selection_action;

  QAction *_indent_selection_action;
  QAction *_unindent_selection_action;

  QAction *_copy_action;
  QAction *_cut_action;
  QAction *_paste_action;
  QAction *_selectall_action;
  QAction *_context_help_action;
  QAction *_context_doc_action;

  QAction *_zoom_in_action;
  QAction *_zoom_out_action;
  QAction *_zoom_normal_action;

  QAction *_find_action;
  QAction *_goto_line_action;
  QAction *_completion_action;

  QAction *_next_bookmark_action;
  QAction *_previous_bookmark_action;
  QAction *_toggle_bookmark_action;
  QAction * _remove_bookmark_action;

  QAction *_print_action;
  QAction *_run_action;
  QAction *_context_run_action;

  QAction *_context_edit_action;
  QAction *_save_action;
  QAction *_save_as_action;
  QAction *_close_action;
  QAction *_close_all_action;
  QAction *_close_others_action;

  QAction *_redo_action;
  QAction *_undo_action;

  QAction *_preferences_action;
  QAction *_styles_preferences_action;

  QTabWidget *_tab_widget;

  int _marker_breakpoint;

  enum { MaxMRUFiles = 10 };
  QMenu *_mru_file_menu;
  QAction *_mru_file_actions[MaxMRUFiles];
  QStringList _mru_files;

};

#endif // FILEEDITORMDISUBWINDOW_H
