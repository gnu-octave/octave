////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_main_window_h)
#define octave_main_window_h 1

// Qt includes
#include <QCloseEvent>
#include <QComboBox>
#include <QMainWindow>
#include <QPointer>
#include <QQueue>
#include <QStatusBar>
#include <QTabWidget>
#include <QThread>
#include <QToolBar>
#include <QToolButton>
#include <QTranslator>

// Editor includes
#include "external-editor-interface.h"
#include "file-editor-interface.h"

// Own includes
#include "dialog.h"
#include "documentation-dock-widget.h"
#include "files-dock-widget.h"
#include "find-files-dialog.h"
#include "history-dock-widget.h"
#include "interpreter-qobject.h"
#include "led-indicator.h"
#include "octave-dock-widget.h"
#include "octave-qobject.h"
#include "qt-interpreter-events.h"
#include "set-path-dialog.h"
#include "terminal-dock-widget.h"
#include "variable-editor.h"
#include "workspace-view.h"

class octave_value;

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class settings_dialog;

//! Represents the main window.

class main_window : public QMainWindow
{
  Q_OBJECT

public:

  typedef std::pair <std::string, std::string> name_pair;
  typedef std::pair <int, int> int_pair;

  main_window (base_qobject& oct_qobj);

  ~main_window (void);

  void make_dock_widget_connections (octave_dock_widget *dw);

  bool command_window_has_focus (void) const;

  void focus_command_window (void);

  bool confirm_shutdown (void);

signals:

  // Note: CLOSE_GUI_SIGNAL is currently only used by the new
  // experimental terminal widget.
  void close_gui_signal (void);

  void active_dock_changed (octave_dock_widget *, octave_dock_widget *);
  void editor_focus_changed (bool);

  void settings_changed (const gui_settings *);
  void init_terminal_size_signal (void);
  void init_window_menu (void);
  void new_file_signal (const QString&);
  void open_file_signal (const QString&);
  void open_file_signal (const QString& file, const QString& enc, int line);
  void step_into_file_signal (void);

  void show_community_news_signal (int serial);
  void show_release_notes_signal (void);

  void update_gui_lexer_signal (bool);

  void insert_debugger_pointer_signal (const QString& file, int line);
  void delete_debugger_pointer_signal (const QString& file, int line);
  void update_breakpoint_marker_signal (bool insert, const QString& file,
                                        int line, const QString& cond);

  void copyClipboard_signal (void);
  void pasteClipboard_signal (void);
  void selectAll_signal (void);
  void undo_signal (void);

  void add_actions_signal (QList <QAction *> action_list);

  void warning_function_not_found_signal (const QString& message);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

  void execute_command_signal (const QString& command);

public slots:

  void focus_changed (QWidget *w_old, QWidget *w_new);
  void focus_window (const QString& win_name);
  void request_reload_settings (void);

  void report_status_message (const QString& statusMessage);
  void handle_save_workspace_request (void);
  void handle_load_workspace_request (const QString& file = QString ());
  void handle_open_any_request (const QString& file = QString ());
  void handle_clear_workspace_request (void);
  void handle_clear_command_window_request (void);
  void handle_clear_history_request (void);
  void handle_undo_request (void);
  void modify_path (const QStringList& dir_list, bool rm, bool subdirs);
  void edit_mfile (const QString&, int);
  void file_remove_proxy (const QString& o, const QString& n);
  void open_online_documentation_page (void);
  void open_bug_tracker_page (void);
  void open_octave_packages_page (void);
  void open_contribute_page (void);
  void open_donate_page (void);
  void process_settings_dialog_request (const QString& desired_tab
                                        = QString ());
  void show_about_octave (void);
  void notice_settings (const gui_settings *settings,
                        bool update_by_worker = false);
  QPalette getFusionDarkPalette();
  void prepare_to_exit (void);
  void go_to_previous_widget (void);
  void reset_windows (void);
  void do_reset_windows (bool show = true, bool save = true,
                         bool force_all = false);

  void update_octave_directory (const QString& dir);
  void browse_for_directory (void);
  void set_current_working_directory (const QString& dir);
  void change_directory_up (void);
  void accept_directory_line_edit (void);

  void execute_command_in_terminal (const QString& dir);
  void run_file_in_terminal (const QFileInfo& info);

  void handle_new_figure_request (void);

  void handle_enter_debugger (void);
  void handle_exit_debugger (void);
  void debug_continue (void);
  void debug_step_into (void);
  void debug_step_over (void);
  void debug_step_out (void);
  void debug_quit (void);
  void editor_tabs_changed (bool, bool);

  void request_open_file (void);
  void request_new_script (const QString& commands = QString ());
  void request_new_function (bool triggered = true);
  void handle_edit_mfile_request (const QString& name, const QString& file,
                                  const QString& curr_dir, int line);

  void handle_insert_debugger_pointer_request (const QString& file, int line);
  void handle_delete_debugger_pointer_request (const QString& file, int line);
  void handle_update_breakpoint_marker_request (bool insert,
                                                const QString& file, int line,
                                                const QString& cond);

  void read_settings (void);
  void init_terminal_size (void);
  void set_window_layout (gui_settings *settings);
  void write_settings (void);

  void copyClipboard (void);
  void pasteClipboard (void);
  void selectAll (void);

  void handle_gui_status_update (const QString& feature, const QString& status);

  void focus_console_after_command (void);

  void profiler_session (void);
  void profiler_session_resume (void);
  void profiler_stop (void);
  void handle_profiler_status_update (bool);
  void profiler_show (void);

  void handle_octave_ready ();

  void handle_set_path_dialog_request (void);

  //! Find files dialog.
  //!@{
  void find_files (const QString& startdir = QDir::currentPath ());
  void find_files_finished (int) { }
  //!@}

  void set_screen_size (int ht, int wd);

  //! Handling the clipboard.
  //!@{
  void clipboard_has_changed (void);
  void clear_clipboard ();
  //!@}

  //! Returns a list of dock widgets.

  QList<octave_dock_widget *> get_dock_widget_list (void)
  {
    return dock_widget_list ();
  }

private slots:

  void disable_menu_shortcuts (bool disable);
  void restore_create_file_setting (void);
  void set_file_encoding (const QString& new_encoding);
  void request_open_files (const QStringList& open_file_names);

  void warning_function_not_found (const QString& message);

protected:

  void closeEvent (QCloseEvent *closeEvent);

private:

  void adopt_dock_widgets (void);

  void adopt_terminal_widget (void);
  void adopt_documentation_widget (void);
  void adopt_file_browser_widget (void);
  void adopt_history_widget (void);
  void adopt_workspace_widget (void);
  void adopt_editor_widget (void);
  void adopt_variable_editor_widget (void);

  void construct_central_widget (void);

  void construct (void);

  void construct_octave_qt_link (void);

  QAction * add_action (QMenu *menu, const QIcon& icon,
                        const QString& text, const char *member,
                        const QWidget *receiver = nullptr);

  QMenu * m_add_menu (QMenuBar *p, QString text);
  void construct_menu_bar (void);
  void construct_file_menu (QMenuBar *p);
  void construct_new_menu (QMenu *p);
  void construct_edit_menu (QMenuBar *p);
  QAction * construct_debug_menu_item (const char *icon, const QString& item,
                                       const char *member);
  void construct_debug_menu (QMenuBar *p);
  QAction * construct_window_menu_item (QMenu *p, const QString& item,
                                        bool checkable, QWidget *);
  void construct_tools_menu (QMenuBar *p);
  void construct_window_menu (QMenuBar *p);
  void construct_help_menu (QMenuBar *p);
  void construct_documentation_menu (QMenu *p);

  void construct_news_menu (QMenuBar *p);

  void construct_tool_bar (void);

  void configure_shortcuts (void);

  QList<octave_dock_widget *> dock_widget_list (void);

  void update_default_encoding (const QString& default_encoding);

  void set_default_geometry (void);
  void resize_dock (QDockWidget *dw, int width, int height);

  base_qobject& m_octave_qobj;

  QHash<QMenu *, QStringList> m_hash_menu_text;

  QString m_default_encoding;

  QString m_default_style;
  QPalette m_default_palette;

  //! Toolbar.

  QStatusBar *m_status_bar;
  led_indicator *m_profiler_status_indicator;

  //! Dock widgets.
  //!@{
  QPointer<terminal_dock_widget> m_command_window;
  QPointer<history_dock_widget> m_history_window;
  QPointer<files_dock_widget> m_file_browser_window;
  QPointer<documentation_dock_widget> m_doc_browser_window;
  QPointer<file_editor_interface> m_editor_window;
  QPointer<workspace_view> m_workspace_window;
  QPointer<variable_editor> m_variable_editor_window;
  //!@}

  external_editor_interface *m_external_editor;
  QWidget *m_active_editor;

  octave_dock_widget *m_previous_dock;
  octave_dock_widget *m_active_dock;

  QToolBar *m_main_tool_bar;

  QMenu *m_debug_menu;

  QMenuBar *m_editor_menubar;

  QAction *m_debug_continue;
  QAction *m_debug_step_into;
  QAction *m_debug_step_over;
  QAction *m_debug_step_out;
  QAction *m_debug_quit;

  QAction *m_new_script_action;
  QAction *m_new_function_action;
  QAction *m_open_action;
  QAction *m_new_figure_action;
  QAction *m_load_workspace_action;
  QAction *m_save_workspace_action;
  QAction *m_set_path_action;
  QAction *m_preferences_action;
  QAction *m_exit_action;

  QAction *m_copy_action;
  QAction *m_paste_action;
  QAction *m_clear_clipboard_action;
  QAction *m_undo_action;
  QAction *m_clear_command_window_action;
  QAction *m_clear_command_history_action;
  QAction *m_clear_workspace_action;
  QAction *m_find_files_action;
  QAction *m_select_all_action;

  QAction *m_profiler_start;
  QAction *m_profiler_resume;
  QAction *m_profiler_stop;
  QAction *m_profiler_show;

  QAction *m_show_command_window_action;
  QAction *m_show_history_action;
  QAction *m_show_workspace_action;
  QAction *m_show_file_browser_action;
  QAction *m_show_editor_action;
  QAction *m_show_documentation_action;
  QAction *m_show_variable_editor_action;
  QAction *m_command_window_action;
  QAction *m_history_action;
  QAction *m_workspace_action;
  QAction *m_file_browser_action;
  QAction *m_editor_action;
  QAction *m_documentation_action;
  QAction *m_variable_editor_action;
  QAction *m_previous_dock_action;
  QAction *m_reset_windows_action;

  QAction *m_ondisk_doc_action;
  QAction *m_online_doc_action;
  QAction *m_report_bug_action;
  QAction *m_octave_packages_action;
  QAction *m_contribute_action;
  QAction *m_developer_action;
  QAction *m_about_octave_action;

  QAction *m_release_notes_action;
  QAction *m_current_news_action;

  //! For Toolbars.
  //!@{
  QComboBox *m_current_directory_combo_box;
  static const int current_directory_max_visible = 16;
  static const int current_directory_max_count = 16;
  QLineEdit *m_current_directory_line_edit;
  //!@}

  //! Settings dialog as guarded pointer (set to 0 when deleted).

  QPointer<settings_dialog> m_settings_dlg;

  //! Find files dialog.

  find_files_dialog *m_find_files_dlg;

  //! Set path dialog
  QPointer<set_path_dialog> m_set_path_dlg;

  //! Release notes window.

  QWidget *m_release_notes_window;

  QClipboard *m_clipboard;

  //! Some class global flags.
  //!@{
  bool m_prevent_readline_conflicts;
  bool m_prevent_readline_conflicts_menu;
  bool m_suppress_dbg_location;
  bool m_editor_has_tabs;
  bool m_editor_is_octave_file;

  //! Flag for closing the whole application.

  bool m_closing;
  //!@}

  QString m_file_encoding;
};

OCTAVE_END_NAMESPACE(octave)

#endif
