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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <utility>

#include <QAction>
#include <QApplication>
#include <QClipboard>
#include <QDateTime>
#include <QDebug>
#include <QDesktopServices>
#include <QFileDialog>
#include <QIcon>
#include <QInputDialog>
#include <QKeySequence>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QScreen>
#include <QStyle>
#include <QStyleFactory>
#include <QTextBrowser>
#include <QTextStream>
#include <QThread>
#include <QTimer>
#include <QToolBar>
#include <QWindow>

// QTerminal includes
#include "QTerminal.h"

#if defined (HAVE_QSCINTILLA)
#  include "file-editor.h"
#  include "command-widget.h"
#endif
#include "gui-preferences-cs.h"
#include "gui-preferences-dw.h"
#include "gui-preferences-ed.h"
#include "gui-preferences-global.h"
#include "gui-preferences-mw.h"
#include "gui-preferences-nr.h"
#include "gui-preferences-sc.h"
#include "gui-settings.h"
#include "gui-utils.h"
#include "interpreter-qobject.h"
#include "main-window.h"
#include "news-reader.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "settings-dialog.h"
#include "shortcut-manager.h"
#include "welcome-wizard.h"

#include "cmd-edit.h"
#include "oct-env.h"
#include "url-transfer.h"

#include "builtin-defun-decls.h"
#include "defaults.h"
#include "interpreter.h"
#include "load-path.h"
#include "utils.h"
#include "syminfo.h"
#include "version.h"

OCTAVE_BEGIN_NAMESPACE(octave)

main_window::main_window (base_qobject& oct_qobj)
: QMainWindow (), m_octave_qobj (oct_qobj),
  m_status_bar (nullptr),
  m_command_window (nullptr),
  m_history_window (nullptr),
  m_file_browser_window (nullptr),
  m_editor_window (nullptr),
  m_workspace_window (nullptr),
  m_external_editor (new external_editor_interface (this, m_octave_qobj)),
  m_active_editor (m_external_editor), m_settings_dlg (nullptr),
  m_find_files_dlg (nullptr), m_set_path_dlg (nullptr),
  m_clipboard (QApplication::clipboard ()),
  m_prevent_readline_conflicts (true),
  m_prevent_readline_conflicts_menu (false),
  m_suppress_dbg_location (true),
  m_closing (false), m_file_encoding (QString ())
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  if (rmgr.is_first_run ())
    {
      // Before wizard.
      m_octave_qobj.config_translators ();

      welcome_wizard welcomeWizard (m_octave_qobj);

      if (welcomeWizard.exec () == QDialog::Rejected)
        exit (1);

      // Install settings file.
      rmgr.reload_settings ();
    }
  else
    {
      // Get settings file.
      rmgr.reload_settings ();

      // After settings.
      m_octave_qobj.config_translators ();
    }

  setObjectName (gui_obj_name_main_window);

  rmgr.config_icon_theme ();

  rmgr.update_network_settings ();

  // We provide specific terminal capabilities, so ensure that
  // TERM is always set appropriately.

#if defined (OCTAVE_USE_WINDOWS_API)
  sys::env::putenv ("TERM", "cygwin");
#else
  sys::env::putenv ("TERM", "xterm");
#endif

  // FIXME: can we do this job when creating the shortcut manager?
  // A quick look shows that it may require some coordination with the
  // resource manager.  Startup is complicated, but maybe we can make
  // it simpler?
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();
  scmgr.init_data ();

  construct_central_widget ();

  m_status_bar = new QStatusBar (this);
  m_profiler_status_indicator = new led_indicator ();
  QLabel *text = new QLabel (tr ("Profiler"));
  m_status_bar->addPermanentWidget (text);
  m_status_bar->addPermanentWidget (m_profiler_status_indicator);

  adopt_dock_widgets ();

#if defined (HAVE_QGUIAPPLICATION_SETDESKTOPFILENAME)
  QGuiApplication::setDesktopFileName ("org.octave.Octave.desktop");
#endif

  QApplication *qapp = m_octave_qobj.qapplication ();

  m_default_style = qapp->style ()->objectName ();
  m_default_palette = qapp->palette ();

  gui_settings *settings = rmgr.get_settings ();

  bool connect_to_web = true;
  QDateTime last_checked;
  int serial = 0;
  m_active_dock = nullptr;

  if (settings)
    {
      connect_to_web
        = settings->value (nr_allow_connection).toBool ();

      last_checked
        = settings->value (nr_last_time).toDateTime ();

      serial = settings->value (nr_last_news).toInt ();
      m_default_encoding = settings->value (ed_default_enc).toString ();
    }

  QDateTime current = QDateTime::currentDateTime ();
  QDateTime one_day_ago = current.addDays (-1);

  if (connect_to_web
      && (! last_checked.isValid () || one_day_ago > last_checked))
    emit show_community_news_signal (serial);

  construct_octave_qt_link ();

  // We have to set up all our windows, before we finally launch
  // octave.

  construct ();

  read_settings ();

  init_terminal_size ();

  emit init_window_menu ();

  focus_command_window ();
}

main_window::~main_window (void) { }

void main_window::adopt_dock_widgets (void)
{
  adopt_terminal_widget ();
  adopt_documentation_widget ();
  adopt_file_browser_widget ();
  adopt_history_widget ();
  adopt_workspace_widget ();
  adopt_editor_widget ();
  adopt_variable_editor_widget ();

  m_previous_dock = m_command_window;
}

void main_window::adopt_terminal_widget (void)
{
  m_command_window = m_octave_qobj.terminal_widget (this);

  make_dock_widget_connections (m_command_window);

  connect (this, &main_window::settings_changed,
           m_command_window, &terminal_dock_widget::notice_settings);

  if (! m_octave_qobj.experimental_terminal_widget ())
    {
      QTerminal *cmd_widget = m_command_window->get_qterminal ();

      // The following connections were previously made in
      // QTerminal::construct, QWinTerminalImpl::QWinTerminalImpl, and
      // QUnixTerminalImpl::QUnixTerminalImpl.  Similar actions should
      // probably be possible for the new command widget.

      connect (cmd_widget, &QTerminal::report_status_message,
               this, &main_window::report_status_message);

      connect (cmd_widget, &QTerminal::edit_mfile_request,
               this, &main_window::edit_mfile);

      connect (cmd_widget, &QTerminal::execute_command_in_terminal_signal,
               this, &main_window::execute_command_in_terminal);

      connect (this, &main_window::init_terminal_size_signal,
               cmd_widget, &QTerminal::init_terminal_size);

      connect (this, &main_window::copyClipboard_signal,
               cmd_widget, &QTerminal::copyClipboard);

      connect (this, &main_window::pasteClipboard_signal,
               cmd_widget, &QTerminal::pasteClipboard);

      connect (this, &main_window::selectAll_signal,
               cmd_widget, &QTerminal::selectAll);

      connect (cmd_widget, &QTerminal::request_edit_mfile_signal,
               this, &main_window::edit_mfile);

      connect (cmd_widget, &QTerminal::request_open_file_signal,
               this, QOverload<const QString&, const QString&, int>::of (&main_window::open_file_signal));

      connect (cmd_widget, &QTerminal::set_screen_size_signal,
               this, &main_window::set_screen_size);

      connect (cmd_widget, &QTerminal::clear_command_window_request,
               this, &main_window::handle_clear_command_window_request);
    }
  else
    {
      connect (this, &main_window::execute_command_signal,
               m_command_window, &terminal_dock_widget::execute_command_signal);
    }
}

void main_window::adopt_documentation_widget (void)
{
  m_doc_browser_window = m_octave_qobj.documentation_widget (this);

  make_dock_widget_connections (m_doc_browser_window);
}

void main_window::adopt_file_browser_widget (void)
{
  m_file_browser_window = m_octave_qobj.file_browser_widget (this);

  make_dock_widget_connections (m_file_browser_window);

  connect (m_file_browser_window, &files_dock_widget::open_file,
           this, QOverload<const QString&>::of (&main_window::open_file_signal));
  connect (m_file_browser_window,
           &files_dock_widget::displayed_directory_changed,
           this, &main_window::set_current_working_directory);

  connect (m_file_browser_window, &files_dock_widget::modify_path_signal,
           this, &main_window::modify_path);

  connect (m_file_browser_window, &files_dock_widget::run_file_signal,
           this, &main_window::run_file_in_terminal);

  connect (m_file_browser_window, &files_dock_widget::load_file_signal,
           this, &main_window::handle_load_workspace_request);

  connect (m_file_browser_window, &files_dock_widget::open_any_signal,
           this, &main_window::handle_open_any_request);

  connect (m_file_browser_window, &files_dock_widget::find_files_signal,
           this, &main_window::find_files);
}

void main_window::adopt_history_widget (void)
{
  m_history_window = m_octave_qobj.history_widget (this);

  make_dock_widget_connections (m_history_window);

  connect (m_history_window, &history_dock_widget::command_create_script,
           this, &main_window::new_file_signal);

  connect (m_history_window, &history_dock_widget::command_double_clicked,
           this, &main_window::execute_command_in_terminal);
}

void main_window::adopt_workspace_widget (void)
{
  m_workspace_window = m_octave_qobj.workspace_widget (this);

  make_dock_widget_connections (m_workspace_window);

  connect (m_workspace_window, &workspace_view::command_requested,
           this, &main_window::execute_command_in_terminal);
}

void main_window::adopt_editor_widget (void)
{
  interpreter_qobject *interp_qobj = m_octave_qobj.interpreter_qobj ();

  qt_interpreter_events *qt_link = interp_qobj->qt_link ();

#if defined (HAVE_QSCINTILLA)
  file_editor *editor = new file_editor (this, m_octave_qobj);

  make_dock_widget_connections (editor);

  // The editor is currently different from other dock widgets.  Until
  // those differences are resolved, make interpreter_event
  // connections here instead of in base_qobject::editor_widget.
  m_octave_qobj.connect_interpreter_events (editor);

  connect (editor, &file_editor::request_settings_dialog,
           this, QOverload<const QString&>::of (&main_window::process_settings_dialog_request));

  connect (editor, &file_editor::request_dbcont_signal,
           this, &main_window::debug_continue);

  connect (this, &main_window::update_gui_lexer_signal,
           editor, &file_editor::update_gui_lexer_signal);

  connect (editor, &file_editor::execute_command_in_terminal_signal,
           this, &main_window::execute_command_in_terminal);

  connect (editor, &file_editor::focus_console_after_command_signal,
           this, &main_window::focus_console_after_command);

  connect (editor, &file_editor::run_file_signal,
           this, &main_window::run_file_in_terminal);

  connect (editor, &file_editor::edit_mfile_request,
           this, &main_window::handle_edit_mfile_request);

  connect (editor, &file_editor::debug_quit_signal,
           this, &main_window::debug_quit);

  connect (this, &main_window::editor_focus_changed,
           editor, &file_editor::enable_menu_shortcuts);

  connect (this, &main_window::step_into_file_signal,
           editor, &file_editor::request_step_into_file);

  connect (editor, &file_editor::editor_tabs_changed_signal,
           this, &main_window::editor_tabs_changed);

  connect (editor, &file_editor::request_open_file_external,
           m_external_editor, &external_editor_interface::call_custom_editor);

  connect (m_external_editor, &external_editor_interface::request_settings_dialog,
           this, &main_window::process_settings_dialog_request);

  connect (this, &main_window::insert_debugger_pointer_signal,
           editor, &file_editor::handle_insert_debugger_pointer_request);

  connect (this, &main_window::delete_debugger_pointer_signal,
           editor, &file_editor::handle_delete_debugger_pointer_request);

  connect (this, &main_window::update_breakpoint_marker_signal,
           editor, &file_editor::handle_update_breakpoint_marker_request);

  // Signals for removing/renaming files/dirs in the file browser
  connect (m_file_browser_window, &files_dock_widget::file_remove_signal,
           editor, &file_editor::handle_file_remove);

  connect (m_file_browser_window, &files_dock_widget::file_renamed_signal,
           editor, &file_editor::handle_file_renamed);

  // Signals for removing/renaming files/dirs in the terminal window
  connect (qt_link, &qt_interpreter_events::file_renamed_signal,
           editor, &file_editor::handle_file_renamed);

  // Signals for entering/exiting debug mode
  connect (qt_link, &qt_interpreter_events::enter_debugger_signal,
           editor, &file_editor::handle_enter_debug_mode);

  connect (qt_link, &qt_interpreter_events::exit_debugger_signal,
           editor, &file_editor::handle_exit_debug_mode);

  connect (qt_link, &qt_interpreter_events::directory_changed_signal,
           editor, &file_editor::update_octave_directory);

  m_editor_window = editor;

  m_editor_menubar = m_editor_window->menubar ();

  m_active_editor = m_editor_window;

  m_editor_window->enable_menu_shortcuts (false);
#else
  m_editor_window = nullptr;

  m_editor_menubar = nullptr;

  m_active_editor = m_external_editor;
#endif

  connect (qt_link, SIGNAL (edit_file_signal (const QString&)),
           m_active_editor, SLOT (handle_edit_file_request (const QString&)));
}

void main_window::adopt_variable_editor_widget (void)
{
  m_variable_editor_window = m_octave_qobj.variable_editor_widget (this);

  make_dock_widget_connections (m_variable_editor_window);
}

void main_window::make_dock_widget_connections (octave_dock_widget *dw)
{
  connect (this, &main_window::init_window_menu,
           dw, &octave_dock_widget::init_window_menu_entry);

  connect (this, &main_window::settings_changed,
           dw, &octave_dock_widget::handle_settings);

  connect (this, &main_window::active_dock_changed,
           dw, &octave_dock_widget::handle_active_dock_changed);

  // FIXME: shouldn't this action should be associated with closing
  // the main window, not with exiting the application?  At one time,
  // those two actions happened together, but now it is possible to
  // close the main window without exiting the application.
  connect (qApp, &QApplication::aboutToQuit,
           dw, &octave_dock_widget::save_settings);

  // The following is required when the exp. terminal widget is used
  // and the main window is closed (no exit via interpreter)
  connect (this, &main_window::close_gui_signal,
           dw, &octave_dock_widget::save_settings);
}

bool main_window::command_window_has_focus (void) const
{
  return m_command_window->has_focus ();
}

void main_window::focus_command_window (void)
{
  m_command_window->activate ();
}

void main_window::focus_window (const QString& win_name)
{
  if (win_name == "command")
    m_command_window->activate ();
  else if (win_name == "history")
    m_history_window->activate ();
  else if (win_name == "workspace")
    m_workspace_window->activate ();
  else if (win_name == "filebrowser")
    m_file_browser_window->activate ();
}

bool main_window::confirm_shutdown (void)
{
  bool closenow = true;

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (settings->value (global_prompt_to_exit.key,
                       global_prompt_to_exit.def).toBool ())
    {
      int ans = QMessageBox::question (this, tr ("Octave"),
                                       tr ("Are you sure you want to exit Octave?"),
                                       (QMessageBox::Ok
                                        | QMessageBox::Cancel),
                                       QMessageBox::Ok);

      if (ans != QMessageBox::Ok)
        closenow = false;
    }

#if defined (HAVE_QSCINTILLA)
  if (closenow)
    closenow = m_editor_window->check_closing ();
#endif

  return closenow;
}

// catch focus changes and determine the active dock widget
void main_window::focus_changed (QWidget *, QWidget *new_widget)
{
  // If there is no new widget or the new widget is a menu bar
  // (when pressing <alt>), we can return immediately and reset the
  // focus to the previous widget
  if (! new_widget
      || (new_widget == menuBar ())
      || (new_widget == m_editor_menubar))
    {
      if (m_active_dock)
        m_active_dock->setFocus ();

      return;
    }

  octave_dock_widget *dock = nullptr;
  QWidget *w_new = new_widget;  // get a copy of new focus widget
  QWidget *start = w_new;       // Save it as start of our search
  int count = 0;                // fallback to prevent endless loop

  QList<octave_dock_widget *> w_list = dock_widget_list ();

  while (w_new && w_new != m_main_tool_bar && count < 100)
    {
      // Go through all dock widgets and check whether the current widget
      // with focus is a child of one of them.
      for (auto w : w_list)
        {
          if (w->isAncestorOf (w_new))
            dock = w;
        }

      if (dock)
        break;

      // If not yet found (in case w_new is not a child of its dock widget),
      // test next widget in the focus chain
      w_new = qobject_cast<QWidget *> (w_new->previousInFocusChain ());

      // Measures preventing an endless loop
      if (w_new == start)
        break;  // We have arrived where we began ==> exit loop
      count++;  // Limited number of trials
    }

  // editor and terminal needs extra handling
  octave_dock_widget *edit_dock_widget
    = static_cast<octave_dock_widget *> (m_editor_window);
  octave_dock_widget *cmd_dock_widget
    = static_cast<octave_dock_widget *> (m_command_window);

  // if new dock has focus, emit signal and store active focus
  // except editor changes to a dialog (dock=0)
  if ((dock || m_active_dock != edit_dock_widget) && (dock != m_active_dock))
    {
      // signal to all dock widgets for updating the style
      emit active_dock_changed (m_active_dock, dock);

      if (dock)
        {
          QList<QDockWidget *> tabbed = tabifiedDockWidgets (dock);
          if (tabbed.contains (m_active_dock))
            dock->set_predecessor_widget (m_active_dock);
        }

      // Check whether editor loses or gains focus
      int editor = 0;
      if (edit_dock_widget == dock)
        {
          emit editor_focus_changed (true);
          editor = 1;
        }
      else if (edit_dock_widget == m_active_dock)
        {
          emit editor_focus_changed (false);
          editor = -1;
        }

      // Check whether terminal loses or gains focus
      int cmd_involved = 0;
      if (cmd_dock_widget == dock)
        cmd_involved = 1;
      else if (cmd_dock_widget == m_active_dock)
        cmd_involved = -1;

      // If we have to take care of Alt+? accelerators of the main
      // window, take result of test for terminal widget above
      int command = 0;
      if (m_prevent_readline_conflicts_menu)
        command = cmd_involved;

      // If editor or command gets/looses focus, disable/enable
      // main menu accelerators (Alt + ?)
      if (editor || command)
        {
          int sum = editor + command;
          if (sum > 0)
            disable_menu_shortcuts (true);
          else if (sum < 0)
            disable_menu_shortcuts (false);
        }

      if (m_active_dock)
        m_previous_dock = m_active_dock;
      m_active_dock = dock;

      // En-/disable global shortcuts (preventing conflicts with
      // readline. Do it here because it relies on m_active_dock
      if (cmd_involved)
        configure_shortcuts ();
    }
}

void main_window::request_reload_settings (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (settings)
    emit settings_changed (settings);
}

void main_window::report_status_message (const QString& statusMessage)
{
  m_status_bar->showMessage (statusMessage, 1000);
}

void main_window::handle_save_workspace_request (void)
{
  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  int opts = 0;  // No options by default.
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    opts = QFileDialog::DontUseNativeDialog;

  QString file
    = QFileDialog::getSaveFileName (this, tr ("Save Workspace As"), ".",
                                    nullptr, nullptr, QFileDialog::Option (opts));

  if (! file.isEmpty ())
    {
      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          Fsave (interp, ovl (file.toStdString ()));
        });
    }
}

void main_window::handle_load_workspace_request (const QString& file_arg)
{
  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  int opts = 0;  // No options by default.
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    opts = QFileDialog::DontUseNativeDialog;

  QString file = file_arg;

  if (file.isEmpty ())
    file = QFileDialog::getOpenFileName (this, tr ("Load Workspace"), ".",
                                         nullptr, nullptr, QFileDialog::Option (opts));

  if (! file.isEmpty ())
    {
      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          Fload (interp, ovl (file.toStdString ()));

          tree_evaluator& tw = interp.get_evaluator ();

          event_manager& xevmgr = interp.get_event_manager ();

          xevmgr.set_workspace (true, tw.get_symbol_info ());
        });
    }
}

void main_window::handle_open_any_request (const QString& file_arg)
{
  if (! file_arg.isEmpty ())
    {
      std::string file = file_arg.toStdString ();

      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          interp.feval ("open", ovl (file));

          // Update the workspace since open.m may have loaded new
          // variables.
          tree_evaluator& tw = interp.get_evaluator ();

          event_manager& xevmgr = interp.get_event_manager ();

          xevmgr.set_workspace (true, tw.get_symbol_info ());
        });
    }
}

void main_window::handle_clear_workspace_request (void)
{
  emit interpreter_event
    ([] (interpreter& interp)
    {
      // INTERPRETER THREAD

      Fclear (interp);
    });
}

void main_window::handle_clear_command_window_request (void)
{
  emit interpreter_event
    ([] (void)
    {
      // INTERPRETER THREAD

      command_editor::kill_full_line ();
      command_editor::clear_screen ();
    });
}

void main_window::handle_clear_history_request (void)
{
  emit interpreter_event
    ([] (interpreter& interp)
    {
      // INTERPRETER THREAD

      history_system& history_sys = interp.get_history_system ();

      history_sys.do_history (ovl ("-c"));
    });
}

void main_window::handle_undo_request (void)
{
  if (command_window_has_focus ())
    {
      emit interpreter_event
        ([] (void)
        {
          // INTERPRETER THREAD

          command_editor::undo ();
          command_editor::redisplay ();
        });
    }
  else
    emit undo_signal ();
}

void main_window::modify_path (const QStringList& dir_list,
                               bool rm, bool subdirs)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      octave_value_list paths;

      // Loop over all directories in order to get all subdirs
      for (octave_idx_type i = 0; i < dir_list.length (); i++)
        {
          std::string dir = dir_list.at(i).toStdString ();

          if (subdirs)
            paths.append (Fgenpath (ovl (dir)));
          else
            paths.append (dir);
        }

      if (rm)
        Frmpath (interp, paths);
      else
        Faddpath (interp, paths);
    });
}

void main_window::edit_mfile (const QString& name, int line)
{
  handle_edit_mfile_request (name, QString (), QString (), line);
}

void main_window::file_remove_proxy (const QString& o, const QString& n)
{
  interpreter_qobject *interp_qobj = m_octave_qobj.interpreter_qobj ();

  qt_interpreter_events *qt_link = interp_qobj->qt_link ();

  // Wait for worker to suspend
  qt_link->lock ();
  // Close the file if opened
#if defined (HAVE_QSCINTILLA)
  m_editor_window->handle_file_remove (o, n);
#else
  octave_unused_parameter (o);
  octave_unused_parameter (n);
#endif

  // We are done: Unlock and wake the worker thread
  qt_link->unlock ();
  qt_link->wake_all ();
}

void main_window::open_online_documentation_page (void)
{
  QDesktopServices::openUrl
    (QUrl ("https://octave.org/doc/interpreter/index.html"));
}

void main_window::open_bug_tracker_page (void)
{
  QDesktopServices::openUrl (QUrl ("https://octave.org/bugs.html"));
}

void main_window::open_octave_packages_page (void)
{
  QDesktopServices::openUrl (QUrl ("https://packages.octave.org/index.html"));
}

void main_window::open_contribute_page (void)
{
  QDesktopServices::openUrl (QUrl ("https://octave.org/contribute.html"));
}

void main_window::open_donate_page (void)
{
  QDesktopServices::openUrl (QUrl ("https://octave.org/donate.html"));
}

void main_window::process_settings_dialog_request (const QString& desired_tab)
{
  if (m_settings_dlg)  // m_settings_dlg is a guarded pointer!
    {
      // here the dialog is still open and called once again
      if (! desired_tab.isEmpty ())
        m_settings_dlg->show_tab (desired_tab);
      return;
    }

  m_settings_dlg = new settings_dialog (this, m_octave_qobj, desired_tab);

  connect (m_settings_dlg, &settings_dialog::apply_new_settings,
           this, &main_window::request_reload_settings);

  m_settings_dlg->setModal (false);
  m_settings_dlg->setAttribute (Qt::WA_DeleteOnClose);
  m_settings_dlg->show ();
}

void main_window::show_about_octave (void)
{
  std::string message
    = octave_name_version_copyright_copying_warranty_and_bugs (true);

  QMessageBox::about (this, tr ("About Octave"),
                      QString::fromStdString (message));
}

void main_window::notice_settings (const gui_settings *settings,
                                   bool update_by_worker)
{
  if (! settings)
    return;

  // Get desired style from preferences or take the default one if
  // the desired one is not found
  QString preferred_style = settings->value (global_style).toString ();

  if (preferred_style == global_style.def.toString ())
    preferred_style = m_default_style;

  QApplication* qapp = m_octave_qobj.qapplication();

  if (preferred_style == global_extra_styles.at (EXTRA_STYLE_FUSION_DARK))
    {
      QStyle *new_style = QStyleFactory::create (QStringLiteral("Fusion"));
      if (new_style)
        qapp->setStyle (new_style);
      qapp->setPalette (getFusionDarkPalette());
      qapp->setStyleSheet ("QToolTip { color: #ffffff; background-color: #2a82da; border: 1px solid white; }");
    }
  else
    {
      QStyle *new_style = QStyleFactory::create (preferred_style);
      if (new_style)
        {
          qapp->setPalette (m_default_palette);
          qapp->setStyle (new_style);
        }
    }

  // the widget's icons (when floating)
  QString icon_set = settings->value (dw_icon_set).toString ();

  QString icon;
  for (auto *widget : dock_widget_list ())
    {
      QString name = widget->objectName ();
      if (! name.isEmpty ())
        {
          // if child has a name
          icon = dw_icon_set_names[icon_set];
          if (icon_set != "NONE")
            icon += name + ".png"; // add widget name and ext.
          widget->setWindowIcon (QIcon (icon));
        }
    }

  int size_idx = settings->value (global_icon_size).toInt ();
  size_idx = (size_idx > 0) - (size_idx < 0) + 1;  // Make valid index from 0 to 2

  QStyle *st = style ();
  int icon_size = st->pixelMetric (global_icon_sizes[size_idx]);
  m_main_tool_bar->setIconSize (QSize (icon_size, icon_size));

  if (settings->value (global_status_bar).toBool ())
    m_status_bar->show ();
  else
    m_status_bar->hide ();

  m_prevent_readline_conflicts
    = settings->value (sc_prevent_rl_conflicts).toBool ();

  m_prevent_readline_conflicts_menu
    = settings->value (sc_prevent_rl_conflicts_menu).toBool ();

  m_suppress_dbg_location
    = ! settings->value (cs_dbg_location).toBool ();

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  rmgr.update_network_settings ();

  emit active_dock_changed (nullptr, m_active_dock); // update dock widget styles

  configure_shortcuts ();

  bool do_disable_main_menu_shortcuts
    = (m_active_dock == m_editor_window)
    || (m_prevent_readline_conflicts_menu
        && (m_active_dock == m_command_window));

  disable_menu_shortcuts (do_disable_main_menu_shortcuts);

  // Check whether some octave internal preferences have to be updated
  QString new_default_encoding
    = settings->value (ed_default_enc).toString ();
  // Do not update internal pref only if a) this update was not initiated
  // by the worker and b) the pref has really changes
  if (! update_by_worker && (new_default_encoding != m_default_encoding))
    update_default_encoding (new_default_encoding);

  // Set cursor blinking depending on the settings
  // Cursor blinking: consider old terminal related setting if not yet set
  // TODO: This pref. can be deprecated / removed if Qt adds support for
  //       getting the cursor blink preferences from all OS environments
  bool cursor_blinking;

  if (settings->contains (global_cursor_blinking.key))
    cursor_blinking = settings->value (global_cursor_blinking).toBool ();
  else
    cursor_blinking = settings->value (cs_cursor_blinking).toBool ();

  if (cursor_blinking)
    QApplication::setCursorFlashTime (1000);  // 1000 ms flash time
  else
    QApplication::setCursorFlashTime (0);  // no flashing

}

QPalette main_window::getFusionDarkPalette()
{
  QPalette darkPalette;
  darkPalette.setColor(QPalette::Window, QColor(53, 53, 53));
  darkPalette.setColor(QPalette::WindowText, Qt::white);
  darkPalette.setColor(QPalette::Disabled, QPalette::WindowText, QColor(127, 127, 127));
  darkPalette.setColor(QPalette::Base, QColor(42, 42, 42));
  darkPalette.setColor(QPalette::AlternateBase, QColor(66, 66, 66));
  darkPalette.setColor(QPalette::ToolTipBase, Qt::white);
  darkPalette.setColor(QPalette::ToolTipText, Qt::white);
  darkPalette.setColor(QPalette::Text, Qt::white);
  darkPalette.setColor(QPalette::Disabled, QPalette::Text, QColor(127, 127, 127));
  darkPalette.setColor(QPalette::Dark, QColor(35, 35, 35));
  darkPalette.setColor(QPalette::Shadow, QColor(20, 20, 20));
  darkPalette.setColor(QPalette::Button, QColor(53, 53, 53));
  darkPalette.setColor(QPalette::ButtonText, Qt::white);
  darkPalette.setColor(QPalette::Disabled, QPalette::ButtonText, QColor(127, 127, 127));
  darkPalette.setColor(QPalette::BrightText, Qt::red);
  darkPalette.setColor(QPalette::Link, QColor(42, 130, 218));
  darkPalette.setColor(QPalette::Highlight, QColor(42, 130, 218));
  darkPalette.setColor(QPalette::Disabled, QPalette::Highlight, QColor(80, 80, 80));
  darkPalette.setColor(QPalette::HighlightedText, Qt::white);
  darkPalette.setColor(QPalette::Disabled, QPalette::HighlightedText, QColor(127, 127, 127));

  return darkPalette;
}

void main_window::prepare_to_exit (void)
{
  // Find files dialog is constructed dynamically, not at time of main_window
  // construction.  Connecting it to qApp aboutToQuit signal would have
  // caused it to run after gui_settings is deleted.
  if (m_find_files_dlg)
    m_find_files_dlg->save_settings ();

  if (m_set_path_dlg)
    m_set_path_dlg->save_settings ();

  write_settings ();

  // No more active dock, otherwise, focus_changed would try to set
  // the focus to a dock widget that might not exist anymore
  m_active_dock = nullptr;
}

void main_window::go_to_previous_widget (void)
{
  m_previous_dock->activate ();
}

void main_window::update_octave_directory (const QString& dir)
{
  // Remove existing entry, if any, then add new directory at top and
  // mark it as the current directory.  Finally, update the file list
  // widget.

  int index = m_current_directory_combo_box->findText (dir);

  if (index >= 0)
    m_current_directory_combo_box->removeItem (index);

  m_current_directory_combo_box->insertItem (0, dir);
  m_current_directory_combo_box->setCurrentIndex (0);
}

void main_window::browse_for_directory (void)
{
  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  int opts = QFileDialog::ShowDirsOnly;
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    opts = QFileDialog::DontUseNativeDialog;

  QString dir
    = QFileDialog::getExistingDirectory (this, tr ("Browse directories"), nullptr,
                                         QFileDialog::Option (opts));

  set_current_working_directory (dir);

  // FIXME: on Windows systems, the command window freezes after the
  // previous actions.  Forcing the focus appears to unstick it.

  focus_command_window ();
}

void main_window::set_current_working_directory (const QString& dir)
{
  // Change to dir if it is an existing directory.

  QString xdir = (dir.isEmpty () ? "." : dir);

  QFileInfo fileInfo (xdir);

  if (fileInfo.exists () && fileInfo.isDir ())
    {
      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          interp.chdir (xdir.toStdString ());
        });
    }
}

void main_window::change_directory_up (void)
{
  set_current_working_directory ("..");
}

// Slot that is called if return is pressed in the line edit of the
// combobox to change to a new directory or a directory that is already
// in the drop down list.

void main_window::accept_directory_line_edit (void)
{
  // Get new directory name, and change to it if it is new.  Otherwise,
  // the combo box will trigger the "activated" signal to change to the
  // directory.

  QString dir = m_current_directory_combo_box->currentText ();

  int index = m_current_directory_combo_box->findText (dir);

  if (index < 0)
    set_current_working_directory (dir);
}

void main_window::execute_command_in_terminal (const QString& command)
{
  if (m_octave_qobj.experimental_terminal_widget ())
    {
      emit execute_command_signal (command);
    }
  else
    {
      emit interpreter_event
        ([=] (void)
        {
          // INTERPRETER THREAD

          std::string pending_input = command_editor::get_current_line ();

          command_editor::set_initial_input (pending_input);
          command_editor::replace_line (command.toStdString ());
          command_editor::redisplay ();
          command_editor::interrupt_event_loop ();
          command_editor::accept_line ();
        });
    }

  focus_console_after_command ();
}

void main_window::run_file_in_terminal (const QFileInfo& info)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      QString function_name = info.fileName ();
      function_name.chop (info.suffix ().length () + 1);
      std::string file_path = info.absoluteFilePath ().toStdString ();

      std::string pending_input = command_editor::get_current_line ();

      if (valid_identifier (function_name.toStdString ()))
        {
          // Valid identifier: call as function with possibility to
          // debug.

          load_path& lp = interp.get_load_path ();

          std::string path = info.absolutePath ().toStdString ();

          if (lp.contains_file_in_dir (file_path, path))
            command_editor::replace_line (function_name.toStdString ());
        }
      else
        {
          // No valid identifier: use equivalent of Fsource (), no
          // debug possible.

          interp.source_file (file_path);

          command_editor::replace_line ("");
        }

      command_editor::set_initial_input (pending_input);
      command_editor::redisplay ();
      command_editor::interrupt_event_loop ();
      command_editor::accept_line ();
    });

  focus_console_after_command ();
}

void main_window::handle_new_figure_request (void)
{
  emit interpreter_event
    ([] (interpreter& interp)
    {
      // INTERPRETER THREAD

      Fbuiltin (interp, ovl ("figure"));
      Fdrawnow (interp);
    });
}

void main_window::handle_enter_debugger (void)
{
  setWindowTitle ("Octave (Debugging)");

  m_debug_continue->setEnabled (true);
  m_debug_step_into->setEnabled (true);
  m_debug_step_over->setEnabled (true);
  m_debug_step_out->setEnabled (true);
  m_debug_quit->setEnabled (true);
}

void main_window::handle_exit_debugger (void)
{
  setWindowTitle ("Octave");

  m_debug_continue->setEnabled (false);
  m_debug_step_into->setEnabled (false);
  m_debug_step_over->setEnabled (m_editor_has_tabs && m_editor_is_octave_file);
  m_debug_step_out->setEnabled (false);
  m_debug_quit->setEnabled (false);
}

void main_window::debug_continue (void)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      F__db_next_breakpoint_quiet__ (interp, ovl (m_suppress_dbg_location));
      Fdbcont (interp);

      command_editor::interrupt (true);
    });
}

void main_window::debug_step_into (void)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      F__db_next_breakpoint_quiet__ (interp, ovl (m_suppress_dbg_location));
      Fdbstep (interp, ovl ("in"));

      command_editor::interrupt (true);
    });
}

void main_window::debug_step_over (void)
{
  if (m_debug_quit->isEnabled ())
    {
      // We are in debug mode, just call dbstep.

      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          F__db_next_breakpoint_quiet__ (interp,
                                         ovl (m_suppress_dbg_location));
          Fdbstep (interp);

          command_editor::interrupt (true);
        });
    }
  else
    {
      // Not in debug mode: "step into" the current editor file
      emit step_into_file_signal ();
    }
}

void main_window::debug_step_out (void)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      F__db_next_breakpoint_quiet__ (interp, ovl (m_suppress_dbg_location));
      Fdbstep (interp, ovl ("out"));

      command_editor::interrupt (true);
    });
}

void main_window::debug_quit (void)
{
  emit interpreter_event
    ([] (interpreter& interp)
    {
      // INTERPRETER THREAD

      Fdbquit (interp);

      command_editor::interrupt (true);
    });
}

//
// Functions related to file editing
//
// These are moved from editor to here for also using them when octave
// is built without qscintilla
//
void main_window::request_open_file (void)
{
  // Open file isn't a file_editor_tab or editor function since the file
  // might be opened in an external editor.  Hence, functionality is here.

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  bool is_internal = m_editor_window
    && ! settings->value (global_use_custom_editor.key,
                          global_use_custom_editor.def).toBool ();

  // Create a NonModal message.
  QWidget *p = this;
  if (is_internal)
    p = m_editor_window;
  QFileDialog *fileDialog = new QFileDialog (p);
  fileDialog->setNameFilter (tr ("Octave Files (*.m);;All Files (*)"));

  fileDialog->setAcceptMode (QFileDialog::AcceptOpen);
  fileDialog->setViewMode (QFileDialog::Detail);
  fileDialog->setFileMode (QFileDialog::ExistingFiles);
  fileDialog->setDirectory (m_current_directory_combo_box->itemText (0));

  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  if (! settings->value (global_use_native_dialogs).toBool ())
    fileDialog->setOption(QFileDialog::DontUseNativeDialog);

  connect (fileDialog, &QFileDialog::filesSelected,
           this, &main_window::request_open_files);

  fileDialog->setWindowModality (Qt::NonModal);
  fileDialog->setAttribute (Qt::WA_DeleteOnClose);
  fileDialog->show ();
}

// Create a new script
void main_window::request_new_script (const QString& commands)
{
  emit new_file_signal (commands);
}

// Create a new function and open it
void main_window::request_new_function (bool)
{
  bool ok;
  // Get the name of the new function: Parent of the input dialog is the
  // editor window or the main window.  The latter is chosen, if a custom
  // editor is used or qscintilla is not available
  QWidget *p = m_editor_window;
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! p || settings->value (global_use_custom_editor.key,
                              global_use_custom_editor.def).toBool ())
    p = this;
  QString new_name = QInputDialog::getText (p, tr ("New Function"),
                                            tr ("New function name:\n"), QLineEdit::Normal, "", &ok);

  if (ok && new_name.length () > 0)
    {
      // append suffix if it does not already exist
      if (new_name.rightRef (2) != ".m")
        new_name.append (".m");
      // check whether new files are created without prompt
      if (! settings->value (ed_create_new_file).toBool ())
        {
          // no, so enable this settings and wait for end of new file loading
          settings->setValue (ed_create_new_file.key, true);
          connect (m_editor_window, SIGNAL (file_loaded_signal (void)),
                   this, SLOT (restore_create_file_setting (void)));
        }
      // start the edit command
      execute_command_in_terminal ("edit " + new_name);
    }
}

void main_window::handle_edit_mfile_request (const QString& fname,
                                             const QString& ffile,
                                             const QString& curr_dir,
                                             int line)
{
  // The interpreter_event callback function below emits a signal.
  // Because we don't control when that happens, use a guarded pointer
  // so that the callback can abort if this object is no longer valid.

  QPointer<main_window> this_mw (this);

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // We can skip the entire callback function because it does not
      // make any changes to the interpreter state.

      if (this_mw.isNull ())
        return;

      // Split possible subfunctions
      QStringList fcn_list = fname.split ('>');
      QString fcn_name = fcn_list.at (0) + ".m";

      // FIXME: could use symbol_exist directly, but we may also want
      // to fix that to be a member function in the interpreter
      // class?

      // Is it a regular function within the search path? (Call Fexist)
      octave_value_list fct = Fexist (interp, ovl (fname.toStdString ()),0);
      int type = fct (0).int_value ();

      QString message = QString ();
      QString filename = QString ();

      switch (type)
        {
        case 3:
        case 5:
        case 103:
          message = tr ("%1 is a built-in, compiled or inline\n"
                        "function and can not be edited.");
          break;

        case 2:
          // FIXME: could use a load_path function directly.
          octave_value_list file_path
            = Ffile_in_loadpath (interp, ovl (fcn_name.toStdString ()), 0);
          if (file_path.length () > 0)
            filename = QString::fromStdString (file_path (0).string_value ());
          break;
        }

      if (filename.isEmpty () && message.isEmpty ())
        {
          // No error so far, but function still not known
          // -> try directory of edited file
          // get directory
          QDir dir;
          if (ffile.isEmpty ())
            {
              if (curr_dir.isEmpty ())
                dir = QDir (m_current_directory_combo_box->itemText (0));
              else
                dir = QDir (curr_dir);
            }
          else
            dir = QDir (QFileInfo (ffile).canonicalPath ());

          QFileInfo file = QFileInfo (dir, fcn_name);
          if (file.exists ())
            filename = file.canonicalFilePath (); // local file exists
          else
            {
              // local file does not exist -> try private directory
              file = QFileInfo (ffile);
              file = QFileInfo (QDir (file.canonicalPath () + "/private"),
                                fcn_name);
              if (file.exists ())
                filename = file.canonicalFilePath ();  // private function exists
              else
                message = tr ("Can not find function %1");  // no file found
            }
        }

      if (! message.isEmpty ())
        {
          emit warning_function_not_found_signal (message.arg (fname));
          return;
        }

      if (! filename.endsWith (".m"))
        filename.append (".m");

      // default encoding
      emit open_file_signal (filename, QString (), line);
    });
}

void main_window::warning_function_not_found (const QString& message)
{
  QMessageBox *msgBox = new QMessageBox (QMessageBox::Critical,
                                         tr ("Octave Editor"),
                                         message, QMessageBox::Ok, this);
  msgBox->setWindowModality (Qt::NonModal);
  msgBox->setAttribute (Qt::WA_DeleteOnClose);
  msgBox->show ();
}

void main_window::handle_insert_debugger_pointer_request (const QString& file,
                                                          int line)
{
  bool cmd_focus = command_window_has_focus ();

  emit insert_debugger_pointer_signal (file, line);

  if (cmd_focus)
    focus_command_window ();
}

void main_window::handle_delete_debugger_pointer_request (const QString& file,
                                                          int line)
{
  bool cmd_focus = command_window_has_focus ();

  emit delete_debugger_pointer_signal (file, line);

  if (cmd_focus)
    focus_command_window ();
}

void main_window::handle_update_breakpoint_marker_request (bool insert,
                                                           const QString& file,
                                                           int line,
                                                           const QString& cond)
{
  bool cmd_focus = command_window_has_focus ();

  emit update_breakpoint_marker_signal (insert, file, line, cond);

  if (cmd_focus)
    focus_command_window ();
}

void main_window::read_settings (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (! settings)
    {
      qDebug ("Error: gui_settings pointer from resource manager is NULL.");
      return;
    }

  set_window_layout (settings);

  // restore the list of the last directories
  QStringList curr_dirs = settings->value (mw_dir_list).toStringList ();
  for (int i=0; i < curr_dirs.size (); i++)
    {
      m_current_directory_combo_box->addItem (curr_dirs.at (i));
    }
  emit settings_changed (settings);
}

void main_window::init_terminal_size (void)
{
  emit init_terminal_size_signal ();
}

void main_window::set_window_layout (gui_settings *settings)
{
  // For resetting from some inconsistent state, first reset layout
  // without saving or showing it
  do_reset_windows (true, false);

  // Restore main window state and geometry from settings file or, in case
  // of an error (no pref values yet), from the default layout.
  if (! restoreGeometry (settings->value (mw_geometry).toByteArray ()))
    {
      do_reset_windows (true);
      return;
    }

  if (isMaximized())
    {
      // If the window state is restored to maximized layout, the
      // horizontal layout is not preserved. This cann be avoided by
      // setting the geometry to the max. available geometry. However, on
      // X11, the available geometry (excluding task bar etc.) is equal to
      // the total geometry leading to a full screen mode without window
      // decorations. This in turn can be avoided by explicitly adding
      // a title bar in the window flags.

      // Get available geometry for current screen and set this
      // window's geometry to it.
      QScreen *s = windowHandle ()->screen ();
      QRect av_geom = s->availableGeometry ();
      setGeometry (av_geom);  // Set (correct) available geometry

      // Force full title bar
      setWindowFlags(Qt::WindowTitleHint
                     | Qt::WindowMinMaxButtonsHint
                     | Qt::WindowSystemMenuHint
                     | Qt::WindowCloseButtonHint);
    }

  if (! restoreState (settings->value (mw_state).toByteArray ()))
    {
      do_reset_windows (true);
      return;
    }

  // Restore the geometry of all dock-widgets

  for (auto *widget : dock_widget_list ())
    {
      // Leave any widgets that existed before main_window was created
      // as they were.

      if (widget->adopted ())
        continue;

      QString name = widget->objectName ();

      if (! name.isEmpty ())
        {
          bool floating = false;
          bool visible = true;

          floating = settings->value
            (dw_is_floating.key.arg (name), dw_is_floating.def).toBool ();
          visible = settings->value
            (dw_is_visible.key.arg (name), dw_is_visible.def).toBool ();

          // If floating, make window from widget.
          if (floating)
            {
              widget->make_window ();

              if (visible)
                {
                  if (settings->value (dw_is_minimized.key.arg (name),
                                       dw_is_minimized.def).toBool ())
                    widget->showMinimized ();
                  else
                    widget->setVisible (true);
                }
              else
                widget->setVisible (false);
            }
          else  // not floating
            {
              if (! widget->parent ())        // should not be floating but is
                widget->make_widget (false);  // no docking, just reparent

              widget->make_widget ();
              widget->setVisible (visible);   // not floating -> show
            }
        }
    }

  show ();
}

void main_window::write_settings (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings)
    {
      qDebug ("Error: gui_settings pointer from resource manager is NULL.");
      return;
    }

  settings->setValue (mw_geometry.key, saveGeometry ());
  settings->setValue (mw_state.key, saveState ());
  // write the list of recently used directories
  QStringList curr_dirs;
  for (int i=0; i<m_current_directory_combo_box->count (); i++)
    {
      curr_dirs.append (m_current_directory_combo_box->itemText (i));
    }
  settings->setValue (mw_dir_list.key, curr_dirs);
  settings->sync ();
}

void main_window::copyClipboard (void)
{
  if (m_current_directory_combo_box->hasFocus ())
    {
      QLineEdit *edit = m_current_directory_combo_box->lineEdit ();
      if (edit && edit->hasSelectedText ())
        {
          QClipboard *clipboard = QApplication::clipboard ();
          clipboard->setText (edit->selectedText ());
        }
    }
  else
    emit copyClipboard_signal ();
}

void main_window::pasteClipboard (void)
{
  if (m_current_directory_combo_box->hasFocus ())
    {
      QLineEdit *edit = m_current_directory_combo_box->lineEdit ();
      QClipboard *clipboard = QApplication::clipboard ();
      QString str = clipboard->text ();
      if (edit && str.length () > 0)
        {
          edit->insert (str);
        }
    }
  else
    emit pasteClipboard_signal ();
}

void main_window::selectAll (void)
{
  if (m_current_directory_combo_box->hasFocus ())
    {
      QLineEdit *edit = m_current_directory_combo_box->lineEdit ();
      if (edit)
        {
          edit->selectAll ();
        }
    }
  else
    emit selectAll_signal ();
}

void main_window::handle_gui_status_update (const QString& feature,
                                            const QString& status)
{
  // Put actions that are required for updating a gui features here

  // Profiler on/off
  if (! feature.compare ("profiler"))
    {
      if (! status.compare ("on", Qt::CaseInsensitive))
        handle_profiler_status_update (true);
      else if (! status.compare ("off", Qt::CaseInsensitive))
        handle_profiler_status_update (false);
    }
}

void main_window::handle_octave_ready (void)
{
  // actions after the startup files are executed
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  QDir startup_dir = QDir ();    // current octave dir after startup

  if (settings)
    {
      if (settings->value (global_restore_ov_dir).toBool ())
        {
          // restore last dir from previous session
          QStringList curr_dirs
            = settings->value (mw_dir_list).toStringList ();
          if (curr_dirs.length () > 0)
            startup_dir = QDir (curr_dirs.at (0));  // last dir prev. session
        }
      else if (! settings->value (global_ov_startup_dir).toString ().isEmpty ())
        {
          // do not restore but there is a startup dir configured
          startup_dir
            = QDir (settings->value (global_ov_startup_dir).toString ());
        }

      update_default_encoding (settings->value (ed_default_enc).toString ());
    }

  if (! startup_dir.exists ())
    {
      // the configured startup dir does not exist, take actual one
      startup_dir = QDir ();
    }

  set_current_working_directory (startup_dir.absolutePath ());

  if (m_editor_window)
    {
#if defined (HAVE_QSCINTILLA)
      // Octave ready, determine whether to create an empty script.
      // This can not be done when the editor is created because all functions
      // must be known for the lexer's auto completion information
      m_editor_window->empty_script (true, false);
      m_editor_window->restore_session (settings);
#endif
    }

  if (m_octave_qobj.experimental_terminal_widget ())
    {
      // Set initial prompt.

      emit interpreter_event
        ([] (interpreter& interp)
        {
          // INTERPRETER_THREAD

          event_manager& evmgr = interp.get_event_manager ();
          input_system& input_sys = interp.get_input_system ();

          input_sys.PS1 (">> ");
          std::string prompt = input_sys.PS1 ();

          evmgr.update_prompt (command_editor::decode_prompt_string (prompt));
        });
    }

  m_command_window->init_command_prompt ();
  focus_command_window ();  // make sure that the command window has focus
}

void main_window::handle_set_path_dialog_request (void)
{
  if (m_set_path_dlg)  // m_set_path_dlg is a guarded pointer!
    return;

  m_set_path_dlg = new set_path_dialog (this, m_octave_qobj);

  m_set_path_dlg->setModal (false);
  m_set_path_dlg->setAttribute (Qt::WA_DeleteOnClose);
  m_set_path_dlg->show ();

  // Any interpreter_event signal from a set_path_dialog object is
  // handled the same as for the main_window object.

  connect (m_set_path_dlg, QOverload<const fcn_callback&>::of (&set_path_dialog::interpreter_event),
           this, QOverload<const fcn_callback&>::of (&main_window::interpreter_event));

  connect (m_set_path_dlg, QOverload<const meth_callback&>::of (&set_path_dialog::interpreter_event),
           this, QOverload<const meth_callback&>::of (&main_window::interpreter_event));

  connect (m_set_path_dlg, &set_path_dialog::modify_path_signal,
           this, &main_window::modify_path);

  interpreter_qobject *interp_qobj = m_octave_qobj.interpreter_qobj ();

  qt_interpreter_events *qt_link = interp_qobj->qt_link ();

  connect (qt_link, &qt_interpreter_events::update_path_dialog_signal,
           m_set_path_dlg, &set_path_dialog::update_model);

  // Now that all the signal connections are in place for the dialog
  // we can set the initial value of the path in the model.

  m_set_path_dlg->update_model ();
}

void main_window::find_files (const QString& start_dir)
{

  if (! m_find_files_dlg)
    {
      m_find_files_dlg = new find_files_dialog (this, m_octave_qobj);

      connect (m_find_files_dlg, &find_files_dialog::finished,
               this, &main_window::find_files_finished);

      connect (m_find_files_dlg, &find_files_dialog::dir_selected,
               m_file_browser_window, &files_dock_widget::set_current_directory);

      connect (m_find_files_dlg, &find_files_dialog::file_selected,
               this, QOverload<const QString&>::of (&main_window::open_file_signal));

      m_find_files_dlg->setWindowModality (Qt::NonModal);
    }

  if (! m_find_files_dlg->isVisible ())
    {
      m_find_files_dlg->show ();
    }

  m_find_files_dlg->set_search_dir (start_dir);

  m_find_files_dlg->activateWindow ();

}

void main_window::set_screen_size (int ht, int wd)
{
  emit interpreter_event
    ([=] (void)
    {
      // INTERPRETER THREAD

      command_editor::set_screen_size (ht, wd);
    });
}

void main_window::clipboard_has_changed (void)
{
  if (m_clipboard->text ().isEmpty ())
    {
      m_paste_action->setEnabled (false);
      m_clear_clipboard_action->setEnabled (false);
    }
  else
    {
      m_paste_action->setEnabled (true);
      m_clear_clipboard_action->setEnabled (true);
    }
}

void main_window::clear_clipboard (void)
{
  m_clipboard->clear (QClipboard::Clipboard);
}

void main_window::disable_menu_shortcuts (bool disable)
{
  QHash<QMenu *, QStringList>::const_iterator i = m_hash_menu_text.constBegin ();

  while (i != m_hash_menu_text.constEnd ())
    {
      i.key ()->setTitle (i.value ().at (disable));
      ++i;
    }
}

void main_window::restore_create_file_setting (void)
{
  // restore the new files creation setting
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  settings->setValue (ed_create_new_file.key, false);
  disconnect (m_editor_window, SIGNAL (file_loaded_signal (void)),
              this, SLOT (restore_create_file_setting (void)));
}

void main_window::set_file_encoding (const QString& new_encoding)
{
  m_file_encoding = new_encoding;
}

// The following slot is called after files have been selected in the
// open file dialog, possibly with a new selected encoding stored in
// m_file_encoding
void main_window::request_open_files (const QStringList& open_file_names)
{
  for (int i = 0; i < open_file_names.count (); i++)
    emit open_file_signal (open_file_names.at (i), m_file_encoding, -1);
}

void main_window::profiler_session (void)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      Ffeval (interp, ovl ("profile","on"));
    });
}

void main_window::profiler_session_resume (void)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      Ffeval (interp, ovl ("profile","resume"));
    });
}

void main_window::profiler_stop (void)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      Ffeval (interp, ovl ("profile","off"));
    });
}

void main_window::handle_profiler_status_update (bool active)
{
  m_profiler_start->setEnabled (! active);
  m_profiler_resume->setEnabled (! active);
  m_profiler_stop->setEnabled (active);

  led_indicator::led_state state = led_indicator::LED_STATE_INACTIVE;
  if (active)
    state = led_indicator::LED_STATE_ACTIVE;
  m_profiler_status_indicator->set_state (state);
}

void main_window::profiler_show (void)
{
  // Do not use a separate interpreter event as in the other
  // profiler slots since the output of the command "profshow"
  // would obscure the prompt and we do not need to emimt a signal
  // for action that is required in the gui after rhe command
  execute_command_in_terminal ("profshow");
}

void main_window::closeEvent (QCloseEvent *e)
{
  write_settings ();

  if (confirm_shutdown ())
    {
      // FIXME: Instead of ignoring the event and posting an
      // interpreter event, should we just accept the event and
      // shutdown and clean up the interpreter as part of closing the
      // GUI?  Going that route might make it easier to close the GUI
      // without having to stop the interpreter, for example, if the
      // GUI is started from the interpreter command line.

      e->ignore ();

      if (m_octave_qobj.experimental_terminal_widget ()
          && ! m_octave_qobj.is_gui_app ())
        emit close_gui_signal ();
      else
        {
          emit interpreter_event
            ([] (interpreter& interp)
            {
              // INTERPRETER THREAD

              interp.quit (0, false, false);
            });
        }
    }
  else
    e->ignore ();
}

void main_window::construct_central_widget (void)
{
  // Create and set the central widget.  QMainWindow takes ownership of
  // the widget (pointer) so there is no need to delete the object upon
  // destroying this main_window.

  QWidget *dummyWidget = new QWidget ();
  dummyWidget->setObjectName ("CentralDummyWidget");
  dummyWidget->resize (10, 10);
  dummyWidget->setSizePolicy (QSizePolicy::Fixed, QSizePolicy::Fixed);
  dummyWidget->hide ();
  setCentralWidget (dummyWidget);
}

// Main subroutine of the constructor

void main_window::construct (void)
{
  setWindowIcon (QIcon (dw_icon_set_names["NONE"]));

  interpreter_qobject *interp_qobj = m_octave_qobj.interpreter_qobj ();

  qt_interpreter_events *qt_link = interp_qobj->qt_link ();

  construct_menu_bar ();

  construct_tool_bar ();

  // FIXME: Is this action intended to be about quitting application
  // or closing the main window?
  connect (qApp, &QApplication::aboutToQuit,
           this, &main_window::prepare_to_exit);

  connect (qApp, &QApplication::focusChanged,
           this, &main_window::focus_changed);

  connect (this, &main_window::settings_changed,
           this, [=] (const gui_settings *settings) { notice_settings (settings); });

  // Connections for signals from the interpreter thread where the slot
  // should be executed by the gui thread

  connect (this, &main_window::warning_function_not_found_signal,
           this, &main_window::warning_function_not_found);

  setWindowTitle ("Octave");

  setStatusBar (m_status_bar);

  // Signals for removing/renaming files/dirs in the temrinal window
  connect (qt_link, &qt_interpreter_events::file_remove_signal,
           this, &main_window::file_remove_proxy);

  connect (this, QOverload<const fcn_callback&>::of (&main_window::interpreter_event),
           &m_octave_qobj, QOverload<const fcn_callback&>::of (&base_qobject::interpreter_event));

  connect (this, QOverload<const meth_callback&>::of (&main_window::interpreter_event),
           &m_octave_qobj, QOverload<const meth_callback&>::of (&base_qobject::interpreter_event));

  configure_shortcuts ();
}

void main_window::construct_octave_qt_link (void)
{
  interpreter_qobject *interp_qobj = m_octave_qobj.interpreter_qobj ();

  qt_interpreter_events *qt_link = interp_qobj->qt_link ();

  connect (qt_link, &qt_interpreter_events::settings_changed,
           this, &main_window::notice_settings);

  connect (qt_link, &qt_interpreter_events::apply_new_settings,
           this, &main_window::request_reload_settings);

  connect (qt_link, &qt_interpreter_events::directory_changed_signal,
           this, &main_window::update_octave_directory);

  connect (qt_link, &qt_interpreter_events::execute_command_in_terminal_signal,
           this, &main_window::execute_command_in_terminal);

  connect (qt_link, &qt_interpreter_events::enter_debugger_signal,
           this, &main_window::handle_enter_debugger);

  connect (qt_link, &qt_interpreter_events::exit_debugger_signal,
           this, &main_window::handle_exit_debugger);

  connect (qt_link, &qt_interpreter_events::show_preferences_signal,
           this, [=] () { process_settings_dialog_request (); });

  connect (qt_link, &qt_interpreter_events::insert_debugger_pointer_signal,
           this, &main_window::handle_insert_debugger_pointer_request);

  connect (qt_link, &qt_interpreter_events::delete_debugger_pointer_signal,
           this, &main_window::handle_delete_debugger_pointer_request);

  connect (qt_link, &qt_interpreter_events::update_breakpoint_marker_signal,
           this, &main_window::handle_update_breakpoint_marker_request);

  connect (qt_link, &qt_interpreter_events::gui_status_update_signal,
           this, &main_window::handle_gui_status_update);

  connect (qt_link, &qt_interpreter_events::update_gui_lexer_signal,
           this, &main_window::update_gui_lexer_signal);
}

QAction* main_window::add_action (QMenu *menu, const QIcon& icon,
                                  const QString& text, const char *member,
                                  const QWidget *receiver)
{
  QAction *a;

  if (receiver)
    a = menu->addAction (icon, text, receiver, member);
  else
    a = menu->addAction (icon, text, this, member);

  addAction (a);  // important for shortcut context
  a->setShortcutContext (Qt::ApplicationShortcut);
  return a;
}

QMenu* main_window::m_add_menu (QMenuBar *p, QString name)
{
  QMenu *menu = p->addMenu (name);

  QString base_name = name;  // get a copy
  // replace intended '&' ("&&") by a temp. string
  base_name.replace ("&&", "___octave_amp_replacement___");
  // remove single '&' (shortcut)
  base_name.remove ("&");
  // restore intended '&'
  base_name.replace ("___octave_amp_replacement___", "&&");

  // remember names with and without shortcut
  m_hash_menu_text[menu] = QStringList ({ name, base_name });

  return menu;
}

void main_window::construct_menu_bar (void)
{
  QMenuBar *menu_bar = menuBar ();

  construct_file_menu (menu_bar);

  construct_edit_menu (menu_bar);

  construct_debug_menu (menu_bar);

  construct_tools_menu (menu_bar);

  construct_window_menu (menu_bar);

  construct_help_menu (menu_bar);

  construct_news_menu (menu_bar);

#if defined (HAVE_QSCINTILLA)
  // call the editor to add actions which should also be available in the
  // editor's menu and tool bar
  QList<QAction *> shared_actions = {
    m_new_script_action,
    m_new_function_action,
    m_open_action,
    m_find_files_action,
    m_undo_action,
    m_copy_action,
    m_paste_action,
    m_select_all_action
  };
  m_editor_window->insert_global_actions (shared_actions);
#endif
}

void main_window::construct_file_menu (QMenuBar *p)
{
  QMenu *file_menu = m_add_menu (p, tr ("&File"));

  construct_new_menu (file_menu);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  m_open_action = add_action (
                              file_menu, rmgr.icon ("document-open"), tr ("Open..."),
                              SLOT (request_open_file (void)), this);
  m_open_action->setToolTip (tr ("Open an existing file in editor"));

#if defined (HAVE_QSCINTILLA)
  file_menu->addMenu (m_editor_window->get_mru_menu ());
#endif

  file_menu->addSeparator ();

  m_load_workspace_action = add_action (
                                        file_menu, QIcon (), tr ("Load Workspace..."),
                                        SLOT (handle_load_workspace_request (void)), this);

  m_save_workspace_action = add_action (
                                        file_menu, QIcon (), tr ("Save Workspace As..."),
                                        SLOT (handle_save_workspace_request (void)), this);

  file_menu->addSeparator ();

  m_exit_action = add_action (
                              file_menu, QIcon (), tr ("Exit"),
                              SLOT (close (void)), this);
  m_exit_action->setMenuRole (QAction::QuitRole);

  // Connect signal related to opening or creating editor files
  connect (this, SIGNAL (new_file_signal (const QString&)),
           m_active_editor, SLOT (request_new_file (const QString&)));

  connect (this, SIGNAL (open_file_signal (const QString&)),
           m_active_editor, SLOT (request_open_file (const QString&)));

  connect (this,
           SIGNAL (open_file_signal (const QString&, const QString&, int)),
           m_active_editor,
           SLOT (request_open_file (const QString&, const QString&, int)));
}

void main_window::construct_new_menu (QMenu *p)
{
  QMenu *new_menu = p->addMenu (tr ("New"));

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  m_new_script_action = add_action (
                                    new_menu, rmgr.icon ("document-new"), tr ("New Script"),
                                    SLOT (request_new_script (void)), this);

  m_new_function_action = add_action (
                                      new_menu, QIcon (), tr ("New Function..."),
                                      SLOT (request_new_function (void)), this);

  m_new_figure_action = add_action (
                                    new_menu, QIcon (), tr ("New Figure"),
                                    SLOT (handle_new_figure_request (void)), this);
}

void main_window::construct_edit_menu (QMenuBar *p)
{
  QMenu *edit_menu = m_add_menu (p, tr ("&Edit"));

  QKeySequence ctrl_shift = Qt::ControlModifier + Qt::ShiftModifier;

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  m_undo_action
    = edit_menu->addAction (rmgr.icon ("edit-undo"), tr ("Undo"));
  m_undo_action->setShortcutContext (Qt::ApplicationShortcut);

  edit_menu->addSeparator ();

  m_copy_action
    = edit_menu->addAction (rmgr.icon ("edit-copy"), tr ("Copy"), this,
                            &main_window::copyClipboard);
  m_copy_action->setShortcutContext (Qt::ApplicationShortcut);

  m_paste_action
    = edit_menu->addAction (rmgr.icon ("edit-paste"), tr ("Paste"), this,
                            &main_window::pasteClipboard);
  m_paste_action->setShortcutContext (Qt::ApplicationShortcut);

  m_select_all_action
    = edit_menu->addAction (tr ("Select All"), this,
                            &main_window::selectAll);
  m_select_all_action->setShortcutContext (Qt::ApplicationShortcut);

  m_clear_clipboard_action
    = edit_menu->addAction (tr ("Clear Clipboard"), this,
                            &main_window::clear_clipboard);

  edit_menu->addSeparator ();

  m_find_files_action
    = edit_menu->addAction (rmgr.icon ("edit-find"), tr ("Find Files..."));

  edit_menu->addSeparator ();

  m_clear_command_window_action
    = edit_menu->addAction (tr ("Clear Command Window"));

  m_clear_command_history_action
    = edit_menu->addAction (tr ("Clear Command History"));

  m_clear_workspace_action
    = edit_menu->addAction (tr ("Clear Workspace"));

  edit_menu->addSeparator ();

  m_set_path_action
    = edit_menu->addAction (tr ("Set Path"));

  m_preferences_action
    = edit_menu->addAction (rmgr.icon ("preferences-system"),
                            tr ("Preferences..."));

  connect (m_find_files_action, &QAction::triggered,
           this, [=] () { find_files (); });

  connect (m_clear_command_window_action, &QAction::triggered,
           this, &main_window::handle_clear_command_window_request);

  connect (m_clear_command_history_action, &QAction::triggered,
           this, &main_window::handle_clear_history_request);

  connect (m_clear_workspace_action, &QAction::triggered,
           this, &main_window::handle_clear_workspace_request);

  connect (m_clipboard, &QClipboard::dataChanged,
           this, &main_window::clipboard_has_changed);
  clipboard_has_changed ();
#if defined (Q_OS_WIN32)
  // Always enable paste action (unreliable clipboard signals in windows)
  // FIXME: This has to be removed, when the clipboard signals in windows
  //        are working again
  m_paste_action->setEnabled (true);
  m_clear_clipboard_action->setEnabled (true);
#endif

  connect (m_preferences_action, &QAction::triggered,
           this, [=] () { process_settings_dialog_request (); });

  connect (m_set_path_action, &QAction::triggered,
           this, &main_window::handle_set_path_dialog_request);

}

QAction * main_window::construct_debug_menu_item (const char *icon,
                                                  const QString& item,
                                                  const char *member)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  QAction *action = add_action (m_debug_menu, rmgr.icon (QString (icon)),
                                item, member);

  action->setEnabled (false);

#if defined (HAVE_QSCINTILLA)
  m_editor_window->debug_menu ()->addAction (action);
  m_editor_window->toolbar ()->addAction (action);
#endif

  return action;
}

void main_window::construct_debug_menu (QMenuBar *p)
{
  m_debug_menu = m_add_menu (p, tr ("De&bug"));

  m_debug_step_over
    = construct_debug_menu_item ("db-step", tr ("Step"),
                                 SLOT (debug_step_over (void)));

  m_debug_step_into
    = construct_debug_menu_item ("db-step-in", tr ("Step In"),
                                 SLOT (debug_step_into (void)));

  m_debug_step_out
    = construct_debug_menu_item ("db-step-out", tr ("Step Out"),
                                 SLOT (debug_step_out (void)));

  m_debug_continue
    = construct_debug_menu_item ("db-cont", tr ("Continue"),
                                 SLOT (debug_continue (void)));

  m_debug_menu->addSeparator ();
#if defined (HAVE_QSCINTILLA)
  m_editor_window->debug_menu ()->addSeparator ();
#endif

  m_debug_quit
    = construct_debug_menu_item ("db-stop", tr ("Quit Debug Mode"),
                                 SLOT (debug_quit (void)));
}

void main_window::construct_tools_menu (QMenuBar *p)
{
  QMenu *tools_menu = m_add_menu (p, tr ("&Tools"));

  m_profiler_start = add_action (tools_menu, QIcon (),
                                 tr ("Start &Profiler Session"), SLOT (profiler_session ()));

  m_profiler_resume = add_action (tools_menu, QIcon (),
                                  tr ("&Resume Profiler Session"), SLOT (profiler_session_resume ()));

  m_profiler_stop = add_action (tools_menu, QIcon (),
                                tr ("&Stop Profiler"), SLOT (profiler_stop ()));
  m_profiler_stop->setEnabled (false);

  m_profiler_show = add_action (tools_menu, QIcon (),
                                tr ("&Show Profile Data"), SLOT (profiler_show ()));
}

void main_window::editor_tabs_changed (bool have_tabs, bool is_octave)
{
  // Set state of actions which depend on the existence of editor tabs
  m_editor_has_tabs = have_tabs;
  m_editor_is_octave_file = is_octave;
  m_debug_step_over->setEnabled (have_tabs && is_octave);
}

QAction * main_window::construct_window_menu_item (QMenu *p,
                                                   const QString& item,
                                                   bool checkable,
                                                   QWidget *widget)
{
  QAction *action = p->addAction (QIcon (), item);

  addAction (action);  // important for shortcut context
  action->setCheckable (checkable);
  action->setShortcutContext (Qt::ApplicationShortcut);

  if (widget)  // might be zero for m_editor_window
    {
      if (checkable)
        {
          // action for visibility of dock widget
          connect (action, SIGNAL (toggled (bool)),
                   widget, SLOT (setVisible (bool)));

          connect (widget, SIGNAL (active_changed (bool)),
                   action, SLOT (setChecked (bool)));
        }
      else
        {
          // action for focus of dock widget
          connect (action, SIGNAL (triggered (void)),
                   widget, SLOT (activate (void)));
        }
    }
  else
    {
      action->setEnabled (false);
    }

  return action;
}

void main_window::construct_window_menu (QMenuBar *p)
{
  QMenu *window_menu = m_add_menu (p, tr ("&Window"));

  m_show_command_window_action = construct_window_menu_item
    (window_menu, tr ("Show Command Window"), true, m_command_window);

  m_show_history_action = construct_window_menu_item
    (window_menu, tr ("Show Command History"), true, m_history_window);

  m_show_file_browser_action = construct_window_menu_item
    (window_menu, tr ("Show File Browser"), true, m_file_browser_window);

  m_show_workspace_action = construct_window_menu_item
    (window_menu, tr ("Show Workspace"), true, m_workspace_window);

  m_show_editor_action = construct_window_menu_item
    (window_menu, tr ("Show Editor"), true, m_editor_window);

  m_show_documentation_action = construct_window_menu_item
    (window_menu, tr ("Show Documentation"), true, m_doc_browser_window);

  m_show_variable_editor_action = construct_window_menu_item
    (window_menu, tr ("Show Variable Editor"), true, m_variable_editor_window);

  window_menu->addSeparator ();

  m_command_window_action = construct_window_menu_item
    (window_menu, tr ("Command Window"), false, m_command_window);

  m_history_action = construct_window_menu_item
    (window_menu, tr ("Command History"), false, m_history_window);

  m_file_browser_action = construct_window_menu_item
    (window_menu, tr ("File Browser"), false, m_file_browser_window);

  m_workspace_action = construct_window_menu_item
    (window_menu, tr ("Workspace"), false, m_workspace_window);

  m_editor_action = construct_window_menu_item
    (window_menu, tr ("Editor"), false, m_editor_window);

  m_documentation_action = construct_window_menu_item
    (window_menu, tr ("Documentation"), false, m_doc_browser_window);

  m_variable_editor_action = construct_window_menu_item
    (window_menu, tr ("Variable Editor"), false, m_variable_editor_window);

  window_menu->addSeparator ();

  m_previous_dock_action = add_action (window_menu, QIcon (),
                                       tr ("Previous Widget"), SLOT (go_to_previous_widget (void)));

  window_menu->addSeparator ();

  m_reset_windows_action = add_action (window_menu, QIcon (),
                                       tr ("Reset Default Window Layout"), SLOT (reset_windows (void)));
}

void main_window::construct_help_menu (QMenuBar *p)
{
  QMenu *help_menu = m_add_menu (p, tr ("&Help"));

  construct_documentation_menu (help_menu);

  help_menu->addSeparator ();

  m_report_bug_action = add_action (help_menu, QIcon (),
                                    tr ("Report Bug"), SLOT (open_bug_tracker_page ()));

  m_octave_packages_action = add_action (help_menu, QIcon (),
                                         tr ("Octave Packages"), SLOT (open_octave_packages_page ()));

  m_contribute_action = add_action (help_menu, QIcon (),
                                    tr ("Contribute"), SLOT (open_contribute_page ()));

  m_developer_action = add_action (help_menu, QIcon (),
                                   tr ("Donate to Octave"), SLOT (open_donate_page ()));

  help_menu->addSeparator ();

  m_about_octave_action = add_action (help_menu, QIcon (),
                                      tr ("About Octave"), SLOT (show_about_octave ()));
}

void main_window::construct_documentation_menu (QMenu *p)
{
  QMenu *doc_menu = p->addMenu (tr ("Documentation"));

  m_ondisk_doc_action = add_action (doc_menu, QIcon (),
                                    tr ("On Disk"), SLOT (activate ()), m_doc_browser_window);

  m_online_doc_action = add_action (doc_menu, QIcon (),
                                    tr ("Online"), SLOT (open_online_documentation_page ()));
}

void main_window::construct_news_menu (QMenuBar *p)
{
  QMenu *news_menu = m_add_menu (p, tr ("&News"));

  m_release_notes_action
    = news_menu->addAction (QIcon (), tr ("Release Notes"),
                            [=] () {
                              emit show_release_notes_signal ();
                            });
  addAction (m_release_notes_action);
  m_release_notes_action->setShortcutContext (Qt::ApplicationShortcut);

  m_current_news_action
    = news_menu->addAction (QIcon (), tr ("Community News"),
                            [=] () {
                              emit show_community_news_signal (-1);
                            });
  addAction (m_current_news_action);
  m_current_news_action->setShortcutContext (Qt::ApplicationShortcut);
}

void main_window::construct_tool_bar (void)
{
  m_main_tool_bar = addToolBar (tr ("Toolbar"));
  m_main_tool_bar->setStyleSheet (m_main_tool_bar->styleSheet ()
                                  + global_toolbar_style);

  m_main_tool_bar->setObjectName ("MainToolBar");
  m_main_tool_bar->addAction (m_new_script_action);
  m_main_tool_bar->addAction (m_open_action);

  m_main_tool_bar->addSeparator ();

  m_main_tool_bar->addAction (m_copy_action);
  m_main_tool_bar->addAction (m_paste_action);
  m_main_tool_bar->addAction (m_undo_action);

  m_main_tool_bar->addSeparator ();

  m_current_directory_combo_box = new QComboBox (this);
  QFontMetrics fm = m_current_directory_combo_box->fontMetrics ();
  m_current_directory_combo_box->setFixedWidth (48*fm.averageCharWidth ());
  m_current_directory_combo_box->setEditable (true);
  m_current_directory_combo_box->setInsertPolicy (QComboBox::NoInsert);
  m_current_directory_combo_box->setToolTip (tr ("Enter directory name"));
  m_current_directory_combo_box->setMaxVisibleItems (current_directory_max_visible);
  m_current_directory_combo_box->setMaxCount (current_directory_max_count);
  QSizePolicy sizePol (QSizePolicy::Preferred, QSizePolicy::Preferred);
  m_current_directory_combo_box->setSizePolicy (sizePol);

  // addWidget takes ownership of the objects so there is no
  // need to delete these upon destroying this main_window.
  m_main_tool_bar->addWidget (new QLabel (tr ("Current Directory: ")));
  m_main_tool_bar->addWidget (m_current_directory_combo_box);
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  QAction *current_dir_up
    = m_main_tool_bar->addAction (rmgr.icon ("folder-up", false, "go-up"),
                                  tr ("One directory up"));
  QAction *current_dir_search
    = m_main_tool_bar->addAction (rmgr.icon ("folder"),
                                  tr ("Browse directories"));

  connect (m_current_directory_combo_box, SIGNAL (activated (const QString&)),
           this, SLOT (set_current_working_directory (const QString&)));

  connect (m_current_directory_combo_box->lineEdit (),
           &QLineEdit::returnPressed,
           this, &main_window::accept_directory_line_edit);

  connect (current_dir_search, &QAction::triggered,
           this, &main_window::browse_for_directory);

  connect (current_dir_up, &QAction::triggered,
           this, &main_window::change_directory_up);

  connect (m_undo_action, &QAction::triggered,
           this, &main_window::handle_undo_request);
}

void main_window::focus_console_after_command (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (settings->value (cs_focus_cmd).toBool ())
    focus_command_window ();
}

void main_window::configure_shortcuts (void)
{
  bool enable
    = ! ((m_active_dock == m_command_window) && m_prevent_readline_conflicts);

  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();

  // file menu
  scmgr.set_shortcut (m_open_action, sc_main_file_open_file, enable);
  scmgr.set_shortcut (m_new_script_action, sc_main_file_new_file, enable);
  scmgr.set_shortcut (m_new_function_action, sc_main_file_new_function, enable);
  scmgr.set_shortcut (m_new_figure_action, sc_main_file_new_figure, enable);
  scmgr.set_shortcut (m_load_workspace_action, sc_main_file_load_workspace, enable);
  scmgr.set_shortcut (m_save_workspace_action, sc_main_file_save_workspace, enable);
  scmgr.set_shortcut (m_exit_action, sc_main_file_exit, enable);

  // edit menu
  scmgr.set_shortcut (m_copy_action, sc_main_edit_copy, enable);
  scmgr.set_shortcut (m_paste_action, sc_main_edit_paste, enable);
  scmgr.set_shortcut (m_undo_action, sc_main_edit_undo, enable);
  scmgr.set_shortcut (m_select_all_action, sc_main_edit_select_all, enable);
  scmgr.set_shortcut (m_clear_clipboard_action, sc_main_edit_clear_clipboard, enable);
  scmgr.set_shortcut (m_find_files_action, sc_main_edit_find_in_files, enable);
  scmgr.set_shortcut (m_clear_command_history_action, sc_main_edit_clear_history, enable);
  scmgr.set_shortcut (m_clear_command_window_action, sc_main_edit_clear_command_window, enable);
  scmgr.set_shortcut (m_clear_workspace_action, sc_main_edit_clear_workspace, enable);
  scmgr.set_shortcut (m_set_path_action, sc_main_edit_set_path, enable);
  scmgr.set_shortcut (m_preferences_action, sc_main_edit_preferences, enable);

  // debug menu
  scmgr.set_shortcut (m_debug_step_over, sc_main_debug_step_over, enable);
  scmgr.set_shortcut (m_debug_step_into, sc_main_debug_step_into, enable);
  scmgr.set_shortcut (m_debug_step_out, sc_main_debug_step_out, enable);
  scmgr.set_shortcut (m_debug_continue, sc_main_debug_continue, enable);
  scmgr.set_shortcut (m_debug_quit, sc_main_debug_quit, enable);

  // tools menu
  scmgr.set_shortcut (m_profiler_start, sc_main_tools_start_profiler, enable);
  scmgr.set_shortcut (m_profiler_resume, sc_main_tools_resume_profiler, enable);
  scmgr.set_shortcut (m_profiler_stop, sc_main_tools_start_profiler, enable); // same, toggling
  scmgr.set_shortcut (m_profiler_show, sc_main_tools_show_profiler, enable);

  // window menu
  scmgr.set_shortcut (m_show_command_window_action, sc_main_window_show_command, enable);
  scmgr.set_shortcut (m_show_history_action, sc_main_window_show_history, enable);
  scmgr.set_shortcut (m_show_workspace_action, sc_main_window_show_workspace, enable);
  scmgr.set_shortcut (m_show_file_browser_action, sc_main_window_show_file_browser, enable);
  scmgr.set_shortcut (m_show_editor_action, sc_main_window_show_editor, enable);
  scmgr.set_shortcut (m_show_documentation_action, sc_main_window_show_doc, enable);
  scmgr.set_shortcut (m_show_variable_editor_action, sc_main_window_show_variable_editor, enable);
  scmgr.set_shortcut (m_reset_windows_action, sc_main_window_reset, enable);
  scmgr.set_shortcut (m_command_window_action, sc_main_window_command, enable);
  // Switching to the other widgets (including the previous one) is always enabled
  scmgr.set_shortcut (m_history_action, sc_main_window_history, true);
  scmgr.set_shortcut (m_workspace_action, sc_main_window_workspace, true);
  scmgr.set_shortcut (m_file_browser_action, sc_main_window_file_browser, true);
  scmgr.set_shortcut (m_editor_action, sc_main_window_editor, true);
  scmgr.set_shortcut (m_documentation_action, sc_main_window_doc, true);
  scmgr.set_shortcut (m_variable_editor_action, sc_main_window_variable_editor, true);
  scmgr.set_shortcut (m_previous_dock_action, sc_main_window_previous_dock, true);

  // help menu
  scmgr.set_shortcut (m_ondisk_doc_action, sc_main_help_ondisk_doc, enable);
  scmgr.set_shortcut (m_online_doc_action, sc_main_help_online_doc, enable);
  scmgr.set_shortcut (m_report_bug_action, sc_main_help_report_bug, enable);
  scmgr.set_shortcut (m_octave_packages_action, sc_main_help_packages, enable);
  scmgr.set_shortcut (m_contribute_action, sc_main_help_contribute, enable);
  scmgr.set_shortcut (m_developer_action, sc_main_help_developer, enable);
  scmgr.set_shortcut (m_about_octave_action, sc_main_help_about, enable);

  // news menu
  scmgr.set_shortcut (m_release_notes_action, sc_main_news_release_notes, enable);
  scmgr.set_shortcut (m_current_news_action, sc_main_news_community_news, enable);
}

QList<octave_dock_widget *> main_window::dock_widget_list (void)
{
  QList<octave_dock_widget *> list = QList<octave_dock_widget *> ();
  list.append (static_cast<octave_dock_widget *> (m_command_window));
  list.append (static_cast<octave_dock_widget *> (m_history_window));
  list.append (static_cast<octave_dock_widget *> (m_file_browser_window));
  list.append (static_cast<octave_dock_widget *> (m_doc_browser_window));
#if defined (HAVE_QSCINTILLA)
  list.append (static_cast<octave_dock_widget *> (m_editor_window));
#endif
  list.append (static_cast<octave_dock_widget *> (m_workspace_window));
  list.append (static_cast<octave_dock_widget *> (m_variable_editor_window));
  return list;
}

void main_window::update_default_encoding (const QString& default_encoding)
{
  m_default_encoding = default_encoding;
  std::string mfile_encoding = m_default_encoding.toStdString ();
  if (m_default_encoding.startsWith ("SYSTEM", Qt::CaseInsensitive))
    mfile_encoding = "SYSTEM";

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      F__mfile_encoding__ (interp, ovl (mfile_encoding));
    });
}

void main_window::resize_dock (QDockWidget *dw, int width, int height)
{
#if defined (HAVE_QMAINWINDOW_RESIZEDOCKS)
  // resizeDockWidget was added to Qt in Qt 5.6
  if (width >= 0)
    resizeDocks ({dw}, {width}, Qt::Horizontal);
  if (height >= 0)
    resizeDocks ({dw}, {height}, Qt::Vertical);
#else
  // This replacement of resizeDockWidget is not very reliable.
  // But even if Qt4 is not yet
  QSize s = dw->widget ()->size ();
  if (width >= 0)
    s.setWidth (width);
  if (height >= 0)
    s.setHeight (height);
  dw->widget ()->resize (s);
  dw->adjustSize ();
#endif
}

// The default main window size relative to the desktop size
void main_window::set_default_geometry ()
{
  int win_x, win_y;
  get_screen_geometry (win_x, win_y);

  move (0, 0);
  resize (2*win_x/3, 7*win_y/8);
}

void main_window::reset_windows (void)
{
  // Slot for resetting the window layout to the default one
  hide ();
  showNormal ();              // Unmaximize
  do_reset_windows (true, true, true);   // Add all widgets

  // Re-add after giving time: This seems to be a reliable way to
  // reset the main window's layout

  // JWE says: The following also works for me with 0 delay, so I
  // think the problem might just be that the event loop needs to run
  // somewhere in the sequence of resizing and adding widgets.  Maybe
  // some actions in do_reset_windows should be using signal/slot
  // connections so that the event loop can do what it needs to do.
  // But I haven't been able to find the magic sequence.

  QTimer::singleShot (250, this, [=] () { do_reset_windows (true, true, true); });
}

// Create the default layout of the main window. Do not use
// restoreState () and restoreGeometry () with default values since
// this might lead to problems when the Qt version changes
void main_window::do_reset_windows (bool show, bool save, bool force_all)
{
  // Set main window default geometry and store its width for
  // later resizing the command window
  set_default_geometry ();
  int win_x = geometry ().width ();

  // Resize command window (if docked),
  //the important one in the default layout
  if (dockWidgetArea (m_command_window) != Qt::NoDockWidgetArea)
    resize_dock (m_command_window, 7*win_x/8, -1);

  // See Octave bug #53409 and https://bugreports.qt.io/browse/QTBUG-55357
#if (QT_VERSION < 0x050601) || (QT_VERSION >= 0x050701)
  setDockOptions (QMainWindow::AnimatedDocks
                  | QMainWindow::AllowNestedDocks
                  | QMainWindow::AllowTabbedDocks);
#else
  setDockNestingEnabled (true);
#endif

  // Add the dock widgets and show them
  if (! m_file_browser_window->adopted () || force_all)
    {
      // FIXME: Maybe there should be a main_window::add_dock_widget
      // function that combines both of these actions?

      addDockWidget (Qt::LeftDockWidgetArea, m_file_browser_window);
      m_file_browser_window->set_adopted (false);
    }

  if (! m_workspace_window->adopted () || force_all)
    {
      addDockWidget (Qt::LeftDockWidgetArea, m_workspace_window);
      m_workspace_window->set_adopted (false);
    }

  if (! m_history_window->adopted () || force_all)
    {
      addDockWidget (Qt::LeftDockWidgetArea, m_history_window);
      m_history_window->set_adopted (false);
    }

  if (! m_command_window->adopted () || force_all)
    {
      addDockWidget (Qt::RightDockWidgetArea, m_command_window);
      m_command_window->set_adopted (false);
    }

  if (! m_doc_browser_window->adopted () || force_all)
    {
      addDockWidget (Qt::RightDockWidgetArea, m_doc_browser_window);
      tabifyDockWidget (m_command_window, m_doc_browser_window);
      m_doc_browser_window->set_adopted (false);
    }

  if (! m_variable_editor_window->adopted () || force_all)
    {
      addDockWidget (Qt::RightDockWidgetArea, m_variable_editor_window);
      tabifyDockWidget (m_command_window, m_variable_editor_window);
      m_variable_editor_window->set_adopted (false);
    }

#if defined (HAVE_QSCINTILLA)
  addDockWidget (Qt::RightDockWidgetArea, m_editor_window);
  tabifyDockWidget (m_command_window, m_editor_window);
#endif

  // Resize command window, the important one in the default layout
  resize_dock (m_command_window, 2*win_x/3, -1);

  // Show main wibdow, save state and geometry of main window and
  // all dock widgets
  if (show)
    {
      // Show all dock widgets
      for (auto *widget : dock_widget_list ())
        widget->show ();

      // Show main window and store size and state
      showNormal ();

      if (save)
        {
          resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
          gui_settings *settings = rmgr.get_settings ();

          settings->setValue (mw_geometry.key, saveGeometry ());
          settings->setValue (mw_state.key, saveState ());
        }

      focus_command_window ();
    }
}

OCTAVE_END_NAMESPACE(octave)
