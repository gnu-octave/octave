/*

Copyright (C) 2013 John W. Eaton
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QApplication>
#include <QLabel>
#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QSettings>
#include <QStyle>
#include <QToolBar>
#include <QDesktopServices>
#include <QDesktopWidget>
#include <QFileDialog>
#include <QMessageBox>
#include <QIcon>

#ifdef HAVE_QSCINTILLA
#include "file-editor.h"
#endif
#include "main-window.h"
#include "settings-dialog.h"

#include "builtins.h"
#include "defaults.h"
#include "load-save.h"
#include "toplev.h"
#include "version.h"

#include "cmd-edit.h"
#include "cmd-hist.h"
#include "oct-env.h"

static file_editor_interface *
create_default_editor (QWidget *p)
{
#ifdef HAVE_QSCINTILLA
  return new file_editor (p);
#else
  return 0;
#endif
}

main_window::main_window (QWidget *p)
  : QMainWindow (p),
    _workspace_model (new workspace_model ()),
    status_bar (new QStatusBar ()),
    command_window (new terminal_dock_widget (this)),
    history_window (new history_dock_widget (this)),
    file_browser_window (new files_dock_widget (this)),
    doc_browser_window (new documentation_dock_widget (this)),
    editor_window (create_default_editor (this)),
    workspace_window (new workspace_view (this))
{
  // We have to set up all our windows, before we finally launch octave.
  construct ();
}

main_window::~main_window (void)
{
  delete _workspace_model;
  delete status_bar;
  delete command_window;
  delete history_window;
  delete file_browser_window;
  delete doc_browser_window;
  delete editor_window;
  delete workspace_window;

  // Clean up all dynamically created objects to ensure they are
  // deleted before this main_window is.  Otherwise, some will be
  // attached to a non-existent parent.

  if (_octave_qt_event_listener)
    delete _octave_qt_event_listener;

  octave_link::connect_link (0);
  delete _octave_qt_link;
}

void
main_window::focus_command_window (void)
{
  command_window->focus ();
}

void
main_window::new_file (const QString& commands)
{
  emit new_file_signal (commands);
}

void
main_window::open_file (const QString& file_name)
{
  emit open_file_signal (file_name);
}

void
main_window::report_status_message (const QString& statusMessage)
{
  status_bar->showMessage (statusMessage, 1000);
}

void
main_window::handle_save_workspace_request (void)
{
  QString selectedFile =
    QFileDialog::getSaveFileName (this, tr ("Save Workspace As"),
                                  resource_manager::get_home_path ());
  if (!selectedFile.isEmpty ())
    octave_link::post_event (this, &main_window::save_workspace_callback,
                             selectedFile.toStdString ());
}

void
main_window::handle_load_workspace_request (void)
{
  QString selectedFile =
    QFileDialog::getOpenFileName (this, tr ("Load Workspace"),
                                  resource_manager::get_home_path ());
  if (!selectedFile.isEmpty ())
    octave_link::post_event (this, &main_window::load_workspace_callback,
                             selectedFile.toStdString ());
}

void
main_window::handle_clear_workspace_request (void)
{
  octave_link::post_event (this, &main_window::clear_workspace_callback);
}

void
main_window::handle_clear_history_request (void)
{
  octave_link::post_event (this, &main_window::clear_history_callback);
}

void
main_window::handle_command_double_clicked (const QString& command)
{
  emit relay_command_signal (command);

  command_window->focus ();
}

void
main_window::open_online_documentation_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://gnu.org/software/octave/doc/interpreter"));
}

void
main_window::open_bug_tracker_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://bugs.octave.org"));
}

void
main_window::open_octave_forge_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://octave.sourceforge.net/"));
}

void
main_window::open_agora_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://agora.octave.org/"));
}

void
main_window::process_settings_dialog_request (void)
{
  settings_dialog *settingsDialog = new settings_dialog (this);
  int change_settings = settingsDialog->exec ();
  if (change_settings == QDialog::Accepted)
    {
      settingsDialog->write_changed_settings ();
      QSettings *settings = resource_manager::get_settings ();
      if (settings)
        emit settings_changed (settings);
    }
  delete settingsDialog;
}


void
main_window::notice_settings (const QSettings *settings)
{
  // QSettings pointer is checked before emitting.

  // the widget's icons (when floating)
  QString icon_set
    = settings->value ("DockWidgets/widget_icon_set", "NONE").toString ();

  static struct
    {
      QString name;
      QString path;
    }

  widget_icon_data[] =
    { // array of possible icon sets (name, path (complete for NONE))
      // the first entry here is the default!
      {"NONE",    ":/actions/icons/logo.png"},
      {"GRAPHIC", ":/actions/icons/graphic_logo_"},
      {"LETTER",  ":/actions/icons/letter_logo_"},
      {"", ""} // end marker has empty name
    };

  int count = 0;
  int icon_set_found = 0; // default

  while (!widget_icon_data[count].name.isEmpty ())
    { // while not end of data
      if (widget_icon_data[count].name == icon_set)
        { // data of desired icon set found
          icon_set_found = count;
          break;
        }
      count++;
    }

  QString icon;
  foreach (QObject *obj, children ())
    {
      QString name = obj->objectName ();
      if (obj->inherits ("QDockWidget") && ! name.isEmpty ())
        { // if children is a dockwidget with a name
          QDockWidget *widget = qobject_cast<QDockWidget *> (obj);
          icon = widget_icon_data[icon_set_found].path; // prefix or octave-logo
          if (widget_icon_data[icon_set_found].name != "NONE")
            icon = icon + name + ".png"; // add widget name and ext.
          widget->setWindowIcon (QIcon (icon));
        }
    }

  resource_manager::update_network_settings ();
}


void
main_window::prepare_for_quit (void)
{
  write_settings ();
}

void
main_window::reset_windows ()
{
  // TODO: Implement.
}

void
main_window::update_workspace (void)
{
  workspace_window->model_changed ();
}

void
main_window::change_directory (const QString& dir)
{
  // Remove existing entry, if any, then add new directory at top and
  // mark it as the current directory.  Finally, update the file list
  // widget.

  int index = _current_directory_combo_box->findText (dir);

  if (index >= 0)
    _current_directory_combo_box->removeItem (index);

  _current_directory_combo_box->insertItem (0, dir);
  _current_directory_combo_box->setCurrentIndex (0);

  file_browser_window->display_directory (dir);
}

void
main_window::browse_for_directory (void)
{
  QString dir =
    QFileDialog::getExistingDirectory (this, tr ("Set working directory"));

  if (! dir.isEmpty ())
    octave_link::post_event (this,
                             &main_window::change_directory_callback,
                             dir.toStdString ());
}

void
main_window::set_current_working_directory (const QString& dir)
{
  // Change to dir if it is an existing directory.

  QString xdir = dir.isEmpty () ? "." : dir;
    
  QFileInfo fileInfo (xdir);

  if (fileInfo.exists () && fileInfo.isDir ())
    octave_link::post_event (this, &main_window::change_directory_callback,
                             xdir.toStdString ());
}

void
main_window::change_directory_up (void)
{
  QDir dir ("..");

  set_current_working_directory (dir.absolutePath ());
}

// Slot that is called if return is pressed in the line edit of the
// combobox to change to a new directory or a directory that is already
// in the drop down list.

void
main_window::accept_directory_line_edit (void)
{
  // Get new directory name, and change to it if it is new.  Otherwise,
  // the combo box will triggers the "activated" signal to change to the
  // directory.

  QString dir = _current_directory_line_edit->text ();

  int index = _current_directory_combo_box->findText (dir);

  if (index < 0)
    set_current_working_directory (dir);
}

void
main_window::handle_enter_debugger (void)
{
  setWindowTitle ("Octave (Debugging)");

  _debug_continue->setEnabled (true);
  _debug_step_into->setEnabled (true);
  _debug_step_over->setEnabled (true);
  _debug_step_out->setEnabled (true);
  _debug_quit->setEnabled (true);

#ifdef HAVE_QSCINTILLA
  editor_window->handle_enter_debug_mode ();
#endif
}

void
main_window::handle_exit_debugger (void)
{
  setWindowTitle ("Octave");

  _debug_continue->setEnabled (false);
  _debug_step_into->setEnabled (false);
  _debug_step_over->setEnabled (false);
  _debug_step_out->setEnabled (false);
  _debug_quit->setEnabled (false);

#ifdef HAVE_QSCINTILLA
  editor_window->handle_exit_debug_mode ();
#endif
}

void
main_window::debug_continue (void)
{
  octave_link::post_event (this, &main_window::debug_continue_callback);
}

void
main_window::debug_step_into (void)
{
  octave_link::post_event (this, &main_window::debug_step_into_callback);
}

void
main_window::debug_step_over (void)
{
  octave_link::post_event (this, &main_window::debug_step_over_callback);
}

void
main_window::debug_step_out (void)
{
  octave_link::post_event (this, &main_window::debug_step_out_callback);
}

void
main_window::debug_quit (void)
{
  octave_link::post_event (this, &main_window::debug_quit_callback);
}

void
main_window::show_about_octave (void)
{
  QString message = OCTAVE_STARTUP_MESSAGE;

  QMessageBox::about (this, tr ("About Octave"), message);
}

void
main_window::closeEvent (QCloseEvent *e)
{
  e->ignore ();
  octave_link::post_event (this, &main_window::exit_callback);
}

void
main_window::read_settings (void)
{
  QSettings *settings = resource_manager::get_settings ();
  if (!settings)
    {
      qDebug("Error: QSettings pointer from resource manager is NULL.");
      return;
    }

  restoreState (settings->value ("MainWindow/windowState").toByteArray ());
  settings->beginGroup ("DockWidgets");
  // restoring the geometry of all dock-widgets
  foreach (QObject *obj, children ())
    {
      QString name = obj->objectName ();
      if (obj->inherits ("QDockWidget") && ! name.isEmpty ())
        {
          QDockWidget *widget = qobject_cast<QDockWidget *> (obj);
          QVariant val = settings->value (name);
          widget->restoreGeometry (val.toByteArray ());
          bool floating = settings->value (name+"Floating", false).toBool ();
          bool visible = settings->value (name+"Visible", true).toBool ();
          if (floating)
            widget->setWindowFlags (Qt::Window); // if floating, make window from widget
          widget->setVisible (visible);          // make widget visible if desired (setWindowFlags hides widget)
        }
    }
  settings->endGroup();
  restoreGeometry (settings->value ("MainWindow/geometry").toByteArray ());
  // restore the list of the last directories
  QStringList curr_dirs = settings->value ("MainWindow/current_directory_list").toStringList ();
  for (int i=0; i < curr_dirs.size (); i++)
    {
      _current_directory_combo_box->addItem (curr_dirs.at (i));
    }
  emit settings_changed (settings);
}

void
main_window::write_settings (void)
{
  QSettings *settings = resource_manager::get_settings ();
  if (!settings)
    {
      qDebug("Error: QSettings pointer from resource manager is NULL.");
      return;
    }

  settings->setValue ("MainWindow/geometry", saveGeometry ());
  settings->beginGroup ("DockWidgets");
  // saving the geometry of all widgets
  foreach (QObject *obj, children())
    {
      QString name = obj->objectName ();
      if (obj->inherits ("QDockWidget") && ! name.isEmpty ())
        {
          QDockWidget *widget = qobject_cast<QDockWidget *> (obj);
          settings->setValue (name, widget->saveGeometry ());
          bool floating = widget->isFloating ();
          bool visible = widget->isVisible ();
          settings->setValue (name+"Floating", floating);  // store floating state
          settings->setValue (name+"Visible", visible);    // store visibility
          if (floating)
            widget->setWindowFlags (Qt::Widget); // if floating, recover the widget state such that the widget's
        }                                       // state is correctly saved by the saveSate () below
    }
  settings->endGroup();
  settings->setValue ("MainWindow/windowState", saveState ());
  // write the list of recent used directories
  QStringList curr_dirs;
  for (int i=0; i<_current_directory_combo_box->count (); i++)
    {
      curr_dirs.append (_current_directory_combo_box->itemText (i));
    }
  settings->setValue ("MainWindow/current_directory_list", curr_dirs);
  settings->sync ();
}


// Connecting the signals emitted when the visibility of a widget changes.
// This has to be done after the window is shown (see octave-gui.cc)
void
main_window::connect_visibility_changed (void)
{
  command_window->connect_visibility_changed ();
  history_window->connect_visibility_changed ();
  file_browser_window->connect_visibility_changed ();
  doc_browser_window->connect_visibility_changed ();
#ifdef HAVE_QSCINTILLA
  editor_window->connect_visibility_changed ();
#endif
  workspace_window->connect_visibility_changed ();
}


// Main subroutine of the constructor
void
main_window::construct (void)
{
  _closing = false;   // flag for editor files when closed
  setWindowIcon (QIcon (":/actions/icons/logo.png"));

  workspace_window->setModel (_workspace_model);

  connect (_workspace_model, SIGNAL (model_changed ()),
           workspace_window, SLOT (model_changed ()));

  // Create and set the central widget.  QMainWindow takes ownership of
  // the widget (pointer) so there is no need to delete the object upon
  // destroying this main_window.

  QWidget *dummyWidget = new QWidget ();
  dummyWidget->setObjectName ("CentralDummyWidget");
  dummyWidget->resize (10, 10);
  dummyWidget->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Minimum);
  dummyWidget->hide ();
  setCentralWidget (dummyWidget);

  construct_menu_bar ();

  construct_tool_bar ();

  connect (qApp, SIGNAL (aboutToQuit ()),
           this, SLOT (prepare_for_quit ()));

  connect (this, SIGNAL (settings_changed (const QSettings *)),
           this, SLOT (notice_settings (const QSettings *)));

  setWindowTitle ("Octave");

  setDockOptions (QMainWindow::AnimatedDocks
                  | QMainWindow::AllowNestedDocks
                  | QMainWindow::AllowTabbedDocks);

  addDockWidget (Qt::RightDockWidgetArea, command_window);
  addDockWidget (Qt::RightDockWidgetArea, doc_browser_window);
  tabifyDockWidget (command_window, doc_browser_window);

#ifdef HAVE_QSCINTILLA
  addDockWidget (Qt::RightDockWidgetArea, editor_window);
  tabifyDockWidget (command_window, editor_window);
#endif

  addDockWidget (Qt::LeftDockWidgetArea, file_browser_window);
  addDockWidget (Qt::LeftDockWidgetArea, workspace_window);
  addDockWidget (Qt::LeftDockWidgetArea, history_window);

  int win_x = QApplication::desktop()->width();
  int win_y = QApplication::desktop()->height();

  if (win_x > 960)
    win_x = 960;

  if (win_y > 720)
    win_y = 720;

  setGeometry (0, 0, win_x, win_y);

  setStatusBar (status_bar);

  _octave_qt_event_listener = new octave_qt_event_listener ();

  connect (_octave_qt_event_listener, SIGNAL (update_workspace_signal ()),
           this, SLOT (update_workspace ()));

  // FIXME -- is it possible to eliminate the event_listenter?

  construct_octave_qt_link ();

  QDir curr_dir;
  set_current_working_directory (curr_dir.absolutePath ());
}

void
main_window::construct_octave_qt_link (void)
{
  _octave_qt_link = new octave_qt_link ();

  connect (_octave_qt_link, SIGNAL (change_directory_signal (QString)),
           this, SLOT (change_directory (QString)));

  connect (_octave_qt_link,
           SIGNAL (set_history_signal (const QStringList&)),
           history_window, SLOT (set_history (const QStringList&)));

  connect (_octave_qt_link,
           SIGNAL (append_history_signal (const QString&)),
           history_window, SLOT (append_history (const QString&)));

  connect (_octave_qt_link,
           SIGNAL (clear_history_signal (void)),
           history_window, SLOT (clear_history (void)));

  connect (_octave_qt_link, SIGNAL (enter_debugger_signal ()),
           this, SLOT (handle_enter_debugger ()));

  connect (_octave_qt_link, SIGNAL (exit_debugger_signal ()),
           this, SLOT (handle_exit_debugger ()));

  connect (_octave_qt_link,
           SIGNAL (update_breakpoint_marker_signal (bool, const QString&, int)),
           editor_window,
           SLOT (handle_update_breakpoint_marker_request (bool, const QString&, int)));

  connect (_octave_qt_link,
           SIGNAL (edit_file_signal (const QString&)),
           editor_window,
           SLOT (handle_edit_file_request (const QString&)));

  connect (_octave_qt_link,
           SIGNAL (insert_debugger_pointer_signal (const QString&, int)),
           editor_window,
           SLOT (handle_insert_debugger_pointer_request (const QString&, int)));

  connect (_octave_qt_link,
           SIGNAL (delete_debugger_pointer_signal (const QString&, int)),
           editor_window,
           SLOT (handle_delete_debugger_pointer_request (const QString&, int)));

  _octave_qt_link->execute_interpreter ();

  octave_link::connect_link (_octave_qt_link);

  octave_link::register_event_listener (_octave_qt_event_listener);
}

void
main_window::construct_menu_bar (void)
{
  QMenuBar *menu_bar = menuBar ();

  construct_file_menu (menu_bar);

  construct_edit_menu (menu_bar);

  construct_debug_menu (menu_bar);

  construct_desktop_menu (menu_bar);

  construct_window_menu (menu_bar);

  construct_help_menu (menu_bar);
}

void
main_window::construct_file_menu (QMenuBar *p)
{
  QMenu *file_menu = p->addMenu (tr ("&File"));

  construct_new_menu (file_menu);

  _open_action
    = file_menu->addAction (QIcon (":/actions/icons/fileopen.png"),
                            tr ("Open..."));
  _open_action->setShortcut (QKeySequence::Open);
  _open_action->setShortcutContext (Qt::ApplicationShortcut);

#ifdef HAVE_QSCINTILLA
  file_menu->addMenu (editor_window->get_mru_menu ());
#endif

  QAction *close_command_window_action
    = file_menu->addAction (tr ("Close Command Window"));
  close_command_window_action->setShortcut (QKeySequence::Close);
  close_command_window_action->setEnabled (false); // TODO: Make this work.

  file_menu->addSeparator ();

  QAction *import_data_action
    = file_menu->addAction (tr ("Import Data"));
  import_data_action->setEnabled (false); // TODO: Make this work.

  QAction *save_workspace_action
    = file_menu->addAction (tr ("Save Workspace As"));

  file_menu->addSeparator ();

  QAction *preferences_action
    = file_menu->addAction (QIcon (":/actions/icons/configure.png"),
                            tr ("Preferences..."));

  file_menu->addSeparator ();

  QAction *page_setup_action
    = file_menu->addAction (tr ("Page Setup..."));
  page_setup_action->setEnabled (false); // TODO: Make this work.

  QAction *print_action
    = file_menu->addAction (tr ("Print"));
  print_action->setShortcut (QKeySequence::Print);
  print_action->setEnabled (false); // TODO: Make this work.

  QAction *print_selection_action
    = file_menu->addAction (tr ("Print Selection..."));
  print_selection_action->setEnabled (false); // TODO: Make this work.

  file_menu->addSeparator ();

  QAction *exit_action = file_menu->addAction (tr ("Exit"));
  exit_action->setShortcut (QKeySequence::Quit);

  connect (preferences_action, SIGNAL (triggered ()),
           this, SLOT (process_settings_dialog_request ()));

  connect (_open_action, SIGNAL (triggered ()),
           editor_window, SLOT (request_open_file ()));

  connect (save_workspace_action, SIGNAL (triggered ()),
           this, SLOT (handle_save_workspace_request ()));

  connect (exit_action, SIGNAL (triggered ()),
           this, SLOT (close ()));
}

void
main_window::construct_new_menu (QMenu *p)
{
  QMenu *new_menu = p->addMenu (tr ("New"));

  _new_script_action
    = new_menu->addAction (QIcon (":/actions/icons/filenew.png"), tr ("Script"));
  _new_script_action->setShortcut (QKeySequence::New);
  _new_script_action->setShortcutContext (Qt::ApplicationShortcut);

  QAction *new_function_action = new_menu->addAction (tr ("Function"));
  new_function_action->setEnabled (false); // TODO: Make this work.

  QAction *new_class_action = new_menu->addAction (tr ("Class"));
  new_class_action->setEnabled (false); // TODO: Make this work.

  QAction *new_enumeration_action = new_menu->addAction (tr ("Enumeration"));
  new_enumeration_action->setEnabled (false); // TODO: Make this work.

  QAction *new_figure_action = new_menu->addAction (tr ("Figure"));
  new_figure_action->setEnabled (false); // TODO: Make this work.

  QAction *new_variable_action = new_menu->addAction (tr ("Variable"));
  new_variable_action->setEnabled (false); // TODO: Make this work.

  QAction *new_model_action = new_menu->addAction (tr ("Model"));
  new_model_action->setEnabled (false); // TODO: Make this work.

  QAction *new_gui_action = new_menu->addAction (tr ("GUI"));
  new_gui_action->setEnabled (false); // TODO: Make this work.

  connect (_new_script_action, SIGNAL (triggered ()),
           editor_window, SLOT (request_new_file ()));
}

void
main_window::construct_edit_menu (QMenuBar *p)
{
  QMenu *edit_menu = p->addMenu (tr ("&Edit"));

  QKeySequence ctrl_shift = Qt::ControlModifier + Qt::ShiftModifier;

  _undo_action
    = edit_menu->addAction (QIcon (":/actions/icons/undo.png"), tr ("Undo"));
  _undo_action->setShortcut (QKeySequence::Undo);

  _redo_action
    = edit_menu->addAction (QIcon (":/actions/icons/redo.png"), tr ("Redo"));
  _redo_action->setShortcut (QKeySequence::Redo);

  edit_menu->addSeparator ();

  _cut_action
    = edit_menu->addAction (QIcon (":/actions/icons/editcut.png"), tr ("Cut"));
  _cut_action->setShortcut (ctrl_shift + Qt::Key_X);

  _copy_action
    = edit_menu->addAction (QIcon (":/actions/icons/editcopy.png"), tr ("Copy"));
  _copy_action->setShortcut (ctrl_shift + Qt::Key_C);

  _paste_action
    = edit_menu->addAction (QIcon (":/actions/icons/editpaste.png"), tr ("Paste"));
  _paste_action->setShortcut (ctrl_shift + Qt::Key_V);

  QAction *paste_to_workspace_action
    = edit_menu->addAction (tr ("Paste To Workspace..."));
  paste_to_workspace_action->setEnabled (false); // TODO: Make this work.

  edit_menu->addSeparator ();

  QAction *select_all_action
    = edit_menu->addAction (tr ("Select All"));
  select_all_action->setEnabled (false); // TODO: Make this work.

  QAction *delete_action
    = edit_menu->addAction (tr ("Delete"));
  delete_action->setShortcut (Qt::Key_Delete);
  delete_action->setEnabled (false); // TODO: Make this work.

  edit_menu->addSeparator ();

  QAction *find_action
    = edit_menu->addAction (tr ("Find..."));
  find_action->setEnabled (false); // TODO: Make this work.

  QAction *find_files_action
    = edit_menu->addAction (tr ("Find Files..."));
  find_files_action->setShortcut (ctrl_shift + Qt::Key_F);
  find_files_action->setEnabled (false); // TODO: Make this work.

  edit_menu->addSeparator ();

  QAction *clear_command_window_action
    = edit_menu->addAction (tr ("Clear Command Window"));
  clear_command_window_action->setEnabled (false); // TODO: Make this work.

  QAction *clear_command_history
    = edit_menu->addAction(tr ("Clear Command History"));

  QAction *clear_workspace_action
    = edit_menu->addAction (tr ("Clear Workspace"));

  connect (_copy_action, SIGNAL (triggered()),
           command_window, SLOT (copyClipboard ()));

  connect (_paste_action, SIGNAL (triggered()),
           command_window, SLOT (pasteClipboard ()));

  connect (clear_command_history, SIGNAL (triggered ()),
           this, SLOT (handle_clear_history_request ()));

  connect (clear_workspace_action, SIGNAL (triggered ()),
           this, SLOT (handle_clear_workspace_request ()));
}

QAction *
main_window::construct_debug_menu_item (const char *icon_file,
                                        const QString& item,
                                        const QKeySequence& key)
{
  QAction *action = _debug_menu->addAction (QIcon (icon_file), item);

  action->setEnabled (false);
  action->setShortcut (key);

#ifdef HAVE_QSCINTILLA
  editor_window->debug_menu ()->addAction (action);
  editor_window->toolbar ()->addAction (action);
#endif

  return action;
}

void
main_window::construct_debug_menu (QMenuBar *p)
{
  _debug_menu = p->addMenu (tr ("De&bug"));

  _debug_step_over = construct_debug_menu_item
    (":/actions/icons/db_step.png", tr ("Step"), Qt::Key_F10);

  _debug_step_into = construct_debug_menu_item
    (":/actions/icons/db_step_in.png", tr ("Step in"), Qt::Key_F11);

  _debug_step_out = construct_debug_menu_item
    (":/actions/icons/db_step_out.png", tr ("Step out"),
     Qt::ShiftModifier + Qt::Key_F11);

  _debug_continue = construct_debug_menu_item
    (":/actions/icons/db_cont.png", tr ("Continue"), Qt::Key_F5);

  _debug_menu->addSeparator ();
#ifdef HAVE_QSCINTILLA
  editor_window->debug_menu ()->addSeparator ();
#endif

  _debug_quit = construct_debug_menu_item
    (":/actions/icons/db_stop.png", tr ("Exit Debug Mode"),
     Qt::ShiftModifier + Qt::Key_F5);

  connect (_debug_step_over, SIGNAL (triggered ()),
           this, SLOT (debug_step_over ()));

  connect (_debug_step_into, SIGNAL (triggered ()),
           this, SLOT (debug_step_into ()));

  connect (_debug_step_out, SIGNAL (triggered ()),
           this, SLOT (debug_step_out ()));

  connect (_debug_continue, SIGNAL (triggered ()),
           this, SLOT (debug_continue ()));

  connect (_debug_quit, SIGNAL (triggered ()),
           this, SLOT (debug_quit ()));
}

void
main_window::construct_desktop_menu (QMenuBar *p)
{
  QMenu *desktop_menu = p->addMenu (tr ("&Desktop"));

  QAction *load_workspace_action = desktop_menu->addAction (tr ("Load workspace"));

  connect (load_workspace_action, SIGNAL (triggered ()),
           this, SLOT (handle_load_workspace_request ()));
}

QAction *
main_window::construct_window_menu_item (QMenu *p, const QString& item,
                                         bool checkable,
                                         const QKeySequence& key)
{
  QAction *action = p->addAction (item);

  action->setCheckable (checkable);
  action->setShortcut (key);
  action->setShortcutContext (Qt::ApplicationShortcut);

  return action;
}

void
main_window::construct_window_menu (QMenuBar *p)
{
  QMenu *window_menu = p->addMenu (tr ("&Window"));

  QKeySequence ctrl = Qt::ControlModifier;
  QKeySequence ctrl_shift = Qt::ControlModifier + Qt::ShiftModifier;

  QAction *show_command_window_action = construct_window_menu_item
    (window_menu, tr ("Show Command Window"), true, ctrl_shift + Qt::Key_0);

  QAction *show_history_action = construct_window_menu_item
    (window_menu, tr ("Show Command History"), true, ctrl_shift + Qt::Key_1);

  QAction *show_file_browser_action =  construct_window_menu_item
    (window_menu, tr ("Show Current Directory"), true, ctrl_shift + Qt::Key_2);

  QAction *show_workspace_action = construct_window_menu_item
    (window_menu, tr ("Show Workspace"), true, ctrl_shift + Qt::Key_3);

  QAction *show_editor_action = construct_window_menu_item
    (window_menu, tr ("Show Editor"), true, ctrl_shift + Qt::Key_4);

  QAction *show_documentation_action = construct_window_menu_item
    (window_menu, tr ("Show Documentation"), true, ctrl_shift + Qt::Key_5);

  window_menu->addSeparator ();

  QAction *command_window_action = construct_window_menu_item
    (window_menu, tr ("Command Window"), false, ctrl + Qt::Key_0);

  QAction *history_action = construct_window_menu_item
    (window_menu, tr ("Command History"), false, ctrl + Qt::Key_1);

  QAction *file_browser_action = construct_window_menu_item
    (window_menu, tr ("Current Directory"), false, ctrl + Qt::Key_2);

  QAction *workspace_action = construct_window_menu_item
    (window_menu, tr ("Workspace"), false, ctrl + Qt::Key_3);

  QAction *editor_action = construct_window_menu_item
    (window_menu, tr ("Editor"), false, ctrl + Qt::Key_4);

  QAction *documentation_action = construct_window_menu_item
    (window_menu, tr ("Documentation"), false, ctrl + Qt::Key_5);

  window_menu->addSeparator ();

  QAction *reset_windows_action
    = window_menu->addAction (tr ("Reset Windows"));

  reset_windows_action->setEnabled (false); // TODO: Make this work.

  connect (show_command_window_action, SIGNAL (toggled (bool)),
           command_window, SLOT (setVisible (bool)));

  connect (command_window, SIGNAL (active_changed (bool)),
           show_command_window_action, SLOT (setChecked (bool)));

  connect (show_workspace_action, SIGNAL (toggled (bool)),
           workspace_window, SLOT (setVisible (bool)));

  connect (workspace_window, SIGNAL (active_changed (bool)),
           show_workspace_action, SLOT (setChecked (bool)));

  connect (show_history_action, SIGNAL (toggled (bool)),
           history_window, SLOT (setVisible (bool)));

  connect (history_window, SIGNAL (active_changed (bool)),
           show_history_action, SLOT (setChecked (bool)));

  connect (show_file_browser_action, SIGNAL (toggled (bool)),
           file_browser_window, SLOT (setVisible (bool)));

  connect (file_browser_window, SIGNAL (active_changed (bool)),
           show_file_browser_action, SLOT (setChecked (bool)));

#ifdef HAVE_QSCINTILLA
  connect (show_editor_action, SIGNAL (toggled (bool)),
           editor_window, SLOT (setVisible (bool)));

  connect (editor_window, SIGNAL (active_changed (bool)),
           show_editor_action, SLOT (setChecked (bool)));
#endif

  connect (show_documentation_action, SIGNAL (toggled (bool)),
           doc_browser_window, SLOT (setVisible (bool)));

  connect (doc_browser_window, SIGNAL (active_changed (bool)),
           show_documentation_action, SLOT (setChecked (bool)));

  connect (command_window_action, SIGNAL (triggered ()),
           command_window, SLOT (focus ()));

  connect (workspace_action, SIGNAL (triggered ()),
           workspace_window, SLOT (focus ()));

  connect (history_action, SIGNAL (triggered ()),
           history_window, SLOT (focus ()));

  connect (file_browser_action, SIGNAL (triggered ()),
           file_browser_window, SLOT (focus ()));

  connect (editor_action, SIGNAL (triggered ()),
           editor_window, SLOT (focus ()));

  connect (documentation_action, SIGNAL (triggered ()),
           doc_browser_window, SLOT (focus ()));

  connect (reset_windows_action, SIGNAL (triggered ()),
           this, SLOT (reset_windows ()));
}

void
main_window::construct_help_menu (QMenuBar *p)
{
  QMenu *help_menu = p->addMenu (tr ("&Help"));

  construct_documentation_menu (help_menu);

  help_menu->addSeparator ();

  QAction *report_bug_action
    = help_menu->addAction (tr ("Report Bug"));

  QAction *octave_forge_action
    = help_menu->addAction (tr ("Visit Octave Forge"));

  QAction *agora_action
    = help_menu->addAction (tr ("Visit Agora"));

  help_menu->addSeparator ();

  QAction *about_octave_action
    = help_menu->addAction (tr ("About Octave"));

  connect (report_bug_action, SIGNAL (triggered ()),
           this, SLOT (open_bug_tracker_page ()));

  connect (octave_forge_action, SIGNAL (triggered ()),
           this, SLOT (open_octave_forge_page ()));

  connect (agora_action, SIGNAL (triggered ()),
           this, SLOT (open_agora_page ()));

  connect (about_octave_action, SIGNAL (triggered ()),
           this, SLOT (show_about_octave ()));
}

void
main_window::construct_documentation_menu (QMenu *p)
{
  QMenu *documentation_menu = p->addMenu (tr ("Documentation"));

  QAction *ondisk_documentation_action
    = documentation_menu->addAction (tr ("On Disk"));

  QAction *online_documentation_action
    = documentation_menu->addAction (tr ("Online"));

  connect (ondisk_documentation_action, SIGNAL (triggered ()),
           doc_browser_window, SLOT (focus ()));

  connect (online_documentation_action, SIGNAL (triggered ()),
           this, SLOT (open_online_documentation_page ()));
}

void
main_window::construct_tool_bar (void)
{
  QToolBar *main_tool_bar = addToolBar ("Main");

  main_tool_bar->setObjectName ("MainToolBar");
  main_tool_bar->addAction (_new_script_action);
  main_tool_bar->addAction (_open_action);

  main_tool_bar->addSeparator ();

  main_tool_bar->addAction (_cut_action);
  main_tool_bar->addAction (_copy_action);
  main_tool_bar->addAction (_paste_action);
  main_tool_bar->addAction (_undo_action);
  main_tool_bar->addAction (_redo_action);

  main_tool_bar->addSeparator ();

  _current_directory_line_edit = new QLineEdit (this);
  _current_directory_combo_box = new QComboBox (this);
  _current_directory_combo_box->setFixedWidth (current_directory_width);
  _current_directory_combo_box->setEditable (true);
  // setLineEdit takes ownership -> no need to delete line_edit in ~main_window
  _current_directory_combo_box->setLineEdit (_current_directory_line_edit);
  _current_directory_combo_box->setInsertPolicy (QComboBox::InsertAtTop);
  _current_directory_combo_box->setMaxVisibleItems (current_directory_max_visible);
  _current_directory_combo_box->setMaxCount (current_directory_max_count);

  QToolButton *current_directory_tool_button = new QToolButton (this);
  current_directory_tool_button->setIcon (QIcon (":/actions/icons/search.png"));

  QToolButton *current_directory_up_tool_button = new QToolButton (this);
  current_directory_up_tool_button->setIcon (QIcon (":/actions/icons/up.png"));

  // addWidget takes ownership of the objects so there is no
  // need to delete these upon destroying this main_window.
  main_tool_bar->addWidget (new QLabel (tr ("Current Directory:")));
  main_tool_bar->addWidget (_current_directory_combo_box);
  main_tool_bar->addWidget (current_directory_tool_button);
  main_tool_bar->addWidget (current_directory_up_tool_button);

  connect (_current_directory_combo_box, SIGNAL (activated (QString)),
           this, SLOT (set_current_working_directory (QString)));

  connect (_current_directory_line_edit, SIGNAL (returnPressed ()),
           this, SLOT (accept_directory_line_edit ()));

  connect (current_directory_tool_button, SIGNAL (clicked ()),
           this, SLOT (browse_for_directory ()));

  connect (current_directory_up_tool_button, SIGNAL (clicked ()),
           this, SLOT (change_directory_up ()));
}

void
main_window::save_workspace_callback (const std::string& file)
{
  Fsave (ovl (file));
}

void
main_window::load_workspace_callback (const std::string& file)
{
  Fload (ovl (file));
}

void
main_window::clear_workspace_callback (void)
{
  Fclear ();
}

void
main_window::clear_history_callback (void)
{
  Fhistory (ovl ("-c"));
}

void
main_window::change_directory_callback (const std::string& directory)
{
  Fcd (ovl (directory));
}

void
main_window::debug_continue_callback (void)
{
  Fdbcont ();

  command_editor::interrupt (true);
}

// The next three callbacks are invoked by GUI buttons.  Those buttons
// should only be active when we are doing debugging, which means that
// Octave is waiting for input in get_debug_input.  Calling
// command_editor::interrupt will force readline to return even if it
// has not read any input, and then get_debug_input will return,
// allowing the evaluator to continue and execute the next statement.

void
main_window::debug_step_into_callback (void)
{
  Fdbstep (ovl ("in"));

  command_editor::interrupt (true);
}

void
main_window::debug_step_over_callback (void)
{
  Fdbstep ();

  command_editor::interrupt (true);
}

void
main_window::debug_step_out_callback (void)
{
  Fdbstep (ovl ("out"));

  command_editor::interrupt (true);
}

void
main_window::debug_quit_callback (void)
{
  Fdbquit ();

  command_editor::interrupt (true);
}

void
main_window::exit_callback (void)
{
  Fquit ();
}
