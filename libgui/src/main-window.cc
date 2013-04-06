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

main_window::main_window (QWidget *p)
  : QMainWindow (p), _workspace_model (), status_bar (),
    command_window (this), history_window (this)
{
  // We have to set up all our windows, before we finally launch octave.
  construct ();
}

main_window::~main_window ()
{
  // Clean up all dynamically created objects to ensure they are
  // deleted before this main_window is.  Otherwise, some will be
  // attached to a non-existent parent.

  if (_octave_qt_event_listener)
    delete _octave_qt_event_listener;

  octave_link::connect_link (0);
  delete _octave_qt_link;

#ifdef HAVE_QSCINTILLA
  if (_file_editor)
    delete _file_editor;
#endif

  if (_documentation_dock_widget)
    delete _documentation_dock_widget;

  if (_files_dock_widget)
    delete _files_dock_widget;

  delete _workspace_view;
}

void
main_window::focus_command_window (void)
{
  command_window.focus ();
}

void
main_window::new_file ()
{
#ifdef HAVE_QSCINTILLA
  _file_editor->request_new_file ();
#endif
}

void
main_window::open_file ()
{
#ifdef HAVE_QSCINTILLA
  _file_editor->request_open_file ();
#endif
}

void
main_window::open_file (const QString& file_name)
{
#ifdef HAVE_QSCINTILLA
  _file_editor->request_open_file (file_name);
#endif
}

void
main_window::report_status_message (const QString& statusMessage)
{
  status_bar.showMessage (statusMessage, 1000);
}

void
main_window::handle_save_workspace_request ()
{
  QString selectedFile =
    QFileDialog::getSaveFileName (this, tr ("Save Workspace As"),
                                  resource_manager::get_home_path ());
  if (!selectedFile.isEmpty ())
    octave_link::post_event (this, &main_window::save_workspace_callback,
                             selectedFile.toStdString ());
}

void
main_window::handle_load_workspace_request ()
{
  QString selectedFile =
    QFileDialog::getOpenFileName (this, tr ("Load Workspace"),
                                  resource_manager::get_home_path ());
  if (!selectedFile.isEmpty ())
    octave_link::post_event (this, &main_window::load_workspace_callback,
                             selectedFile.toStdString ());
}

void
main_window::handle_clear_workspace_request ()
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

  command_window.focus ();
}

void
main_window::open_online_documentation_page ()
{
  QDesktopServices::openUrl (QUrl ("http://gnu.org/software/octave/doc/interpreter"));
}

void
main_window::open_bug_tracker_page ()
{
  QDesktopServices::openUrl (QUrl ("http://bugs.octave.org"));
}

void
main_window::open_octave_forge_page ()
{
  QDesktopServices::openUrl (QUrl ("http://octave.sourceforge.net/"));
}

void
main_window::open_agora_page ()
{
  QDesktopServices::openUrl (QUrl ("http://agora.octave.org/"));
}

void
main_window::process_settings_dialog_request ()
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
  QString icon_set = settings->value ("DockWidgets/widget_icon_set","NONE").
                                      toString ();
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
      if (obj->inherits("QDockWidget") && ! name.isEmpty ())
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
main_window::prepare_for_quit ()
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
  _workspace_view->model_changed ();
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

  _files_dock_widget->display_directory (dir);
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
main_window::focus_current_directory ()
{
  if (!_files_dock_widget->isVisible ())
    {
      _files_dock_widget->setVisible (true);
    }

  _files_dock_widget->setFocus ();
  _files_dock_widget->activateWindow ();
  _files_dock_widget->raise ();
}

void
main_window::focus_workspace ()
{
  if (!_workspace_view->isVisible ())
    {
      _workspace_view->setVisible (true);
    }

  _workspace_view->setFocus ();
  _workspace_view->activateWindow ();
  _workspace_view->raise ();
}


void
main_window::focus_editor ()
{
#ifdef HAVE_QSCINTILLA
  // call own function of editor in order to set focus to the current editor tab
  _file_editor->set_focus ();
#endif
}

void
main_window::focus_documentation ()
{
  if (!_documentation_dock_widget->isVisible ())
    {
      _documentation_dock_widget->setVisible (true);
    }

  _documentation_dock_widget->setFocus ();
  _documentation_dock_widget->activateWindow ();
  _documentation_dock_widget->raise ();
}

void
main_window::handle_current_directory_visible (bool visible)
{
  // if changed to visible and widget is not floating
  if (visible && !_files_dock_widget->isFloating ())
    focus_current_directory ();
}

void
main_window::handle_workspace_visible (bool visible)
{
  // if changed to visible and widget is not floating
  if (visible && !_workspace_view->isFloating ())
    focus_workspace ();
}

void
main_window::handle_editor_visible (bool visible)
{
  // if changed to visible and widget is not floating
#ifdef HAVE_QSCINTILLA
  if (visible && !_file_editor->isFloating ())
    focus_editor ();
#endif
}

void
main_window::handle_documentation_visible (bool visible)
{
  // if changed to visible and widget is not floating
  if (visible && !_documentation_dock_widget->isFloating ())
    focus_documentation ();
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
  _file_editor->handle_enter_debug_mode ();
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
  _file_editor->handle_exit_debug_mode ();
#endif
}

void
main_window::debug_continue ()
{
  octave_link::post_event (this, &main_window::debug_continue_callback);
}

void
main_window::debug_step_into ()
{
  octave_link::post_event (this, &main_window::debug_step_into_callback);
}

void
main_window::debug_step_over ()
{
  octave_link::post_event (this, &main_window::debug_step_over_callback);
}

void
main_window::debug_step_out ()
{
  octave_link::post_event (this, &main_window::debug_step_out_callback);
}

void
main_window::debug_quit ()
{
  octave_link::post_event (this, &main_window::debug_quit_callback);
}

void
main_window::show_about_octave ()
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
main_window::read_settings ()
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
      if (obj->inherits("QDockWidget") && ! name.isEmpty ())
        {
          QDockWidget *widget = qobject_cast<QDockWidget *> (obj);
          QVariant val = settings->value (name);
          widget->restoreGeometry (val.toByteArray ());
          bool floating = settings->value (name+"Floating",false).toBool ();
          bool visible = settings->value (name+"Visible",true).toBool ();
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
main_window::write_settings ()
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
          settings->setValue (name+"Floating",floating);  // store floating state
          settings->setValue (name+"Visible",visible);    // store visibility
          if (floating)
            widget->setWindowFlags(Qt::Widget); // if floating, recover the widget state such that the widget's
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
  settings->setValue ("MainWindow/current_directory_list",curr_dirs);
  settings->sync ();
}


// Connecting the signals emitted when the visibility of a widget changes.
// This has to be done after the window is shown (see octave-gui.cc)
void
main_window::connect_visibility_changed ()
{
  command_window.connect_visibility_changed ();
  history_window.connect_visibility_changed ();

  connect (_workspace_view,       SIGNAL (visibilityChanged (bool)),
           this,                  SLOT (handle_workspace_visible (bool)));
  connect (_files_dock_widget,    SIGNAL (visibilityChanged (bool)),
           this,                  SLOT (handle_current_directory_visible (bool)));
#ifdef HAVE_QSCINTILLA
  connect (_file_editor,          SIGNAL (visibilityChanged (bool)),
           this,                  SLOT (handle_editor_visible (bool)));
#endif
  connect (_documentation_dock_widget,  SIGNAL (visibilityChanged (bool)),
           this,                  SLOT (handle_documentation_visible (bool)));
}


// Main subroutine of the constructor
void
main_window::construct ()
{
  _closing = false;   // flag for editor files when closed
  setWindowIcon (QIcon(":/actions/icons/logo.png"));

  // Setup dockable widgets and the status bar.
  _workspace_view           = new workspace_view (this);

  _workspace_view->setModel (&_workspace_model);
  _workspace_view->setStatusTip (tr ("View the variables in the active workspace."));

  connect (&_workspace_model, SIGNAL (model_changed ()),
           _workspace_view, SLOT (model_changed ()));

  _files_dock_widget        = new files_dock_widget (this);
  _files_dock_widget->setStatusTip (tr ("Browse your files."));
  _documentation_dock_widget= new documentation_dock_widget (this);
  _documentation_dock_widget->setStatusTip (tr ("See the documentation for help."));

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
  current_directory_tool_button->setIcon (QIcon(":/actions/icons/search.png"));

  QToolButton *current_directory_up_tool_button = new QToolButton (this);
  current_directory_up_tool_button->setIcon (QIcon(":/actions/icons/up.png"));

  // Create and set the central widget.  QMainWindow takes ownership of
  // the widget (pointer) so there is no need to delete the object upon
  // destroying this main_window.
  QWidget *dummyWidget = new QWidget ();
  dummyWidget->setObjectName ("CentralDummyWidget");
  dummyWidget->resize (10, 10);
  dummyWidget->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Minimum);
  dummyWidget->hide ();
  setCentralWidget (dummyWidget);

#ifdef HAVE_QSCINTILLA
  _file_editor = new file_editor (this);
#endif

  QMenu *file_menu = menuBar ()->addMenu (tr ("&File"));

  QMenu *new_menu = file_menu->addMenu(tr ("New"));

  QAction *new_script_action
    = new_menu->addAction (QIcon(":/actions/icons/filenew.png"), tr ("Script"));
  new_script_action->setShortcut (QKeySequence::New);
  new_script_action->setShortcutContext (Qt::ApplicationShortcut);

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

  QAction *open_action
    = file_menu->addAction (QIcon(":/actions/icons/fileopen.png"), tr ("Open..."));
  open_action->setShortcut (QKeySequence::Open);
  open_action->setShortcutContext (Qt::ApplicationShortcut);

#ifdef HAVE_QSCINTILLA
  file_menu->addMenu(_file_editor->get_mru_menu ());
#endif

  QAction *close_command_window_action
    = file_menu->addAction (tr ("Close Command Window"));
  close_command_window_action->setShortcut (QKeySequence::Close);
  close_command_window_action->setEnabled (false); // TODO: Make this work.

  file_menu->addSeparator (); /////

  QAction *import_data_action
    = file_menu->addAction (tr ("Import Data"));
  import_data_action->setEnabled (false); // TODO: Make this work.

  QAction *save_workspace_action
    = file_menu->addAction (tr ("Save Workspace As"));

  file_menu->addSeparator (); /////

  QAction *preferences_action
    = file_menu->addAction (QIcon(":/actions/icons/configure.png"),
                            tr ("Preferences..."));

  file_menu->addSeparator (); /////

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

  file_menu->addSeparator (); /////

  QAction *exit_action = file_menu->addAction (tr ("Exit"));
  exit_action->setShortcut (QKeySequence::Quit);


  QMenu *edit_menu = menuBar ()->addMenu (tr ("&Edit"));
  QAction *undo_action
    = edit_menu->addAction (QIcon(":/actions/icons/undo.png"), tr ("Undo"));
  undo_action->setShortcut (QKeySequence::Undo);

  QAction *redo_action
    = edit_menu->addAction (QIcon(":/actions/icons/redo.png"), tr ("Redo"));
  redo_action->setShortcut (QKeySequence::Redo);

  edit_menu->addSeparator (); /////

  QAction *cut_action
    = edit_menu->addAction (QIcon(":/actions/icons/editcut.png"), tr ("Cut"));
  cut_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier + Qt::Key_X);

  QAction *copy_action
    = edit_menu->addAction (QIcon(":/actions/icons/editcopy.png"), tr ("Copy"));
  copy_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier + Qt::Key_C);

  QAction *paste_action
    = edit_menu->addAction (QIcon(":/actions/icons/editpaste.png"), tr ("Paste"));
  paste_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier + Qt::Key_V);

  QAction *paste_to_workspace_action
    = edit_menu->addAction (tr ("Paste To Workspace..."));
  paste_to_workspace_action->setEnabled (false); // TODO: Make this work.

  edit_menu->addSeparator (); /////

  QAction *select_all_action
    = edit_menu->addAction (tr ("Select All"));
  select_all_action->setEnabled (false); // TODO: Make this work.
  QAction *delete_action
    = edit_menu->addAction (tr ("Delete"));
  delete_action->setShortcut (Qt::Key_Delete);
  delete_action->setEnabled (false); // TODO: Make this work.

  edit_menu->addSeparator (); /////

  QAction *find_action
    = edit_menu->addAction (tr ("Find..."));
  find_action->setEnabled (false); // TODO: Make this work.
  QAction *find_files_action
    = edit_menu->addAction (tr ("Find Files..."));
  find_files_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier
                                  + Qt::Key_F);
  find_files_action->setEnabled (false); // TODO: Make this work.

  edit_menu->addSeparator (); /////

  QAction *clear_command_window_action
    = edit_menu->addAction (tr ("Clear Command Window"));
  clear_command_window_action->setEnabled (false); // TODO: Make this work.
  QAction *clear_command_history
    = edit_menu->addAction(tr ("Clear Command History"));
  QAction * clear_workspace_action
    = edit_menu->addAction (tr ("Clear Workspace"));

  _debug_menu = menuBar ()->addMenu (tr ("De&bug"));

  _debug_step_over = _debug_menu->addAction (QIcon (":/actions/icons/db_step.png"), tr ("Step"));
  _debug_step_over->setEnabled (false);
#ifdef HAVE_QSCINTILLA
  _file_editor->debug_menu ()->addAction (_debug_step_over);
  _file_editor->toolbar ()->addAction (_debug_step_over);
#endif
  _debug_step_over->setShortcut (Qt::Key_F10);

  _debug_step_into = _debug_menu->addAction (QIcon (":/actions/icons/db_step_in.png"), tr ("Step in"));
  _debug_step_into->setEnabled (false);
#ifdef HAVE_QSCINTILLA
  _file_editor->debug_menu ()->addAction (_debug_step_into);
  _file_editor->toolbar ()->addAction (_debug_step_into);
#endif
  _debug_step_into->setShortcut (Qt::Key_F11);

  _debug_step_out = _debug_menu->addAction (QIcon (":/actions/icons/db_step_out.png"), tr ("Step out"));
  _debug_step_out->setEnabled (false);
#ifdef HAVE_QSCINTILLA
  _file_editor->debug_menu ()->addAction (_debug_step_out);
  _file_editor->toolbar ()->addAction (_debug_step_out);
#endif
  _debug_step_out->setShortcut (Qt::ShiftModifier + Qt::Key_F11);

  _debug_continue = _debug_menu->addAction (QIcon (":/actions/icons/db_cont.png"), tr ("Continue"));
  _debug_continue->setEnabled (false);
#ifdef HAVE_QSCINTILLA
  _file_editor->debug_menu ()->addAction (_debug_continue);
  _file_editor->toolbar ()->addAction (_debug_continue);
#endif
  _debug_continue->setShortcut (Qt::Key_F5);

  _debug_menu->addSeparator (); /////
#ifdef HAVE_QSCINTILLA
  _file_editor->debug_menu ()->addSeparator (); /////
#endif

  _debug_quit = _debug_menu->addAction (QIcon (":/actions/icons/db_stop.png"), tr ("Exit Debug Mode"));
  _debug_quit->setEnabled (false);
#ifdef HAVE_QSCINTILLA
  _file_editor->debug_menu ()->addAction (_debug_quit);
  _file_editor->toolbar ()->addAction (_debug_quit);
#endif
  _debug_quit->setShortcut (Qt::ShiftModifier + Qt::Key_F5);

  //QMenu *parallelMenu = menuBar ()->addMenu (tr ("&Parallel"));

  QMenu *   desktop_menu = menuBar ()->addMenu (tr ("&Desktop"));
  QAction * load_workspace_action       = desktop_menu->addAction (tr ("Load workspace"));


  // Window menu
  QMenu *   window_menu = menuBar ()->addMenu (tr ("&Window"));
  QAction * show_command_window_action
    = window_menu->addAction (tr ("Show Command Window"));
  show_command_window_action->setCheckable (true);
  show_command_window_action->setShortcut (Qt::ControlModifier
                                           + Qt::ShiftModifier + Qt::Key_0);
  show_command_window_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * show_history_action
    = window_menu->addAction (tr ("Show Command History"));
  show_history_action->setCheckable (true);
  show_history_action->setShortcut (Qt::ControlModifier
                                    + Qt::ShiftModifier + Qt::Key_1);
  show_history_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * show_file_browser_action
    = window_menu->addAction (tr ("Show Current Directory"));
  show_file_browser_action->setCheckable (true);
  show_file_browser_action->setShortcut (Qt::ControlModifier
                                         + Qt::ShiftModifier + Qt::Key_2);
  show_file_browser_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * show_workspace_action
    = window_menu->addAction (tr ("Show Workspace"));
  show_workspace_action->setCheckable (true);
  show_workspace_action->setShortcut (Qt::ControlModifier
                                      + Qt::ShiftModifier + Qt::Key_3);
  show_workspace_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * show_editor_action = window_menu->addAction (tr ("Show Editor"));
  show_editor_action->setCheckable (true);
  show_editor_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier
                                   + Qt::Key_4);
  show_editor_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * show_documentation_action = window_menu->addAction (tr ("Show Documentation"));
  show_documentation_action->setCheckable (true);
  show_documentation_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier
                                          + Qt::Key_5);
  show_documentation_action->setShortcutContext (Qt::ApplicationShortcut);
  window_menu->addSeparator (); /////

  QAction * command_window_action
    = window_menu->addAction (tr ("Command Window"));
  command_window_action->setShortcut (Qt::ControlModifier + Qt::Key_0);
  command_window_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * history_action
    = window_menu->addAction (tr ("Command History"));
  history_action->setShortcut (Qt::ControlModifier + Qt::Key_1);
  history_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * file_browser_action
    = window_menu->addAction (tr ("Current Directory"));
  file_browser_action->setShortcut (Qt::ControlModifier + Qt::Key_2);
  file_browser_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * workspace_action
    = window_menu->addAction (tr ("Workspace"));
  workspace_action->setShortcut (Qt::ControlModifier + Qt::Key_3);
  workspace_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * editor_action
    = window_menu->addAction (tr ("Editor"));
  editor_action->setShortcut (Qt::ControlModifier + Qt::Key_4);
  editor_action->setShortcutContext (Qt::ApplicationShortcut);
  QAction * documentation_action
    = window_menu->addAction (tr ("Documentation"));
  documentation_action->setShortcut (Qt::ControlModifier + Qt::Key_5);
  documentation_action->setShortcutContext (Qt::ApplicationShortcut);

  window_menu->addSeparator (); /////

  QAction * reset_windows_action
    = window_menu->addAction (tr ("Reset Windows"));
  reset_windows_action->setEnabled (false); // TODO: Make this work.

  // Help menu
  QMenu *   help_menu = menuBar ()->addMenu (tr ("&Help"));
  QMenu * documentation_menu
    = help_menu->addMenu (tr ("Documentation"));
  QAction * ondisk_documentation_action
    = documentation_menu->addAction (tr ("On Disk"));
  QAction * online_documentation_action
    = documentation_menu->addAction (tr ("Online"));

  help_menu->addSeparator (); /////

  QAction * report_bug_action
    = help_menu->addAction (tr ("Report Bug"));
  QAction * octave_forge_action
    = help_menu->addAction (tr ("Visit Octave Forge"));
  QAction * agora_action
    = help_menu->addAction (tr ("Visit Agora"));

  help_menu->addSeparator (); /////

  QAction * about_octave_action
    = help_menu->addAction (tr ("About Octave"));

  // Toolbars
  QToolBar *main_tool_bar = addToolBar ("Main");
  main_tool_bar->setObjectName ("MainToolBar");
  main_tool_bar->addAction (new_script_action);
  main_tool_bar->addAction (open_action);

  main_tool_bar->addSeparator (); /////

  main_tool_bar->addAction (cut_action);
  main_tool_bar->addAction (copy_action);
  main_tool_bar->addAction (paste_action);
  main_tool_bar->addAction (undo_action);
  main_tool_bar->addAction (redo_action);

  main_tool_bar->addSeparator (); /////

  // addWidget takes ownership of the objects so there is no
  // need to delete these upon destroying this main_window.
  main_tool_bar->addWidget (new QLabel (tr ("Current Directory:")));
  main_tool_bar->addWidget (_current_directory_combo_box);
  main_tool_bar->addWidget (current_directory_tool_button);
  main_tool_bar->addWidget (current_directory_up_tool_button);

  connect (qApp,                        SIGNAL (aboutToQuit ()),
           this,                        SLOT   (prepare_for_quit ()));
  connect (preferences_action,          SIGNAL (triggered ()),
           this,                        SLOT   (process_settings_dialog_request ()));
  connect (exit_action,                 SIGNAL (triggered ()),
           this,                        SLOT   (close ()));
  connect (new_script_action,           SIGNAL (triggered ()),
           this,                        SLOT   (new_file ()));
  connect (open_action,                 SIGNAL (triggered ()),
           this,                        SLOT   (open_file ()));
  connect (online_documentation_action, SIGNAL (triggered ()),
           this,                        SLOT   (open_online_documentation_page ()));
  connect (report_bug_action,           SIGNAL (triggered ()),
           this,                        SLOT   (open_bug_tracker_page ()));
  connect (octave_forge_action,         SIGNAL (triggered ()),
           this,                        SLOT   (open_octave_forge_page ()));
  connect (agora_action,                SIGNAL (triggered ()),
           this,                        SLOT   (open_agora_page ()));
  connect (about_octave_action,         SIGNAL (triggered ()),
           this,                        SLOT   (show_about_octave ()));
  connect (show_command_window_action,  SIGNAL (toggled (bool)),
           &command_window,             SLOT   (setVisible (bool)));
  connect (&command_window,             SIGNAL (active_changed (bool)),
           show_command_window_action,  SLOT   (setChecked (bool)));
  connect (show_workspace_action,       SIGNAL (toggled (bool)),
           _workspace_view,             SLOT   (setVisible (bool)));
  connect (_workspace_view,             SIGNAL (active_changed (bool)),
           show_workspace_action,       SLOT   (setChecked (bool)));
  connect (show_history_action,         SIGNAL (toggled (bool)),
           &history_window,             SLOT   (setVisible (bool)));
  connect (&history_window,             SIGNAL (active_changed (bool)),
           show_history_action,         SLOT   (setChecked (bool)));
  connect (show_file_browser_action,    SIGNAL (toggled (bool)),
           _files_dock_widget,          SLOT   (setVisible (bool)));
  connect (_files_dock_widget,          SIGNAL (active_changed (bool)),
           show_file_browser_action,    SLOT   (setChecked (bool)));
#ifdef HAVE_QSCINTILLA
  connect (show_editor_action,          SIGNAL (toggled (bool)),
           _file_editor,                SLOT   (setVisible (bool)));
  connect (_file_editor,                SIGNAL (active_changed (bool)),
           show_editor_action,          SLOT   (setChecked (bool)));
#endif
  connect (show_documentation_action,   SIGNAL (toggled (bool)),
           _documentation_dock_widget,  SLOT   (setVisible (bool)));
  connect (_documentation_dock_widget,  SIGNAL (active_changed (bool)),
           show_documentation_action,   SLOT   (setChecked (bool)));

  connect (command_window_action,       SIGNAL (triggered ()),
           &command_window,             SLOT (focus ()));

  connect (workspace_action,            SIGNAL (triggered ()),
           this,                        SLOT (focus_workspace ()));
  connect (history_action,              SIGNAL (triggered ()),
           &history_window,             SLOT (focus ()));
  connect (file_browser_action,         SIGNAL (triggered ()),
           this,                        SLOT (focus_current_directory ()));
  connect (editor_action,               SIGNAL (triggered ()),
           this,                        SLOT (focus_editor ()));
  connect (documentation_action,        SIGNAL (triggered ()),
           this,                        SLOT (focus_documentation ()));
  connect (ondisk_documentation_action, SIGNAL (triggered ()),
           this,                        SLOT (focus_documentation ()));

  connect (reset_windows_action,        SIGNAL (triggered ()),
           this,                        SLOT   (reset_windows ()));
#ifdef HAVE_QSCINTILLA
  connect (this,                        SIGNAL (settings_changed (const QSettings *)),
           _file_editor,                SLOT   (notice_settings (const QSettings *)));
#endif
  connect (this,                        SIGNAL (settings_changed (const QSettings *)),
           &command_window,             SLOT   (notice_settings (const QSettings *)));
  connect (this,                        SIGNAL (settings_changed (const QSettings *)),
           _files_dock_widget,          SLOT   (notice_settings (const QSettings *)));
  connect (this,                        SIGNAL (settings_changed (const QSettings *)),
           this,                        SLOT   (notice_settings (const QSettings *)));
  connect (_files_dock_widget,          SIGNAL (open_file (QString)),
           this,                        SLOT   (open_file (QString)));
  connect (_files_dock_widget,          SIGNAL (displayed_directory_changed(QString)),
           this,                        SLOT   (set_current_working_directory(QString)));
  connect (this,                        SIGNAL (relay_command_signal (const QString&)),
           &command_window,             SLOT   (relay_command (const QString&)));
  connect (save_workspace_action,       SIGNAL (triggered ()),
           this,                        SLOT   (handle_save_workspace_request ()));
  connect (load_workspace_action,       SIGNAL (triggered ()),
           this,                        SLOT   (handle_load_workspace_request ()));
  connect (clear_workspace_action,      SIGNAL (triggered ()),
           this,                        SLOT   (handle_clear_workspace_request ()));
  connect (current_directory_tool_button, SIGNAL (clicked ()),
           this,                        SLOT   (browse_for_directory ()));
  connect (current_directory_up_tool_button, SIGNAL (clicked ()),
           this,                        SLOT   (change_directory_up ()));
  connect (copy_action,                 SIGNAL (triggered()),
           &command_window,             SLOT   (copyClipboard ()));
  connect (paste_action,                SIGNAL (triggered()),
           &command_window,             SLOT   (pasteClipboard ()));
  connect (_current_directory_combo_box, SIGNAL (activated (QString)),
           this,                        SLOT (set_current_working_directory (QString)));
  connect (_current_directory_line_edit, SIGNAL (returnPressed ()),
           this,                        SLOT (accept_directory_line_edit ()));
  connect (_debug_continue,             SIGNAL (triggered ()),
           this,                        SLOT (debug_continue ()));
  connect (_debug_step_into,            SIGNAL (triggered ()),
           this,                        SLOT (debug_step_into ()));
  connect (_debug_step_over,            SIGNAL (triggered ()),
           this,                        SLOT (debug_step_over ()));
  connect (_debug_step_out,             SIGNAL (triggered ()),
           this,                        SLOT (debug_step_out ()));
  connect (_debug_quit,                 SIGNAL (triggered ()),
           this,                        SLOT (debug_quit ()));

  connect (clear_command_history,       SIGNAL (triggered ()),
           this,                        SLOT (handle_clear_history_request ()));

  setWindowTitle ("Octave");
  setDockOptions(QMainWindow::AnimatedDocks | QMainWindow::AllowNestedDocks | QMainWindow::AllowTabbedDocks);
  addDockWidget (Qt::RightDockWidgetArea, &command_window);
  addDockWidget (Qt::RightDockWidgetArea, _documentation_dock_widget);
  tabifyDockWidget (&command_window, _documentation_dock_widget);
#ifdef HAVE_QSCINTILLA
  addDockWidget (Qt::RightDockWidgetArea, _file_editor);
  tabifyDockWidget (&command_window, _file_editor);
#endif
  addDockWidget (Qt::LeftDockWidgetArea, _files_dock_widget);
  addDockWidget (Qt::LeftDockWidgetArea, _workspace_view);
  addDockWidget (Qt::LeftDockWidgetArea, &history_window);

  int win_x = QApplication::desktop()->width();
  int win_y = QApplication::desktop()->height();
  if (win_x > 960)
    win_x = 960;
  if (win_y > 720)
    win_y = 720;
  setGeometry (0,0,win_x,win_y);

  setStatusBar (&status_bar);

  _octave_qt_event_listener = new octave_qt_event_listener ();

  connect (_octave_qt_event_listener,
           SIGNAL (update_workspace_signal ()),
           this,
           SLOT (update_workspace ()));

  // FIXME -- is it possible to eliminate the event_listenter?

  _octave_qt_link = new octave_qt_link ();

  connect (_octave_qt_link, SIGNAL (change_directory_signal (QString)),
           this, SLOT (change_directory (QString)));

  connect (_octave_qt_link,
           SIGNAL (set_history_signal (const QStringList&)),
           &history_window, SLOT (set_history (const QStringList&)));

  connect (_octave_qt_link,
           SIGNAL (append_history_signal (const QString&)),
           &history_window, SLOT (append_history (const QString&)));

  connect (_octave_qt_link,
           SIGNAL (clear_history_signal (void)),
           &history_window, SLOT (clear_history (void)));

  connect (_octave_qt_link, SIGNAL (enter_debugger_signal ()),
           this, SLOT (handle_enter_debugger ()));

  connect (_octave_qt_link, SIGNAL (exit_debugger_signal ()),
           this, SLOT (handle_exit_debugger ()));

  connect (_octave_qt_link,
           SIGNAL (update_breakpoint_marker_signal (bool, const QString&, int)),
           _file_editor,
           SLOT (handle_update_breakpoint_marker_request (bool, const QString&, int)));

  connect (_octave_qt_link,
           SIGNAL (edit_file_signal (const QString&)),
           _file_editor,
           SLOT (handle_edit_file_request (const QString&)));

  connect (_octave_qt_link,
           SIGNAL (insert_debugger_pointer_signal (const QString&, int)),
           _file_editor,
           SLOT (handle_insert_debugger_pointer_request (const QString&, int)));

  connect (_octave_qt_link,
           SIGNAL (delete_debugger_pointer_signal (const QString&, int)),
           _file_editor,
           SLOT (handle_delete_debugger_pointer_request (const QString&, int)));

  _octave_qt_link->execute_interpreter ();

  octave_link::connect_link (_octave_qt_link);

  octave_link::register_event_listener (_octave_qt_event_listener);

  QDir curr_dir;
  set_current_working_directory (curr_dir.absolutePath ());
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
