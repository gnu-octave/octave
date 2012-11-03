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
#include <QFileDialog>
#include <QMessageBox>
#include <QIcon>

#include "file-editor.h"
#include "main-window.h"
#include "octave-link.h"
#include "settings-dialog.h"

#include "builtins.h"
#include "defaults.h"
#include "load-save.h"
#include "toplev.h"
#include "version.h"

#include "cmd-hist.h"
#include "oct-env.h"

main_window::main_window (QWidget *p)
  : QMainWindow (p)
{
  // We have to set up all our windows, before we finally launch octave.
  construct ();
  octave_link::launch_octave ();
}

main_window::~main_window ()
{
}

void
main_window::new_file ()
{
  _file_editor->request_new_file ();
  focus_editor ();
}

void
main_window::open_file ()
{
  _file_editor->request_open_file ();
  focus_editor ();
}

void
main_window::open_file (const QString& file_name)
{
  _file_editor->request_open_file (file_name);
  focus_editor ();
}

void
main_window::report_status_message (const QString& statusMessage)
{
  _status_bar->showMessage (statusMessage, 1000);
}

void
main_window::handle_save_workspace_request ()
{
  QString selectedFile =
    QFileDialog::getSaveFileName (this, tr ("Save Workspace"),
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
main_window::handle_clear_history_request()
{
  octave_link::post_event (this, &main_window::clear_history_callback);
}

void
main_window::handle_command_double_clicked (const QString& command)
{
  _terminal->sendText (command);
  _terminal->setFocus ();
}

void
main_window::open_bug_tracker_page ()
{
  QDesktopServices::openUrl (QUrl ("http://bugs.octave.org"));
}

void
main_window::open_agora_page ()
{
  QDesktopServices::openUrl (QUrl ("http://agora.octave.org/"));
}

void
main_window::open_octave_forge_page ()
{
  QDesktopServices::openUrl (QUrl ("http://octave.sourceforge.net/"));
}

void
main_window::process_settings_dialog_request ()
{
  settings_dialog *settingsDialog = new settings_dialog (this);
  int change_settings = settingsDialog->exec ();
  if (change_settings == QDialog::Accepted)
    {
      settingsDialog->write_changed_settings ();
      emit settings_changed ();
    }
  delete settingsDialog;
}

void
main_window::notice_settings ()
{
  // Set terminal font:
  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

  QFont term_font = QFont();
  term_font.setFamily(settings->value("terminal/fontName").toString());
  term_font.setPointSize(settings->value("terminal/fontSize").toInt ());
  _terminal->setTerminalFont (term_font);

  QString cursorType = settings->value ("terminal/cursorType").toString ();
  bool cursorBlinking = settings->value ("terminal/cursorBlinking").toBool ();
  if (cursorType == "ibeam")
    _terminal->setCursorType(QTerminalInterface::IBeamCursor, cursorBlinking);
  else if (cursorType == "block")
    _terminal->setCursorType(QTerminalInterface::BlockCursor, cursorBlinking);
  else if (cursorType == "underline")
    _terminal->setCursorType(QTerminalInterface::UnderlineCursor,
                             cursorBlinking);

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
main_window::current_working_directory_has_changed (const QString& directory)
{
  int index = _current_directory_combo_box->findText (directory);
  if ( index >= 0 )  // directory already in list -> remove it
    { 
      _current_directory_combo_box->removeItem (index);
    }
  _current_directory_combo_box->insertItem (0,directory);  // add (on top)
  _current_directory_combo_box->setCurrentIndex (0);  // top is actual
  _files_dock_widget->set_current_directory (directory);
}

void
main_window::change_current_working_directory ()
{
  QString selectedDirectory =
    QFileDialog::getExistingDirectory(this, tr ("Set working direcotry"));

  if (!selectedDirectory.isEmpty ())
    octave_link::post_event (this, &main_window::change_directory_callback,
                             selectedDirectory.toStdString ());
}

void
main_window::set_current_working_directory (const QString& directory)
{
  octave_link::post_event (this, &main_window::change_directory_callback,
                           directory.toStdString ());
}

void
main_window::current_working_directory_up ()
{
  set_current_working_directory ("..");
}

void
main_window::focus_command_window ()
{
  if (!_terminal_dock_widget->isVisible ())
    {
      _terminal_dock_widget->setVisible (true);
    }

  _terminal_dock_widget->setFocus ();
  _terminal_dock_widget->activateWindow ();
  _terminal_dock_widget->raise ();

  _terminal->setFocus ();
  _terminal->activateWindow ();
  _terminal->raise ();
}

void
main_window::focus_command_history ()
{
  if (!_history_dock_widget->isVisible ())
    {
      _history_dock_widget->setVisible (true);
    }

  _history_dock_widget->setFocus ();
  _history_dock_widget->activateWindow ();
  _history_dock_widget->raise ();
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
  if (!_file_editor->isVisible ())
    {
      _file_editor->setVisible (true);
    }

  _file_editor->setFocus ();
  _file_editor->activateWindow ();
  _file_editor->raise ();
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
main_window::handle_entered_debug_mode ()
{
  setWindowTitle ("Octave (Debugging)");
  _debug_continue->setEnabled (true);
  _debug_step_into->setEnabled (true);
  _debug_step_over->setEnabled (true);
  _debug_step_out->setEnabled (true);
  _debug_quit->setEnabled (true);
  _file_editor->handle_entered_debug_mode ();
}

void
main_window::handle_quit_debug_mode ()
{
  setWindowTitle ("Octave");
  _debug_continue->setEnabled (false);
  _debug_step_into->setEnabled (false);
  _debug_step_over->setEnabled (false);
  _debug_step_out->setEnabled (false);
  _debug_quit->setEnabled (false);
  _file_editor->handle_quit_debug_mode ();
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

  // FIXME -- what should happen if settings is 0?

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
  emit settings_changed ();
}

void
main_window::write_settings ()
{
  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

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

void
main_window::construct ()
{
  _closing = false;   // flag for editor files when closed
  setWindowIcon (QIcon(":/actions/icons/logo.png"));

  // Setup dockable widgets and the status bar.
  _workspace_view           = new workspace_view (this);
  _workspace_view->setStatusTip (tr ("View the variables in the active workspace."));
  _history_dock_widget      = new history_dock_widget (this);
  _history_dock_widget->setStatusTip (tr ("Browse and search the command history."));
  _files_dock_widget        = new files_dock_widget (this);
  _files_dock_widget->setStatusTip (tr ("Browse your files."));
  _documentation_dock_widget= new documentation_dock_widget (this);
  _documentation_dock_widget->setStatusTip (tr ("See the documentation for help."));
  _status_bar               = new QStatusBar (this);

  _current_directory_combo_box = new QComboBox (this);
  _current_directory_combo_box->setFixedWidth (300);
  _current_directory_combo_box->setEditable (true);
  _current_directory_combo_box->setInsertPolicy (QComboBox::InsertAtTop);
  _current_directory_combo_box->setMaxVisibleItems (16);
  _current_directory_combo_box->setMaxCount (16);

  _current_directory_tool_button = new QToolButton (this);
  _current_directory_tool_button->setIcon (QIcon(":/actions/icons/search.png"));

  _current_directory_up_tool_button = new QToolButton (this);
  _current_directory_up_tool_button->setIcon (QIcon(":/actions/icons/up.png"));

  // Octave Terminal subwindow.
  _terminal = new QTerminal (this);
  _terminal->setObjectName ("OctaveTerminal");
  _terminal->setFocusPolicy (Qt::StrongFocus);
  _terminal_dock_widget = new terminal_dock_widget (_terminal, this);

  QWidget *dummyWidget = new QWidget ();
  dummyWidget->setObjectName ("CentralDummyWidget");
  dummyWidget->resize (10, 10);
  dummyWidget->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Minimum);
  dummyWidget->hide ();
  setCentralWidget (dummyWidget);

  _file_editor = new file_editor (_terminal, this);

  QMenu *file_menu = menuBar ()->addMenu (tr ("&File"));

  QMenu *new_menu = file_menu->addMenu(tr ("New"));

  QAction *new_script_action
    = new_menu->addAction (QIcon(":/actions/icons/filenew.png"), tr ("Script"));
  new_script_action->setShortcut (Qt::ControlModifier + Qt::Key_N);

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
  open_action->setShortcut (Qt::ControlModifier + Qt::Key_O);

  QAction *close_command_window_action
    = file_menu->addAction (tr ("Close Command Window"));
  close_command_window_action->setShortcut (Qt::ControlModifier + Qt::Key_W);
  close_command_window_action->setEnabled (false); // TODO: Make this work.

  file_menu->addSeparator ();
  QAction *import_data_action
    = file_menu->addAction (tr ("Import Data..."));
  import_data_action->setEnabled (false); // TODO: Make this work.

  QAction *save_workspace_action
    = file_menu->addAction (tr ("Save Workspace..."));
  save_workspace_action->setShortcut (Qt::ControlModifier + Qt::Key_S);
  file_menu->addSeparator ();

  QAction *preferences_action
    = file_menu->addAction (QIcon(":/actions/icons/configure.png"),
                            tr ("Preferences..."));
  file_menu->addSeparator ();
  QAction *page_setup_action
    = file_menu->addAction (tr ("Page Setup..."));
  page_setup_action->setEnabled (false); // TODO: Make this work.
  QAction *print_action
    = file_menu->addAction (tr ("Print"));
  print_action->setShortcut (Qt::ControlModifier + Qt::Key_P);
  print_action->setEnabled (false); // TODO: Make this work.
  QAction *print_selection_action
    = file_menu->addAction (tr ("Print Selection..."));
  print_selection_action->setEnabled (false); // TODO: Make this work.

  file_menu->addSeparator ();
  QAction *exit_action = file_menu->addAction (tr ("Exit"));
  exit_action->setShortcut (Qt::ControlModifier + Qt::Key_Q);


  QMenu *edit_menu = menuBar ()->addMenu (tr ("&Edit"));
  QAction *undo_action
    = edit_menu->addAction (QIcon(":/actions/icons/undo.png"), tr ("Undo"));
  undo_action->setShortcut (QKeySequence::Undo);

  QAction *redo_action
    = edit_menu->addAction (QIcon(":/actions/icons/redo.png"), tr ("Redo"));
  redo_action->setShortcut (QKeySequence::Redo);
  edit_menu->addSeparator ();

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
  find_files_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier
                                  + Qt::Key_F);
  find_files_action->setEnabled (false); // TODO: Make this work.
  edit_menu->addSeparator ();

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
  _file_editor->debug_menu ()->addAction (_debug_step_over);
  _file_editor->toolbar ()->addAction (_debug_step_over);
  _debug_step_over->setShortcut (Qt::Key_F10);

  _debug_step_into = _debug_menu->addAction (QIcon (":/actions/icons/db_step_in.png"), tr ("Step in"));
  _debug_step_into->setEnabled (false);
  _file_editor->debug_menu ()->addAction (_debug_step_into);
  _file_editor->toolbar ()->addAction (_debug_step_into);
  _debug_step_into->setShortcut (Qt::Key_F11);

  _debug_step_out = _debug_menu->addAction (QIcon (":/actions/icons/db_step_out.png"), tr ("Step out"));
  _debug_step_out->setEnabled (false);
  _file_editor->debug_menu ()->addAction (_debug_step_out);
  _file_editor->toolbar ()->addAction (_debug_step_out);
  _debug_step_out->setShortcut (Qt::ShiftModifier + Qt::Key_F11);

  _debug_continue = _debug_menu->addAction (QIcon (":/actions/icons/db_cont.png"), tr ("Continue"));
  _debug_continue->setEnabled (false);
  _file_editor->debug_menu ()->addAction (_debug_continue);
  _file_editor->toolbar ()->addAction (_debug_continue);
  _debug_continue->setShortcut (Qt::Key_F5);

  _debug_menu->addSeparator ();
  _file_editor->debug_menu ()->addSeparator ();

  _debug_quit = _debug_menu->addAction (QIcon (":/actions/icons/db_stop.png"), tr ("Exit Debug Mode"));
  _debug_quit->setEnabled (false);
  _file_editor->debug_menu ()->addAction (_debug_quit);
  _file_editor->toolbar ()->addAction (_debug_quit);
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

  QAction * show_history_action
    = window_menu->addAction (tr ("Show Command History"));
  show_history_action->setCheckable (true);
  show_history_action->setShortcut (Qt::ControlModifier
                                    + Qt::ShiftModifier + Qt::Key_1);
  QAction * show_file_browser_action
    = window_menu->addAction (tr ("Show Current Directory"));
  show_file_browser_action->setCheckable (true);
  show_file_browser_action->setShortcut (Qt::ControlModifier
                                         + Qt::ShiftModifier + Qt::Key_2);

  QAction * show_workspace_action
    = window_menu->addAction (tr ("Show Workspace"));
  show_workspace_action->setCheckable (true);
  show_workspace_action->setShortcut (Qt::ControlModifier
                                      + Qt::ShiftModifier + Qt::Key_3);

  QAction * show_editor_action = window_menu->addAction (tr ("Show Editor"));
  show_editor_action->setCheckable (true);
  show_editor_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier
                                   + Qt::Key_4);

  QAction * show_documentation_action = window_menu->addAction (tr ("Show Documentation"));
  show_documentation_action->setCheckable (true);
  show_documentation_action->setShortcut (Qt::ControlModifier + Qt::ShiftModifier
                                          + Qt::Key_5);
  window_menu->addSeparator ();

  QAction * command_window_action
    = window_menu->addAction (tr ("Command Window"));
  command_window_action->setShortcut (Qt::ControlModifier + Qt::Key_0);

  QAction * history_action
    = window_menu->addAction (tr ("Command History"));
  history_action->setShortcut (Qt::ControlModifier + Qt::Key_1);

  QAction * file_browser_action
    = window_menu->addAction (tr ("Current Directory"));
  file_browser_action->setShortcut (Qt::ControlModifier + Qt::Key_2);

  QAction * workspace_action
    = window_menu->addAction (tr ("Workspace"));
  workspace_action->setShortcut (Qt::ControlModifier + Qt::Key_3);

  QAction * editor_action
    = window_menu->addAction (tr ("Editor"));
  editor_action->setShortcut (Qt::ControlModifier + Qt::Key_4);

  QAction * documentation_action
    = window_menu->addAction (tr ("Documentation"));
  documentation_action->setShortcut (Qt::ControlModifier + Qt::Key_5);

  window_menu->addSeparator ();
  QAction * reset_windows_action
    = window_menu->addAction (tr ("Reset Windows"));
  reset_windows_action->setEnabled (false); // TODO: Make this work.

  // Help menu
  QMenu *   help_menu = menuBar ()->addMenu (tr ("&Help"));
  QAction * report_bug_action
    = help_menu->addAction (tr ("Report Bug"));
  QAction * agora_action
    = help_menu->addAction (tr ("Visit Agora"));
  QAction * octave_forge_action
    = help_menu->addAction (tr ("Visit Octave Forge"));
  help_menu->addSeparator ();

  QAction * about_octave_action
    = help_menu->addAction (tr ("About Octave"));

  // Toolbars
  QToolBar *main_tool_bar = addToolBar ("Main");
  main_tool_bar->addAction (new_script_action);
  main_tool_bar->addAction (open_action);
  main_tool_bar->addSeparator ();
  main_tool_bar->addAction (cut_action);
  main_tool_bar->addAction (copy_action);
  main_tool_bar->addAction (paste_action);
  main_tool_bar->addAction (undo_action);
  main_tool_bar->addAction (redo_action);
  main_tool_bar->addSeparator ();
  main_tool_bar->addWidget (new QLabel (tr ("Current Directory:")));
  main_tool_bar->addWidget (_current_directory_combo_box);
  main_tool_bar->addWidget (_current_directory_tool_button);
  main_tool_bar->addWidget (_current_directory_up_tool_button);

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
  connect (report_bug_action,           SIGNAL (triggered ()),
           this,                        SLOT   (open_bug_tracker_page ()));
  connect (agora_action,                SIGNAL (triggered ()),
           this,                        SLOT   (open_agora_page ()));
  connect (octave_forge_action,         SIGNAL (triggered ()),
           this,                        SLOT   (open_octave_forge_page ()));
  connect (about_octave_action,         SIGNAL (triggered ()),
           this,                        SLOT   (show_about_octave ()));
  connect (show_command_window_action,  SIGNAL (toggled (bool)),
           _terminal_dock_widget,       SLOT   (setVisible (bool)));
  connect (_terminal_dock_widget,       SIGNAL (active_changed (bool)),
           show_command_window_action,  SLOT   (setChecked (bool)));
  connect (show_workspace_action,       SIGNAL (toggled (bool)),
           _workspace_view,             SLOT   (setVisible (bool)));
  connect (_workspace_view,             SIGNAL (active_changed (bool)),
           show_workspace_action,       SLOT   (setChecked (bool)));
  connect (show_history_action,         SIGNAL (toggled (bool)),
           _history_dock_widget,        SLOT   (setVisible (bool)));
  connect (_history_dock_widget,        SIGNAL (active_changed (bool)),
           show_history_action,         SLOT   (setChecked (bool)));
  connect (show_file_browser_action,    SIGNAL (toggled (bool)),
           _files_dock_widget,          SLOT   (setVisible (bool)));
  connect (_files_dock_widget,          SIGNAL (active_changed (bool)),
           show_file_browser_action,    SLOT   (setChecked (bool)));
  connect (show_editor_action,          SIGNAL (toggled (bool)),
           _file_editor,                SLOT   (setVisible (bool)));
  connect (_file_editor,                SIGNAL (active_changed (bool)),
           show_editor_action,          SLOT   (setChecked (bool)));
  connect (show_documentation_action,   SIGNAL (toggled (bool)),
           _documentation_dock_widget,  SLOT   (setVisible (bool)));
  connect (_documentation_dock_widget,  SIGNAL (active_changed (bool)),
           show_documentation_action,   SLOT   (setChecked (bool)));

  connect (command_window_action,       SIGNAL (triggered ()),
           this,                        SLOT (focus_command_window ()));
  connect (workspace_action,            SIGNAL (triggered ()),
           this,                        SLOT (focus_workspace ()));
  connect (history_action,              SIGNAL (triggered ()),
           this,                        SLOT (focus_command_history ()));
  connect (file_browser_action,         SIGNAL (triggered ()),
           this,                        SLOT (focus_current_directory ()));
  connect (editor_action,               SIGNAL (triggered ()),
           this,                        SLOT (focus_editor ()));
  connect (documentation_action,        SIGNAL (triggered ()),
           this,                        SLOT (focus_documentation ()));

  connect (reset_windows_action,        SIGNAL (triggered ()),
           this,                        SLOT   (reset_windows ()));
  connect (this,                        SIGNAL (settings_changed ()),
           _file_editor,                SLOT   (notice_settings ()));
  connect (this,                        SIGNAL (settings_changed ()),
           _files_dock_widget,          SLOT   (notice_settings ()));
  connect (this,                        SIGNAL (settings_changed ()),
           this,                        SLOT   (notice_settings ()));
  connect (_files_dock_widget,          SIGNAL (open_file (QString)),
           this,                        SLOT   (open_file (QString)));
  connect (_files_dock_widget,          SIGNAL (displayed_directory_changed(QString)),
           this,                        SLOT   (set_current_working_directory(QString)));
  connect (_history_dock_widget,        SIGNAL (information (QString)),
           this,                        SLOT   (report_status_message (QString)));
  connect (_history_dock_widget,        SIGNAL (command_double_clicked (QString)),
           this,                        SLOT   (handle_command_double_clicked (QString)));
  connect (save_workspace_action,       SIGNAL (triggered ()),
           this,                        SLOT   (handle_save_workspace_request ()));
  connect (load_workspace_action,       SIGNAL (triggered ()),
           this,                        SLOT   (handle_load_workspace_request ()));
  connect (clear_workspace_action,      SIGNAL (triggered ()),
           this,                        SLOT   (handle_clear_workspace_request ()));
  connect (_current_directory_tool_button, SIGNAL (clicked ()),
           this,                        SLOT   (change_current_working_directory ()));
  connect (_current_directory_up_tool_button, SIGNAL (clicked ()),
           this,                        SLOT   (current_working_directory_up()));
  connect (copy_action,                 SIGNAL (triggered()),
           _terminal,                   SLOT   (copyClipboard ()));
  connect (paste_action,                SIGNAL (triggered()),
           _terminal,                   SLOT   (pasteClipboard ()));
  connect (_current_directory_combo_box, SIGNAL (activated (QString)),
           this,                        SLOT (set_current_working_directory (QString)));
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
  addDockWidget (Qt::LeftDockWidgetArea, _workspace_view);
  addDockWidget (Qt::LeftDockWidgetArea, _history_dock_widget);
  addDockWidget (Qt::RightDockWidgetArea, _files_dock_widget);
  addDockWidget (Qt::RightDockWidgetArea, _file_editor);
  addDockWidget (Qt::BottomDockWidgetArea, _terminal_dock_widget);
  addDockWidget (Qt::RightDockWidgetArea, _documentation_dock_widget);
  setStatusBar (_status_bar);

  _octave_qt_event_listener = new octave_qt_event_listener ();
  octave_link::register_event_listener (_octave_qt_event_listener);

  connect (_octave_qt_event_listener,
           SIGNAL (current_directory_has_changed_signal (QString)),
           this,
           SLOT (current_working_directory_has_changed (QString)));

  connect (_octave_qt_event_listener,
           SIGNAL (entered_debug_mode_signal ()),
           this,
           SLOT(handle_entered_debug_mode ()));

  connect (_octave_qt_event_listener,
           SIGNAL (quit_debug_mode_signal ()),
           this,
           SLOT (handle_quit_debug_mode ()));
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
  command_history::clear ();

  _history_dock_widget->reset_model ();
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
}

void
main_window::debug_step_into_callback (void)
{
  Fdbstep (ovl ("in"));
}

void
main_window::debug_step_over_callback (void)
{
  Fdbstep ();
}

void
main_window::debug_step_out_callback (void)
{
  Fdbstep (ovl ("out"));
}

void
main_window::debug_quit_callback (void)
{
  Fdbquit ();
}

void
main_window::exit_callback (void)
{
  Fquit ();
}
