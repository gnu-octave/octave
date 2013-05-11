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

#ifdef HAVE_QSCINTILLA

#include "file-editor.h"
#include "resource-manager.h"
#include <QVBoxLayout>
#include <QApplication>
#include <QFile>
#include <QFont>
#include <QFileDialog>
#include <QMessageBox>
#include <QStyle>
#include <QTextStream>
#include <QProcess>

#include "octave-link.h"
#include "utils.h"

file_editor::file_editor (QWidget *p)
  : file_editor_interface (p)
{
  // Set current editing directory before construct because loaded
  // files will change ced accordingly.
  ced = QDir::currentPath ();

  construct ();

  setVisible (false);
}

file_editor::~file_editor (void)
{
  QSettings *settings = resource_manager::get_settings ();

  editor_tab_map.clear ();

  if (settings->value ("editor/restoreSession", true).toBool ())
    {
      // Have all file editor tabs signal what their file names are.
      emit fetab_file_name_query (0);
    }
  QStringList fetFileNames;
  for (editor_tab_map_const_iterator p = editor_tab_map.begin ();
       p != editor_tab_map.end (); p++)
    fetFileNames.append (p->first);

  settings->setValue ("editor/savedSessionTabs", fetFileNames);
  settings->sync ();

  if (_mru_file_menu)
    delete _mru_file_menu;
}

void
file_editor::focus (void)
{
  set_focus ();
}

void
file_editor::handle_visibility (bool visible)
{
  if (visible && ! isFloating ())
    focus ();
}

void
file_editor::connect_visibility_changed (void)
{
  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility (bool)));
}

// set focus to editor and its current tab
void
file_editor::set_focus (void)
{
  if (!isVisible ())
    setVisible (true);
  setFocus ();
  activateWindow ();
  raise ();
  QWidget *fileEditorTab = _tab_widget->currentWidget ();
  if (fileEditorTab)
    emit fetab_set_focus (fileEditorTab);
}

QMenu *
file_editor::debug_menu (void)
{
  return _debug_menu;
}

QToolBar *
file_editor::toolbar (void)
{
  return _tool_bar;
}

void
file_editor::handle_enter_debug_mode (void)
{
  _run_action->setEnabled (false);
}

void
file_editor::handle_exit_debug_mode (void)
{
  _run_action->setEnabled (true);
}

void
file_editor::request_new_file (const QString& commands)
{
  // New file isn't a file_editor_tab function since the file
  // editor tab has yet to be created and there is no object to
  // pass a signal to.  Hence, functionality is here.

  file_editor_tab *fileEditorTab = new file_editor_tab (ced);
  if (fileEditorTab)
    {
      add_file_editor_tab (fileEditorTab, "");  // new tab with empty title
      fileEditorTab->new_file (commands);       // title is updated here
      set_focus ();                             // focus editor and new tab
    }
}

void
file_editor::request_new_script (const QString& commands)
{
  request_new_file (commands);
}

void
file_editor::request_new_function (const QString& commands)
{
  QString text = commands;

  if (text.isEmpty ())
    text = "## Copyright (C)\n"
      "\n"
      "## -*- texinfo -*-\n"
      "## @deftypefn {Function File} {[outputs] =} unamed_function (inputs)\n"
      "## @end deftypefn\n"
      "\n"
      "function [outputs] = unnamed_function (inputs)\n"
      "\n"
      "endfunction\n";

  request_new_file (text);
}

void
file_editor::request_open_file (void)
{
  // Open file isn't a file_editor_tab function since the file
  // editor tab has yet to be created and there is no object to
  // pass a signal to.  Hence, functionality is here.

  // Create a NonModal message.
  QFileDialog *fileDialog = new QFileDialog (this);
  fileDialog->setNameFilter (tr ("Octave Files (*.m);;All Files (*)"));
  fileDialog->setAcceptMode (QFileDialog::AcceptOpen);
  fileDialog->setViewMode (QFileDialog::Detail);
  fileDialog->setDirectory (ced);

  connect (fileDialog, SIGNAL (fileSelected (const QString&)),
           this, SLOT (request_open_file (const QString&)));

  fileDialog->setWindowModality (Qt::NonModal);
  fileDialog->setAttribute (Qt::WA_DeleteOnClose);
  fileDialog->show ();
}

// Check whether this file is already open in the editor.
QWidget *
file_editor::find_tab_widget (const QString& file) const
{
  QWidget *retval = 0;

  for (editor_tab_map_const_iterator p = editor_tab_map.begin ();
       p != editor_tab_map.end (); p++)
    {
      QString tab_file = p->first;

      if (same_file (file.toStdString (), tab_file.toStdString ()))
        {
          retval = p->second;
          break;
        }
    }

  return retval;
}    

void
file_editor::request_open_file (const QString& openFileName, int line,
                                bool debug_pointer,
                                bool breakpoint_marker, bool insert)
{
  // Check if the user wants to use a custom file editor.
  QSettings *settings = resource_manager::get_settings ();
  if (settings->value ("useCustomFileEditor").toBool ())
    {
      QString editor = settings->value ("customFileEditor").toString ();
      editor.replace ("%f", openFileName);
      editor.replace ("%l", QString::number (line));
      QProcess::startDetached (editor);
      if (line < 0)
        handle_mru_add_file (QDir::cleanPath (openFileName));
      return;
    }

  if (openFileName.isEmpty ())
    {
      // ??  Not sure this will happen.  This routine isn't even called
      // if the user hasn't selected a file.
    }
  else
    {
      // Have all file editor tabs signal what their file names are.
      editor_tab_map.clear ();
      emit fetab_file_name_query (0);

      // Check whether this file is already open in the editor.
      QWidget *tab = find_tab_widget (openFileName);

      if (tab)
        {
          _tab_widget->setCurrentWidget (tab);

          if (line > 0)
            {
              emit fetab_goto_line (tab, line);

              if (debug_pointer)
                emit fetab_insert_debugger_pointer (tab, line);

              if (breakpoint_marker)
                emit fetab_do_breakpoint_marker (insert, tab, line);
            }

          emit fetab_set_focus (tab);
        }
      else
        {
          file_editor_tab *fileEditorTab = new file_editor_tab ();
          if (fileEditorTab)
            {
              QString result = fileEditorTab->load_file (openFileName);
              if (result == "")
                {
                  // Supply empty title then have the file_editor_tab update
                  // with full or short name.
                  add_file_editor_tab (fileEditorTab, "");
                  fileEditorTab->update_window_title (false);
                  // file already loaded, add file to mru list here
                  handle_mru_add_file (QDir::cleanPath (openFileName));

                  if (line > 0)
                    {
                      emit fetab_goto_line (fileEditorTab, line);

                      if (debug_pointer)
                        emit fetab_insert_debugger_pointer (fileEditorTab,
                                                            line);
                      if (breakpoint_marker)
                        emit fetab_do_breakpoint_marker (insert, fileEditorTab,
                                                         line);
                    }
                }
              else
                {
                  delete fileEditorTab;
                  // Create a NonModal message about error.
                  QMessageBox *msgBox
                    = new QMessageBox (QMessageBox::Critical,
                                       tr ("Octave Editor"),
                                       tr ("Could not open file %1 for read:\n%2.").
                                       arg (openFileName).arg (result),
                                       QMessageBox::Ok, 0);

                  msgBox->setWindowModality (Qt::NonModal);
                  msgBox->setAttribute (Qt::WA_DeleteOnClose);
                  msgBox->show ();
                }
            }

          // really show editor and the current editor tab
          set_focus ();
        }
    }
}

// open a file from the mru list
void
file_editor::request_mru_open_file (QAction *action)
{
  if (action)
    {
      request_open_file (action->data ().toString ());
    }
}


void
file_editor::check_conflict_save (const QString& saveFileName, bool remove_on_success)
{
  // Have all file editor tabs signal what their file names are.
  editor_tab_map.clear ();
  emit fetab_file_name_query (0);

  // Check whether this file is already open in the editor.
  QWidget *tab = find_tab_widget (saveFileName);

  if (tab)
    {
      // Note: to overwrite the contents of some other file editor tab
      // with the same name requires identifying which file editor tab
      // that is (not too difficult) then close that tab.  Of course,
      // that could trigger another dialog box if the file editor tab
      // with the same name has modifications in it.  This could become
      // somewhat confusing to the user.  For now, opt to do nothing.

      // Create a NonModal message about error.
      QMessageBox *msgBox
        = new QMessageBox (QMessageBox::Critical, tr ("Octave Editor"),
                           tr ("File not saved! A file with the selected name\n%1\n"
                               "is already open in the editor").
                           arg (saveFileName),
                           QMessageBox::Ok, 0);

      msgBox->setWindowModality (Qt::NonModal);
      msgBox->setAttribute (Qt::WA_DeleteOnClose);
      msgBox->show ();

      return;
    }

  QObject *saveFileObject = sender ();
  QWidget *saveFileWidget = 0;

  for (int i = 0; i < _tab_widget->count (); i++)
    {
      if (_tab_widget->widget (i) == saveFileObject)
        {
          saveFileWidget = _tab_widget->widget (i);
          break;
        }
    }
  if (!saveFileWidget)
    {
      // Create a NonModal message about error.
      QMessageBox *msgBox
        = new QMessageBox (QMessageBox::Critical, tr ("Octave Editor"),
                           tr ("The associated file editor tab has disappeared.  It was likely closed by some means."),
                           QMessageBox::Ok, 0);

      msgBox->setWindowModality (Qt::NonModal);
      msgBox->setAttribute (Qt::WA_DeleteOnClose);
      msgBox->show ();

      return;
    }

  // Can save without conflict, have the file editor tab do so.
  emit fetab_save_file (saveFileWidget, saveFileName, remove_on_success);
}

void
file_editor::handle_insert_debugger_pointer_request (const QString& file,
                                                     int line)
{
  request_open_file (file, line, true);
}

void
file_editor::handle_delete_debugger_pointer_request (const QString& file,
                                                     int line)
{
  if (! file.isEmpty ())
    {
      // Have all file editor tabs signal what their file names are.
      editor_tab_map.clear ();
      emit fetab_file_name_query (0);

      // Check whether this file is already open in the editor.
      QWidget *tab = find_tab_widget (file);

      if (tab)
        {
          _tab_widget->setCurrentWidget (tab);

          if (line > 0)
            emit fetab_delete_debugger_pointer (tab, line);

          emit fetab_set_focus (tab);
        }
    }
}

void
file_editor::handle_update_breakpoint_marker_request (bool insert,
                                                      const QString& file,
                                                      int line)
{
  request_open_file (file, line, false, true, insert);
}

void
file_editor::handle_edit_file_request (const QString& file)
{
  request_open_file (file);
}

void
file_editor::request_undo (void)
{
  emit fetab_undo (_tab_widget->currentWidget ());
}

void
file_editor::request_redo (void)
{
  emit fetab_redo (_tab_widget->currentWidget ());
}

void
file_editor::request_copy (void)
{
  emit fetab_copy (_tab_widget->currentWidget ());
}

void
file_editor::request_cut (void)
{
  emit fetab_cut (_tab_widget->currentWidget ());
}

void
file_editor::request_paste (void)
{
  emit fetab_paste (_tab_widget->currentWidget ());
}

void
file_editor::request_save_file (void)
{
  emit fetab_save_file (_tab_widget->currentWidget ());
}

void
file_editor::request_save_file_as (void)
{
   emit fetab_save_file_as (_tab_widget->currentWidget ());
}

void
file_editor::request_print_file (void)
{
   emit fetab_print_file (_tab_widget->currentWidget ());
}


void
file_editor::request_run_file (void)
{
  emit fetab_run_file (_tab_widget->currentWidget ());
}

void
file_editor::request_toggle_bookmark (void)
{
  emit fetab_toggle_bookmark (_tab_widget->currentWidget ());
}

void
file_editor::request_next_bookmark (void)
{
  emit fetab_next_bookmark (_tab_widget->currentWidget ());
}

void
file_editor::request_previous_bookmark (void)
{
  emit fetab_previous_bookmark (_tab_widget->currentWidget ());
}

void
file_editor::request_remove_bookmark (void)
{
  emit fetab_remove_bookmark (_tab_widget->currentWidget ());
}

void
file_editor::request_toggle_breakpoint (void)
{
  emit fetab_toggle_breakpoint (_tab_widget->currentWidget ());
}

void
file_editor::request_next_breakpoint (void)
{
  emit fetab_next_breakpoint (_tab_widget->currentWidget ());
}

void
file_editor::request_previous_breakpoint (void)
{
  emit fetab_previous_breakpoint (_tab_widget->currentWidget ());
}

void
file_editor::request_remove_breakpoint (void)
{
  emit fetab_remove_all_breakpoints (_tab_widget->currentWidget ());
}

void
file_editor::request_comment_selected_text (void)
{
  emit fetab_comment_selected_text (_tab_widget->currentWidget ());
}

void
file_editor::request_uncomment_selected_text (void)
{
  emit fetab_uncomment_selected_text (_tab_widget->currentWidget ());
}

void
file_editor::request_find (void)
{
  emit fetab_find (_tab_widget->currentWidget ());
}

void
file_editor::request_goto_line (void)
{
  emit fetab_goto_line (_tab_widget->currentWidget ());
}


void
file_editor::handle_mru_add_file (const QString& file_name)
{
  _mru_files.removeAll (file_name);
  _mru_files.prepend (file_name);
  mru_menu_update ();
}

void
file_editor::mru_menu_update (void)
{
  int num_files = qMin (_mru_files.size (), int (MaxMRUFiles));

  // configure and show active actions of mru-menu
  for (int i = 0; i < num_files; ++i)
    {
      QString text = tr ("&%1 %2").
          arg ((i+1) % int (MaxMRUFiles)).arg (_mru_files.at (i));
      _mru_file_actions[i]->setText (text);
      _mru_file_actions[i]->setData (_mru_files.at (i));
      _mru_file_actions[i]->setVisible (true);
    }

    // hide unused mru-menu entries
    for (int j = num_files; j < MaxMRUFiles; ++j)
      _mru_file_actions[j]->setVisible (false);

    // delete entries in string-list beyond MaxMRUFiles
    while (_mru_files.size () > MaxMRUFiles)
      _mru_files.removeLast ();

    // save actual mru-list in settings
    QSettings *settings = resource_manager::get_settings ();

    // FIXME -- what should happen if settings is 0?
    settings->setValue ("editor/mru_file_list", _mru_files);
    settings->sync ();
}

void
file_editor::handle_file_name_changed (const QString& fname,
                                       const QString& tip)
{
  QObject *fileEditorTab = sender ();
  if (fileEditorTab)
    {
      for (int i = 0; i < _tab_widget->count (); i++)
        {
          if (_tab_widget->widget (i) == fileEditorTab)
            {
              _tab_widget->setTabText (i, fname);
              _tab_widget->setTabToolTip (i, tip);
            }
        }
    }
}

void
file_editor::request_close_file (bool)
{
  emit fetab_close_request (_tab_widget->currentWidget ());
}

void
file_editor::request_close_all_files (bool)
{
  // loop over all tabs starting from last one otherwise deletion changes index
  for (int index = _tab_widget->count ()-1; index >= 0; index--)
    emit fetab_close_request (_tab_widget->widget (index));
}

void
file_editor::request_close_other_files (bool)
{
  QWidget *tabID = _tab_widget->currentWidget ();
  // loop over all tabs starting from last one otherwise deletion changes index
  for (int index = _tab_widget->count ()-1; index >= 0; index--)
    {
      if (tabID != _tab_widget->widget (index))
        emit fetab_close_request (_tab_widget->widget (index));
    }
}


void
file_editor::handle_tab_close_request (int index)
{
  // Signal to the tabs a request to close whomever matches the identifying
  // tag (i.e., unique widget pointer).  The reason for this indirection is
  // that it will enable a file editor widget to toss up a non-static
  // dialog box and later signal that it wants to be removed.
  QWidget *tabID = _tab_widget->widget (index);
  emit fetab_close_request (tabID);
}

void
file_editor::handle_tab_remove_request (void)
{
  QObject *fileEditorTab = sender ();
  if (fileEditorTab)
    {
      for (int i = 0; i < _tab_widget->count (); i++)
        {
          if (_tab_widget->widget (i) == fileEditorTab)
            {
              _tab_widget->removeTab (i);
              delete fileEditorTab;
            }
        }
    }
}

void
file_editor::handle_add_filename_to_list (const QString& fileName, QWidget *ID)
{
  // Should we allow multiple tabs for a single file?

  editor_tab_map[fileName] = ID;
}

void
file_editor::active_tab_changed (int index)
{
  emit fetab_change_request (_tab_widget->widget (index));
}

void
file_editor::handle_editor_state_changed (bool copy_available,
                                          const QString& file_name)
{
  // In case there is some scenario where traffic could be coming from
  // all the file editor tabs, just process info from the current active tab.
  if (sender () == _tab_widget->currentWidget ())
    {
      _copy_action->setEnabled (copy_available);
      _cut_action->setEnabled (copy_available);

      if (!file_name.isEmpty ())
        {
          ced = QDir::cleanPath (file_name);
          int lastslash = ced.lastIndexOf ('/');

          // Test against > 0 because if somehow the directory is "/" the
          // slash should be retained.  Otherwise, last slash is removed.
          if (lastslash > 0 && lastslash != ced.count ())
            ced = ced.left (lastslash);
        }

      setFocusProxy (_tab_widget->currentWidget ());
    }
}

void
file_editor::notice_settings (const QSettings *settings)
{
  int icon_size = settings->value ("toolbar_icon_size", 24).toInt ();
  _tool_bar->setIconSize (QSize (icon_size, icon_size));
  // Relay signal to file editor tabs.
  emit fetab_settings_changed (settings);
}

void
file_editor::construct (void)
{
  QWidget *editor_widget = new QWidget (this);

  // FIXME -- what was the intended purpose of this unused variable?
  // QStyle *editor_style = QApplication::style ();

  _menu_bar = new QMenuBar (editor_widget);
  _tool_bar = new QToolBar (editor_widget);
  _tab_widget = new QTabWidget (editor_widget);
  _tab_widget->setTabsClosable (true);

  QAction *new_action = new QAction (QIcon (":/actions/icons/filenew.png"),
                                     tr ("&New File"), _tool_bar);

  QAction *open_action = new QAction (QIcon (":/actions/icons/fileopen.png"),
                                      tr ("&Open File"), _tool_bar);

  QAction *save_action = new QAction (QIcon (":/actions/icons/filesave.png"),
                                      tr ("&Save File"), _tool_bar);

  QAction *save_as_action
    = new QAction (QIcon (":/actions/icons/filesaveas.png"),
                   tr ("Save File &As"), _tool_bar);

  QAction *print_action
    = new QAction ( QIcon (":/actions/icons/fileprint.png"),
                    tr ("Print"), _tool_bar);

  QAction *undo_action = new QAction (QIcon (":/actions/icons/undo.png"),
                                      tr ("&Undo"), _tool_bar);

  QAction *redo_action = new QAction (QIcon (":/actions/icons/redo.png"),
                                      tr ("&Redo"), _tool_bar);

  _copy_action = new QAction (QIcon (":/actions/icons/editcopy.png"),
                              tr ("&Copy"), _tool_bar);

  _cut_action = new QAction (QIcon (":/actions/icons/editcut.png"),
                              tr ("Cu&t"), _tool_bar);

  QAction *paste_action
    = new QAction (QIcon (":/actions/icons/editpaste.png"),
                   tr ("Paste"), _tool_bar);

  QAction *next_bookmark_action
    = new QAction (tr ("&Next Bookmark"), _tool_bar);

  QAction *previous_bookmark_action
    = new QAction (tr ("Pre&vious Bookmark"), _tool_bar);

  QAction *toggle_bookmark_action
    = new QAction (tr ("Toggle &Bookmark"), _tool_bar);

  QAction *remove_bookmark_action
    = new QAction (tr ("&Remove All Bookmarks"), _tool_bar);

  QAction *next_breakpoint_action
    = new QAction (QIcon (":/actions/icons/bp_next.png"),
                   tr ("&Next breakpoint"), _tool_bar);
  QAction *previous_breakpoint_action
    = new QAction (QIcon (":/actions/icons/bp_prev.png"),
                   tr ("Pre&vious breakpoint"), _tool_bar);
  QAction *toggle_breakpoint_action
    = new QAction (QIcon (":/actions/icons/bp_toggle.png"),
                   tr ("Toggle &breakpoint"), _tool_bar);
  QAction *remove_all_breakpoints_action
    = new QAction (QIcon (":/actions/icons/bp_rm_all.png"),
                   tr ("&Remove All breakpoints"), _tool_bar);

  QAction *comment_selection_action
    = new QAction (tr ("&Comment Selected Text"), _tool_bar);

  QAction *uncomment_selection_action
    = new QAction (tr ("&Uncomment Selected Text"), _tool_bar);

  QAction *find_action = new QAction (QIcon (":/actions/icons/search.png"),
                                      tr ("&Find and Replace"), _tool_bar);

  _run_action = new QAction (QIcon (":/actions/icons/artsbuilderexecute.png"),
                             tr ("Save File And Run"), _tool_bar);

  QAction *goto_line_action = new QAction (tr ("Go&to Line"), _tool_bar);

  // the mru-list and an empty array of actions
  QSettings *settings = resource_manager::get_settings ();
  // FIXME -- what should happen if settings is 0?
  _mru_files = settings->value ("editor/mru_file_list").toStringList ();
  for (int i = 0; i < MaxMRUFiles; ++i)
    {
       _mru_file_actions[i] = new QAction (this);
       _mru_file_actions[i]->setVisible (false);
    }

  // some actions are disabled from the beginning
  _copy_action->setEnabled (false);
  _cut_action->setEnabled (false);
  _run_action->setShortcut (Qt::ControlModifier+ Qt::Key_R);
  _run_action->setShortcutContext (Qt::WindowShortcut);
  save_action->setShortcut (QKeySequence::Save);
  save_action->setShortcutContext (Qt::WindowShortcut);
  save_as_action->setShortcut (QKeySequence::SaveAs);
  save_as_action->setShortcutContext (Qt::WindowShortcut);

  print_action->setShortcut (QKeySequence::Print);
  print_action->setShortcutContext (Qt::WindowShortcut);

  next_bookmark_action->setShortcut (Qt::Key_F2);
  next_bookmark_action->setShortcutContext (Qt::WindowShortcut);
  previous_bookmark_action->setShortcut (Qt::SHIFT + Qt::Key_F2);
  previous_bookmark_action->setShortcutContext (Qt::WindowShortcut);
  toggle_bookmark_action->setShortcut (Qt::Key_F7);
  toggle_bookmark_action->setShortcutContext (Qt::WindowShortcut);
  comment_selection_action->setShortcut (Qt::ControlModifier + Qt::Key_7);
  comment_selection_action->setShortcutContext (Qt::WindowShortcut);
  uncomment_selection_action->setShortcut (Qt::ControlModifier + Qt::Key_8);
  uncomment_selection_action->setShortcutContext (Qt::WindowShortcut);
  find_action->setShortcut (QKeySequence::Find);
  find_action->setShortcutContext (Qt::WindowShortcut);
  goto_line_action->setShortcut (Qt::ControlModifier+ Qt::Key_G);
  goto_line_action->setShortcutContext (Qt::WindowShortcut);

  // toolbar
  _tool_bar->addAction (new_action);
  _tool_bar->addAction (open_action);
  _tool_bar->addAction (save_action);
  _tool_bar->addAction (save_as_action);
  _tool_bar->addSeparator ();
  _tool_bar->addAction (print_action);
  _tool_bar->addSeparator ();
  _tool_bar->addAction (undo_action);
  _tool_bar->addAction (redo_action);
  _tool_bar->addAction (_copy_action);
  _tool_bar->addAction (_cut_action);
  _tool_bar->addAction (paste_action);
  _tool_bar->addSeparator ();
  _tool_bar->addAction (find_action);
  _tool_bar->addAction (_run_action);
  _tool_bar->addSeparator ();
  _tool_bar->addAction (toggle_breakpoint_action);
  _tool_bar->addAction (next_breakpoint_action);
  _tool_bar->addAction (previous_breakpoint_action);
  _tool_bar->addAction (remove_all_breakpoints_action);

  // menu bar
  QMenu *fileMenu = new QMenu (tr ("&File"), _menu_bar);

  _mru_file_menu = new QMenu (tr ("&Recent Editor Files"), fileMenu);
  for (int i = 0; i < MaxMRUFiles; ++i)
    _mru_file_menu->addAction (_mru_file_actions[i]);

  fileMenu->addAction (new_action);
  fileMenu->addAction (open_action);
  fileMenu->addMenu (_mru_file_menu);

  fileMenu->addSeparator ();
  fileMenu->addAction (save_action);
  fileMenu->addAction (save_as_action);

  fileMenu->addSeparator ();
  fileMenu->addAction (QIcon::fromTheme("window-close",
                                      QIcon (":/actions/icons/fileclose.png")),
                       tr ("&Close"),
                       this, SLOT (request_close_file (bool)),
                             QKeySequence::Close);
  fileMenu->addAction (QIcon::fromTheme("window-close",
                                      QIcon (":/actions/icons/fileclose.png")),
                       tr ("Close All"),
                       this, SLOT (request_close_all_files (bool)));
  fileMenu->addAction (QIcon::fromTheme("window-close",
                                      QIcon (":/actions/icons/fileclose.png")),
                       tr ("Close Other Files"),
                       this, SLOT (request_close_other_files (bool)));

  fileMenu->addSeparator ();
  fileMenu->addAction (print_action);

  _menu_bar->addMenu (fileMenu);


  QMenu *editMenu = new QMenu (tr ("&Edit"), _menu_bar);
  editMenu->addAction (undo_action);
  editMenu->addAction (redo_action);
  editMenu->addSeparator ();
  editMenu->addAction (_copy_action);
  editMenu->addAction (_cut_action);
  editMenu->addAction (paste_action);
  editMenu->addSeparator ();
  editMenu->addAction (find_action);
  editMenu->addSeparator ();
  editMenu->addAction (comment_selection_action);
  editMenu->addAction (uncomment_selection_action);
  editMenu->addSeparator ();
  editMenu->addAction (toggle_bookmark_action);
  editMenu->addAction (next_bookmark_action);
  editMenu->addAction (previous_bookmark_action);
  editMenu->addAction (remove_bookmark_action);
  editMenu->addSeparator ();
  editMenu->addAction (goto_line_action);
  _menu_bar->addMenu (editMenu);

  _debug_menu = new QMenu (tr ("&Debug"), _menu_bar);
  _debug_menu->addAction (toggle_breakpoint_action);
  _debug_menu->addAction (next_breakpoint_action);
  _debug_menu->addAction (previous_breakpoint_action);
  _debug_menu->addAction (remove_all_breakpoints_action);
  _debug_menu->addSeparator ();
  // The other debug actions will be added by the main window.
  _menu_bar->addMenu (_debug_menu);

  QMenu *_run_menu = new QMenu (tr ("&Run"), _menu_bar);
  _run_menu->addAction (_run_action);
  _menu_bar->addMenu (_run_menu);

  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->addWidget (_menu_bar);
  vbox_layout->addWidget (_tool_bar);
  vbox_layout->addWidget (_tab_widget);
  vbox_layout->setMargin (0);
  editor_widget->setLayout (vbox_layout);
  setWidget (editor_widget);

  connect (parent (), SIGNAL (new_file_signal (const QString&)),
           this, SLOT (request_new_file (const QString&)));

  connect (parent (), SIGNAL (open_file_signal (const QString&)),
           this, SLOT (request_open_file (const QString&)));

  connect (new_action, SIGNAL (triggered ()),
           this, SLOT (request_new_file ()));

  connect (open_action, SIGNAL (triggered ()),
           this, SLOT (request_open_file ()));

  connect (undo_action, SIGNAL (triggered ()),
           this, SLOT (request_undo ()));

  connect (redo_action, SIGNAL (triggered ()),
           this, SLOT (request_redo ()));

  connect (_copy_action, SIGNAL (triggered ()),
           this, SLOT (request_copy ()));

  connect (_cut_action, SIGNAL (triggered ()),
           this, SLOT (request_cut ()));

  connect (paste_action, SIGNAL (triggered ()),
           this, SLOT (request_paste ()));

  connect (save_action, SIGNAL (triggered ()),
           this, SLOT (request_save_file ()));

  connect (save_as_action, SIGNAL (triggered ()),
           this, SLOT (request_save_file_as ()));

  connect (print_action, SIGNAL (triggered ()),
           this, SLOT (request_print_file ()));

  connect (_run_action, SIGNAL (triggered ()),
           this, SLOT (request_run_file ()));

  connect (toggle_bookmark_action, SIGNAL (triggered ()),
           this, SLOT (request_toggle_bookmark ()));

  connect (next_bookmark_action, SIGNAL (triggered ()),
           this, SLOT (request_next_bookmark ()));

  connect (previous_bookmark_action, SIGNAL (triggered ()),
           this, SLOT (request_previous_bookmark ()));

  connect (remove_bookmark_action, SIGNAL (triggered ()),
           this, SLOT (request_remove_bookmark ()));

  connect (toggle_breakpoint_action, SIGNAL (triggered ()),
           this, SLOT (request_toggle_breakpoint ()));

  connect (next_breakpoint_action, SIGNAL (triggered ()),
           this, SLOT (request_next_breakpoint ()));

  connect (previous_breakpoint_action, SIGNAL (triggered ()),
           this, SLOT (request_previous_breakpoint ()));

  connect (remove_all_breakpoints_action, SIGNAL (triggered ()),
           this, SLOT (request_remove_breakpoint ()));

  connect (comment_selection_action, SIGNAL (triggered ()),
           this, SLOT (request_comment_selected_text ()));

  connect (uncomment_selection_action, SIGNAL (triggered ()),
           this, SLOT (request_uncomment_selected_text ()));

  connect (find_action, SIGNAL (triggered ()),
           this, SLOT (request_find ()));

  connect (goto_line_action, SIGNAL (triggered ()),
           this, SLOT (request_goto_line ()));

  connect (_mru_file_menu, SIGNAL (triggered (QAction *)),
           this, SLOT (request_mru_open_file (QAction *)));

  mru_menu_update ();

  connect (_tab_widget, SIGNAL (tabCloseRequested (int)),
           this, SLOT (handle_tab_close_request (int)));

  connect (_tab_widget, SIGNAL (currentChanged (int)),
           this, SLOT (active_tab_changed (int)));

  resize (500, 400);
  setWindowIcon (QIcon (":/actions/icons/logo.png"));
  setWindowTitle ("Editor");

  //restore previous session
  if (settings->value ("editor/restoreSession", true).toBool ())
    {
      QStringList sessionFileNames
        = settings->value ("editor/savedSessionTabs", QStringList ()).toStringList ();

      for (int n = 0; n < sessionFileNames.count (); ++n)
        request_open_file (sessionFileNames.at (n));
    }
}

void
file_editor::add_file_editor_tab (file_editor_tab *f, const QString& fn)
{
  _tab_widget->addTab (f, fn);

  // Signals from the file editor_tab
  connect (f, SIGNAL (file_name_changed (const QString&, const QString&)),
           this, SLOT (handle_file_name_changed (const QString&,
                                                 const QString&)));

  connect (f, SIGNAL (editor_state_changed (bool, const QString&)),
           this, SLOT (handle_editor_state_changed (bool, const QString&)));

  connect (f, SIGNAL (tab_remove_request ()),
           this, SLOT (handle_tab_remove_request ()));

  connect (f, SIGNAL (add_filename_to_list (const QString&, QWidget*)),
           this, SLOT (handle_add_filename_to_list (const QString&, QWidget*)));

  connect (f, SIGNAL (editor_check_conflict_save (const QString&, bool)),
           this, SLOT (check_conflict_save (const QString&, bool)));

  connect (f, SIGNAL (mru_add_file (const QString&)),
           this, SLOT (handle_mru_add_file (const QString&)));

  connect (f, SIGNAL (run_file_signal (const QFileInfo&)),
           parent (), SLOT (run_file_in_terminal (const QFileInfo&)));
  
  // Signals from the file_editor non-trivial operations
  connect (this, SIGNAL (fetab_settings_changed (const QSettings *)),
           f, SLOT (notice_settings (const QSettings *)));

  connect (this, SIGNAL (fetab_close_request (const QWidget*)),
           f, SLOT (conditional_close (const QWidget*)));

  connect (this, SIGNAL (fetab_change_request (const QWidget*)),
           f, SLOT (change_editor_state (const QWidget*)));

  connect (this, SIGNAL (fetab_file_name_query (const QWidget*)),
           f, SLOT (file_name_query (const QWidget*)));

  connect (this, SIGNAL (fetab_save_file (const QWidget*, const QString&,
                                          bool)),
           f, SLOT (save_file (const QWidget*, const QString&, bool)));

  // Signals from the file_editor trivial operations
  connect (this, SIGNAL (fetab_undo (const QWidget*)),
           f, SLOT (undo (const QWidget*)));

  connect (this, SIGNAL (fetab_redo (const QWidget*)),
           f, SLOT (redo (const QWidget*)));

  connect (this, SIGNAL (fetab_copy (const QWidget*)),
           f, SLOT (copy (const QWidget*)));

  connect (this, SIGNAL (fetab_cut (const QWidget*)),
           f, SLOT (cut (const QWidget*)));

  connect (this, SIGNAL (fetab_paste (const QWidget*)),
           f, SLOT (paste (const QWidget*)));

  connect (this, SIGNAL (fetab_save_file (const QWidget*)),
           f, SLOT (save_file (const QWidget*)));

  connect (this, SIGNAL (fetab_save_file_as (const QWidget*)),
           f, SLOT (save_file_as (const QWidget*)));

  connect (this, SIGNAL (fetab_print_file (const QWidget*)),
           f, SLOT (print_file (const QWidget*)));

  connect (this, SIGNAL (fetab_run_file (const QWidget*)),
           f, SLOT (run_file (const QWidget*)));

  connect (this, SIGNAL (fetab_toggle_bookmark (const QWidget*)),
           f, SLOT (toggle_bookmark (const QWidget*)));

  connect (this, SIGNAL (fetab_next_bookmark (const QWidget*)),
           f, SLOT (next_bookmark (const QWidget*)));

  connect (this, SIGNAL (fetab_previous_bookmark (const QWidget*)),
           f, SLOT (previous_bookmark (const QWidget*)));

  connect (this, SIGNAL (fetab_remove_bookmark (const QWidget*)),
           f, SLOT (remove_bookmark (const QWidget*)));

  connect (this, SIGNAL (fetab_toggle_breakpoint (const QWidget*)),
           f, SLOT (toggle_breakpoint (const QWidget*)));

  connect (this, SIGNAL (fetab_next_breakpoint (const QWidget*)),
           f, SLOT (next_breakpoint (const QWidget*)));

  connect (this, SIGNAL (fetab_previous_breakpoint (const QWidget*)),
           f, SLOT (previous_breakpoint (const QWidget*)));

  connect (this, SIGNAL (fetab_remove_all_breakpoints (const QWidget*)),
           f, SLOT (remove_all_breakpoints (const QWidget*)));

  connect (this, SIGNAL (fetab_comment_selected_text (const QWidget*)),
           f, SLOT (comment_selected_text (const QWidget*)));

  connect (this, SIGNAL (fetab_uncomment_selected_text (const QWidget*)),
           f, SLOT (uncomment_selected_text (const QWidget*)));

  connect (this, SIGNAL (fetab_find (const QWidget*)),
           f, SLOT (find (const QWidget*)));

  connect (this, SIGNAL (fetab_goto_line (const QWidget*, int)),
           f, SLOT (goto_line (const QWidget*, int)));

  connect (this, SIGNAL (fetab_set_focus (const QWidget*)),
           f, SLOT (set_focus (const QWidget*)));

  connect (this, SIGNAL (fetab_insert_debugger_pointer (const QWidget*, int)),
           f, SLOT (insert_debugger_pointer (const QWidget*, int)));

  connect (this, SIGNAL (fetab_delete_debugger_pointer (const QWidget*, int)),
           f, SLOT (delete_debugger_pointer (const QWidget*, int)));

  connect (this, SIGNAL (fetab_do_breakpoint_marker (bool, const QWidget*,
                                                     int)),
           f, SLOT (do_breakpoint_marker (bool, const QWidget*, int)));

  _tab_widget->setCurrentWidget (f);
}

#endif
