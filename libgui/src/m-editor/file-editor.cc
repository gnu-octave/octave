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

file_editor::file_editor (QTerminal *terminal, main_window *m)
  : file_editor_interface(terminal, m)
{
  construct ();

  _terminal = terminal;
  _main_window = m;
  setVisible (false);
}

file_editor::~file_editor ()
{
  QSettings *settings = resource_manager::get_settings ();
  QStringList sessionFileNames;
  if (settings->value ("editor/restoreSession",true).toBool ())
  {
    for (int n=0;n<_tab_widget->count();++n)
      {
        file_editor_tab* tab = dynamic_cast<file_editor_tab*> (_tab_widget->widget (n));
        if (!tab)
          continue;
        sessionFileNames.append (tab->get_file_name ());
      }
  }
  settings->setValue ("editor/savedSessionTabs", sessionFileNames);
  settings->sync ();
}

QTerminal *
file_editor::terminal ()
{
  return _terminal;
}

main_window *
file_editor::get_main_window ()
{
  return _main_window;
}

QMenu *
file_editor::debug_menu ()
{
  return _debug_menu;
}

QToolBar *
file_editor::toolbar ()
{
  return _tool_bar;
}

void
file_editor::handle_entered_debug_mode ()
{
  _run_action->setEnabled (false);
}

void
file_editor::handle_quit_debug_mode ()
{
  _run_action->setEnabled (true);
}

void
file_editor::request_new_file ()
{
  file_editor_tab *fileEditorTab = new file_editor_tab (this);
  if (fileEditorTab)
    {
      add_file_editor_tab (fileEditorTab);
      fileEditorTab->new_file ();
    }
}

void
file_editor::request_open_file ()
{
  file_editor_tab *current_tab = active_editor_tab ();
  int curr_tab_index = _tab_widget->currentIndex ();
  file_editor_tab *fileEditorTab = new file_editor_tab (this);
  if (fileEditorTab)
    {
      add_file_editor_tab (fileEditorTab);
      QString dir = QDir::currentPath ();
      // get the filename of the last active tab to open a new file from there
      if (current_tab)
        dir = QDir::cleanPath (current_tab->get_file_name ());
      if (!fileEditorTab->open_file (dir))
        {
          // If no file was loaded, remove the tab again.
          _tab_widget->removeTab (_tab_widget->indexOf (fileEditorTab));
          // restore focus to previous tab
          if (curr_tab_index>=0)
            _tab_widget->setCurrentIndex (curr_tab_index);
        }
    }
}

void
file_editor::request_open_file (const QString& fileName, bool silent)
{
  if (!isVisible ())
    {
      show ();
    }

  file_editor_tab *fileEditorTab = new file_editor_tab (this);
  int curr_tab_index = _tab_widget->currentIndex ();
  if (fileEditorTab)
    {
      add_file_editor_tab (fileEditorTab);
      if (!fileEditorTab->load_file (fileName, silent))
        {
          // If no file was loaded, remove the tab again.
          _tab_widget->removeTab (_tab_widget->indexOf (fileEditorTab));
          // restore focus to previous tab
          _tab_widget->setCurrentIndex (curr_tab_index);
        }
    }
}

void
file_editor::request_undo ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->undo ();
}

void
file_editor::request_redo ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->redo ();
}

void
file_editor::request_copy ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->copy ();
}

void
file_editor::request_cut ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->cut ();
}

void
file_editor::request_paste ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->paste ();
}

void
file_editor::request_save_file ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->save_file ();
}

void
file_editor::request_save_file_as ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->save_file_as ();
}

void
file_editor::request_run_file ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->run_file ();
}

void
file_editor::request_toggle_bookmark ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->toggle_bookmark ();
}

void
file_editor::request_next_bookmark ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->next_bookmark ();
}

void
file_editor::request_previous_bookmark ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->previous_bookmark ();
}

void
file_editor::request_remove_bookmark ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->remove_bookmark ();
}

void
file_editor::request_toggle_breakpoint ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->toggle_breakpoint ();
}

void
file_editor::request_next_breakpoint ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->next_breakpoint ();
}

void
file_editor::request_previous_breakpoint ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->previous_breakpoint ();
}

void
file_editor::request_remove_breakpoint ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->remove_all_breakpoints ();
}

void
file_editor::request_comment_selected_text ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->comment_selected_text ();
}

void
file_editor::request_uncomment_selected_text ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->uncomment_selected_text ();
}

void
file_editor::request_find ()
{
  file_editor_tab *_active_file_editor_tab = active_editor_tab ();
  if (_active_file_editor_tab)
    _active_file_editor_tab->find ();
}

void
file_editor::handle_file_name_changed (const QString& fileName)
{
  QObject *senderObject = sender ();
  file_editor_tab *fileEditorTab
    = dynamic_cast<file_editor_tab*> (senderObject);
  if (fileEditorTab)
    {
      for(int i = 0; i < _tab_widget->count (); i++)
        {
          if (_tab_widget->widget (i) == fileEditorTab)
            {
              _tab_widget->setTabText (i, fileName);
            }
        }
    }
}

void
file_editor::handle_tab_close_request (int index)
{
  file_editor_tab *fileEditorTab
    = dynamic_cast <file_editor_tab*> (_tab_widget->widget (index));
  if (fileEditorTab)
    if (fileEditorTab->close ())
      {
        _tab_widget->removeTab (index);
        delete fileEditorTab;
      }
}

void
file_editor::handle_tab_close_request ()
{
  file_editor_tab *fileEditorTab = dynamic_cast <file_editor_tab*> (sender ());
  if (fileEditorTab)
    if (fileEditorTab->close ())
      {
        _tab_widget->removeTab (_tab_widget->indexOf (fileEditorTab));
        delete fileEditorTab;
      }
}

void
file_editor::active_tab_changed (int index)
{
  Q_UNUSED (index);
  handle_editor_state_changed ();
}

void
file_editor::handle_editor_state_changed ()
{
  file_editor_tab *f = active_editor_tab ();
  if (f)
    {
      bool copy_available = f->copy_available ();
      _copy_action->setEnabled (copy_available);
      _cut_action->setEnabled (copy_available);
      setFocusProxy (f);
    }
}

void
file_editor::construct ()
{
  QWidget *widget = new QWidget (this);
  QStyle *style = QApplication::style ();

  _menu_bar = new QMenuBar (widget);
  _tool_bar = new QToolBar (widget);
  _tab_widget = new QTabWidget (widget);
  _tab_widget->setTabsClosable (true);

  QAction *new_action = new QAction (QIcon(":/actions/icons/filenew.png"),
        tr("&New File"), _tool_bar);

  QAction *open_action = new QAction (QIcon(":/actions/icons/fileopen.png"),
        tr("&Open File"), _tool_bar);

  QAction *save_action = new QAction (QIcon(":/actions/icons/filesave.png"),
        tr("&Save File"), _tool_bar);

  QAction *save_as_action
    = new QAction (QIcon(":/actions/icons/filesaveas.png"),
                   tr("Save File &As"), _tool_bar);

  QAction *undo_action = new QAction (QIcon(":/actions/icons/undo.png"),
        tr("&Undo"), _tool_bar);

  QAction *redo_action = new QAction (QIcon(":/actions/icons/redo.png"),
        tr("&Redo"), _tool_bar);

  _copy_action = new QAction (QIcon(":/actions/icons/editcopy.png"),
                              tr ("&Copy"), _tool_bar);

  _cut_action  = new QAction (QIcon(":/actions/icons/editcut.png"),
                              tr ("Cu&t"), _tool_bar);

  QAction *paste_action
      = new QAction (QIcon (":/actions/icons/editpaste.png"),
                     tr("Paste"), _tool_bar);
  QAction *next_bookmark_action       = new QAction (tr ("&Next Bookmark"),_tool_bar);
  QAction *previous_bookmark_action   = new QAction (tr ("Pre&vious Bookmark"),_tool_bar);
  QAction *toggle_bookmark_action     = new QAction (tr ("Toggle &Bookmark"),_tool_bar);
  QAction *remove_bookmark_action     = new QAction (tr ("&Remove All Bookmarks"),_tool_bar);

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

  QAction *comment_selection_action   = new QAction (tr ("&Comment Selected Text"),_tool_bar);
  QAction *uncomment_selection_action = new QAction (tr ("&Uncomment Selected Text"),_tool_bar);

  QAction *find_action = new QAction (QIcon(":/actions/icons/find.png"),
                                      tr ("&Find and Replace"), _tool_bar);

  _run_action = new QAction (QIcon(":/actions/icons/artsbuilderexecute.png"),
        tr("Save File And Run"), _tool_bar);

  // some actions are disabled from the beginning
  _copy_action->setEnabled(false);
  _cut_action->setEnabled(false);
  _run_action->setShortcut                      (Qt::ControlModifier+ Qt::Key_R);
  _run_action->setShortcutContext               (Qt::WindowShortcut);
  next_bookmark_action->setShortcut             (Qt::Key_F2);
  next_bookmark_action->setShortcutContext      (Qt::WindowShortcut);
  previous_bookmark_action->setShortcut         (Qt::SHIFT + Qt::Key_F2);
  previous_bookmark_action->setShortcutContext  (Qt::WindowShortcut);
  toggle_bookmark_action->setShortcut           (Qt::Key_F7);
  toggle_bookmark_action->setShortcutContext    (Qt::WindowShortcut);
  comment_selection_action->setShortcut         (Qt::ControlModifier + Qt::Key_7);
  comment_selection_action->setShortcutContext  (Qt::WindowShortcut);
  uncomment_selection_action->setShortcut       (Qt::ControlModifier + Qt::Key_8);
  uncomment_selection_action->setShortcutContext(Qt::WindowShortcut);
  find_action->setShortcut                      (Qt::ControlModifier+Qt::Key_F);
  find_action->setShortcutContext               (Qt::WindowShortcut);

  // toolbar
  _tool_bar->addAction (new_action);
  _tool_bar->addAction (open_action);
  _tool_bar->addAction (save_action);
  _tool_bar->addAction (save_as_action);
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
  fileMenu->addAction (new_action);
  fileMenu->addAction (open_action);
  fileMenu->addAction (save_action);
  fileMenu->addAction (save_as_action);
  fileMenu->addSeparator ();
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

  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (_menu_bar);
  layout->addWidget (_tool_bar);
  layout->addWidget (_tab_widget);
  layout->setMargin (0);
  widget->setLayout (layout);
  setWidget (widget);

  connect (new_action,
           SIGNAL (triggered ()), this, SLOT (request_new_file ()));
  connect (open_action,              
           SIGNAL (triggered ()), this, SLOT (request_open_file ()));
  connect (undo_action,
           SIGNAL (triggered ()), this, SLOT (request_undo ()));
  connect (redo_action,
           SIGNAL (triggered ()), this, SLOT (request_redo ()));
  connect (_copy_action,
           SIGNAL (triggered ()), this, SLOT (request_copy ()));
  connect (_cut_action,
           SIGNAL (triggered ()), this, SLOT (request_cut ()));
  connect (paste_action,
           SIGNAL (triggered ()), this, SLOT (request_paste ()));
  connect (save_action,
           SIGNAL (triggered ()), this, SLOT (request_save_file ()));
  connect (save_as_action,
           SIGNAL (triggered ()), this, SLOT (request_save_file_as ()));
  connect (_run_action,
           SIGNAL (triggered ()), this, SLOT (request_run_file ()));
  connect (toggle_bookmark_action,
           SIGNAL (triggered ()), this, SLOT (request_toggle_bookmark ()));
  connect (next_bookmark_action,
           SIGNAL (triggered ()), this, SLOT (request_next_bookmark ()));
  connect (previous_bookmark_action,
           SIGNAL (triggered ()), this, SLOT (request_previous_bookmark ()));
  connect (remove_bookmark_action,
           SIGNAL (triggered ()), this, SLOT (request_remove_bookmark ()));
  connect (toggle_breakpoint_action,
           SIGNAL (triggered ()), this, SLOT (request_toggle_breakpoint ()));
  connect (next_breakpoint_action,
           SIGNAL (triggered ()), this, SLOT (request_next_breakpoint ()));
  connect (previous_breakpoint_action,
           SIGNAL (triggered ()), this, SLOT (request_previous_breakpoint ()));
  connect (remove_all_breakpoints_action,
           SIGNAL (triggered ()), this, SLOT (request_remove_breakpoint ()));
  connect (comment_selection_action,
           SIGNAL (triggered ()), this, SLOT (request_comment_selected_text ()));
  connect (uncomment_selection_action,
           SIGNAL (triggered ()), this, SLOT (request_uncomment_selected_text ()));
  connect (find_action,
           SIGNAL (triggered ()), this, SLOT (request_find ()));
  connect (_tab_widget,
           SIGNAL (tabCloseRequested (int)), this, SLOT (handle_tab_close_request (int)));
  connect (_tab_widget,
           SIGNAL (currentChanged(int)), this, SLOT (active_tab_changed (int)));

  resize (500, 400);
  setWindowIcon (QIcon::fromTheme ("accessories-text-editor",
                                   style->standardIcon (QStyle::SP_FileIcon)));
  setWindowTitle ("Octave Editor");

  //restore previous session
  QSettings *settings = resource_manager::get_settings ();
  if (settings->value ("editor/restoreSession",true).toBool ())
  {
    QStringList sessionFileNames = settings->value("editor/savedSessionTabs", QStringList()).toStringList ();

    for (int n=0;n<sessionFileNames.count();++n)
      request_open_file(sessionFileNames.at(n), true);
  }
}

void
file_editor::add_file_editor_tab (file_editor_tab *f)
{
  _tab_widget->addTab (f, "");
  connect (f, SIGNAL (file_name_changed(QString)),
           this, SLOT(handle_file_name_changed(QString)));
  connect (f, SIGNAL (editor_state_changed ()),
           this, SLOT (handle_editor_state_changed ()));
  connect (f, SIGNAL (close_request ()),
           this, SLOT (handle_tab_close_request ()));
  _tab_widget->setCurrentWidget (f);
}

file_editor_tab *
file_editor::active_editor_tab ()
{
  return dynamic_cast<file_editor_tab*> (_tab_widget->currentWidget ());
}
