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

#include "resource-manager.h"
#include "files-dockwidget.h"

#include <QApplication>
#include <QFileInfo>
#include <QCompleter>
#include <QSettings>
#include <QProcess>
#include <QDebug>
#include <QHeaderView>

files_dock_widget::files_dock_widget (QWidget *p)
  : octave_dock_widget (p)
{
  setObjectName ("FilesDockWidget");
  setWindowIcon (QIcon(":/actions/icons/logo.png"));
  setWindowTitle (tr ("Current Directory"));
  setWidget (new QWidget (this));

  // Create a toolbar
  _navigation_tool_bar = new QToolBar ("", widget ());
  _navigation_tool_bar->setAllowedAreas (Qt::TopToolBarArea);
  _navigation_tool_bar->setMovable (false);
  _navigation_tool_bar->setIconSize (QSize (20, 20));

  _directory_icon = QIcon(":/actions/icons/up.png");
  _directory_up_action = new QAction (_directory_icon, "", _navigation_tool_bar);
  _directory_up_action->setStatusTip (tr ("Move up one directory."));

  _last_current_directory = "";
  _current_directory = new QLineEdit (_navigation_tool_bar);
  _current_directory->setStatusTip (tr ("Enter the path or filename."));

  _navigation_tool_bar->addAction (_directory_up_action);
  _navigation_tool_bar->addWidget (_current_directory);
  connect (_directory_up_action, SIGNAL (triggered ()), this,
           SLOT (do_up_directory ()));

  // TODO: Add other buttons for creating directories

  // Create the QFileSystemModel starting in the home directory
  QString homePath = QDir::homePath ();

  _file_system_model = new QFileSystemModel (this);
  _file_system_model->setFilter (QDir::NoDotAndDotDot | QDir::AllEntries);
  QModelIndex rootPathIndex = _file_system_model->setRootPath (homePath);

  // Attach the model to the QTreeView and set the root index
  _file_tree_view = new QTreeView (widget ());
  _file_tree_view->setModel (_file_system_model);
  _file_tree_view->setRootIndex (rootPathIndex);
  _file_tree_view->setSortingEnabled (true);
  _file_tree_view->setAlternatingRowColors (true);
  _file_tree_view->setAnimated (true);
  _file_tree_view->setColumnHidden (1, true);
  _file_tree_view->setColumnHidden (2, true);
  _file_tree_view->setColumnHidden (3, true);
  _file_tree_view->setStatusTip (tr ("Doubleclick a file to open it."));

  // get sort column and order as well as cloumn state (order and width)
  QSettings *settings = resource_manager::get_settings ();
  // FIXME -- what should happen if settings is 0?
  _file_tree_view->sortByColumn (
              settings->value ("filesdockwidget/sort_files_by_column",0).toInt (),
              static_cast<Qt::SortOrder>(settings->value ("filesdockwidget/sort_files_by_order",Qt::AscendingOrder).toUInt ())
  );
  _file_tree_view->header ()->restoreState (settings->value ("filesdockwidget/column_state").toByteArray ());
  
  _current_directory->setText(_file_system_model->fileInfo (rootPathIndex).
                              absoluteFilePath ());

  connect (_file_tree_view, SIGNAL (doubleClicked (const QModelIndex &)), this,
           SLOT (item_double_clicked (const QModelIndex &)));

  // Layout the widgets vertically with the toolbar on top
  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->setSpacing (0);
  vbox_layout->addWidget (_navigation_tool_bar);
  vbox_layout->addWidget (_file_tree_view);
  vbox_layout->setMargin (1);
  widget ()->setLayout (vbox_layout);
  // TODO: Add right-click contextual menus for copying, pasting, deleting files (and others)

  connect (_current_directory, SIGNAL (returnPressed ()),
           this, SLOT (handle_directory_entered ()));

  QCompleter *
    completer = new QCompleter (_file_system_model, this);
  _current_directory->setCompleter (completer);

  setFocusProxy (_current_directory);
}

files_dock_widget::~files_dock_widget ()
{
  QSettings *settings = resource_manager::get_settings ();
  int sort_column = _file_tree_view->header ()->sortIndicatorSection ();
  Qt::SortOrder sort_order = _file_tree_view->header ()->sortIndicatorOrder ();
  settings->setValue ("filesdockwidget/sort_files_by_column", sort_column);
  settings->setValue ("filesdockwidget/sort_files_by_order", sort_order);
  settings->setValue ("filesdockwidget/column_state", _file_tree_view->header ()->saveState ());
  settings->sync ();
}

void
files_dock_widget::item_double_clicked (const QModelIndex & index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = _file_system_model->fileInfo (index);
  display_directory (fileInfo.absoluteFilePath ());
}

void
files_dock_widget::set_current_directory (const QString& currentDirectory)
{
  display_directory (currentDirectory);
}

void
files_dock_widget::handle_directory_entered ()
{
  display_directory (_current_directory->text ());
}

void
files_dock_widget::do_up_directory ()
{
  QDir dir = QDir (_file_system_model->filePath (_file_tree_view->rootIndex ()));
  dir.cdUp ();
  display_directory (dir.absolutePath ());
}

void
files_dock_widget::display_directory (const QString& directory)
{
  QFileInfo fileInfo (directory);
  if (fileInfo.exists ())
    {
      if (fileInfo.isDir ())
        {
          _file_tree_view->setRootIndex (_file_system_model->
                                         index (fileInfo.absoluteFilePath ()));
          _file_system_model->setRootPath (fileInfo.absoluteFilePath ());
          _current_directory->setText (fileInfo.absoluteFilePath ());

          if (_last_current_directory != fileInfo.absoluteFilePath ())
            {
              emit displayed_directory_changed (fileInfo.absoluteFilePath ());
            }

          _last_current_directory = fileInfo.absoluteFilePath ();
        }
      else
        {
          if (QFile::exists (fileInfo.absoluteFilePath ()))
            emit open_file (fileInfo.absoluteFilePath ());
        }
    }
}

void
files_dock_widget::notice_settings ()
{
  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

  _file_tree_view->setColumnHidden (0, !settings->value ("showFilenames").toBool ());
  _file_tree_view->setColumnHidden (1, !settings->value ("showFileSize").toBool ());
  _file_tree_view->setColumnHidden (2, !settings->value ("showFileType").toBool ());
  _file_tree_view->setColumnHidden (3, !settings->value ("showLastModified").toBool ());
  _file_tree_view->setAlternatingRowColors (settings->value ("useAlternatingRowColors").toBool ());
  //if (settings.value ("showHiddenFiles").toBool ())
  // TODO: React on option for hidden files.
}
