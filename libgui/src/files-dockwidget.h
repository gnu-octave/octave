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

#ifndef FILESDOCKWIDGET_H
#define FILESDOCKWIDGET_H

#include <QListView>
#include <QDate>
#include <QObject>
#include <QWidget>
#include <QListWidget>
#include <QFileSystemModel>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>
#include <QAction>
#include <QTreeView>
#include <QSettings>

#include <QLineEdit>
#include "octave-dock-widget.h"

/**
   \class files_dock_widget
   \brief Dock widget to display files in the current directory.
*/
class files_dock_widget : public octave_dock_widget
{
  Q_OBJECT
  public:
  /** Constructs a new files_dock_widget. */
  files_dock_widget (QWidget *parent = 0);
  ~files_dock_widget ();

public slots:

  /** Slot for handling a change in directory via double click. */
  void item_double_clicked (const QModelIndex & index);

  /** Slot for handling the up-directory button in the toolbar. */
  void do_up_directory ();

  /** Sets the current directory being displayed. */
  void set_current_directory (const QString& currentDirectory);

  /** Accepts user input a the line edit for the current directory. */
  void handle_directory_entered ();

  void display_directory (const QString& directory);

  /** Tells the widget to react on changed settings. */
  void notice_settings (const QSettings *settings);

signals:
  /** Emitted, whenever the user requested to open a file. */
  void open_file (const QString& fileName);

  /** Emitted, whenever the currently displayed directory changed. */
  void displayed_directory_changed (const QString& directory);

protected:

private:
  // TODO: Add toolbar with buttons for navigating the path, creating dirs, etc

  QString           _last_current_directory;

  /** Toolbar for file and directory manipulation. */
  QToolBar *        _navigation_tool_bar;

  /** Variables for the up-directory action. */
  QIcon             _directory_icon;
  QAction *         _directory_up_action;
  QToolButton *     _up_directory_button;

  /** The file system model. */
  QFileSystemModel *_file_system_model;

  /** The file system view. */
  QTreeView *       _file_tree_view;
  QLineEdit *       _current_directory;
};

#endif // FILESDOCKWIDGET_H
