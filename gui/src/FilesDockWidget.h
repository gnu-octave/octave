/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 John P. Swensen, Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

#include <vector>
#include <string>

#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include "octave/config.h"
#include "octave/octave.h"
#include "octave/str-vec.h"
#include "octave/cmd-hist.h"
#include <QDockWidget>
#include <QLineEdit>

class FilesDockWidget:public QDockWidget
{
  Q_OBJECT
public:
  FilesDockWidget (QWidget * parent = 0);

public slots:
  /** Slot for handling a change in directory via double click. */
  void itemDoubleClicked (const QModelIndex & index);

  /** Slot for handling the up-directory button in the toolbar. */
  void onUpDirectory ();

  void setCurrentDirectory (QString currentDirectory);

  void currentDirectoryEntered ();

  /** Tells the widget to notice settings that are probably new. */
  void noticeSettings ();

signals:
  void openFile (QString fileName);

private:
  // TODO: Add toolbar with buttons for navigating the path, creating dirs, etc

    /** Toolbar for file and directory manipulation. */
    QToolBar * m_navigationToolBar;

    /** Variables for the up-directory action. */
  QIcon m_directoryIcon;
  QAction *m_directoryUpAction;
  QToolButton *upDirectoryButton;

    /** The file system model. */
  QFileSystemModel *m_fileSystemModel;

    /** The file system view. */
  QTreeView *m_fileTreeView;
  QLineEdit *m_currentDirectory;
};

#endif // FILESDOCKWIDGET_H
