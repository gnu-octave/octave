/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
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

#include "FilesDockWidget.h"

#include <QApplication>
#include <QFileInfo>
#include <QCompleter>
#include <QSettings>
#include <QProcess>
#include <QDesktopServices>

FilesDockWidget::FilesDockWidget (QWidget * parent):QDockWidget (parent)
{
  setObjectName ("FilesDockWidget");
  setWindowTitle (tr ("Current Folder"));
  setWidget (new QWidget (this));

  // Create a toolbar
  m_navigationToolBar = new QToolBar ("", widget ());
  m_navigationToolBar->setAllowedAreas (Qt::TopToolBarArea);
  m_navigationToolBar->setMovable (false);
  m_navigationToolBar->setIconSize (QSize (20, 20));

  // Add a button to the toolbar with the QT standard icon for up-directory
  // TODO: Maybe change this to be an up-directory icon that is OS specific???
  QStyle *style = QApplication::style ();
  m_directoryIcon = style->standardIcon (QStyle::SP_FileDialogToParent);
  m_directoryUpAction = new QAction (m_directoryIcon, "", m_navigationToolBar);
  m_currentDirectory = new QLineEdit (m_navigationToolBar);

  m_navigationToolBar->addAction (m_directoryUpAction);
  m_navigationToolBar->addWidget (m_currentDirectory);
  connect (m_directoryUpAction, SIGNAL (triggered ()), this,
	   SLOT (onUpDirectory ()));

  // TODO: Add other buttons for creating directories

  // Create the QFileSystemModel starting in the home directory
  QString
    homePath = QDir::homePath ();
  // TODO: This should occur after Octave has been initialized and the startup directory of Octave is established

  m_fileSystemModel = new QFileSystemModel (this);
  m_fileSystemModel->setFilter (QDir::NoDotAndDotDot | QDir::AllEntries);
  QModelIndex
    rootPathIndex = m_fileSystemModel->setRootPath (homePath);

  // Attach the model to the QTreeView and set the root index
  m_fileTreeView = new QTreeView (widget ());
  m_fileTreeView->setModel (m_fileSystemModel);
  m_fileTreeView->setRootIndex (rootPathIndex);
  m_fileTreeView->setSortingEnabled (true);
  m_fileTreeView->setAlternatingRowColors (true);
  m_fileTreeView->setAnimated (true);
  setCurrentDirectory (m_fileSystemModel->fileInfo (rootPathIndex).
		       absoluteFilePath ());

  connect (m_fileTreeView, SIGNAL (doubleClicked (const QModelIndex &)), this,
	   SLOT (itemDoubleClicked (const QModelIndex &)));

  // Layout the widgets vertically with the toolbar on top
  QVBoxLayout *
    layout = new QVBoxLayout ();
  layout->setSpacing (0);
  layout->addWidget (m_navigationToolBar);
  layout->addWidget (m_fileTreeView);
  widget ()->setLayout (layout);
  // TODO: Add right-click contextual menus for copying, pasting, deleting files (and others)

  connect (m_currentDirectory, SIGNAL (returnPressed ()), this,
	   SLOT (currentDirectoryEntered ()));
  QCompleter *
    completer = new QCompleter (m_fileSystemModel, this);
  m_currentDirectory->setCompleter (completer);
}

void
FilesDockWidget::itemDoubleClicked (const QModelIndex & index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = m_fileSystemModel->fileInfo (index);

  // If it is a directory, cd into it.
  if (fileInfo.isDir ())
    {
      m_fileSystemModel->setRootPath (fileInfo.absolutePath ());
      m_fileTreeView->setRootIndex (index);
      setCurrentDirectory (m_fileSystemModel->fileInfo (index).
			   absoluteFilePath ());
    }
  // Otherwise attempt to open it.
  else
    {
      // Check if the user wants to use a custom file editor.
      QDesktopServices desktopServices;
      QString settingsFile =
        desktopServices.storageLocation (QDesktopServices::HomeLocation) +
        "/.quint/settings.ini";
      QSettings settings (settingsFile, QSettings::IniFormat);
      if (settings.value ("useCustomFileEditor").toBool ())
        {
          QString editor = settings.value ("customFileEditor").toString ();
          QStringList arguments;
          arguments << fileInfo.filePath ();
          QProcess::execute (editor, arguments);
        }
      else
        {
          emit openFile (fileInfo.filePath ());
        }
    }
}

void
FilesDockWidget::setCurrentDirectory (QString currentDirectory)
{
  m_currentDirectory->setText (currentDirectory);
}

void
FilesDockWidget::onUpDirectory (void)
{
  QDir dir =
    QDir (m_fileSystemModel->filePath (m_fileTreeView->rootIndex ()));
  dir.cdUp ();
  m_fileSystemModel->setRootPath (dir.absolutePath ());
  m_fileTreeView->setRootIndex (m_fileSystemModel->
				index (dir.absolutePath ()));
  setCurrentDirectory (dir.absolutePath ());
}

void
FilesDockWidget::currentDirectoryEntered ()
{
  QFileInfo fileInfo (m_currentDirectory->text ());
  if (fileInfo.isDir ())
    {
      m_fileTreeView->setRootIndex (m_fileSystemModel->
				    index (fileInfo.absolutePath ()));
      m_fileSystemModel->setRootPath (fileInfo.absolutePath ());
      setCurrentDirectory (fileInfo.absoluteFilePath ());
    }
  else
    {
      if (QFile::exists (fileInfo.absoluteFilePath ()))
	emit openFile (fileInfo.absoluteFilePath ());
    }
}
