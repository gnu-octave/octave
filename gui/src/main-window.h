/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

// Qt includes
#include <QtGui/QMainWindow>
#include <QThread>
#include <QTabWidget>
#include <QMdiArea>
#include <QStatusBar>
#include <QToolBar>
#include <QQueue>
#include <QMdiSubWindow>
#include <QCloseEvent>
#include <QToolButton>
#include <QComboBox>

// Editor includes
#include "file-editor-interface.h"

// QTerminal includes
#include "QTerminal.h"

// Own includes
#include "resource-manager.h"
#include "octave-link.h"
#include "workspace-view.h"
#include "history-dockwidget.h"
#include "files-dockwidget.h"
#include "terminal-dockwidget.h"

/**
  * \class MainWindow
  *
  * Represents the main window.
  */
class main_window:public QMainWindow
{
Q_OBJECT public:
  main_window (QWidget * parent = 0);
  ~main_window ();

  QTerminal *terminalView ()
  {
    return m_terminal;
  }

  history_dock_widget *historyDockWidget ()
  {
    return m_historyDockWidget;
  }
  files_dock_widget *filesDockWidget ()
  {
    return m_filesDockWidget;
  }
  bool closing ()
  {
    return m_closing;
  }

signals:
  void settingsChanged ();

public slots:
  void reportStatusMessage (QString statusMessage);
  void handleSaveWorkspaceRequest ();
  void handleLoadWorkspaceRequest ();
  void handleClearWorkspaceRequest ();
  void handleCommandDoubleClicked (QString command);
  void newFile ();
  void openFile ();
  void openBugTrackerPage ();
  void openAgoraPage ();
  void openOctaveForgePage ();
  void processSettingsDialogRequest ();
  void showAboutOctave ();
  void noticeSettings ();
  void prepareForQuit ();
  void resetWindows ();
  void updateCurrentWorkingDirectory (QString directory);
  void changeCurrentWorkingDirectory ();
  void changeCurrentWorkingDirectory (QString directory);
  void currentWorkingDirectoryUp ();

protected:
  void closeEvent (QCloseEvent * closeEvent);
  void readSettings ();
  void writeSettings ();

private:
  void construct ();
  void establishOctaveLink ();

  QTerminal *m_terminal;
  file_editor_interface *m_fileEditor;

  // Dock widgets.
  workspace_view *m_workspaceView;
  history_dock_widget *m_historyDockWidget;
  files_dock_widget *m_filesDockWidget;
  terminal_dock_widget *m_terminalDockWidget;

  // Toolbars.
  QStatusBar *m_statusBar;

  QComboBox *m_currentDirectoryComboBox;
  QToolButton *m_currentDirectoryToolButton;
  QToolButton *m_currentDirectoryUpToolButton;

  // Flag for closing whole application
  bool m_closing;
};

#endif // MAINWINDOW_H
