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

// Editor includes
#include "FileEditorInterface.h"

// QTerminal includes
#include "QTerminal.h"

// Own includes
#include "ResourceManager.h"
#include "OctaveLink.h"
#include "WorkspaceView.h"
#include "HistoryDockWidget.h"
#include "FilesDockWidget.h"
#include "TerminalDockWidget.h"

/**
  * \class MainWindow
  *
  * Represents the main window.
  */
class MainWindow:public QMainWindow
{
Q_OBJECT public:
  MainWindow (QWidget * parent = 0);
  ~MainWindow ();

  QTerminal *terminalView ()
  {
    return m_terminal;
  }

  HistoryDockWidget *historyDockWidget ()
  {
    return m_historyDockWidget;
  }
  FilesDockWidget *filesDockWidget ()
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

protected:
  void closeEvent (QCloseEvent * closeEvent);
  void readSettings ();
  void writeSettings ();

private:
  void construct ();
  void establishOctaveLink ();

  QTerminal *m_terminal;
  FileEditorInterface *m_fileEditor;

  // Dock widgets.
  WorkspaceView *m_workspaceView;
  HistoryDockWidget *m_historyDockWidget;
  FilesDockWidget *m_filesDockWidget;
  TerminalDockWidget *m_terminalDockWidget;

  // Toolbars.
  QStatusBar *m_statusBar;

  QLineEdit *m_currentDirectoryLineEdit;
  QToolButton *m_currentDirectoryToolButton;
  QToolButton *m_currentDirectoryUpToolButton;

  // Flag for closing whole application
  bool m_closing;
};

#endif // MAINWINDOW_H
