/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QMainWindow>
#include <QThread>
#include <QTabWidget>
#include <QMdiArea>
#include <QStatusBar>
#include <QToolBar>
#include <QQueue>
#include <Qsci/qsciapis.h>
#include <QMdiSubWindow>
#include "ResourceManager.h"
#include "AbstractTerminalView.h"
#include "OctaveLink.h"
#include "WorkspaceView.h"
#include "HistoryDockWidget.h"
#include "FilesDockWidget.h"
#include "BrowserWidget.h"
#include "irc/IRCWidget.h"
#include "lexer/lexeroctavegui.h"

class NonClosableMdiSubWindow : public QMdiSubWindow
{
  Q_OBJECT
public:
  explicit NonClosableMdiSubWindow (QWidget *parent = 0)
    : QMdiSubWindow (parent) { }
  virtual ~NonClosableMdiSubWindow () { }
protected:
  void closeEvent (QCloseEvent *closeEvent)
  {
    closeEvent->ignore ();
  }
};

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

  AbstractTerminalView *terminalView ()
  {
    return m_terminalView;
  }

  HistoryDockWidget *historyDockWidget ()
  {
    return m_historyDockWidget;
  }
  FilesDockWidget *filesDockWidget ()
  {
    return m_filesDockWidget;
  }
  bool isCloseApplication ()
  {
    return m_closeApplication;
  }

signals:
  void settingsChanged ();

public slots:
  void handleOpenFileRequest (QString fileName);
  void reportStatusMessage (QString statusMessage);
  void openWebPage (QString url);
  void handleSaveWorkspaceRequest ();
  void handleLoadWorkspaceRequest ();
  void handleClearWorkspaceRequest ();
  void handleCommandDoubleClicked (QString command);
  void handleUnreadMessages (bool yes);
  void alignMdiWindows ();
  void openEditor ();
  void openEditorFile (QString fileName);
  void openBugTrackerPage ();
  void openAgoraPage ();
  void openOctaveForgePage ();
  void processSettingsDialogRequest ();
  void showAboutOctave ();
  void showAboutQt ();

protected:
  void closeEvent (QCloseEvent * closeEvent);
  void readSettings ();
  void writeSettings ();

private:
  void construct ();
  void establishOctaveLink ();
  QMdiArea *m_centralMdiArea;

  // Mdi sub windows.
  AbstractTerminalView *m_terminalView;
  BrowserWidget *m_documentationWidget;
  IRCWidget *m_ircWidget;

  NonClosableMdiSubWindow *m_terminalViewSubWindow;
  NonClosableMdiSubWindow *m_documentationWidgetSubWindow;
  NonClosableMdiSubWindow *m_ircWidgetSubWindow;

  // Dock widgets.
  WorkspaceView *m_workspaceView;
  HistoryDockWidget *m_historyDockWidget;
  FilesDockWidget *m_filesDockWidget;

  // Editor's lexer
  LexerOctaveGui *m_lexer;
  QsciAPIs *m_lexerAPI;

  // Toolbars.
  QStatusBar *m_statusBar;

  // Flag for closing whole application
  bool m_closeApplication;

};

#endif // MAINWINDOW_H
