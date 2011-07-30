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
#include "ResourceManager.h"
#include "OctaveTerminal.h"
#include "OctaveLink.h"
#include "VariablesDockWidget.h"
#include "HistoryDockWidget.h"
#include "FilesDockWidget.h"
#include "BrowserWidget.h"
#include "IRCWidget.h"
#include "lexer/lexeroctavegui.h"

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

  OctaveTerminal *octaveTerminal ()
  {
    return m_octaveTerminal;
  }
  VariablesDockWidget *variablesDockWidget ()
  {
    return m_variablesDockWidget;
  }
  HistoryDockWidget *historyDockWidget ()
  {
    return m_historyDockWidget;
  }
  FilesDockWidget *filesDockWidget ()
  {
    return m_filesDockWidget;
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
  OctaveTerminal *m_octaveTerminal;
  BrowserWidget *m_documentationWidget;
  IRCWidget *m_ircWidget;

  QMdiSubWindow *m_octaveTerminalSubWindow;
  QMdiSubWindow *m_documentationWidgetSubWindow;
  QMdiSubWindow *m_ircWidgetSubWindow;

  // Dock widgets.
  VariablesDockWidget *m_variablesDockWidget;
  HistoryDockWidget *m_historyDockWidget;
  FilesDockWidget *m_filesDockWidget;

  // Editor's lexer
  LexerOctaveGui *m_lexer;
  QsciAPIs *m_lexerAPI;

  // Toolbars.
  QStatusBar *m_statusBar;
};

#endif // MAINWINDOW_H
