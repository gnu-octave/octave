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

#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QSettings>
#include <QDesktopServices>
#include <QFileDialog>
#include "MainWindow.h"
#include "FileEditorMdiSubWindow.h"
#include "ImageViewerMdiSubWindow.h"
#include "SettingsDialog.h"
#include "cmd-edit.h"

#define VERSION_STRING "Octave GUI (0.6.0)"

MainWindow::MainWindow (QWidget * parent):QMainWindow (parent),
m_isRunning (true)
{
  QDesktopServices desktopServices;
  m_settingsFile =
    desktopServices.storageLocation (QDesktopServices::HomeLocation) +
    "/.quint/settings.ini";
  construct ();
  establishOctaveLink ();
}

MainWindow::~MainWindow ()
{
}

void
MainWindow::handleOpenFileRequest (QString fileName)
{
  reportStatusMessage (tr ("Opening file."));
  QPixmap pixmap;
  if (pixmap.load (fileName))
    {
//        ImageViewerMdiSubWindow *subWindow = new ImageViewerMdiSubWindow(pixmap, this);
//        subWindow->setAttribute(Qt::WA_DeleteOnClose);
//        m_centralMdiArea->addSubWindow(subWindow);
//        subWindow->setWindowTitle(fileName);
    }
  else
    {
      FileEditorMdiSubWindow *subWindow = new FileEditorMdiSubWindow (m_centralMdiArea);
      subWindow->setAttribute (Qt::WA_DeleteOnClose);
      subWindow->loadFile (fileName);
    }
}

void
MainWindow::reportStatusMessage (QString statusMessage)
{
  m_statusBar->showMessage (statusMessage, 1000);
}

void
MainWindow::openWebPage (QString url)
{
  m_documentationWidget->load (QUrl (url));
}

void
MainWindow::handleSaveWorkspaceRequest ()
{
  QDesktopServices desktopServices;
  QString selectedFile =
    QFileDialog::getSaveFileName (this, tr ("Save Workspace"),
				  desktopServices.
				  storageLocation (QDesktopServices::
						   HomeLocation) +
				  "/.quint/workspace");
  m_octaveTerminal->sendText (QString ("save \'%1\'\n").arg (selectedFile));
  m_octaveTerminal->setFocus ();
}

void
MainWindow::handleLoadWorkspaceRequest ()
{
  QDesktopServices desktopServices;
  QString selectedFile =
    QFileDialog::getOpenFileName (this, tr ("Load Workspace"),
				  desktopServices.
				  storageLocation (QDesktopServices::
						   HomeLocation) +
				  "/.quint/workspace");
  m_octaveTerminal->sendText (QString ("load \'%1\'\n").arg (selectedFile));
  m_octaveTerminal->setFocus ();
}

void
MainWindow::handleClearWorkspaceRequest ()
{
  m_octaveTerminal->sendText ("clear\n");
  m_octaveTerminal->setFocus ();
}

void
MainWindow::handleCommandDoubleClicked (QString command)
{
  m_octaveTerminal->sendText (command);
  m_octaveTerminal->setFocus ();
}

void
MainWindow::alignMdiWindows ()
{
  m_centralMdiArea->tileSubWindows ();
}

void
MainWindow::openEditor ()
{
  FileEditorMdiSubWindow *subWindow = new FileEditorMdiSubWindow (m_centralMdiArea);
  subWindow->setAttribute (Qt::WA_DeleteOnClose);
  subWindow->newFile ();
}

void
MainWindow::openBugTrackerPage ()
{
  QDesktopServices::
    openUrl (QUrl ("http://savannah.gnu.org/bugs/?group=octave"));
}

void
MainWindow::openAgoraPage ()
{
  QDesktopServices::
    openUrl (QUrl ("http://agora.panocha.org.mx/"));
}

void
MainWindow::openOctaveForgePage ()
{
  QDesktopServices::
    openUrl (QUrl ("http://octave.sourceforge.net/"));
}

void
MainWindow::processSettingsDialogRequest ()
{
  SettingsDialog settingsDialog (this, m_settingsFile);
  settingsDialog.exec ();
}

void
MainWindow::closeEvent (QCloseEvent * closeEvent)
{
  m_isRunning = false;
  reportStatusMessage (tr ("Saving data and shutting down."));
  writeSettings ();

  m_octaveCallbackThread->terminate ();
  m_octaveCallbackThread->wait ();

  m_octaveMainThread->terminate ();
  QMainWindow::closeEvent (closeEvent);
}

void
MainWindow::readSettings ()
{
  QSettings settings (m_settingsFile, QSettings::IniFormat);
  restoreGeometry (settings.value ("MainWindow/geometry").toByteArray ());
  restoreState (settings.value ("MainWindow/windowState").toByteArray ());
}

void
MainWindow::writeSettings ()
{
  QSettings settings (m_settingsFile, QSettings::IniFormat);
  settings.setValue ("MainWindow/geometry", saveGeometry ());
  settings.setValue ("MainWindow/windowState", saveState ());
}

void
MainWindow::construct ()
{

  if (QFile::exists ("../media/logo.png"))
    setWindowIcon (QIcon ("../media/logo.png"));
  else
    setWindowIcon (QIcon ("/usr/share/octave/quint/media/logo.png"));

  // Initialize MDI area.
  m_centralMdiArea = new QMdiArea (this);
  m_centralMdiArea->setObjectName ("CentralMdiArea");
  m_centralMdiArea->setViewMode (QMdiArea::TabbedView);

  // Setup dockable widgets and the status bar.
  m_variablesDockWidget = new VariablesDockWidget (this);
  m_historyDockWidget = new HistoryDockWidget (this);
  m_filesDockWidget = new FilesDockWidget (this);
  m_statusBar = new QStatusBar (this);

  // Setup essential MDI Windows.
  m_octaveTerminal = new OctaveTerminal (this);
  m_documentationWidget = new BrowserWidget (this);
  m_ircWidget = new IRCWidget (this, m_settingsFile);

  m_documentationWidgetSubWindow =
    m_centralMdiArea->addSubWindow (m_documentationWidget,
				    Qt::WindowTitleHint | Qt::
				    WindowMinMaxButtonsHint);
  m_documentationWidgetSubWindow->
    setObjectName ("DocumentationWidgetSubWindow");
  m_documentationWidgetSubWindow->setWindowTitle (tr ("Documentation"));
  m_documentationWidgetSubWindow->
    setWindowIcon (QIcon ("../media/help_index.png"));

  m_octaveTerminalSubWindow =
    m_centralMdiArea->addSubWindow (m_octaveTerminal,
				    Qt::WindowTitleHint | Qt::
				    WindowMinMaxButtonsHint);
  m_octaveTerminalSubWindow->setObjectName ("OctaveTerminalSubWindow");
  m_octaveTerminalSubWindow->setWindowTitle (tr ("Terminal"));
  m_octaveTerminalSubWindow->setWindowIcon (QIcon ("../media/terminal.png"));

  m_ircWidgetSubWindow = m_centralMdiArea->addSubWindow (m_ircWidget,
							 Qt::
							 WindowTitleHint |
							 Qt::
							 WindowMinMaxButtonsHint);
  m_ircWidgetSubWindow->setObjectName ("ChatWidgetSubWindow");
  m_ircWidgetSubWindow->setWindowTitle (tr ("Chat"));
  m_ircWidgetSubWindow->setWindowIcon (QIcon ("../media/chat.png"));

  QMenu *controlMenu = menuBar ()->addMenu (tr ("Octave"));
  QAction *settingsAction = controlMenu->addAction (tr ("Settings"));
  controlMenu->addSeparator ();
  QAction *exitAction = controlMenu->addAction (tr ("Exit"));

  QMenu *interfaceMenu = menuBar ()->addMenu (tr ("Interface"));

  QAction *alignWindowsAction = interfaceMenu->addAction (tr ("Align Windows"));
  interfaceMenu->addSeparator ();
  QAction *showWorkspaceAction = interfaceMenu->addAction (tr ("Workspace"));
  showWorkspaceAction->setCheckable (true);

  QAction *showHistoryAction = interfaceMenu->addAction (tr ("History"));
  showHistoryAction->setCheckable (true);

  QAction *showFileBrowserAction = interfaceMenu->addAction (tr ("File Browser"));
  showFileBrowserAction->setCheckable (true);

  interfaceMenu->addSeparator ();
  QAction *openEditorAction = interfaceMenu->addAction (tr ("Open New Editor Window"));

  QMenu *workspaceMenu = menuBar ()->addMenu (tr ("Workspace"));
  QAction *loadWorkspaceAction = workspaceMenu->addAction (tr ("Load"));
  QAction *saveWorkspaceAction = workspaceMenu->addAction (tr ("Save"));
  workspaceMenu->addSeparator ();
  QAction *clearWorkspaceAction = workspaceMenu->addAction (tr ("Clear"));

  QMenu *communityMenu = menuBar ()->addMenu (tr ("Community"));
  QAction *reportBugAction = communityMenu->addAction (tr ("Report Bug"));
  QAction *agoraAction = communityMenu->addAction (tr ("Agora"));
  QAction *octaveForgeAction = communityMenu->addAction (tr ("Octave Forge"));

  connect (settingsAction, SIGNAL (triggered ()), this, SLOT (processSettingsDialogRequest ()));
  connect (exitAction, SIGNAL (triggered ()), this, SLOT (close ()));
  connect (alignWindowsAction, SIGNAL (triggered ()), this, SLOT (alignMdiWindows ()));
  connect (openEditorAction, SIGNAL (triggered ()), this, SLOT (openEditor ()));
  connect (reportBugAction, SIGNAL (triggered ()), this, SLOT (openBugTrackerPage ()));
  connect (agoraAction, SIGNAL (triggered ()), this, SLOT (openAgoraPage ()));
  connect (octaveForgeAction, SIGNAL (triggered ()), this, SLOT (openOctaveForgePage ()));

  connect (showWorkspaceAction, SIGNAL (toggled (bool)), m_variablesDockWidget, SLOT (setShown (bool)));
  connect (m_variablesDockWidget, SIGNAL (visibilityChanged (bool)), showWorkspaceAction, SLOT (setChecked (bool)));
  connect (showHistoryAction, SIGNAL (toggled (bool)), m_historyDockWidget, SLOT (setShown (bool)));
  connect (m_variablesDockWidget, SIGNAL (visibilityChanged (bool)), showHistoryAction, SLOT (setChecked (bool)));
  connect (showFileBrowserAction, SIGNAL (toggled (bool)), m_filesDockWidget, SLOT (setShown (bool)));
  connect (m_filesDockWidget, SIGNAL (visibilityChanged (bool)), showFileBrowserAction, SLOT (setChecked (bool)));

  setWindowTitle (QString (VERSION_STRING));

  setCentralWidget (m_centralMdiArea);
  addDockWidget (Qt::LeftDockWidgetArea, m_variablesDockWidget);
  addDockWidget (Qt::LeftDockWidgetArea, m_historyDockWidget);
  addDockWidget (Qt::RightDockWidgetArea, m_filesDockWidget);
  setStatusBar (m_statusBar);

  readSettings ();

  connect (m_filesDockWidget, SIGNAL (openFile (QString)), this,
	   SLOT (handleOpenFileRequest (QString)));
  connect (m_historyDockWidget, SIGNAL (information (QString)), this,
	   SLOT (reportStatusMessage (QString)));
  connect (saveWorkspaceAction, SIGNAL (triggered ()), this,
	   SLOT (handleSaveWorkspaceRequest ()));
  connect (loadWorkspaceAction, SIGNAL (triggered ()), this,
	   SLOT (handleLoadWorkspaceRequest ()));
  connect (clearWorkspaceAction, SIGNAL (triggered ()), this,
	   SLOT (handleClearWorkspaceRequest ()));

  openWebPage ("http://www.gnu.org/software/octave/doc/interpreter/");
}

void
MainWindow::establishOctaveLink ()
{
  m_octaveTerminal->openTerminal ();

  m_octaveMainThread = new OctaveMainThread (this);
  m_octaveMainThread->start ();

  m_octaveCallbackThread = new OctaveCallbackThread (this, this);
  connect (m_octaveMainThread, SIGNAL(ready()), m_octaveCallbackThread, SLOT(start()));
  reportStatusMessage (tr ("Established link to Octave."));
}
