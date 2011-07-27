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

#define VERSION_STRING "Octave GUI (0.6.9)"

MainWindow::MainWindow (QWidget * parent):QMainWindow (parent)
{
  construct ();
  OctaveLink::instance ()->launchOctave();
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
  QString selectedFile =
    QFileDialog::getSaveFileName (this, tr ("Save Workspace"),
                                  ResourceManager::instance ()->homePath ());
  m_octaveTerminal->sendText (QString ("save \'%1\'\n").arg (selectedFile));
  m_octaveTerminal->setFocus ();
}

void
MainWindow::handleLoadWorkspaceRequest ()
{
  QString selectedFile =
    QFileDialog::getOpenFileName (this, tr ("Load Workspace"),
                                  ResourceManager::instance ()->homePath ());
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
  QDesktopServices::openUrl (QUrl ("http://savannah.gnu.org/bugs/?group=octave"));
}

void
MainWindow::openAgoraPage ()
{
  QDesktopServices::openUrl (QUrl ("http://agora.panocha.org.mx/"));
}

void
MainWindow::openOctaveForgePage ()
{
  QDesktopServices::openUrl (QUrl ("http://octave.sourceforge.net/"));
}

void
MainWindow::processSettingsDialogRequest ()
{
  SettingsDialog settingsDialog (this);
  settingsDialog.exec ();
  emit settingsChanged ();
}

void
MainWindow::showAboutOctave ()
{
  QString message =
      "GNU Octave\n"
      "Copyright (C) 2009 John W. Eaton and others.\n"
      "This is free software; see the source code for copying conditions."
      "There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or"
      "FITNESS FOR A PARTICULAR PURPOSE.  For details, type `warranty'.\n"
      "\n"
      "Octave was configured for \"x86_64-pc-linux-gnu\".\n"
      "\n"
      "Additional information about Octave is available at http://www.octave.org.\n"
      "\n"
      "Please contribute if you find this software useful."
      "For more information, visit http://www.octave.org/help-wanted.html\n"
      "\n"
      "Report bugs to <bug@octave.org> (but first, please read"
      "http://www.octave.org/bugs.html to learn how to write a helpful report).\n"
      "\n"
      "For information about changes from previous versions, type `news'.\n";

  QMessageBox::about (this, tr ("About Octave"), message);
}

void
MainWindow::showAboutQt ()
{
  QMessageBox::aboutQt (this);
}

void
MainWindow::closeEvent (QCloseEvent * closeEvent)
{
  reportStatusMessage (tr ("Saving data and shutting down."));
  writeSettings ();

  OctaveLink::instance ()->terminateOctave();
  QMainWindow::closeEvent (closeEvent);
}

void
MainWindow::readSettings ()
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  restoreGeometry (settings->value ("MainWindow/geometry").toByteArray ());
  restoreState (settings->value ("MainWindow/windowState").toByteArray ());
  m_centralMdiArea->restoreGeometry (settings->value ("MdiArea/geometry").toByteArray ());
  emit settingsChanged ();
}

void
MainWindow::writeSettings ()
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  settings->setValue ("MainWindow/geometry", saveGeometry ());
  settings->setValue ("MainWindow/windowState", saveState ());
  settings->setValue ("MdiArea/geometry", m_centralMdiArea->saveGeometry ());
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
  m_variablesDockWidget->setStatusTip (tr ("View the variables in the active workspace."));
  m_historyDockWidget = new HistoryDockWidget (this);
  m_historyDockWidget->setStatusTip (tr ("Browse and search the command history."));
  m_filesDockWidget = new FilesDockWidget (this);
  m_filesDockWidget->setStatusTip (tr ("Browse your files."));
  m_statusBar = new QStatusBar (this);

  // Setup essential MDI Windows.
  m_octaveTerminal = new OctaveTerminal (this);
  m_documentationWidget = new BrowserWidget (this);
  m_ircWidget = new IRCWidget (this);

  m_octaveTerminal->openTerminal ();

  // Octave Terminal subwindow.
  m_octaveTerminalSubWindow =
    m_centralMdiArea->addSubWindow (m_octaveTerminal,
				    Qt::WindowTitleHint | Qt::
				    WindowMinMaxButtonsHint);
  m_octaveTerminalSubWindow->setObjectName ("OctaveTerminalSubWindow");
  m_octaveTerminalSubWindow->setWindowTitle (tr ("Terminal"));
  m_octaveTerminalSubWindow->setWindowIcon (QIcon ("../media/terminal.png"));
  m_octaveTerminalSubWindow->setStatusTip (tr ("Enter your commands into the Octave terminal."));

  // Documentation subwindow.
  m_documentationWidgetSubWindow =
    m_centralMdiArea->addSubWindow (m_documentationWidget,
                                    Qt::WindowTitleHint | Qt::
                                    WindowMinMaxButtonsHint);
  m_documentationWidgetSubWindow->setObjectName ("DocumentationWidgetSubWindow");
  m_documentationWidgetSubWindow->setWindowTitle (tr ("Documentation"));
  m_documentationWidgetSubWindow->setWindowIcon (QIcon ("../media/help_index.png"));
  m_documentationWidgetSubWindow->setStatusTip (tr ("Browse the Octave documentation for help."));

  // Chat subwindow.
  m_ircWidgetSubWindow = m_centralMdiArea->addSubWindow (m_ircWidget,
                                                         Qt::
                                                         WindowTitleHint |
                                                         Qt::
                                                         WindowMinMaxButtonsHint);
  m_ircWidgetSubWindow->setObjectName ("ChatWidgetSubWindow");
  m_ircWidgetSubWindow->setWindowTitle (tr ("Chat"));
  m_ircWidgetSubWindow->setWindowIcon (QIcon ("../media/chat.png"));
  m_ircWidgetSubWindow->setStatusTip(tr ("Instantly chat with other Octave users for help."));

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
  communityMenu->addSeparator ();
  QAction *aboutOctaveAction = communityMenu->addAction (tr ("About Octave"));
  QAction *aboutQt = communityMenu->addAction (tr ("About Qt"));

  connect (settingsAction, SIGNAL (triggered ()), this, SLOT (processSettingsDialogRequest ()));
  connect (exitAction, SIGNAL (triggered ()), this, SLOT (close ()));
  connect (alignWindowsAction, SIGNAL (triggered ()), this, SLOT (alignMdiWindows ()));
  connect (openEditorAction, SIGNAL (triggered ()), this, SLOT (openEditor ()));
  connect (reportBugAction, SIGNAL (triggered ()), this, SLOT (openBugTrackerPage ()));
  connect (agoraAction, SIGNAL (triggered ()), this, SLOT (openAgoraPage ()));
  connect (octaveForgeAction, SIGNAL (triggered ()), this, SLOT (openOctaveForgePage ()));
  connect (aboutOctaveAction, SIGNAL (triggered ()), this, SLOT (showAboutOctave ()));
  connect (aboutQt, SIGNAL (triggered ()), this, SLOT (showAboutQt ()));

  // TODO: Visibility cannot be taken as a signal, because it will be emitted even then
  // the dock widget is tabbed or minimized.
  connect (showWorkspaceAction, SIGNAL (toggled (bool)), m_variablesDockWidget, SLOT (setShown (bool)));
  //connect (m_variablesDockWidget, SIGNAL (visibilityChanged (bool)), showWorkspaceAction, SLOT (setChecked (bool)));
  connect (showHistoryAction, SIGNAL (toggled (bool)), m_historyDockWidget, SLOT (setShown (bool)));
  //connect (m_historyDockWidget, SIGNAL (visibilityChanged (bool)), showHistoryAction, SLOT (setChecked (bool)));
  connect (showFileBrowserAction, SIGNAL (toggled (bool)), m_filesDockWidget, SLOT (setShown (bool)));
  //connect (m_filesDockWidget, SIGNAL (visibilityChanged (bool)), showFileBrowserAction, SLOT (setChecked (bool)));

  connect (this, SIGNAL (settingsChanged ()), m_variablesDockWidget, SLOT (noticeSettings ()));
  connect (this, SIGNAL (settingsChanged ()), m_historyDockWidget, SLOT (noticeSettings ()));
  connect (this, SIGNAL (settingsChanged ()), m_filesDockWidget, SLOT (noticeSettings ()));

  connect (m_filesDockWidget, SIGNAL (openFile (QString)), this, SLOT (handleOpenFileRequest (QString)));
  connect (m_historyDockWidget, SIGNAL (information (QString)), this, SLOT (reportStatusMessage (QString)));
  connect (m_historyDockWidget, SIGNAL (commandDoubleClicked (QString)), this, SLOT (handleCommandDoubleClicked (QString)));
  connect (saveWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleSaveWorkspaceRequest ()));
  connect (loadWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleLoadWorkspaceRequest ()));
  connect (clearWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleClearWorkspaceRequest ()));

  setWindowTitle (QString (VERSION_STRING));

  setCentralWidget (m_centralMdiArea);
  addDockWidget (Qt::LeftDockWidgetArea, m_variablesDockWidget);
  addDockWidget (Qt::LeftDockWidgetArea, m_historyDockWidget);
  addDockWidget (Qt::RightDockWidgetArea, m_filesDockWidget);
  setStatusBar (m_statusBar);

  readSettings ();
  openWebPage ("http://www.gnu.org/software/octave/doc/interpreter/");
}

