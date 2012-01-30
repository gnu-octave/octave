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

#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QSettings>
#include <QDesktopServices>
#include <QFileDialog>
#include <QMessageBox>
#include "MainWindow.h"
#include "FileEditorMdiSubWindow.h"
#include "ImageViewerMdiSubWindow.h"
#include "SettingsDialog.h"

#define VERSION_STRING "Octave GUI (0.8.8)"

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
      openEditorFile(fileName);
    }
}

void
MainWindow::openEditor ()
{
  openEditorFile(QString());
}
void
MainWindow::openEditorFile (QString fileName)
{
  FileEditorMdiSubWindow *subWindow = new FileEditorMdiSubWindow (m_centralMdiArea);
  subWindow->setAttribute (Qt::WA_DeleteOnClose);
  // check whether lexer is already prepared and prepare it if not
  if ( m_lexer == NULL )
    {
      // this has to be done only once, not for each editor
      m_lexer = new LexerOctaveGui();
      // Editor font (default or from settings)
      QSettings *settings = ResourceManager::instance ()->settings ();
      m_lexer->setDefaultFont( QFont(
                  settings->value ("editor/fontName","Courier").toString (),
                  settings->value ("editor/fontSize",10).toInt () ) );
      // TODO: Autoindent not working as it should
      m_lexer->setAutoIndentStyle(QsciScintilla::AiMaintain ||
                                  QsciScintilla::AiOpening  ||
                                  QsciScintilla::AiClosing);
      // The API info that is used for auto completion
      // TODO: Where to store a file with API info (raw or prepared?)?
      // TODO: Also provide infos on octave-forge functions?
      // TODO: Also provide infos on function parameters?
      // By now, use the keywords-list from syntax highlighting
       m_lexerAPI = new QsciAPIs(m_lexer);
       QString keyword;
       QStringList keywordList;
       keyword     = m_lexer->keywords(1);  // get whole string with all keywords
       keywordList = keyword.split(QRegExp("\\s+"));  // split into single strings
       int i;
       for ( i=0; i<keywordList.size(); i++ )
         {
           m_lexerAPI->add(keywordList.at(i));  // add single strings to the API
         }
       m_lexerAPI->prepare();           // prepare API info ... this make take some time
    }
  //subWindow->initEditor(m_terminalView, m_lexer, this);   // init necessary informations for editor

  if ( fileName.isEmpty() )
    subWindow->newFile ();
  else
    subWindow->loadFile (fileName);
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
  m_terminalView->sendText (QString ("save \'%1\'\n").arg (selectedFile));
  m_terminalView->setFocus ();
}

void
MainWindow::handleLoadWorkspaceRequest ()
{
  QString selectedFile =
    QFileDialog::getOpenFileName (this, tr ("Load Workspace"),
                                  ResourceManager::instance ()->homePath ());
  m_terminalView->sendText (QString ("load \'%1\'\n").arg (selectedFile));
  m_terminalView->setFocus ();
}

void
MainWindow::handleClearWorkspaceRequest ()
{
  m_terminalView->sendText ("clear\n");
  m_terminalView->setFocus ();
}

void
MainWindow::handleCommandDoubleClicked (QString command)
{
  m_terminalView->sendText(command);
  m_terminalView->setFocus ();
}

void
MainWindow::handleUnreadMessages (bool yes)
{
  if (yes)
    {
      m_ircWidgetSubWindow
        ->setWindowIcon
          (ResourceManager::instance ()->icon (ResourceManager::ChatNewMessage));
    }
  else
    {
      m_ircWidgetSubWindow
        ->setWindowIcon
          (ResourceManager::instance ()->icon (ResourceManager::Chat));
    }
}

void
MainWindow::alignMdiWindows ()
{
  m_centralMdiArea->tileSubWindows ();
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
  ResourceManager::instance ()->updateNetworkSettings ();
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
  m_closeApplication = true;  // inform editor window that whole application is closed
  OctaveLink::instance ()->terminateOctave();
  m_centralMdiArea->closeAllSubWindows();   // send close events to subwindows
                                            // (editor files can be saved!)
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
  m_closeApplication = false;   // flag for editor files when closed
  setWindowIcon (ResourceManager::instance ()->icon (ResourceManager::Octave));

  // Initialize MDI area.
  m_centralMdiArea = new QMdiArea (this);
  m_centralMdiArea->setObjectName ("CentralMdiArea");
  m_centralMdiArea->setViewMode (QMdiArea::TabbedView);

  // Setup dockable widgets and the status bar.
  m_workspaceView = new WorkspaceView (this);
  m_workspaceView->setStatusTip (tr ("View the variables in the active workspace."));
  m_historyDockWidget = new HistoryDockWidget (this);
  m_historyDockWidget->setStatusTip (tr ("Browse and search the command history."));
  m_filesDockWidget = new FilesDockWidget (this);
  m_filesDockWidget->setStatusTip (tr ("Browse your files."));
  m_statusBar = new QStatusBar (this);

  // Documentation subwindow.
  m_documentationWidget = new BrowserWidget (this);
  m_documentationWidgetSubWindow = new NonClosableMdiSubWindow (this);
  m_documentationWidgetSubWindow->setWidget (m_documentationWidget);
  m_centralMdiArea->addSubWindow (m_documentationWidgetSubWindow, Qt::WindowTitleHint | Qt::WindowMinMaxButtonsHint);

  m_documentationWidgetSubWindow->setObjectName ("DocumentationWidgetSubWindow");
  m_documentationWidgetSubWindow->setWindowTitle (tr ("Documentation"));
  m_documentationWidgetSubWindow
      ->setWindowIcon (ResourceManager::instance ()->icon (ResourceManager::Documentation));
  m_documentationWidgetSubWindow->setFocusProxy (m_documentationWidget);
  m_documentationWidgetSubWindow->setStatusTip (tr ("Browse the Octave documentation for help."));
  m_documentationWidgetSubWindow->setMinimumSize (300, 300);

  // Octave Terminal subwindow.
  m_terminalView = new QTerminal(this);
  m_terminalViewSubWindow = new NonClosableMdiSubWindow (this);
  m_terminalViewSubWindow->setWidget (m_terminalView);
  m_centralMdiArea->addSubWindow (m_terminalViewSubWindow, Qt::WindowTitleHint | Qt::WindowMinMaxButtonsHint);

  m_terminalViewSubWindow->setObjectName ("OctaveTerminalSubWindow");
  m_terminalViewSubWindow->setWindowTitle (tr ("Terminal"));
  m_terminalViewSubWindow
      ->setWindowIcon (ResourceManager::instance ()->icon (ResourceManager::Terminal));
  m_terminalViewSubWindow->setFocusProxy (m_terminalView);
  m_terminalViewSubWindow->setStatusTip (tr ("Enter your commands into the Octave terminal."));
  m_terminalViewSubWindow->setMinimumSize (300, 300);

  // Chat subwindow.
  // Deactivated in the development process.
  /*
  m_ircWidget = new QIRCWidget (this);
  m_ircWidgetSubWindow = new NonClosableMdiSubWindow (this);
  m_ircWidgetSubWindow->setWidget(m_ircWidget);
  m_centralMdiArea->addSubWindow (m_ircWidgetSubWindow, Qt::WindowTitleHint | Qt::WindowMinMaxButtonsHint);

  m_ircWidgetSubWindow->setObjectName ("ChatWidgetSubWindow");
  m_ircWidgetSubWindow->setWindowTitle (tr ("Chat"));
  m_ircWidgetSubWindow
      ->setWindowIcon (ResourceManager::instance ()->icon (ResourceManager::Chat));
  m_ircWidgetSubWindow->setStatusTip(tr ("Instantly chat with other Octave users for help."));
  m_ircWidgetSubWindow->setFocusProxy (m_ircWidget);
  m_ircWidgetSubWindow->setMinimumSize (300, 300);
  //connect (m_ircWidget, SIGNAL (unreadMessages (bool)), this, SLOT (handleUnreadMessages (bool)));

  m_ircWidget->connectToServer("irc.freenode.net", "Octave-GUI-User", "#octave");
  */

  m_lexer = NULL;  // initialise the empty lexer for the edtiors

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

  connect (showWorkspaceAction, SIGNAL (toggled (bool)), m_workspaceView, SLOT (setShown (bool)));
  connect (m_workspaceView, SIGNAL (activeChanged (bool)), showWorkspaceAction, SLOT (setChecked (bool)));
  connect (showHistoryAction, SIGNAL (toggled (bool)), m_historyDockWidget, SLOT (setShown (bool)));
  connect (m_historyDockWidget, SIGNAL (activeChanged (bool)), showHistoryAction, SLOT (setChecked (bool)));
  connect (showFileBrowserAction, SIGNAL (toggled (bool)), m_filesDockWidget, SLOT (setShown (bool)));
  connect (m_filesDockWidget, SIGNAL (activeChanged (bool)), showFileBrowserAction, SLOT (setChecked (bool)));

  //connect (this, SIGNAL (settingsChanged ()), m_workspaceView, SLOT (noticeSettings ()));
  //connect (this, SIGNAL (settingsChanged ()), m_historyDockWidget, SLOT (noticeSettings ()));
  connect (this, SIGNAL (settingsChanged ()), m_filesDockWidget, SLOT (noticeSettings ()));

  connect (m_filesDockWidget, SIGNAL (openFile (QString)), this, SLOT (handleOpenFileRequest (QString)));
  connect (m_historyDockWidget, SIGNAL (information (QString)), this, SLOT (reportStatusMessage (QString)));
  connect (m_historyDockWidget, SIGNAL (commandDoubleClicked (QString)), this, SLOT (handleCommandDoubleClicked (QString)));
  connect (saveWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleSaveWorkspaceRequest ()));
  connect (loadWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleLoadWorkspaceRequest ()));
  connect (clearWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleClearWorkspaceRequest ()));

  setWindowTitle (QString (VERSION_STRING));

  setCentralWidget (m_centralMdiArea);
  addDockWidget (Qt::LeftDockWidgetArea, m_workspaceView);
  addDockWidget (Qt::LeftDockWidgetArea, m_historyDockWidget);
  addDockWidget (Qt::RightDockWidgetArea, m_filesDockWidget);
  setStatusBar (m_statusBar);

  readSettings ();
  openWebPage ("http://www.gnu.org/software/octave/doc/interpreter/");
}

