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

#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QSettings>
#include <QDesktopServices>
#include <QFileDialog>
#include <QMessageBox>
#include "MainWindow.h"
#include "FileEditor.h"
#include "SettingsDialog.h"

#define VERSION_STRING "Octave GUI (0.8.8)"

MainWindow::MainWindow (QWidget * parent):QMainWindow (parent)
{
  // We have to set up all our windows, before we finally launch octave.
  construct ();
  OctaveLink::instance ()->launchOctave();
}

MainWindow::~MainWindow ()
{
}

void
MainWindow::openExistingFile (QString fileName)
{
  reportStatusMessage (tr ("Opening file.."));
  newEditorWindow(fileName);
}

void
MainWindow::newFile ()
{
  newEditorWindow(QString());
}

void
MainWindow::newEditorWindow (QString fileName)
{
  FileEditor *fileEditor = new FileEditor ();
  fileEditor->setAttribute (Qt::WA_DeleteOnClose);
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
  fileEditor->initEditor(m_terminalView, m_lexer, this);   // init necessary informations for editor

  if ( fileName.isEmpty() )
    fileEditor->newFile ();
  else
    fileEditor->loadFile (fileName);
}


void
MainWindow::reportStatusMessage (QString statusMessage)
{
  m_statusBar->showMessage (statusMessage, 1000);
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
  SettingsDialog *settingsDialog = new SettingsDialog (this);
  settingsDialog->exec ();
  delete settingsDialog;
  emit settingsChanged ();
  ResourceManager::instance ()->updateNetworkSettings ();
  updateTerminalFont();
}

void
MainWindow::updateTerminalFont ()
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  QFont font = QFont();
  //font.setStyleHint(QFont::TypeWriter);
  font.setFamily(settings->value("terminal/fontName").toString());
  font.setPointSize(settings->value("terminal/fontSize").toInt ());
  m_terminalView->setTerminalFont(font);
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
MainWindow::closeEvent (QCloseEvent * closeEvent)
{
  reportStatusMessage (tr ("Saving data and shutting down."));
  writeSettings ();
  m_closeApplication = true;  // inform editor window that whole application is closed
  OctaveLink::instance ()->terminateOctave();

  QMainWindow::closeEvent (closeEvent);
}

void
MainWindow::readSettings ()
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  restoreGeometry (settings->value ("MainWindow/geometry").toByteArray ());
  restoreState (settings->value ("MainWindow/windowState").toByteArray ());
  emit settingsChanged ();
}

void
MainWindow::writeSettings ()
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  settings->setValue ("MainWindow/geometry", saveGeometry ());
  settings->setValue ("MainWindow/windowState", saveState ());
}

void
MainWindow::construct ()
{
  // TODO: Check this.
  m_closeApplication = false;   // flag for editor files when closed
  setWindowIcon (ResourceManager::instance ()->icon (ResourceManager::Octave));

  // Setup dockable widgets and the status bar.
  m_workspaceView = new WorkspaceView (this);
  m_workspaceView->setStatusTip (tr ("View the variables in the active workspace."));
  m_historyDockWidget = new HistoryDockWidget (this);
  m_historyDockWidget->setStatusTip (tr ("Browse and search the command history."));
  m_filesDockWidget = new FilesDockWidget (this);
  m_filesDockWidget->setStatusTip (tr ("Browse your files."));
  m_statusBar = new QStatusBar (this);

  // Octave Terminal subwindow.
  m_terminalView = new QTerminal(this);
  setCentralWidget (m_terminalView);

  m_lexer = NULL;  // initialise the empty lexer for the edtiors

  QMenu *controlMenu = menuBar ()->addMenu (tr ("Octave"));
  QAction *settingsAction = controlMenu->addAction (tr ("Settings"));
  controlMenu->addSeparator ();
  QAction *exitAction = controlMenu->addAction (tr ("Exit"));

  QMenu *interfaceMenu = menuBar ()->addMenu (tr ("Interface"));

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

  connect (settingsAction, SIGNAL (triggered ()), this, SLOT (processSettingsDialogRequest ()));
  connect (exitAction, SIGNAL (triggered ()), this, SLOT (close ()));
  connect (openEditorAction, SIGNAL (triggered ()), this, SLOT (newFile ()));
  connect (reportBugAction, SIGNAL (triggered ()), this, SLOT (openBugTrackerPage ()));
  connect (agoraAction, SIGNAL (triggered ()), this, SLOT (openAgoraPage ()));
  connect (octaveForgeAction, SIGNAL (triggered ()), this, SLOT (openOctaveForgePage ()));
  connect (aboutOctaveAction, SIGNAL (triggered ()), this, SLOT (showAboutOctave ()));

  connect (showWorkspaceAction, SIGNAL (toggled (bool)), m_workspaceView, SLOT (setShown (bool)));
  connect (m_workspaceView, SIGNAL (activeChanged (bool)), showWorkspaceAction, SLOT (setChecked (bool)));
  connect (showHistoryAction, SIGNAL (toggled (bool)), m_historyDockWidget, SLOT (setShown (bool)));
  connect (m_historyDockWidget, SIGNAL (activeChanged (bool)), showHistoryAction, SLOT (setChecked (bool)));
  connect (showFileBrowserAction, SIGNAL (toggled (bool)), m_filesDockWidget, SLOT (setShown (bool)));
  connect (m_filesDockWidget, SIGNAL (activeChanged (bool)), showFileBrowserAction, SLOT (setChecked (bool)));

  //connect (this, SIGNAL (settingsChanged ()), m_workspaceView, SLOT (noticeSettings ()));
  //connect (this, SIGNAL (settingsChanged ()), m_historyDockWidget, SLOT (noticeSettings ()));
  connect (this, SIGNAL (settingsChanged ()), m_filesDockWidget, SLOT (noticeSettings ()));

  connect (m_filesDockWidget, SIGNAL (openFile (QString)), this, SLOT (openExistingFile (QString)));
  connect (m_historyDockWidget, SIGNAL (information (QString)), this, SLOT (reportStatusMessage (QString)));
  connect (m_historyDockWidget, SIGNAL (commandDoubleClicked (QString)), this, SLOT (handleCommandDoubleClicked (QString)));
  connect (saveWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleSaveWorkspaceRequest ()));
  connect (loadWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleLoadWorkspaceRequest ()));
  connect (clearWorkspaceAction, SIGNAL (triggered ()), this, SLOT (handleClearWorkspaceRequest ()));

  setWindowTitle (QString (VERSION_STRING));

  addDockWidget (Qt::LeftDockWidgetArea, m_workspaceView);
  addDockWidget (Qt::LeftDockWidgetArea, m_historyDockWidget);
  addDockWidget (Qt::RightDockWidgetArea, m_filesDockWidget);
  setStatusBar (m_statusBar);

  readSettings ();
  updateTerminalFont();
}

