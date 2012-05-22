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
#include <QStyle>
#include <QToolBar>
#include <QDesktopServices>
#include <QFileDialog>
#include <QMessageBox>
#include <QIcon>

#include "MainWindow.h"
#include "FileEditor.h"
#include "SettingsDialog.h"

MainWindow::MainWindow (QWidget * parent):QMainWindow (parent)
{
  // We have to set up all our windows, before we finally launch octave.
  construct ();
  OctaveLink::instance ()->launchOctave();
}

MainWindow::~MainWindow ()
{
  OctaveLink::instance ()->terminateOctave();
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
  FileEditor *fileEditor = new FileEditor (m_terminalView, m_lexer, this);
  fileEditor->setAttribute (Qt::WA_DeleteOnClose);

  if (fileName.isEmpty ())
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
  m_closing = true;  // inform editor window that whole application is closed
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
  QStyle *style = QApplication::style ();
  // TODO: Check this.
  m_closing = false;   // flag for editor files when closed
  setWindowIcon (ResourceManager::instance ()->icon (ResourceManager::Octave));

  // Setup dockable widgets and the status bar.
  m_workspaceView = new WorkspaceView (this);
  m_workspaceView->setStatusTip (tr ("View the variables in the active workspace."));
  m_historyDockWidget = new HistoryDockWidget (this);
  m_historyDockWidget->setStatusTip (tr ("Browse and search the command history."));
  m_filesDockWidget = new FilesDockWidget (this);
  m_filesDockWidget->setStatusTip (tr ("Browse your files."));
  m_statusBar = new QStatusBar (this);

  m_currentDirectoryLineEdit = new QLineEdit (QDir::currentPath (), this);
  m_currentDirectoryLineEdit->setFixedWidth (300);

  m_currentDirectoryToolButton = new QToolButton (this);
  m_currentDirectoryToolButton->setIcon (style->standardIcon (QStyle::SP_DirOpenIcon));

  m_currentDirectoryUpToolButton = new QToolButton (this);
  m_currentDirectoryUpToolButton->setIcon (style->standardIcon (QStyle::SP_FileDialogToParent));

  // Octave Terminal subwindow.
  m_terminalView = new QTerminal(this);
  setCentralWidget (m_terminalView);

  m_lexer = NULL;  // initialise the empty lexer for the edtiors

  QMenu *fileMenu = menuBar ()->addMenu (tr ("&File"));
  QAction *newFileAction
    = fileMenu->addAction (QIcon::fromTheme ("document-new",
      style->standardIcon (QStyle::SP_FileIcon)), tr ("New File"));

  QAction *openFileAction
      = fileMenu->addAction (QIcon::fromTheme ("document-open",
        style->standardIcon (QStyle::SP_FileIcon)), tr ("Open File"));

  QAction *settingsAction = fileMenu->addAction (tr ("Settings"));
  fileMenu->addSeparator ();
  QAction *exitAction = fileMenu->addAction (tr ("Exit"));

  QMenu *editMenu = menuBar ()->addMenu (tr ("&Edit"));
  QAction *cutAction
      = editMenu->addAction (QIcon::fromTheme ("edit-cut",
        style->standardIcon (QStyle::SP_FileIcon)), tr ("Cut"));
  cutAction->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_X));

  QAction *copyAction
      = editMenu->addAction (QIcon::fromTheme ("edit-copy",
        style->standardIcon (QStyle::SP_FileIcon)), tr ("Copy"));
  copyAction->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_C));

  QAction *pasteAction
      = editMenu->addAction (QIcon::fromTheme ("edit-paste",
        style->standardIcon (QStyle::SP_FileIcon)), tr ("Paste"));
  pasteAction->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_V));

  QAction *undoAction
      = editMenu->addAction (QIcon::fromTheme ("edit-undo",
        style->standardIcon (QStyle::SP_FileIcon)), tr ("Undo"));

  QAction *redoAction
      = editMenu->addAction (QIcon::fromTheme ("edit-redo",
        style->standardIcon (QStyle::SP_FileIcon)), tr ("Redo"));

  //QMenu *debugMenu = menuBar ()->addMenu (tr ("De&bug"));
  //QMenu *parallelMenu = menuBar ()->addMenu (tr ("&Parallel"));

  QMenu *desktopMenu = menuBar ()->addMenu (tr ("&Desktop"));
  QAction *loadWorkspaceAction = desktopMenu->addAction (tr ("Load workspace"));
  QAction *saveWorkspaceAction = desktopMenu->addAction (tr ("Save workspace"));
  QAction *clearWorkspaceAction = desktopMenu->addAction (tr ("Clear workspace"));

  // Window menu
  QMenu *windowMenu = menuBar ()->addMenu (tr ("&Window"));
  QAction *showWorkspaceAction = windowMenu->addAction (tr ("Workspace"));
  showWorkspaceAction->setCheckable (true);
  QAction *showHistoryAction = windowMenu->addAction (tr ("History"));
  showHistoryAction->setCheckable (true);
  QAction *showFileBrowserAction = windowMenu->addAction (tr ("File Browser"));
  showFileBrowserAction->setCheckable (true);

  // Help menu
  QMenu *helpMenu = menuBar ()->addMenu (tr ("&Help"));
  QAction *reportBugAction = helpMenu->addAction (tr ("Report Bug"));
  QAction *agoraAction = helpMenu->addAction (tr ("Visit Agora"));
  QAction *octaveForgeAction = helpMenu->addAction (tr ("Visit Octave Forge"));
  helpMenu->addSeparator ();
  QAction *aboutOctaveAction = helpMenu->addAction (tr ("About Octave"));

  // Toolbars

  QToolBar *mainToolBar = addToolBar ("Main");
  mainToolBar->addAction (newFileAction);
  mainToolBar->addAction (openFileAction);
  mainToolBar->addSeparator ();
  mainToolBar->addAction (cutAction);
  mainToolBar->addAction (copyAction);
  mainToolBar->addAction (pasteAction);
  mainToolBar->addAction (undoAction);
  mainToolBar->addAction (redoAction);
  mainToolBar->addSeparator ();
  mainToolBar->addWidget (new QLabel (tr ("Current Directory:")));
  mainToolBar->addWidget (m_currentDirectoryLineEdit);
  mainToolBar->addWidget (m_currentDirectoryToolButton);
  mainToolBar->addWidget (m_currentDirectoryUpToolButton);

  connect (settingsAction, SIGNAL (triggered ()), this, SLOT (processSettingsDialogRequest ()));
  connect (exitAction, SIGNAL (triggered ()), this, SLOT (close ()));
  connect (newFileAction, SIGNAL (triggered ()), this, SLOT (newFile ()));
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

  connect (copyAction, SIGNAL (triggered()), m_terminalView, SLOT(copyClipboard ()));
  connect (pasteAction, SIGNAL (triggered()), m_terminalView, SLOT(pasteClipboard ()));
  setWindowTitle ("Octave");

  addDockWidget (Qt::LeftDockWidgetArea, m_workspaceView);
  addDockWidget (Qt::LeftDockWidgetArea, m_historyDockWidget);
  addDockWidget (Qt::RightDockWidgetArea, m_filesDockWidget);
  setStatusBar (m_statusBar);

  // this has to be done only once, not for each editor
  m_lexer = new LexerOctaveGui ();

  // Editor font (default or from settings)
  QSettings *settings = ResourceManager::instance ()->settings ();
  m_lexer->setDefaultFont (QFont (
                             settings->value ("editor/fontName","Courier").toString (),
                             settings->value ("editor/fontSize",10).toInt ()));

  // TODO: Autoindent not working as it should
  m_lexer->setAutoIndentStyle (QsciScintilla::AiMaintain ||
                               QsciScintilla::AiOpening  ||
                               QsciScintilla::AiClosing);

  // The API info that is used for auto completion
  // TODO: Where to store a file with API info (raw or prepared?)?
  // TODO: Also provide infos on octave-forge functions?
  // TODO: Also provide infos on function parameters?
  // By now, use the keywords-list from syntax highlighting
  m_lexerAPI = new QsciAPIs (m_lexer);

  QString keyword;
  QStringList keywordList;
  keyword = m_lexer->keywords (1);  // get whole string with all keywords
  keywordList = keyword.split (QRegExp ("\\s+"));  // split into single strings
  int i;
  for (i=0; i<keywordList.size(); i++)
    {
      m_lexerAPI->add (keywordList.at (i));  // add single strings to the API
    }
  m_lexerAPI->prepare ();           // prepare API info ... this make take some time

  readSettings ();
  updateTerminalFont();
}

