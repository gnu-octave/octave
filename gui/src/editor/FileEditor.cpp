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

#include "FileEditor.h"
#include <QVBoxLayout>
#include <QApplication>
#include <QFile>
#include <QFont>
#include <QFileDialog>
#include <QMessageBox>
#include <QStyle>
#include <QTextStream>

FileEditor::FileEditor (QTerminal *terminal, MainWindow *mainWindow)
  : FileEditorInterface(terminal, mainWindow)
{
  construct ();

  m_terminal = terminal;
  m_mainWindow = mainWindow;
  setVisible (false);
}

FileEditor::~FileEditor ()
{
}

LexerOctaveGui *
FileEditor::lexer ()
{
  return m_lexer;
}

QTerminal *
FileEditor::terminal ()
{
  return m_terminal;
}

MainWindow *
FileEditor::mainWindow ()
{
  return m_mainWindow;
}

void
FileEditor::requestNewFile ()
{
  FileEditorTab *fileEditorTab = new FileEditorTab (this);
  if (fileEditorTab)
    {
      addFileEditorTab (fileEditorTab);
      fileEditorTab->newFile ();
    }
}

void
FileEditor::requestOpenFile ()
{
  FileEditorTab *fileEditorTab = new FileEditorTab (this);
  if (fileEditorTab)
    {
      addFileEditorTab (fileEditorTab);
      if (!fileEditorTab->openFile ())
        {
          // If no file was loaded, remove the tab again.
          m_tabWidget->removeTab (m_tabWidget->indexOf (fileEditorTab));
        }
    }
}

void
FileEditor::requestOpenFile (QString fileName)
{
  if (!isVisible ())
    {
      show ();
    }

  FileEditorTab *fileEditorTab = new FileEditorTab (this);
  if (fileEditorTab)
    {
      addFileEditorTab (fileEditorTab);
      fileEditorTab->loadFile (fileName);
    }
}

void
FileEditor::requestUndo ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->undo ();
}

void
FileEditor::requestRedo ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->redo ();
}

void
FileEditor::requestCopy ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->copy ();
}

void
FileEditor::requestCut ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->cut ();
}

void
FileEditor::requestPaste ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->paste ();
}

void
FileEditor::requestSaveFile ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->saveFile ();
}

void
FileEditor::requestSaveFileAs ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->saveFileAs ();
}

void
FileEditor::requestRunFile ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->runFile ();
}

void
FileEditor::requestToggleBookmark ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->toggleBookmark ();
}

void
FileEditor::requestNextBookmark ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->nextBookmark ();
}

void
FileEditor::requestPreviousBookmark ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->previousBookmark ();
}

void
FileEditor::requestRemoveBookmark ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->removeBookmark ();
}

void
FileEditor::requestCommentSelectedText ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->commentSelectedText ();
}

void
FileEditor::requestUncommentSelectedText ()
{
  FileEditorTab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->uncommentSelectedText ();
}

void
FileEditor::handleFileNameChanged (QString fileName)
{
  QObject *senderObject = sender ();
  FileEditorTab *fileEditorTab = dynamic_cast<FileEditorTab*> (senderObject);
  if (fileEditorTab)
    {
      for(int i = 0; i < m_tabWidget->count (); i++)
        {
          if (m_tabWidget->widget (i) == fileEditorTab)
            {
              m_tabWidget->setTabText (i, fileName);
            }
        }
    }
}

void
FileEditor::handleTabCloseRequest (int index)
{
  FileEditorTab *fileEditorTab = dynamic_cast <FileEditorTab*> (m_tabWidget->widget (index));
  if (fileEditorTab)
    if (fileEditorTab->close ())
      {
        m_tabWidget->removeTab (index);
        delete fileEditorTab;
      }
}

void
FileEditor::handleTabCloseRequest ()
{
  FileEditorTab *fileEditorTab = dynamic_cast <FileEditorTab*> (sender ());
  if (fileEditorTab)
    if (fileEditorTab->close ())
      {
        m_tabWidget->removeTab (m_tabWidget->indexOf (fileEditorTab));
        delete fileEditorTab;
      }
}

void
FileEditor::activeTabChanged (int index)
{
  Q_UNUSED (index);
  handleEditorStateChanged ();
}

void
FileEditor::handleEditorStateChanged ()
{
  FileEditorTab *fileEditorTab = activeEditorTab ();
  if (fileEditorTab)
    {
      bool copyAvailable = fileEditorTab->copyAvailable ();
      m_copyAction->setEnabled (copyAvailable);
      m_cutAction->setEnabled (copyAvailable);
    }
}

void
FileEditor::construct ()
{
  QWidget *widget = new QWidget (this);
  QSettings *settings = ResourceManager::instance ()->settings ();
  QStyle *style = QApplication::style ();

  m_menuBar = new QMenuBar (widget);
  m_toolBar = new QToolBar (widget);
  m_tabWidget = new QTabWidget (widget);
  m_tabWidget->setTabsClosable (true);

  // Theme icons with QStyle icons as fallback
  QAction *newAction = new QAction (
        QIcon::fromTheme("document-new",style->standardIcon (QStyle::SP_FileIcon)),
        tr("&New File"), m_toolBar);

  QAction *openAction = new QAction (
        QIcon::fromTheme("document-open",style->standardIcon (QStyle::SP_DirOpenIcon)),
        tr("&Open File"), m_toolBar);

  QAction *saveAction = new QAction (
        QIcon::fromTheme("document-save",style->standardIcon (QStyle::SP_DriveHDIcon)),
        tr("&Save File"), m_toolBar);

  QAction *saveAsAction = new QAction (
        QIcon::fromTheme("document-save-as",style->standardIcon (QStyle::SP_DriveFDIcon)),
        tr("Save File &As"), m_toolBar);

  QAction *undoAction = new QAction (
        QIcon::fromTheme("edit-undo",style->standardIcon (QStyle::SP_ArrowLeft)),
        tr("&Undo"), m_toolBar);

  QAction *redoAction = new QAction (
        QIcon::fromTheme("edit-redo",style->standardIcon (QStyle::SP_ArrowRight)),
        tr("&Redo"), m_toolBar);

  m_copyAction = new QAction (QIcon::fromTheme ("edit-copy"), tr ("&Copy"), m_toolBar);
  m_cutAction = new QAction (QIcon::fromTheme ("edit-cut"), tr ("Cu&t"), m_toolBar);

  QAction *pasteAction              = new QAction (QIcon::fromTheme ("edit-paste"), tr ("&Paste"),m_toolBar);
  QAction *nextBookmarkAction       = new QAction (tr ("&Next Bookmark"),m_toolBar);
  QAction *prevBookmarkAction       = new QAction (tr ("Pre&vious Bookmark"),m_toolBar);
  QAction *toggleBookmarkAction     = new QAction (tr ("Toggle &Bookmark"),m_toolBar);
  QAction *removeBookmarkAction     = new QAction (tr ("&Remove All Bookmarks"),m_toolBar);
  QAction *commentSelectedAction    = new QAction (tr ("&Comment Selected Text"),m_toolBar);
  QAction *uncommentSelectedAction  = new QAction (tr ("&Uncomment Selected Text"),m_toolBar);

  QAction *runAction = new QAction (
        QIcon::fromTheme ("media-play", style->standardIcon (QStyle::SP_MediaPlay)),
        tr("&Run File"), m_toolBar);

  // some actions are disabled from the beginning
  m_copyAction->setEnabled(false);
  m_cutAction->setEnabled(false);

  // short cuts
  newAction->setShortcut              (QKeySequence::New);
  openAction->setShortcut             (QKeySequence::Open);
  saveAction->setShortcut             (QKeySequence::Save);
  saveAsAction->setShortcut           (QKeySequence::SaveAs);
  undoAction->setShortcut             (QKeySequence::Undo);
  redoAction->setShortcut             (QKeySequence::Redo);
  m_copyAction->setShortcut           (QKeySequence::Copy);
  m_cutAction->setShortcut            (QKeySequence::Cut);
  pasteAction->setShortcut            (QKeySequence::Paste);
  runAction->setShortcut              (Qt::Key_F5);
  nextBookmarkAction->setShortcut     (Qt::Key_F2);
  prevBookmarkAction->setShortcut     (Qt::SHIFT + Qt::Key_F2);
  toggleBookmarkAction->setShortcut   (Qt::Key_F7);
  commentSelectedAction->setShortcut  (Qt::CTRL + Qt::Key_R);
  uncommentSelectedAction->setShortcut(Qt::CTRL + Qt::Key_T);

  // toolbar
  m_toolBar->addAction (newAction);
  m_toolBar->addAction (openAction);
  m_toolBar->addAction (saveAction);
  m_toolBar->addAction (saveAsAction);
  m_toolBar->addSeparator ();
  m_toolBar->addAction (undoAction);
  m_toolBar->addAction (redoAction);
  m_toolBar->addAction (m_copyAction);
  m_toolBar->addAction (m_cutAction);
  m_toolBar->addAction (pasteAction);
  m_toolBar->addSeparator ();
  m_toolBar->addAction (runAction);

  // menu bar
  QMenu *fileMenu = new QMenu (tr ("&File"), m_menuBar);
  fileMenu->addAction (newAction);
  fileMenu->addAction (openAction);
  fileMenu->addAction (saveAction);
  fileMenu->addAction (saveAsAction);
  fileMenu->addSeparator ();
  m_menuBar->addMenu (fileMenu);

  QMenu *editMenu = new QMenu (tr ("&Edit"), m_menuBar);
  editMenu->addAction (undoAction);
  editMenu->addAction (redoAction);
  editMenu->addSeparator ();
  editMenu->addAction (m_copyAction);
  editMenu->addAction (m_cutAction);
  editMenu->addAction (pasteAction);
  editMenu->addSeparator ();
  editMenu->addAction (commentSelectedAction);
  editMenu->addAction (uncommentSelectedAction);
  editMenu->addSeparator ();
  editMenu->addAction (toggleBookmarkAction);
  editMenu->addAction (nextBookmarkAction);
  editMenu->addAction (prevBookmarkAction);
  editMenu->addAction (removeBookmarkAction);
  m_menuBar->addMenu (editMenu);

  QMenu *runMenu = new QMenu (tr ("&Run"), m_menuBar);
  runMenu->addAction (runAction);
  m_menuBar->addMenu (runMenu);

  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (m_menuBar);
  layout->addWidget (m_toolBar);
  layout->addWidget (m_tabWidget);
  layout->setMargin (0);
  widget->setLayout (layout);
  setWidget (widget);

  connect (newAction,               SIGNAL (triggered ()), this, SLOT (requestNewFile ()));
  connect (openAction,              SIGNAL (triggered ()), this, SLOT (requestOpenFile ()));
  connect (undoAction,              SIGNAL (triggered ()), this, SLOT (requestUndo ()));
  connect (redoAction,              SIGNAL (triggered ()), this, SLOT (requestRedo ()));
  connect (m_copyAction,            SIGNAL (triggered ()), this, SLOT (requestCopy ()));
  connect (m_cutAction,             SIGNAL (triggered ()), this, SLOT (requestCut ()));
  connect (pasteAction,             SIGNAL (triggered ()), this, SLOT (requestPaste ()));
  connect (saveAction,              SIGNAL (triggered ()), this, SLOT (requestSaveFile ()));
  connect (saveAsAction,            SIGNAL (triggered ()), this, SLOT (requestSaveFileAs ()));
  connect (runAction,               SIGNAL (triggered ()), this, SLOT (requestRunFile ()));
  connect (toggleBookmarkAction,    SIGNAL (triggered ()), this, SLOT (requestToggleBookmark ()));
  connect (nextBookmarkAction,      SIGNAL (triggered ()), this, SLOT (requestNextBookmark ()));
  connect (prevBookmarkAction,      SIGNAL (triggered ()), this, SLOT (requestPreviousBookmark ()));
  connect (removeBookmarkAction,    SIGNAL (triggered ()), this, SLOT (requestRemoveBookmark ()));
  connect (commentSelectedAction,   SIGNAL (triggered ()), this, SLOT (requestCommentSelectedText ()));
  connect (uncommentSelectedAction, SIGNAL (triggered ()), this, SLOT (requestUncommentSelectedText ()));
  connect (m_tabWidget, SIGNAL (tabCloseRequested (int)), this, SLOT (handleTabCloseRequest (int)));
  connect (m_tabWidget, SIGNAL (currentChanged(int)), this, SLOT (activeTabChanged (int)));

  // this has to be done only once, not for each editor
  m_lexer = new LexerOctaveGui ();

  // Editor font (default or from settings)
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
  for (i = 0; i < keywordList.size (); i++)
    {
      m_lexerAPI->add (keywordList.at (i));  // add single strings to the API
    }
  m_lexerAPI->prepare ();           // prepare API info ... this make take some time
  resize (500, 400);
  setWindowIcon (QIcon::fromTheme ("accessories-text-editor", style->standardIcon (QStyle::SP_FileIcon)));
  setWindowTitle ("Octave Editor");
}

void
FileEditor::addFileEditorTab (FileEditorTab *fileEditorTab)
{
  m_tabWidget->addTab (fileEditorTab, "");
  connect (fileEditorTab, SIGNAL (fileNameChanged(QString)),
           this, SLOT(handleFileNameChanged(QString)));
  connect (fileEditorTab, SIGNAL (editorStateChanged ()),
           this, SLOT (handleEditorStateChanged ()));
  connect (fileEditorTab, SIGNAL (closeRequest ()),
           this, SLOT (handleTabCloseRequest ()));
  m_tabWidget->setCurrentWidget (fileEditorTab);
}

FileEditorTab *
FileEditor::activeEditorTab ()
{
  return dynamic_cast<FileEditorTab*> (m_tabWidget->currentWidget ());
}
