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

#include "file-editor.h"
#include <QVBoxLayout>
#include <QApplication>
#include <QFile>
#include <QFont>
#include <QFileDialog>
#include <QMessageBox>
#include <QStyle>
#include <QTextStream>

file_editor::file_editor (QTerminal *terminal, main_window *mainWindow)
  : file_editor_interface(terminal, mainWindow)
{
  construct ();

  m_terminal = terminal;
  m_mainWindow = mainWindow;
  setVisible (false);
}

file_editor::~file_editor ()
{
}

lexer_octave_gui *
file_editor::lexer ()
{
  return m_lexer;
}

QTerminal *
file_editor::terminal ()
{
  return m_terminal;
}

main_window *
file_editor::mainWindow ()
{
  return m_mainWindow;
}

void
file_editor::request_new_file ()
{
  file_editor_tab *fileEditorTab = new file_editor_tab (this);
  if (fileEditorTab)
    {
      addFileEditorTab (fileEditorTab);
      fileEditorTab->new_file ();
    }
}

void
file_editor::request_open_file ()
{
  file_editor_tab *fileEditorTab = new file_editor_tab (this);
  if (fileEditorTab)
    {
      addFileEditorTab (fileEditorTab);
      if (!fileEditorTab->open_file ())
        {
          // If no file was loaded, remove the tab again.
          m_tabWidget->removeTab (m_tabWidget->indexOf (fileEditorTab));
        }
    }
}

void
file_editor::request_open_file (QString fileName)
{
  if (!isVisible ())
    {
      show ();
    }

  file_editor_tab *fileEditorTab = new file_editor_tab (this);
  if (fileEditorTab)
    {
      addFileEditorTab (fileEditorTab);
      fileEditorTab->load_file (fileName);
    }
}

void
file_editor::requestUndo ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->undo ();
}

void
file_editor::requestRedo ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->redo ();
}

void
file_editor::requestCopy ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->copy ();
}

void
file_editor::requestCut ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->cut ();
}

void
file_editor::requestPaste ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->paste ();
}

void
file_editor::requestSaveFile ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->save_file ();
}

void
file_editor::requestSaveFileAs ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->save_file_as ();
}

void
file_editor::requestRunFile ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->run_file ();
}

void
file_editor::requestToggleBookmark ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->toggle_bookmark ();
}

void
file_editor::requestNextBookmark ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->next_bookmark ();
}

void
file_editor::requestPreviousBookmark ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->previous_bookmark ();
}

void
file_editor::requestRemoveBookmark ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->remove_bookmark ();
}

void
file_editor::requestCommentSelectedText ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->comment_selected_text ();
}

void
file_editor::requestUncommentSelectedText ()
{
  file_editor_tab *activeFileEditorTab = activeEditorTab ();
  if (activeFileEditorTab)
    activeFileEditorTab->uncomment_selected_text ();
}

void
file_editor::handleFileNameChanged (QString fileName)
{
  QObject *senderObject = sender ();
  file_editor_tab *fileEditorTab = dynamic_cast<file_editor_tab*> (senderObject);
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
file_editor::handleTabCloseRequest (int index)
{
  file_editor_tab *fileEditorTab = dynamic_cast <file_editor_tab*> (m_tabWidget->widget (index));
  if (fileEditorTab)
    if (fileEditorTab->close ())
      {
        m_tabWidget->removeTab (index);
        delete fileEditorTab;
      }
}

void
file_editor::handleTabCloseRequest ()
{
  file_editor_tab *fileEditorTab = dynamic_cast <file_editor_tab*> (sender ());
  if (fileEditorTab)
    if (fileEditorTab->close ())
      {
        m_tabWidget->removeTab (m_tabWidget->indexOf (fileEditorTab));
        delete fileEditorTab;
      }
}

void
file_editor::activeTabChanged (int index)
{
  Q_UNUSED (index);
  handleEditorStateChanged ();
}

void
file_editor::handleEditorStateChanged ()
{
  file_editor_tab *fileEditorTab = activeEditorTab ();
  if (fileEditorTab)
    {
      bool copyAvailable = fileEditorTab->copyAvailable ();
      m_copyAction->setEnabled (copyAvailable);
      m_cutAction->setEnabled (copyAvailable);
    }
}

void
file_editor::construct ()
{
  QWidget *widget = new QWidget (this);
  QSettings *settings = resource_manager::instance ()->settings ();
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

  connect (newAction,               SIGNAL (triggered ()), this, SLOT (request_new_file ()));
  connect (openAction,              SIGNAL (triggered ()), this, SLOT (request_open_file ()));
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
  m_lexer = new lexer_octave_gui ();

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
file_editor::addFileEditorTab (file_editor_tab *fileEditorTab)
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

file_editor_tab *
file_editor::activeEditorTab ()
{
  return dynamic_cast<file_editor_tab*> (m_tabWidget->currentWidget ());
}
