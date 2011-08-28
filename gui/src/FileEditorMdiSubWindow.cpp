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

#include "FileEditorMdiSubWindow.h"
#include <QVBoxLayout>
#include <QApplication>
#include <QFile>
#include <QFont>
#include <QFileDialog>
#include <QMessageBox>
#include <QStyle>
#include <QTextStream>

FileEditorMdiSubWindow::FileEditorMdiSubWindow (QWidget * parent):QMdiSubWindow
  (parent)
{
  construct ();
}

FileEditorMdiSubWindow::~FileEditorMdiSubWindow ()
{
}

void
FileEditorMdiSubWindow::closeEvent(QCloseEvent *event)
{
  if ( m_mainWindow->isCloseApplication() )
    {
      // close wohle application: save file or not if modified
      checkFileModified ("Close Octave GUI",0); // no cancel possible
    }
  else
    {
      // ignore close event if file is not saved and user cancels closing this window
      if (checkFileModified ("Close File",QMessageBox::Cancel)==QMessageBox::Cancel)
        event->ignore();
      else
        event->accept();
    }
}

void
FileEditorMdiSubWindow::handleMarginClicked(int margin, int line, Qt::KeyboardModifiers state)
{
  Q_UNUSED (state);
  if ( margin == 1 )  // marker margin
    {
      unsigned int mask = m_editor->markersAtLine (line);
      if (mask && (1 << MARKER_BOOKMARK))
        m_editor->markerDelete(line,MARKER_BOOKMARK);
      else
        m_editor->markerAdd(line,MARKER_BOOKMARK);
    }
}

void
FileEditorMdiSubWindow::handleModificationChanged(bool modified)
{
  if ( modified )
    {
      QString title(m_fileName);
      setWindowTitle(title.prepend("* "));
    }
  else
     setWindowTitle (m_fileName);
}

void
FileEditorMdiSubWindow::handleCopyAvailable(bool enableCopy)
{
  m_copyAction->setEnabled(enableCopy);
  m_cutAction->setEnabled(enableCopy);
}


void
FileEditorMdiSubWindow::openFile ()
{
    if (checkFileModified ("Open File",QMessageBox::Cancel)==QMessageBox::Cancel)
      {
        return; // existing file not saved and opening another file canceled by user
      }
    QString openFileName;
    QFileDialog dlg(this);
    dlg.setNameFilter(SAVE_FILE_FILTER);
    dlg.setAcceptMode(QFileDialog::AcceptOpen);
    dlg.setViewMode(QFileDialog::Detail);
    if ( dlg.exec() )
      {
        openFileName = dlg.selectedFiles().at(0);
        if (openFileName.isEmpty ())
          return;
        loadFile(openFileName);
      }
}

void
FileEditorMdiSubWindow::loadFile (QString fileName)
{
  QFile file (fileName);
  if (!file.open (QFile::ReadOnly))
    {
      QMessageBox::warning (this, tr ("File Editor"),
			    tr ("Cannot read file %1:\n%2.").arg (fileName).
			    arg (file.errorString ()));
      return;
    }

  QTextStream in (&file);
  QApplication::setOverrideCursor (Qt::WaitCursor);
  m_editor->setText (in.readAll ());
  QApplication::restoreOverrideCursor ();

  m_fileName = fileName;
  setWindowTitle (fileName);
  m_statusBar->showMessage (tr ("File loaded."), 2000);
  m_editor->setModified (false); // loaded file is not modified yet
}

void
FileEditorMdiSubWindow::newFile ()
{
    if (checkFileModified ("Create New File",QMessageBox::Cancel)==QMessageBox::Cancel)
      {
        return; // existing file not saved and creating new file canceled by user
      }
    m_fileName = UNNAMED_FILE;
    setWindowTitle (m_fileName);
    m_editor->setText ("");
    m_editor->setModified (false); // new file is not modified yet
}

int
FileEditorMdiSubWindow::checkFileModified (QString msg, int cancelButton)
{
  int decision = QMessageBox::Yes;
  if (m_editor->isModified ())
    {
      // file is modified but not saved, aks user what to do
      decision = QMessageBox::warning (this,
                                        msg,
                                        tr ("The file %1\n"
                                            "has been modified. Do you want to save the changes?").
                                          arg (m_fileName),
                                        QMessageBox::Save, QMessageBox::Discard, cancelButton );
      if (decision == QMessageBox::Save)
        {
          saveFile ();
          if (m_editor->isModified ())
            {
              // If the user attempted to save the file, but it's still
              // modified, then probably something went wrong, so return cancel
              // for cancel this operation or try to save files as if cancel not
              // possible
              if ( cancelButton )
                return (QMessageBox::Cancel);
              else
                saveFileAs ();
            }
        }
    }
  return (decision);
}

void
FileEditorMdiSubWindow::saveFile ()
{
  saveFile(m_fileName);
}

void
FileEditorMdiSubWindow::saveFile (QString saveFileName)
{
  // it is a new file with the name "<unnamed>" -> call saveFielAs
  if (saveFileName==UNNAMED_FILE || saveFileName.isEmpty ())
    {
      saveFileAs();
      return;
    }

  // open the file for writing
  QFile file (saveFileName);
  if (!file.open (QFile::WriteOnly))
    {
      QMessageBox::warning (this, tr ("File Editor"),
			    tr ("Cannot write file %1:\n%2.").
          arg (saveFileName).arg (file.errorString ()));
      return;
    }

  // save the contents into the file
  QTextStream out (&file);
  QApplication::setOverrideCursor (Qt::WaitCursor);
  out << m_editor->text ();
  QApplication::restoreOverrideCursor ();
  m_fileName = saveFileName;     // save file name for later use
  setWindowTitle(m_fileName);    // set the window title to actual file name
  m_statusBar->showMessage (tr ("File %1 saved").arg(m_fileName), 2000);
  m_editor->setModified (false); // files is save -> not modified
}

void
FileEditorMdiSubWindow::saveFileAs ()
{
  QString saveFileName(m_fileName);
  QFileDialog dlg(this);
  if (saveFileName==UNNAMED_FILE || saveFileName.isEmpty ())
    {
      saveFileName = QDir::homePath();
      dlg.setDirectory(saveFileName);
    }
  else
    {
      dlg.selectFile(saveFileName);
    }
  dlg.setNameFilter(SAVE_FILE_FILTER);
  dlg.setDefaultSuffix("m");
  dlg.setAcceptMode(QFileDialog::AcceptSave);
  dlg.setViewMode(QFileDialog::Detail);
  if ( dlg.exec() )
    {
      saveFileName = dlg.selectedFiles().at(0);
      if (saveFileName.isEmpty ())
        return;
      saveFile(saveFileName);
    }
}

// handle the run command
void
FileEditorMdiSubWindow::runFile ()
{
  if (m_editor->isModified ())
    saveFile(m_fileName);
  m_terminalEmulation->transmitText (QString ("run \'%1\'\n").arg (m_fileName));
  //m_terminalEmulation->setFocus ();
}


// (un)comment selected text
void
FileEditorMdiSubWindow::commentSelectedText ()
{
  doCommentSelectedText (true);
}
void
FileEditorMdiSubWindow::uncommentSelectedText ()
{
  doCommentSelectedText (false);
}
void
FileEditorMdiSubWindow::doCommentSelectedText (bool comment)
{
  if ( m_editor->hasSelectedText() )
    {
      int lineFrom, lineTo, colFrom, colTo, i;
      m_editor->getSelection (&lineFrom,&colFrom,&lineTo,&colTo);
      if ( colTo == 0 )  // the beginning of last line is not selected
        lineTo--;        // stop at line above
      m_editor->beginUndoAction ();
      for ( i=lineFrom; i<=lineTo; i++ )
        {
          if ( comment )
            m_editor->insertAt("%",i,0);
          else
            {
              QString line(m_editor->text(i));
              if ( line.startsWith("%") )
                {
                  m_editor->setSelection(i,0,i,1);
                  m_editor->removeSelectedText();
                }
            }
        }
      m_editor->endUndoAction ();
    }
}


// remove bookmarks
void
FileEditorMdiSubWindow::removeBookmark ()
{
  m_editor->markerDeleteAll(MARKER_BOOKMARK);
}
// toggle bookmark
void
FileEditorMdiSubWindow::toggleBookmark ()
{
  int line,cur;
  m_editor->getCursorPosition(&line,&cur);
  if ( m_editor->markersAtLine (line) && (1 << MARKER_BOOKMARK) )
    m_editor->markerDelete(line,MARKER_BOOKMARK);
  else
    m_editor->markerAdd(line,MARKER_BOOKMARK);
}
// goto next bookmark
void
FileEditorMdiSubWindow::nextBookmark ()
{
  int line,cur,nextline;
  m_editor->getCursorPosition(&line,&cur);
  if ( m_editor->markersAtLine(line) && (1 << MARKER_BOOKMARK) )
    line++; // we have a bookmark here, so start search from next line
  nextline = m_editor->markerFindNext(line,(1 << MARKER_BOOKMARK));
  m_editor->setCursorPosition(nextline,0);
}
// goto previous bookmark
void
FileEditorMdiSubWindow::prevBookmark ()
{
  int line,cur,prevline;
  m_editor->getCursorPosition(&line,&cur);
  if ( m_editor->markersAtLine(line) && (1 << MARKER_BOOKMARK) )
    line--; // we have a bookmark here, so start search from prev line
  prevline = m_editor->markerFindPrevious(line,(1 << MARKER_BOOKMARK));
  m_editor->setCursorPosition(prevline,0);
}

// function for setting the already existing lexer from MainWindow
void
FileEditorMdiSubWindow::initEditor (TerminalEmulation* terminalEmulation,
                                    LexerOctaveGui* lexer,
                                    MainWindow* mainWindow)
{
  m_editor->setLexer(lexer);
  m_terminalEmulation = terminalEmulation; // for sending commands to octave
                       // TODO: make a global commandOctave function?
  m_mainWindow = mainWindow;  // get the MainWindow for chekcing state at subwindow close
}

// TODO: Do we still need tool tips in the status bar? Tool tips are now
//       shown directly at the theme icons
void
FileEditorMdiSubWindow::showToolTipNew ()
{
  m_statusBar->showMessage ("Create a new file", 2000);
}

void
FileEditorMdiSubWindow::showToolTipOpen ()
{
  m_statusBar->showMessage ("Open a file", 2000);
}

void
FileEditorMdiSubWindow::showToolTipSave ()
{
  m_statusBar->showMessage ("Save the file", 2000);
}

void
FileEditorMdiSubWindow::showToolTipSaveAs ()
{
  m_statusBar->showMessage ("Save the file as", 2000);
}

void
FileEditorMdiSubWindow::showToolTipUndo ()
{
  m_statusBar->showMessage ("Revert previous changes", 2000);
}

void
FileEditorMdiSubWindow::showToolTipRedo ()
{
  m_statusBar->showMessage ("Append previous changes", 2000);
}

void
FileEditorMdiSubWindow::registerModified (bool modified)
{
  m_modified = modified;
}

void
FileEditorMdiSubWindow::construct ()
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  QStyle *style = QApplication::style ();
  setWidget (new QWidget ());

  m_menuBar = new QMenuBar (this);
  m_toolBar = new QToolBar (this);
  m_statusBar = new QStatusBar (this);
  m_editor = new QsciScintilla (this);

  // markers
  m_editor->setMarginType (1, QsciScintilla::SymbolMargin);
  m_editor->setMarginSensitivity(1,true);
  m_editor->markerDefine(QsciScintilla::RightTriangle,MARKER_BOOKMARK);
  connect(m_editor,SIGNAL(marginClicked(int,int,Qt::KeyboardModifiers)),
          this,SLOT(handleMarginClicked(int,int,Qt::KeyboardModifiers)));

  // line numbers
  m_editor->setMarginsForegroundColor(QColor(96,96,96));
  m_editor->setMarginsBackgroundColor(QColor(232,232,220));
  if ( settings->value ("editor/showLineNumbers",true).toBool () )
    {
      QFont marginFont( settings->value ("editor/fontName","Courier").toString () ,
                        settings->value ("editor/fontSize",10).toInt () );
      m_editor->setMarginsFont( marginFont );
      QFontMetrics metrics(marginFont);
      m_editor->setMarginType (2, QsciScintilla::TextMargin);
      m_editor->setMarginWidth(2, metrics.width("99999"));
      m_editor->setMarginLineNumbers(2, true);
    }
  // code folding
  m_editor->setMarginType (3, QsciScintilla::SymbolMargin);
  m_editor->setFolding (QsciScintilla::BoxedTreeFoldStyle , 3);
  // other features
  if ( settings->value ("editor/highlightActualLine",true).toBool () )
    {
      m_editor->setCaretLineVisible(true);
      m_editor->setCaretLineBackgroundColor(QColor(255,255,200));
    }
  m_editor->setBraceMatching (QsciScintilla::SloppyBraceMatch);
  m_editor->setAutoIndent (true);
  m_editor->setIndentationWidth (2);
  m_editor->setIndentationsUseTabs (false);
  if ( settings->value ("editor/codeCompletion",true).toBool () )
    {
      m_editor->autoCompleteFromAll();
      m_editor->setAutoCompletionSource(QsciScintilla::AcsAPIs);
      m_editor->setAutoCompletionThreshold (3);
    }
  m_editor->setUtf8 (true);

  // The Actions

  // Theme icons with QStyle icons as fallback
  QAction *closeAction = new QAction (
        QIcon::fromTheme("window-close",style->standardIcon (QStyle::SP_DialogCloseButton)),
        tr("&Close File"), m_toolBar);
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
  m_copyAction = new QAction (QIcon::fromTheme("edit-copy"),tr("&Copy"),m_toolBar);
  m_cutAction = new QAction (QIcon::fromTheme("edit-cut"),tr("Cu&t"),m_toolBar);
  QAction *pasteAction = new QAction (QIcon::fromTheme("edit-paste"),tr("&Paste"),m_toolBar);
  QAction *nextBookmarkAction = new QAction (tr("&Next Bookmark"),m_toolBar);
  QAction *prevBookmarkAction = new QAction (tr("Pre&vious Bookmark"),m_toolBar);
  QAction *toggleBookmarkAction = new QAction (tr("Toggle &Bookmark"),m_toolBar);
  QAction *removeBookmarkAction = new QAction (tr("&Remove All Bookmarks"),m_toolBar);
  QAction *commentSelectedAction = new QAction (tr("&Comment Selected Text"),m_toolBar);
  QAction *uncommentSelectedAction = new QAction (tr("&Uncomment Selected Text"),m_toolBar);
  QAction *runAction = new QAction (
        QIcon::fromTheme("media-play",style->standardIcon (QStyle::SP_MediaPlay)),
        tr("&Run File"), m_toolBar);

  // some actions are disabled from the beginning
  m_copyAction->setEnabled(false);
  m_cutAction->setEnabled(false);
  connect(m_editor,SIGNAL(copyAvailable(bool)),this,SLOT(handleCopyAvailable(bool)));

  // short cuts
  newAction->setShortcut(QKeySequence::New);
  openAction->setShortcut(QKeySequence::Open);
  saveAction->setShortcut(QKeySequence::Save);
  saveAsAction->setShortcut(QKeySequence::SaveAs);
  undoAction->setShortcut(QKeySequence::Undo);
  redoAction->setShortcut(QKeySequence::Redo);
  m_copyAction->setShortcut(QKeySequence::Copy);
  m_cutAction->setShortcut(QKeySequence::Cut);
  pasteAction->setShortcut(QKeySequence::Paste);
  runAction->setShortcut(Qt::Key_F5);
  nextBookmarkAction->setShortcut(Qt::Key_F2);
  prevBookmarkAction->setShortcut(Qt::SHIFT + Qt::Key_F2);
  toggleBookmarkAction->setShortcut(Qt::Key_F7);
  commentSelectedAction->setShortcut(Qt::CTRL + Qt::Key_R);
  uncommentSelectedAction->setShortcut(Qt::CTRL + Qt::Key_T);

  // toolbar
  m_toolBar->setIconSize(QSize(16,16)); // smaller icons (make configurable in user settings?)
  m_toolBar->addAction (closeAction);
  m_toolBar->addAction (newAction);
  m_toolBar->addAction (openAction);
  m_toolBar->addAction (saveAction);
  m_toolBar->addAction (saveAsAction);
  m_toolBar->addSeparator();
  m_toolBar->addAction (undoAction);
  m_toolBar->addAction (redoAction);
  m_toolBar->addAction (m_copyAction);
  m_toolBar->addAction (m_cutAction);
  m_toolBar->addAction (pasteAction);
  m_toolBar->addSeparator();
  m_toolBar->addAction (runAction);

  // menu bar  
  QMenu *fileMenu = new QMenu(tr("&File"),m_menuBar);
  fileMenu->addAction(newAction);
  fileMenu->addAction(openAction);
  fileMenu->addAction(saveAction);
  fileMenu->addAction(saveAsAction);
  fileMenu->addSeparator();
  fileMenu->addAction (closeAction);
  m_menuBar->addMenu(fileMenu);
  QMenu *editMenu = new QMenu(tr("&Edit"),m_menuBar);
  editMenu->addAction(undoAction);
  editMenu->addAction(redoAction);
  editMenu->addSeparator();
  editMenu->addAction(m_copyAction);
  editMenu->addAction(m_cutAction);
  editMenu->addAction(pasteAction);
  editMenu->addSeparator();
  editMenu->addAction(commentSelectedAction);
  editMenu->addAction(uncommentSelectedAction);
  editMenu->addSeparator();
  editMenu->addAction(toggleBookmarkAction);
  editMenu->addAction(nextBookmarkAction);
  editMenu->addAction(prevBookmarkAction);
  editMenu->addAction(removeBookmarkAction);
  m_menuBar->addMenu(editMenu);
  QMenu *runMenu = new QMenu(tr("&Run"),m_menuBar);
  runMenu->addAction(runAction);
  m_menuBar->addMenu(runMenu);


  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (m_menuBar);
  layout->addWidget (m_toolBar);
  layout->addWidget (m_editor);
  layout->addWidget (m_statusBar);
  layout->setMargin (2);
  widget ()->setLayout (layout);

  connect (closeAction, SIGNAL (triggered()), this, SLOT (close()));
  connect (newAction, SIGNAL (triggered ()), this, SLOT (newFile ()));
  connect (openAction, SIGNAL (triggered ()), this, SLOT (openFile ()));
  connect (undoAction, SIGNAL (triggered ()), m_editor, SLOT (undo ()));
  connect (redoAction, SIGNAL (triggered ()), m_editor, SLOT (redo ()));
  connect (m_copyAction, SIGNAL (triggered ()), m_editor, SLOT (copy ()));
  connect (m_cutAction, SIGNAL (triggered ()), m_editor, SLOT (cut ()));
  connect (pasteAction, SIGNAL (triggered ()), m_editor, SLOT (paste ()));
  connect (saveAction, SIGNAL (triggered ()), this, SLOT (saveFile ()));
  connect (saveAsAction, SIGNAL (triggered ()), this, SLOT (saveFileAs ()));
  connect (runAction, SIGNAL (triggered ()), this, SLOT (runFile ()));
  connect (toggleBookmarkAction, SIGNAL (triggered ()), this, SLOT (toggleBookmark ()));
  connect (nextBookmarkAction, SIGNAL (triggered ()), this, SLOT (nextBookmark ()));
  connect (prevBookmarkAction, SIGNAL (triggered ()), this, SLOT (prevBookmark ()));
  connect (removeBookmarkAction, SIGNAL (triggered ()), this, SLOT (removeBookmark ()));
  connect (commentSelectedAction, SIGNAL (triggered ()), this, SLOT (commentSelectedText ()));
  connect (uncommentSelectedAction, SIGNAL (triggered ()), this, SLOT (uncommentSelectedText ()));

  // TODO: Do we still need tool tips in the status bar? Tool tips are now
  //       shown directly at the theme icons
  connect (newAction, SIGNAL (hovered ()), this, SLOT (showToolTipNew ()));
  connect (openAction, SIGNAL (hovered ()), this, SLOT (showToolTipOpen ()));
  connect (undoAction, SIGNAL (hovered ()), this, SLOT (showToolTipUndo ()));
  connect (redoAction, SIGNAL (hovered ()), this, SLOT (showToolTipRedo ()));
  connect (saveAction, SIGNAL (hovered ()), this, SLOT (showToolTipSave ()));
  connect (saveAsAction, SIGNAL (hovered ()), this,SLOT (showToolTipSaveAs ()));

  // connect modified signal
  connect (m_editor, SIGNAL (modificationChanged(bool)), this, SLOT (handleModificationChanged(bool)) );

  m_fileName = "";
  setWindowTitle (m_fileName);
  setWindowIcon(QIcon::fromTheme("accessories-text-editor",style->standardIcon (QStyle::SP_FileIcon)));
  show ();
}
