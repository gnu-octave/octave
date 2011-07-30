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
#include <QFileDialog>
#include <QMessageBox>
#include <QAction>
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
  // ignore close event if file is not saved and user cancels closing this window
  // TODO: This does not work if the main window is closed!
  if (checkFileModified ("Close File")==QMessageBox::Cancel)
    {
      event->ignore();
    }
  else
    {
      event->accept();
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
FileEditorMdiSubWindow::openFile ()
{
    if (checkFileModified ("Open File")==QMessageBox::Cancel)
      {
        return; // existing file not saved and opening another file canceled by user
      }
    QString openFileName =
        QFileDialog::getOpenFileName (this, "Open File", QDir::homePath(), SAVE_FILE_FILTER);
    if (openFileName.isEmpty ())
      {
        return;
      }
    else
      {
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
    if (checkFileModified ("Open New File")==QMessageBox::Cancel)
      {
        return; // existing file not saved and creating new file canceled by user
      }
    m_fileName = UNNAMED_FILE;
    setWindowTitle (m_fileName);
    m_editor->setText ("");
    m_editor->setModified (false); // new file is not modified yet
}

int
FileEditorMdiSubWindow::checkFileModified (QString msg)
{
  int decision = QMessageBox::Yes;
  if (m_editor->isModified ())
    {
      // file is modified but not saved, aks user what to do
      decision = QMessageBox::question (this,
                                        msg,
                                        tr ("Do you want to save the current file\n%1 ?").
                                          arg (m_fileName),
                                        QMessageBox::Cancel,
                                        QMessageBox::No,
                                        QMessageBox::Yes);

      if (decision == QMessageBox::Yes)
        {
          saveFile ();
          if (m_editor->isModified ())
            {
              // If the user attempted to save the file, but it's still
              // modified, then probably something went wrong, so return cancel
              // for cancel this operation
              return (QMessageBox::Cancel);
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
FileEditorMdiSubWindow::saveFile (QString fileName)
{
  // it is a new file with the name "<unnamed>" -> call saveFielAs
  if (fileName==UNNAMED_FILE)
    {
      saveFileAs();
      return;
    }

  // check for a valid file name to save the contents
  QString saveFileName;
  if (fileName.isEmpty ())
    {
      saveFileName = QFileDialog::getSaveFileName (this, "Save File", fileName,SAVE_FILE_FILTER);
      if (saveFileName.isEmpty ())
        return;
    }
  else
    {
    saveFileName = fileName;
    }

  // open the file
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
  QString saveDir(m_fileName);
  if (saveDir==UNNAMED_FILE)
    saveDir = QDir::homePath();
  QString saveFileName = QFileDialog::getSaveFileName(
        this, "Save File As", saveDir,SAVE_FILE_FILTER);
  if(saveFileName.isEmpty())
    return;
  saveFile(saveFileName);
}

void
FileEditorMdiSubWindow::setEditorLexer (LexerOctaveGui* lexer)
{
  m_editor->setLexer(lexer);
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
  QStyle *style = QApplication::style ();
  setWidget (new QWidget ());
  m_toolBar = new QToolBar (this);
  m_statusBar = new QStatusBar (this);
  m_editor = new QsciScintilla (this);

  m_editor->setMarginType (1, QsciScintilla::SymbolMargin);
  m_editor->setMarginType (2, QsciScintilla::TextMargin);
  m_editor->setMarginType (3, QsciScintilla::SymbolMargin);
  m_editor->setFolding (QsciScintilla::BoxedTreeFoldStyle , 3);
  m_editor->setMarginLineNumbers (2, true);
  m_editor->setMarginWidth (2, "99999");
  m_editor->setMarginsForegroundColor (QColor(96,96,96));
  m_editor->setMarginsBackgroundColor (QColor(224,224,224));

  m_editor->setBraceMatching (QsciScintilla::SloppyBraceMatch);
  m_editor->setAutoIndent (true);
  m_editor->setIndentationWidth (2);
  m_editor->setIndentationsUseTabs (false);
  m_editor->autoCompleteFromAll();
  m_editor->setAutoCompletionSource(QsciScintilla::AcsAPIs);
  m_editor->setAutoCompletionThreshold (3);

  // Theme icons with QStyle icons as fallback
  m_toolBar->setIconSize(QSize(20,20)); // smaller icons (make configurable in user settings?)
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

  // short cuts
  newAction->setShortcut(QKeySequence::New);
  openAction->setShortcut(QKeySequence::Open);
  saveAction->setShortcut(QKeySequence::Save);
  saveAsAction->setShortcut(QKeySequence::SaveAs);
  undoAction->setShortcut(QKeySequence::Undo);
  redoAction->setShortcut(QKeySequence::Redo);

  m_toolBar->addAction (closeAction);
  m_toolBar->addAction (newAction);
  m_toolBar->addAction (openAction);
  m_toolBar->addAction (saveAction);
  m_toolBar->addAction (saveAsAction);
  m_toolBar->addAction (undoAction);
  m_toolBar->addAction (redoAction);

  QVBoxLayout *layout = new QVBoxLayout ();
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
  connect (saveAction, SIGNAL (triggered ()), this, SLOT (saveFile ()));
  connect (saveAsAction, SIGNAL (triggered ()), this, SLOT (saveFileAs ()));

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
