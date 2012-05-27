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

#include "FileEditorTab.h"
#include "FileEditor.h"
#include <QMessageBox>
#include <QVBoxLayout>

FileEditorTab::FileEditorTab(FileEditor *fileEditor)
  : QWidget ((QWidget*)fileEditor)
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  m_fileEditor = fileEditor;
  m_fileName = "";
  m_editArea = new QsciScintilla (this);
  m_editArea->setLexer (fileEditor->lexer ());

  // markers
  m_editArea->setMarginType (1, QsciScintilla::SymbolMargin);
  m_editArea->setMarginSensitivity (1, true);
  m_editArea->markerDefine (QsciScintilla::RightTriangle, MARKER_BOOKMARK);
  connect (m_editArea, SIGNAL (marginClicked (int, int, Qt::KeyboardModifiers)),
           this, SLOT (handleMarginClicked (int, int, Qt::KeyboardModifiers)));

  // line numbers
  m_editArea->setMarginsForegroundColor(QColor(96,96,96));
  m_editArea->setMarginsBackgroundColor(QColor(232,232,220));
  if (settings->value ("editor/showLineNumbers",true).toBool ())
    {
      QFont marginFont( settings->value ("editor/fontName","Courier").toString () ,
                        settings->value ("editor/fontSize",10).toInt () );
      m_editArea->setMarginsFont( marginFont );
      QFontMetrics metrics(marginFont);
      m_editArea->setMarginType (2, QsciScintilla::TextMargin);
      m_editArea->setMarginWidth(2, metrics.width("99999"));
      m_editArea->setMarginLineNumbers(2, true);
    }

  // code folding
  m_editArea->setMarginType (3, QsciScintilla::SymbolMargin);
  m_editArea->setFolding (QsciScintilla::BoxedTreeFoldStyle , 3);

  // other features
  if (settings->value ("editor/highlightCurrentLine",true).toBool ())
    {
      m_editArea->setCaretLineVisible(true);
      m_editArea->setCaretLineBackgroundColor(QColor(245,245,245));
    }
  m_editArea->setBraceMatching (QsciScintilla::StrictBraceMatch);
  m_editArea->setAutoIndent (true);
  m_editArea->setIndentationWidth (2);
  m_editArea->setIndentationsUseTabs (false);
  if (settings->value ("editor/codeCompletion",true).toBool ())
    {
      m_editArea->autoCompleteFromAll ();
      m_editArea->setAutoCompletionSource(QsciScintilla::AcsAll);
      m_editArea->setAutoCompletionThreshold (1);
    }
  m_editArea->setUtf8 (true);

  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (m_editArea);
  layout->setMargin (0);
  setLayout (layout);

  // connect modified signal
  connect (m_editArea, SIGNAL (modificationChanged (bool)),
           this, SLOT (newTitle (bool)));
  connect (m_editArea, SIGNAL (copyAvailable (bool)),
           this, SLOT (handleCopyAvailable (bool)));
  connect (&m_fileSystemWatcher, SIGNAL (fileChanged (QString)),
           this, SLOT (fileHasChanged (QString)));

  m_fileName = "";
  newTitle (false);
}

bool
FileEditorTab::copyAvailable ()
{
  return m_copyAvailable;
}

void
FileEditorTab::closeEvent (QCloseEvent *event)
{
  if (m_fileEditor->mainWindow ()->closing ())
    {
      // close whole application: save file or not if modified
      checkFileModified ("Closing Octave", 0); // no cancel possible
      event->accept ();
    }
  else
    {
      // ignore close event if file is not saved and user cancels closing this window
      if (checkFileModified ("Close File", QMessageBox::Cancel) == QMessageBox::Cancel)
        {
          event->ignore ();
        }
      else
        {
          event->accept();
        }
    }
}

void
FileEditorTab::setFileName (QString fileName)
{
  m_fileName = fileName;
  updateTrackedFile ();
}

void
FileEditorTab::handleMarginClicked(int margin, int line, Qt::KeyboardModifiers state)
{
  Q_UNUSED (state);
  if (margin == 1)  // marker margin
    {
      unsigned int mask = m_editArea->markersAtLine (line);
      if (mask && (1 << MARKER_BOOKMARK))
        m_editArea->markerDelete(line,MARKER_BOOKMARK);
      else
        m_editArea->markerAdd(line,MARKER_BOOKMARK);
    }
}

void
FileEditorTab::commentSelectedText ()
{
  doCommentSelectedText (true);
}

void
FileEditorTab::uncommentSelectedText ()
{
  doCommentSelectedText (false);
}

void
FileEditorTab::doCommentSelectedText (bool comment)
{
  if ( m_editArea->hasSelectedText() )
    {
      int lineFrom, lineTo, colFrom, colTo, i;
      m_editArea->getSelection (&lineFrom,&colFrom,&lineTo,&colTo);
      if ( colTo == 0 )  // the beginning of last line is not selected
        lineTo--;        // stop at line above
      m_editArea->beginUndoAction ();
      for ( i=lineFrom; i<=lineTo; i++ )
        {
          if ( comment )
            m_editArea->insertAt("%",i,0);
          else
            {
              QString line(m_editArea->text(i));
              if ( line.startsWith("%") )
                {
                  m_editArea->setSelection(i,0,i,1);
                  m_editArea->removeSelectedText();
                }
            }
        }
      m_editArea->endUndoAction ();
    }
}

void
FileEditorTab::newTitle(bool modified)
{
  QString title(m_fileName);
  if ( !m_longTitle )
    {
      QFileInfo file(m_fileName);
      title = file.fileName();
    }

  if ( modified )
    {
      emit fileNameChanged (title.prepend("* "));
    }
  else
    emit fileNameChanged (title);
}

void
FileEditorTab::handleCopyAvailable(bool enableCopy)
{
  m_copyAvailable = enableCopy;
  emit editorStateChanged ();
}

void
FileEditorTab::updateTrackedFile ()
{
  QStringList trackedFiles = m_fileSystemWatcher.files ();
  if (!trackedFiles.isEmpty ())
    m_fileSystemWatcher.removePaths (trackedFiles);

  if (m_fileName != UNNAMED_FILE)
    m_fileSystemWatcher.addPath (m_fileName);
}

int
FileEditorTab::checkFileModified (QString msg, int cancelButton)
{
  int decision = QMessageBox::Yes;
  if (m_editArea->isModified ())
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
          if (m_editArea->isModified ())
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
FileEditorTab::removeBookmark ()
{
  m_editArea->markerDeleteAll(MARKER_BOOKMARK);
}

void
FileEditorTab::toggleBookmark ()
{
  int line,cur;
  m_editArea->getCursorPosition(&line,&cur);
  if ( m_editArea->markersAtLine (line) && (1 << MARKER_BOOKMARK) )
    m_editArea->markerDelete(line,MARKER_BOOKMARK);
  else
    m_editArea->markerAdd(line,MARKER_BOOKMARK);
}

void
FileEditorTab::nextBookmark ()
{
  int line,cur,nextline;
  m_editArea->getCursorPosition(&line,&cur);
  if ( m_editArea->markersAtLine(line) && (1 << MARKER_BOOKMARK) )
    line++; // we have a bookmark here, so start search from next line
  nextline = m_editArea->markerFindNext(line,(1 << MARKER_BOOKMARK));
  m_editArea->setCursorPosition(nextline,0);
}

void
FileEditorTab::previousBookmark ()
{
  int line,cur,prevline;
  m_editArea->getCursorPosition(&line,&cur);
  if ( m_editArea->markersAtLine(line) && (1 << MARKER_BOOKMARK) )
    line--; // we have a bookmark here, so start search from prev line
  prevline = m_editArea->markerFindPrevious(line,(1 << MARKER_BOOKMARK));
  m_editArea->setCursorPosition(prevline,0);
}

void
FileEditorTab::cut ()
{
  m_editArea->cut ();
}

void
FileEditorTab::copy ()
{
  m_editArea->copy ();
}

void
FileEditorTab::paste ()
{
  m_editArea->paste ();
}

void
FileEditorTab::undo ()
{
  m_editArea->undo ();
}

void
FileEditorTab::redo ()
{
  m_editArea->redo ();
}

void
FileEditorTab::setModified (bool modified)
{
  m_editArea->setModified (modified);
}

bool
FileEditorTab::openFile ()
{
  QString openFileName;
  QFileDialog fileDialog(this);
  fileDialog.setNameFilter(SAVE_FILE_FILTER);
  fileDialog.setAcceptMode(QFileDialog::AcceptOpen);
  fileDialog.setViewMode(QFileDialog::Detail);
  if (fileDialog.exec () == QDialog::Accepted)
    {
      openFileName = fileDialog.selectedFiles().at(0);
      if (openFileName.isEmpty ())
        return false;

      loadFile(openFileName);
      return true;
    }
  else
    {
      return false;
    }
}

void
FileEditorTab::loadFile (QString fileName)
{
  if (!m_fileEditor->isVisible ())
    {
      m_fileEditor->show ();
    }

  QFile file (fileName);
  if (!file.open (QFile::ReadOnly))
    {
      QMessageBox::warning (this, tr ("Octave Editor"),
                            tr ("Could not open file %1 for read:\n%2.").arg (fileName).
                            arg (file.errorString ()));
      return;
    }

  QTextStream in (&file);
  QApplication::setOverrideCursor (Qt::WaitCursor);
  m_editArea->setText (in.readAll ());
  QApplication::restoreOverrideCursor ();

  setFileName (fileName);
  updateTrackedFile ();


  newTitle (false); // window title (no modification)
  m_editArea->setModified (false); // loaded file is not modified yet
}

void
FileEditorTab::newFile ()
{
  if (!m_fileEditor->isVisible ())
    {
      m_fileEditor->show ();
    }

  setFileName (UNNAMED_FILE);
  newTitle (false); // window title (no modification)
  m_editArea->setText ("");
  m_editArea->setModified (false); // new file is not modified yet
}

bool FileEditorTab::saveFile()
{
  return saveFile (m_fileName);
}

bool
FileEditorTab::saveFile (QString saveFileName)
{
  // it is a new file with the name "<unnamed>" -> call saveFielAs
  if (saveFileName == UNNAMED_FILE || saveFileName.isEmpty ())
    {
      return saveFileAs();
    }

  // open the file for writing
  QFile file (saveFileName);
  if (!file.open (QFile::WriteOnly))
    {
      QMessageBox::warning (this, tr ("Octave Editor"),
                            tr ("Could not open file %1 for write:\n%2.").
                            arg (saveFileName).arg (file.errorString ()));
      return false;
    }

  // save the contents into the file
  QTextStream out (&file);
  QApplication::setOverrideCursor (Qt::WaitCursor);
  out << m_editArea->text ();
  QApplication::restoreOverrideCursor ();
  setFileName (saveFileName);  // save file name for later use
  newTitle (false);      // set the window title to actual file name (not modified)
  m_editArea->setModified (false); // files is save -> not modified
  return true;
}

bool
FileEditorTab::saveFileAs ()
{
  QString saveFileName(m_fileName);
  QFileDialog fileDialog(this);
  if (saveFileName == UNNAMED_FILE || saveFileName.isEmpty ())
    {
      saveFileName = QDir::homePath ();
      fileDialog.setDirectory (saveFileName);
    }
  else
    {
      fileDialog.selectFile (saveFileName);
    }
  fileDialog.setNameFilter (SAVE_FILE_FILTER);
  fileDialog.setDefaultSuffix ("m");
  fileDialog.setAcceptMode (QFileDialog::AcceptSave);
  fileDialog.setViewMode (QFileDialog::Detail);

  if (fileDialog.exec ())
    {
      saveFileName = fileDialog.selectedFiles ().at (0);
      if (saveFileName.isEmpty ())
        return false;

      return saveFile (saveFileName);
    }

  return false;
}

void
FileEditorTab::runFile ()
{
  if (m_editArea->isModified ())
    saveFile(m_fileName);

  m_fileEditor->terminal ()->sendText (QString ("run \'%1\'\n").arg (m_fileName));
  m_fileEditor->terminal ()->setFocus ();
}

void
FileEditorTab::fileHasChanged (QString fileName)
{
  Q_UNUSED (fileName);
  if (QFile::exists (m_fileName))
    {
      // Prevent popping up multiple message boxes when the file has been changed multiple times.
      static bool alreadyAsking = false;
      if (!alreadyAsking)
        {
          alreadyAsking = true;

          int decision =
          QMessageBox::warning (this, tr ("Octave Editor"),
                                tr ("It seems that \'%1\' has been modified by another application. Do you want to reload it?").
                                arg (m_fileName), QMessageBox::Yes, QMessageBox::No);

          if (decision == QMessageBox::Yes)
            {
              loadFile (m_fileName);
            }

          alreadyAsking = false;
        }
    }
  else
    {
      int decision =
      QMessageBox::warning (this, tr ("Octave Editor"),
                            tr ("It seems that \'%1\' has been deleted or renamed. Do you want to save it now?").
                            arg (m_fileName), QMessageBox::Save, QMessageBox::Close);
      if (decision == QMessageBox::Save)
        {
          if (!saveFileAs ())
            {
              setFileName (UNNAMED_FILE);
              newTitle (true); // window title (no modification)
              setModified (true);
              updateTrackedFile ();
            }
        }
      else
        {
          emit closeRequest ();
        }
    }
}
