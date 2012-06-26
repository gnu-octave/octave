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

#include "file-editor-tab.h"
#include "file-editor.h"
#include "octave-link.h"
#include <QMessageBox>
#include <QVBoxLayout>

file_editor_tab::file_editor_tab(file_editor *fileEditor)
  : QWidget ((QWidget*)fileEditor), octave_event_observer ()
{
  QSettings *settings = resource_manager::instance ()->get_settings ();
  _file_editor = fileEditor;
  _file_name = "";
  _edit_area = new QsciScintilla (this);
  _edit_area->setLexer (fileEditor->lexer ());

  // markers
  _edit_area->setMarginType (1, QsciScintilla::SymbolMargin);
  _edit_area->setMarginSensitivity (1, true);
  _edit_area->markerDefine (QsciScintilla::RightTriangle, MARKER_BOOKMARK);
  connect (_edit_area, SIGNAL (marginClicked (int, int, Qt::KeyboardModifiers)),
           this, SLOT (handle_margin_clicked (int, int, Qt::KeyboardModifiers)));

  // line numbers
  _edit_area->setMarginsForegroundColor(QColor(96,96,96));
  _edit_area->setMarginsBackgroundColor(QColor(232,232,220));
  if (settings->value ("editor/showLineNumbers",true).toBool ())
    {
      QFont marginFont( settings->value ("editor/fontName","Courier").toString () ,
                        settings->value ("editor/fontSize",10).toInt () );
      _edit_area->setMarginsFont( marginFont );
      QFontMetrics metrics(marginFont);
      _edit_area->setMarginType (2, QsciScintilla::TextMargin);
      _edit_area->setMarginWidth(2, metrics.width("99999"));
      _edit_area->setMarginLineNumbers(2, true);
    }

  // code folding
  _edit_area->setMarginType (3, QsciScintilla::SymbolMargin);
  _edit_area->setFolding (QsciScintilla::BoxedTreeFoldStyle , 3);

  // other features
  if (settings->value ("editor/highlightCurrentLine",true).toBool ())
    {
      _edit_area->setCaretLineVisible(true);
      _edit_area->setCaretLineBackgroundColor(QColor(245,245,245));
    }
  _edit_area->setBraceMatching (QsciScintilla::StrictBraceMatch);
  _edit_area->setAutoIndent (true);
  _edit_area->setIndentationWidth (2);
  _edit_area->setIndentationsUseTabs (false);
  if (settings->value ("editor/codeCompletion",true).toBool ())
    {
      _edit_area->autoCompleteFromAll ();
      _edit_area->setAutoCompletionSource(QsciScintilla::AcsAll);
      _edit_area->setAutoCompletionThreshold (1);
    }
  _edit_area->setUtf8 (true);

  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (_edit_area);
  layout->setMargin (0);
  setLayout (layout);

  // connect modified signal
  connect (_edit_area, SIGNAL (modificationChanged (bool)),
           this, SLOT (update_window_title (bool)));
  connect (_edit_area, SIGNAL (copyAvailable (bool)),
           this, SLOT (handle_copy_available (bool)));
  connect (&_file_system_watcher, SIGNAL (fileChanged (QString)),
           this, SLOT (file_has_changed (QString)));

  _file_name = "";
  update_window_title (false);
}

bool
file_editor_tab::copy_available ()
{
  return _copy_available;
}

void
file_editor_tab::event_accepted (octave_event *e)
{
  if (dynamic_cast<octave_run_file_event*> (e))
    {
      // File was run successfully.
    }
  delete e;
}

void
file_editor_tab::event_reject (octave_event *e)
{
  if (dynamic_cast<octave_run_file_event*> (e))
    {
      // Running file failed.
    }
  delete e;
}

void
file_editor_tab::closeEvent (QCloseEvent *event)
{
  if (_file_editor->get_main_window ()->is_closing ())
    {
      // close whole application: save file or not if modified
      check_file_modified ("Closing Octave", 0); // no cancel possible
      event->accept ();
    }
  else
    {
      // ignore close event if file is not saved and user cancels closing this window
      if (check_file_modified ("Close File", QMessageBox::Cancel) == QMessageBox::Cancel)
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
file_editor_tab::set_file_name (QString fileName)
{
  _file_name = fileName;
  update_tracked_file ();
}

void
file_editor_tab::handle_margin_clicked(int margin, int line, Qt::KeyboardModifiers state)
{
  Q_UNUSED (state);
  if (margin == 1)  // marker margin
    {
      unsigned int mask = _edit_area->markersAtLine (line);
      if (mask && (1 << MARKER_BOOKMARK))
        _edit_area->markerDelete(line,MARKER_BOOKMARK);
      else
        _edit_area->markerAdd(line,MARKER_BOOKMARK);
    }
}

void
file_editor_tab::comment_selected_text ()
{
  do_comment_selected_text (true);
}

void
file_editor_tab::uncomment_selected_text ()
{
  do_comment_selected_text (false);
}

void
file_editor_tab::do_comment_selected_text (bool comment)
{
  if ( _edit_area->hasSelectedText() )
    {
      int lineFrom, lineTo, colFrom, colTo, i;
      _edit_area->getSelection (&lineFrom,&colFrom,&lineTo,&colTo);
      if ( colTo == 0 )  // the beginning of last line is not selected
        lineTo--;        // stop at line above
      _edit_area->beginUndoAction ();
      for ( i=lineFrom; i<=lineTo; i++ )
        {
          if ( comment )
            _edit_area->insertAt("%",i,0);
          else
            {
              QString line(_edit_area->text(i));
              if ( line.startsWith("%") )
                {
                  _edit_area->setSelection(i,0,i,1);
                  _edit_area->removeSelectedText();
                }
            }
        }
      _edit_area->endUndoAction ();
    }
}

void
file_editor_tab::update_window_title(bool modified)
{
  QString title(_file_name);
  if ( !_long_title )
    {
      QFileInfo file(_file_name);
      title = file.fileName();
    }

  if ( modified )
    {
      emit file_name_changed (title.prepend("* "));
    }
  else
    emit file_name_changed (title);
}

void
file_editor_tab::handle_copy_available(bool enableCopy)
{
  _copy_available = enableCopy;
  emit editor_state_changed ();
}

void
file_editor_tab::update_tracked_file ()
{
  QStringList trackedFiles = _file_system_watcher.files ();
  if (!trackedFiles.isEmpty ())
    _file_system_watcher.removePaths (trackedFiles);

  if (_file_name != UNNAMED_FILE)
    _file_system_watcher.addPath (_file_name);
}

int
file_editor_tab::check_file_modified (QString msg, int cancelButton)
{
  int decision = QMessageBox::Yes;
  if (_edit_area->isModified ())
    {
      // file is modified but not saved, aks user what to do
      decision = QMessageBox::warning (this,
                                       msg,
                                       tr ("The file %1\n"
                                           "has been modified. Do you want to save the changes?").
                                       arg (_file_name),
                                       QMessageBox::Save, QMessageBox::Discard, cancelButton );
      if (decision == QMessageBox::Save)
        {
          save_file ();
          if (_edit_area->isModified ())
            {
              // If the user attempted to save the file, but it's still
              // modified, then probably something went wrong, so return cancel
              // for cancel this operation or try to save files as if cancel not
              // possible
              if ( cancelButton )
                return (QMessageBox::Cancel);
              else
                save_file_as ();
            }
        }
    }
  return (decision);
}

void
file_editor_tab::remove_bookmark ()
{
  _edit_area->markerDeleteAll(MARKER_BOOKMARK);
}

void
file_editor_tab::toggle_bookmark ()
{
  int line,cur;
  _edit_area->getCursorPosition(&line,&cur);
  if ( _edit_area->markersAtLine (line) && (1 << MARKER_BOOKMARK) )
    _edit_area->markerDelete(line,MARKER_BOOKMARK);
  else
    _edit_area->markerAdd(line,MARKER_BOOKMARK);
}

void
file_editor_tab::next_bookmark ()
{
  int line,cur,nextline;
  _edit_area->getCursorPosition(&line,&cur);
  if ( _edit_area->markersAtLine(line) && (1 << MARKER_BOOKMARK) )
    line++; // we have a bookmark here, so start search from next line
  nextline = _edit_area->markerFindNext(line,(1 << MARKER_BOOKMARK));
  _edit_area->setCursorPosition(nextline,0);
}

void
file_editor_tab::previous_bookmark ()
{
  int line,cur,prevline;
  _edit_area->getCursorPosition(&line,&cur);
  if ( _edit_area->markersAtLine(line) && (1 << MARKER_BOOKMARK) )
    line--; // we have a bookmark here, so start search from prev line
  prevline = _edit_area->markerFindPrevious(line,(1 << MARKER_BOOKMARK));
  _edit_area->setCursorPosition(prevline,0);
}

void
file_editor_tab::cut ()
{
  _edit_area->cut ();
}

void
file_editor_tab::copy ()
{
  _edit_area->copy ();
}

void
file_editor_tab::paste ()
{
  _edit_area->paste ();
}

void
file_editor_tab::undo ()
{
  _edit_area->undo ();
}

void
file_editor_tab::redo ()
{
  _edit_area->redo ();
}

void
file_editor_tab::set_modified (bool modified)
{
  _edit_area->setModified (modified);
}

bool
file_editor_tab::open_file ()
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

      load_file(openFileName);
      return true;
    }
  else
    {
      return false;
    }
}

void
file_editor_tab::load_file (QString fileName)
{
  if (!_file_editor->isVisible ())
    {
      _file_editor->show ();
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
  _edit_area->setText (in.readAll ());
  QApplication::restoreOverrideCursor ();

  set_file_name (fileName);
  update_tracked_file ();


  update_window_title (false); // window title (no modification)
  _edit_area->setModified (false); // loaded file is not modified yet
}

void
file_editor_tab::new_file ()
{
  if (!_file_editor->isVisible ())
    {
      _file_editor->show ();
    }

  set_file_name (UNNAMED_FILE);
  update_window_title (false); // window title (no modification)
  _edit_area->setText ("");
  _edit_area->setModified (false); // new file is not modified yet
}

bool file_editor_tab::save_file()
{
  return save_file (_file_name);
}

bool
file_editor_tab::save_file (QString saveFileName)
{
  // it is a new file with the name "<unnamed>" -> call saveFielAs
  if (saveFileName == UNNAMED_FILE || saveFileName.isEmpty ())
    {
      return save_file_as();
    }

  QStringList watched_files = _file_system_watcher.files();
  if (!watched_files.isEmpty ())
    _file_system_watcher.removePaths(watched_files);

  // open the file for writing
  QFile file (saveFileName);
  if (!file.open (QFile::WriteOnly))
    {
      QMessageBox::warning (this, tr ("Octave Editor"),
                            tr ("Could not open file %1 for write:\n%2.").
                            arg (saveFileName).arg (file.errorString ()));
      _file_system_watcher.addPaths (watched_files);
      return false;
    }

  // save the contents into the file
  QTextStream out (&file);
  QApplication::setOverrideCursor (Qt::WaitCursor);
  out << _edit_area->text ();
  QApplication::restoreOverrideCursor ();
  _file_name = saveFileName; // save file name for later use
  update_window_title (false);      // set the window title to actual file name (not modified)
  _edit_area->setModified (false); // files is save -> not modified
  file.close();

  if (!watched_files.isEmpty ())
    _file_system_watcher.addPaths (watched_files);
  return true;
}

bool
file_editor_tab::save_file_as ()
{
  QString saveFileName(_file_name);
  QFileDialog fileDialog(this);
  if (saveFileName == UNNAMED_FILE || saveFileName.isEmpty ())
    {
      QString directory = QString::fromStdString
          (octave_link::instance ()->get_last_working_directory ());

      if (directory.isEmpty ())
        {
          directory = QDir::homePath ();
        }

      fileDialog.setDirectory (directory);
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

      return save_file (saveFileName);
    }

  return false;
}

void
file_editor_tab::run_file ()
{
  if (_edit_area->isModified ())
    save_file(_file_name);

  _file_editor->terminal ()->sendText (QString ("run \'%1\'\n").arg (_file_name));
  // TODO: Sending a run event crashes for long scripts. Find out why.
  //  octave_link::instance ()
  //      ->post_event (new octave_run_file_event (*this, _file_name.toStdString ()));
}

void
file_editor_tab::file_has_changed (QString fileName)
{
  Q_UNUSED (fileName);
  if (QFile::exists (_file_name))
    {
      // Prevent popping up multiple message boxes when the file has been changed multiple times.
      static bool alreadyAsking = false;
      if (!alreadyAsking)
        {
          alreadyAsking = true;

          int decision =
          QMessageBox::warning (this, tr ("Octave Editor"),
                                tr ("It seems that \'%1\' has been modified by another application. Do you want to reload it?").
                                arg (_file_name), QMessageBox::Yes, QMessageBox::No);

          if (decision == QMessageBox::Yes)
            {
              load_file (_file_name);
            }

          alreadyAsking = false;
        }
    }
  else
    {
      int decision =
      QMessageBox::warning (this, tr ("Octave Editor"),
                            tr ("It seems that \'%1\' has been deleted or renamed. Do you want to save it now?").
                            arg (_file_name), QMessageBox::Save, QMessageBox::Close);
      if (decision == QMessageBox::Save)
        {
          if (!save_file_as ())
            {
              set_file_name (UNNAMED_FILE);
              update_window_title (true); // window title (no modification)
              set_modified (true);
              update_tracked_file ();
            }
        }
      else
        {
          emit close_request ();
        }
    }
}
