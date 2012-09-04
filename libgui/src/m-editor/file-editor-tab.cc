/*

Copyright (C) 2011-2012 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "file-editor-tab.h"
#include "file-editor.h"
#include "find-dialog.h"
#include "octave-link.h"


#include <Qsci/qsciapis.h>
// Not available in the Debian repos yet!
// #include <Qsci/qscilexeroctave.h>
#include "lexer-octave-gui.h"
#include <Qsci/qscilexercpp.h>
#include <Qsci/qscilexerbash.h>
#include <Qsci/qscilexerperl.h>
#include <Qsci/qscilexerbatch.h>
#include <Qsci/qscilexerdiff.h>
#include "resource-manager.h"
#include <QApplication>
#include <QFileDialog>
#include <QMessageBox>
#include <QTextStream>
#include <QVBoxLayout>

file_editor_tab::file_editor_tab(file_editor *fileEditor)
  : QWidget ((QWidget*)fileEditor), octave_event_observer ()
{
  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

  _file_editor = fileEditor;
  _file_name = "";
  _edit_area = new QsciScintilla (this);

  // symbols
  _edit_area->setMarginType (1, QsciScintilla::SymbolMargin);
  _edit_area->setMarginSensitivity (1, true);
  _edit_area->markerDefine (QsciScintilla::RightTriangle, bookmark);
  _edit_area->markerDefine (QPixmap (":/actions/icons/redled.png"),
                            breakpoint);
  _edit_area->markerDefine (QPixmap (":/actions/icons/arrow_right.png"),
                            debugger_position);

  connect (_edit_area, SIGNAL (marginClicked (int, int,
                                              Qt::KeyboardModifiers)),
           this, SLOT (handle_margin_clicked (int, int,
                                              Qt::KeyboardModifiers)));

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
      _edit_area->setMarginWidth(2, metrics.width("9999"));
      _edit_area->setMarginLineNumbers (2, true);
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

  if (octave_add_breakpoint_event *abe
      = dynamic_cast<octave_add_breakpoint_event*> (e))
    {
      // TODO: Check file.
      _edit_area->markerAdd (abe->get_line (), breakpoint);
    }

  if (octave_remove_breakpoint_event *rbe
      = dynamic_cast<octave_remove_breakpoint_event*> (e))
    {
      // TODO: Check file.
      _edit_area->markerDelete (rbe->get_line (), breakpoint);
    }

  if (octave_remove_all_breakpoints_event *rabe
      = dynamic_cast<octave_remove_all_breakpoints_event*> (e))
    {
      Q_UNUSED (rabe);
      _edit_area->markerDeleteAll (breakpoint);
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
      // ignore close event if file is not saved and user cancels
      // closing this window
      if (check_file_modified ("Close File",
                               QMessageBox::Cancel) == QMessageBox::Cancel)
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
  update_lexer ();
  update_tracked_file ();
}

void
file_editor_tab::handle_margin_clicked(int margin, int line,
                                       Qt::KeyboardModifiers state)
{
  Q_UNUSED (state);
  if (margin == 1)
    {
      unsigned int mask = _edit_area->markersAtLine (line);

      if (state & Qt::ControlModifier)
        {
          if (mask && (1 << bookmark))
            _edit_area->markerDelete(line,bookmark);
          else
            _edit_area->markerAdd(line,bookmark);
        }
      else
        {
          if (mask && (1 << breakpoint))
            {
              request_remove_breakpoint (line);
            }
          else
            {
              request_add_breakpoint (line);
            }
        }
    }
}

void
file_editor_tab::update_lexer ()
{
  QsciLexer *lexer =  _edit_area->lexer ();
  delete lexer;

  if (_file_name.endsWith (".m") || _file_name.endsWith (".M"))
    {
      lexer = new lexer_octave_gui ();

      // The API info that is used for auto completion
      // TODO: Where to store a file with API info (raw or prepared?)?
      // TODO: Also provide infos on octave-forge functions?
      // TODO: Also provide infos on function parameters?
      // By now, use the keywords-list from syntax highlighting

      QsciAPIs *lexer_api = new QsciAPIs (lexer);

      QString keyword;
      QStringList keywordList;

       // get whole string with all keywords
      keyword = lexer->keywords (1);
      // split into single strings
      keywordList = keyword.split (QRegExp ("\\s+"));

      int i;
      for (i = 0; i < keywordList.size (); i++)
        {
           // add single strings to the API
          lexer_api->add (keywordList.at (i));
        }
      // prepare API info ... this make take some time
      lexer_api->prepare ();
    }
  else if (_file_name.endsWith (".c")
        || _file_name.endsWith (".cc")
        || _file_name.endsWith (".cpp")
        || _file_name.endsWith (".cxx")
        || _file_name.endsWith (".c++")
        || _file_name.endsWith (".h")
        || _file_name.endsWith (".hh")
        || _file_name.endsWith (".hpp")
        || _file_name.endsWith (".h++"))
    {
      lexer = new QsciLexerCPP ();
    }
  else if (_file_name.endsWith (".pl"))
    {
      lexer = new QsciLexerPerl ();
    }
  else if (_file_name.endsWith (".bat"))
    {
      lexer = new QsciLexerBatch ();
    }
  else if (_file_name.endsWith (".diff"))
    {
      lexer = new QsciLexerDiff ();
    }
  else // Default to bash lexer.
    {
      lexer = new QsciLexerBash ();
    }

  QSettings *settings = resource_manager::get_settings ();

  // FIXME -- what should happen if settings is 0?

  // Editor font (default or from settings)
  lexer->setDefaultFont (QFont (
                                settings->value ("editor/fontName",
                                                 "Courier").toString (),
                                settings->value ("editor/fontSize",
                                                 10).toInt ()));

  // TODO: Autoindent not working as it should
  lexer->setAutoIndentStyle (QsciScintilla::AiMaintain ||
                             QsciScintilla::AiOpening  ||
                             QsciScintilla::AiClosing);

  _edit_area->setLexer (lexer);
}

void
file_editor_tab::request_add_breakpoint (int line)
{
  QFileInfo file_info (_file_name);
  QString path = file_info.absolutePath ();
  QString function_name = file_info.fileName ();

  // We have to cut off the suffix, because octave appends it.
  function_name.chop (file_info.suffix ().length () + 1);

  octave_link::instance ()->post_event
      (new octave_add_breakpoint_event (*this,
                                        path.toStdString (),
                                        function_name.toStdString (),
                                        line));
}

void
file_editor_tab::request_remove_breakpoint (int line)
{
  QFileInfo file_info (_file_name);
  QString path = file_info.absolutePath ();
  QString function_name = file_info.fileName ();

  // We have to cut off the suffix, because octave appends it.
  function_name.chop (file_info.suffix ().length () + 1);

  octave_link::instance ()->post_event
      (new octave_remove_breakpoint_event (*this,
                                           path.toStdString (),
                                           function_name.toStdString (),
                                           line));
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
file_editor_tab::find ()
{
  find_dialog dialog (_edit_area);
  dialog.exec ();
}

void
file_editor_tab::update_window_title (bool modified)
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
                                       QMessageBox::Save,
                                       QMessageBox::Discard, cancelButton );
      if (decision == QMessageBox::Save)
        {
          save_file ();
          if (_edit_area->isModified ())
            {
              // If the user attempted to save the file, but it's still
              // modified, then probably something went wrong, so return
              // cancel for cancel this operation or try to save files
              // as if cancel not possible
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
  _edit_area->markerDeleteAll (bookmark);
}

void
file_editor_tab::toggle_bookmark ()
{
  int line, cur;
  _edit_area->getCursorPosition (&line,&cur);
  if ( _edit_area->markersAtLine (line) && (1 << bookmark) )
    _edit_area->markerDelete (line, bookmark);
  else
    _edit_area->markerAdd (line, bookmark);
}

void
file_editor_tab::next_bookmark()
{
  int line, cur, nextline;
  _edit_area->getCursorPosition (&line, &cur);
  if ( _edit_area->markersAtLine (line) && (1 << bookmark) )
    line++; // we have a breakpoint here, so start search from next line
  nextline = _edit_area->markerFindNext (line, (1 << bookmark));
  _edit_area->setCursorPosition (nextline, 0);
}

void
file_editor_tab::previous_bookmark ()
{
  int line, cur, prevline;
  _edit_area->getCursorPosition (&line, &cur);
  if ( _edit_area->markersAtLine (line) && (1 << bookmark) )
    line--; // we have a breakpoint here, so start search from prev line
  prevline = _edit_area->markerFindPrevious (line, (1 << bookmark));
  _edit_area->setCursorPosition (prevline, 0);
}

void
file_editor_tab::remove_all_breakpoints ()
{
  QFileInfo file_info (_file_name);
  QString path = file_info.absolutePath ();
  QString function_name = file_info.fileName ();

  // We have to cut off the suffix, because octave appends it.
  function_name.chop (file_info.suffix ().length () + 1);

  octave_link::instance ()->post_event
      (new octave_remove_all_breakpoints_event (*this,
                                                path.toStdString (),
                                                function_name.toStdString ()));
}

void
file_editor_tab::toggle_breakpoint ()
{
  int line, cur;
  _edit_area->getCursorPosition (&line, &cur);
  if ( _edit_area->markersAtLine (line) && (1 << breakpoint) )
    request_remove_breakpoint (line);
  else
    request_add_breakpoint (line);
}

void
file_editor_tab::next_breakpoint ()
{
  int line, cur, nextline;
  _edit_area->getCursorPosition (&line, &cur);
  if ( _edit_area->markersAtLine (line) && (1 << breakpoint) )
    line++; // we have a breakpoint here, so start search from next line
  nextline = _edit_area->markerFindNext (line, (1 << breakpoint));
  _edit_area->setCursorPosition (nextline, 0);
}

void
file_editor_tab::previous_breakpoint ()
{
  int line, cur, prevline;
  _edit_area->getCursorPosition (&line, &cur);
  if ( _edit_area->markersAtLine (line) && (1 << breakpoint) )
    line--; // we have a breakpoint here, so start search from prev line
  prevline = _edit_area->markerFindPrevious (line, (1 << breakpoint));
  _edit_area->setCursorPosition (prevline, 0);
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
file_editor_tab::set_debugger_position (int line)
{
  _edit_area->markerDeleteAll (debugger_position);
  if (line > 0)
    {
      _edit_area->markerAdd (line, debugger_position);
    }
}

void
file_editor_tab::set_modified (bool modified)
{
  _edit_area->setModified (modified);
}

bool
file_editor_tab::open_file (QString dir)
{
  QString openFileName;
  QFileDialog fileDialog(this);
  fileDialog.setNameFilter(SAVE_FILE_FILTER);
  fileDialog.setAcceptMode(QFileDialog::AcceptOpen);
  fileDialog.setViewMode(QFileDialog::Detail);
  fileDialog.setDirectory(dir);
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

  // save file name for later use
  _file_name = saveFileName;
  // set the window title to actual file name (not modified)
  update_window_title (false);
   // files is save -> not modified
  _edit_area->setModified (false);
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

  QFileInfo file_info (_file_name);
  QString path = file_info.absolutePath ();
  //QString current_path = QString::fromStdString
      (octave_link::instance ()->get_last_working_directory ());
  QString function_name = file_info.fileName ();

  // We have to cut off the suffix, because octave appends it.
  function_name.chop (file_info.suffix ().length () + 1);
  _file_editor->terminal ()->sendText (QString ("cd \'%1\'\n%2\n")
    .arg(path).arg (function_name));
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
      // Prevent popping up multiple message boxes when the file has
      // been changed multiple times.
      static bool alreadyAsking = false;
      if (!alreadyAsking)
        {
          alreadyAsking = true;

          int decision =
          QMessageBox::warning (this, tr ("Octave Editor"),
                                tr ("It seems that \'%1\' has been modified by another application. Do you want to reload it?").
                                arg (_file_name), QMessageBox::Yes,
                                QMessageBox::No);

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
                            arg (_file_name), QMessageBox::Save,
                            QMessageBox::Close);
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
