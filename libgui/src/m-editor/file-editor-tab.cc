////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

//! @file file-editor-tab.cc A single GUI file tab.
//!
//! This interfaces QsciScintilla with the rest of Octave.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_QSCINTILLA)

#include <QApplication>
#include <QCheckBox>
#include <QDateTime>
#include <QDesktopServices>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QInputDialog>
#include <QLabel>
#include <QMessageBox>
#include <QPrintDialog>
#include <QPushButton>
#include <QScrollBar>
#include <QSaveFile>
#include <QStyle>
#include <QTextBlock>
#include <QTextCodec>
#include <QTextStream>
#include <QVBoxLayout>
#if defined (HAVE_QSCI_QSCILEXEROCTAVE_H)
#  define HAVE_LEXER_OCTAVE 1
#  include <Qsci/qscilexeroctave.h>
#elif defined (HAVE_QSCI_QSCILEXERMATLAB_H)
#  define HAVE_LEXER_MATLAB 1
#  include <Qsci/qscilexermatlab.h>
#endif
#include <Qsci/qscilexerbash.h>
#include <Qsci/qscilexerbatch.h>
#include <Qsci/qscilexercpp.h>
#include <Qsci/qscilexerjava.h>
#include <Qsci/qscilexerdiff.h>
#include <Qsci/qscilexerperl.h>
#include <Qsci/qsciprinter.h>

#include "file-editor-tab.h"
#include "file-editor.h"
#include "gui-preferences-cs.h"
#include "gui-preferences-ed.h"
#include "gui-preferences-global.h"
#include "gui-utils.h"
#include "marker.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "octave-txt-lexer.h"

#include "cmd-edit.h"
#include "file-ops.h"
#include "localcharset-wrapper.h"
#include "uniconv-wrappers.h"

#include "bp-table.h"
#include "builtin-defun-decls.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "oct-map.h"
#include "ov-usr-fcn.h"
#include "qt-interpreter-events.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "utils.h"
#include "version.h"

OCTAVE_BEGIN_NAMESPACE(octave)

//! A file_editor_tab object consists of a text area and three left margins.
//! The first holds breakpoints, bookmarks, and the debug program counter.
//! The second holds line numbers.  The third holds "fold" marks, to hide
//! sections of text.

// Make parent null for the file editor tab so that warning WindowModal
// messages don't affect grandparents.
file_editor_tab::file_editor_tab (base_qobject& oct_qobj,
                                  const QString& directory_arg)
: m_octave_qobj (oct_qobj)
{
  m_lexer_apis = nullptr;
  m_is_octave_file = true;
  m_lines_changed = false;
  m_autoc_active = false;

  m_ced = directory_arg;

  m_file_name = "";
  m_file_system_watcher.setObjectName ("_qt_autotest_force_engine_poller");

  m_edit_area = new octave_qscintilla (this, m_octave_qobj);
  m_line = 0;
  m_col  = 0;

  m_bp_lines.clear ();      // start with empty lists of breakpoints
  m_bp_conditions.clear ();
  m_bp_restore_count = 0;

  m_breakpoint_info.remove_next = false;
  m_breakpoint_info.remove_line = -1;

  // Initialize last modification date to now
  m_last_modified = QDateTime::currentDateTimeUtc();

  connect (m_edit_area, SIGNAL (cursorPositionChanged (int, int)),
           this, SLOT (handle_cursor_moved (int,int)));

  connect (m_edit_area, SIGNAL (SCN_CHARADDED (int)),
           this, SLOT (handle_char_added (int)));

  connect (m_edit_area, SIGNAL (SCN_DOUBLECLICK (int, int, int)),
           this, SLOT (handle_double_click (int, int, int)));

  connect (m_edit_area, SIGNAL (linesChanged ()),
           this, SLOT (handle_lines_changed ()));

  connect (m_edit_area, &octave_qscintilla::context_menu_edit_signal,
           this, &file_editor_tab::handle_context_menu_edit);

  connect (m_edit_area, &octave_qscintilla::update_rowcol_indicator_signal,
           this, &file_editor_tab::update_rowcol_indicator);

  // create statusbar for row/col indicator and eol mode
  m_status_bar = new QStatusBar (this);

  // row- and col-indicator
  m_row_indicator = new QLabel ("", this);
  QFontMetrics fm = m_row_indicator->fontMetrics ();
  m_row_indicator->setMinimumSize (4.5*fm.averageCharWidth (), 0);
  QLabel *row_label = new QLabel (tr ("line:"), this);
  m_col_indicator = new QLabel ("", this);
  m_col_indicator->setMinimumSize (4*fm.averageCharWidth (), 0);
  QLabel *col_label = new QLabel (tr ("col:"), this);
  m_status_bar->addWidget (row_label, 0);
  m_status_bar->addWidget (m_row_indicator, 0);
  m_status_bar->addWidget (col_label, 0);
  m_status_bar->addWidget (m_col_indicator, 0);

  // status bar: encoding
  QLabel *enc_label = new QLabel (tr ("encoding:"), this);
  m_enc_indicator = new QLabel ("", this);
  m_status_bar->addWidget (enc_label, 0);
  m_status_bar->addWidget (m_enc_indicator, 0);
  m_status_bar->addWidget (new QLabel (" ", this), 0);

  // status bar: eol mode
  QLabel *eol_label = new QLabel (tr ("eol:"), this);
  m_eol_indicator = new QLabel ("", this);
  m_status_bar->addWidget (eol_label, 0);
  m_status_bar->addWidget (m_eol_indicator, 0);
  m_status_bar->addWidget (new QLabel (" ", this), 0);

  // symbols
  m_edit_area->setMarginType (1, QsciScintilla::SymbolMargin);
  m_edit_area->setMarginSensitivity (1, true);
  m_edit_area->markerDefine (QsciScintilla::RightTriangle, marker::bookmark);
  m_edit_area->setMarkerBackgroundColor (QColor (0, 0, 232), marker::bookmark);
  m_edit_area->markerDefine (QsciScintilla::Circle, marker::breakpoint);
  m_edit_area->setMarkerBackgroundColor (QColor (192, 0, 0), marker::breakpoint);
  m_edit_area->markerDefine (QsciScintilla::Circle, marker::cond_break);
  m_edit_area->setMarkerBackgroundColor (QColor (255, 127, 0), marker::cond_break);
  m_edit_area->markerDefine (QsciScintilla::RightArrow,
                             marker::debugger_position);
  m_edit_area->setMarkerBackgroundColor (QColor (255, 255, 0),
                                         marker::debugger_position);
  m_edit_area->markerDefine (QsciScintilla::RightArrow,
                             marker::unsure_debugger_position);
  m_edit_area->setMarkerBackgroundColor (QColor (192, 192, 192),
                                         marker::unsure_debugger_position);

  connect (m_edit_area, SIGNAL (marginClicked (int, int,
                                               Qt::KeyboardModifiers)),
           this, SLOT (handle_margin_clicked (int, int,
                                              Qt::KeyboardModifiers)));

  connect (m_edit_area, &octave_qscintilla::context_menu_break_condition_signal,
           this, &file_editor_tab::handle_context_menu_break_condition);

  // line numbers
  m_edit_area->setMarginsForegroundColor (QColor (96, 96, 96));
  m_edit_area->setMarginsBackgroundColor (QColor (232, 232, 220));
  m_edit_area->setMarginType (2, QsciScintilla::TextMargin);

  // other features
  m_edit_area->setBraceMatching (QsciScintilla::StrictBraceMatch);
  m_edit_area->setAutoIndent (true);
  m_edit_area->setIndentationWidth (2);
  m_edit_area->setIndentationsUseTabs (false);

  m_edit_area->setUtf8 (true);

  // auto completion
  m_edit_area->SendScintilla (QsciScintillaBase::SCI_AUTOCSETCANCELATSTART, false);

  QVBoxLayout *edit_area_layout = new QVBoxLayout ();
  edit_area_layout->addWidget (m_edit_area);
  edit_area_layout->addWidget (m_status_bar);
  edit_area_layout->setMargin (0);
  edit_area_layout->setSpacing (0);
  setLayout (edit_area_layout);

  // Any interpreter_event signal from a file_editor_tab_widget is
  // handled the same as for the parent main_window object.

  connect (m_edit_area, QOverload<const fcn_callback&>::of (&octave_qscintilla::interpreter_event),
           this, QOverload<const fcn_callback&>::of (&file_editor_tab::interpreter_event));

  connect (m_edit_area, QOverload<const meth_callback&>::of (&octave_qscintilla::interpreter_event),
           this, QOverload<const meth_callback&>::of (&file_editor_tab::interpreter_event));

  // connect modified signal
  connect (m_edit_area, SIGNAL (modificationChanged (bool)),
           this, SLOT (update_window_title (bool)));

  connect (m_edit_area, SIGNAL (copyAvailable (bool)),
           this, SLOT (handle_copy_available (bool)));

  connect (&m_file_system_watcher, &QFileSystemWatcher::fileChanged,
           this, [=] (const QString& path) { file_has_changed (path); });

  connect (this, &file_editor_tab::maybe_remove_next,
           this, &file_editor_tab::handle_remove_next);

  connect (this, &file_editor_tab::dbstop_if,
           this, &file_editor_tab::handle_dbstop_if);

  connect (this, &file_editor_tab::request_add_breakpoint,
           this, &file_editor_tab::handle_request_add_breakpoint);

  connect (this, &file_editor_tab::api_entries_added,
           this, &file_editor_tab::handle_api_entries_added);

  connect (this, &file_editor_tab::confirm_dbquit_and_save_signal,
           this, &file_editor_tab::confirm_dbquit_and_save);

  connect (this, &file_editor_tab::do_save_file_signal,
           this, &file_editor_tab::do_save_file);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (settings)
    notice_settings (settings, true);

  // encoding, not updated with the settings
  m_encoding = settings->value (ed_default_enc.key, "UTF-8").toString ();
  m_enc_indicator->setText (m_encoding);
  // no changes in encoding yet
  m_new_encoding = m_encoding;
}

file_editor_tab::~file_editor_tab (void)
{
  // Tell all connected markers to self-destruct.
  emit remove_all_breakpoints_signal ();
  emit remove_all_positions ();

  // Destroy lexer attached to m_edit_area, which is not the parent
  // of lexer
  QsciLexer *lexer = m_edit_area->lexer ();
  if (lexer)
    {
      delete lexer;
      m_edit_area->setLexer (nullptr);
    }
}

void file_editor_tab::set_encoding (const QString& new_encoding)
{
  if (new_encoding.isEmpty ())
    return;

  m_encoding = new_encoding;
  m_enc_indicator->setText (m_encoding);
  if (! m_edit_area->text ().isEmpty ())
    set_modified (true);
}

void file_editor_tab::closeEvent (QCloseEvent *e)
{
  int save_dialog = check_file_modified (true);
  if ((save_dialog == QMessageBox::Cancel) ||
      (save_dialog == QMessageBox::Save))
    {
      // Ignore close event if file is saved or user cancels
      // closing this window.  In case of saving, tab is closed after
      // successful saving.
      e->ignore ();
    }
  else
    {
      e->accept ();
      emit tab_remove_request ();
    }
}

void file_editor_tab::set_current_directory (const QString& dir)
{
  m_ced = dir;
}

void file_editor_tab::handle_context_menu_edit (const QString& word_at_cursor)
{
  // Search for a subfunction in actual file (this is done first because
  // Octave finds this function before others with the same name in the
  // search path.
  QRegExp rxfun1 ("^[\t ]*function[^=]+=[\t ]*"
                  + word_at_cursor + "[\t ]*\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun2 ("^[\t ]*function[\t ]+"
                  + word_at_cursor + "[\t ]*\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun3 ("^[\t ]*function[\t ]+"
                  + word_at_cursor + "[\t ]*$");
  QRegExp rxfun4 ("^[\t ]*function[^=]+=[\t ]*"
                  + word_at_cursor + "[\t ]*$");

  int pos_fct = -1;
  QStringList lines = m_edit_area->text ().split ("\n");

  int line;
  for (line = 0; line < lines.count (); line++)
    {
      if ((pos_fct = rxfun1.indexIn (lines.at (line))) != -1)
        break;
      if ((pos_fct = rxfun2.indexIn (lines.at (line))) != -1)
        break;
      if ((pos_fct = rxfun3.indexIn (lines.at (line))) != -1)
        break;
      if ((pos_fct = rxfun4.indexIn (lines.at (line))) != -1)
        break;
    }

  if (pos_fct > -1)
    {
      // reg expr. found: it is an internal function
      m_edit_area->setCursorPosition (line, pos_fct);
      m_edit_area->SendScintilla (2232, line);     // SCI_ENSUREVISIBLE
      // SCI_VISIBLEFROMDOCLINE
      int vis_line = m_edit_area->SendScintilla (2220, line);
      m_edit_area->SendScintilla (2613, vis_line); // SCI_SETFIRSTVISIBLELINE
      return;
    }

  emit edit_mfile_request (word_at_cursor, m_file_name, m_ced, -1);
}

// If "dbstop if ..." selected from context menu, create a conditional
// breakpoint.  The default condition is (a) the existing condition if there
// is already a breakpoint, (b) any selected text, or (c) empty
void file_editor_tab::handle_context_menu_break_condition (int linenr)
{
  // Ensure editor line numbers match Octave core's line numbers.
  // Give users the option to save modifications if necessary.
  if (! unchanged_or_saved ())
    return;

  QString cond;

  // Search for previous condition.  FIXME: is there a more direct way?
  if (m_edit_area->markersAtLine (linenr) & (1 << marker::cond_break))
    {
      emit report_marker_linenr (m_bp_lines, m_bp_conditions);
      for (int i = 0; i < m_bp_lines.length (); i++)
        if (m_bp_lines.value (i) == linenr)
          {
            cond = m_bp_conditions.value (i);
            break;
          }
      m_bp_lines.clear ();
      m_bp_conditions.clear ();
    }

  // If text selected by the mouse, default to that instead
  // If both present, use the OR of them, to avoid accidental overwriting
  // FIXME: If both are present, show old condition unselected and
  //        the selection (in edit area) selected (in the dialog).
  if (m_edit_area->hasSelectedText ())
    {
      if (cond == "")
        cond = m_edit_area->selectedText ();
      else
        cond = '(' + cond + ") || (" + m_edit_area->selectedText () + ')';
    }

  emit dbstop_if ("dbstop if", linenr+1, cond);
}

// Display dialog in GUI thread to get condition, then emit
// interpreter_event signal to check it in the interpreter thread.
// If the dialog returns a valid condition, then either emit a signal
// to add the breakpoint in the editor tab or a signal to display a
// new dialog.

void file_editor_tab::handle_dbstop_if (const QString& prompt, int line,
                                        const QString& cond)
{
  bool ok;
  QString new_cond
    = QInputDialog::getText (this, tr ("Breakpoint condition"),
                             prompt, QLineEdit::Normal, cond, &ok);

  // If cancel, don't change breakpoint condition.

  if (ok && ! new_cond.isEmpty ())
    {
      // The interpreter_event callback function below emits a signal.
      // Because we don't control when that happens, use a guarded
      // pointer so that the callback can abort if this object is no
      // longer valid.

      QPointer<file_editor_tab> this_fetab (this);

      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          // We are intentionally skipping any side effects that may
          // occur in the evaluation of NEW_COND if THIS_FETAB is no
          // longer valid.

          if (this_fetab.isNull ())
            return;

          error_system& es = interp.get_error_system ();

          unwind_protect frame;

          // Prevent an error in the evaluation here from sending us
          // into the debugger.

          es.interpreter_try (frame);

          bool eval_error = false;
          std::string msg;

          try
            {
              tree_evaluator& tw = interp.get_evaluator ();
              bp_table& bptab = tw.get_bp_table ();

              bptab.condition_valid (new_cond.toStdString ());

              // The condition seems OK, so set the conditional
              // breakpoint.

              emit request_add_breakpoint (line, new_cond);
            }
          catch (const execution_exception& ee)
            {
              interp.recover_from_exception ();

              msg = ee.message ();
              eval_error = true;
            }
          catch (const interrupt_exception&)
            {
              interp.recover_from_exception ();

              msg = "evaluation interrupted";
              eval_error = true;
            }

          if (eval_error)
            {
              // Try again with a prompt that indicates the last
              // attempt was an error.

              QString new_prompt = (tr ("ERROR: ")
                                    + QString::fromStdString (msg)
                                    + "\n\ndbstop if");

              emit dbstop_if (new_prompt, line, "");
            }
        });
    }
}

void file_editor_tab::set_file_name (const QString& fileName)
{
  // update tracked file if we really have a file on disk
  QStringList trackedFiles = m_file_system_watcher.files ();
  if (! trackedFiles.isEmpty ())
    m_file_system_watcher.removePath (m_file_name);
  if (! fileName.isEmpty () && QFile::exists (fileName))
    {
      m_file_system_watcher.addPath (fileName);
      m_last_modified =  QFileInfo (fileName).lastModified ().toUTC ();
    }

  // update lexer and file name variable if file name changes
  if (m_file_name != fileName)
    {
      m_file_name = fileName;
      update_lexer ();
    }

  // set the window title to actual filename (not modified)
  update_window_title (m_edit_area->isModified ());

  // update the file editor with current editing directory
  emit editor_state_changed (m_copy_available, m_is_octave_file,
                             m_edit_area->isModified ());

  // add the new file to the most-recently-used list
  emit mru_add_file (m_file_name, m_encoding);
}

// valid_file_name (file): checks whether "file" names a file.
// By default, "file" is empty; then m_file_name is checked
bool file_editor_tab::valid_file_name (const QString& file)
{
  if (file.isEmpty ())
    {
      if (m_file_name.isEmpty ())
        return false;
      else
        return true;
    }

  return true;
}

void file_editor_tab::enable_file_watcher (bool do_enable)
{
  if (do_enable)
    m_file_system_watcher.addPath (m_file_name);
  else
    m_file_system_watcher.removePath (m_file_name);
}

// We cannot create a breakpoint when the file is modified
// because the line number the editor is providing might
// not match what Octave core is interpreting in the
// file on disk.  This function gives the user the option
// to save before creating the breakpoint.
bool file_editor_tab::unchanged_or_saved (void)
{
  bool retval = true;
  if (m_edit_area->isModified () || ! valid_file_name ())
    {
      int ans = QMessageBox::question (nullptr, tr ("Octave Editor"),
                                       tr ("Cannot add breakpoint to modified or unnamed file.\n"
                                           "Save and add breakpoint, or cancel?"),
                                       QMessageBox::Save | QMessageBox::Cancel, QMessageBox::Save);

      if (ans == QMessageBox::Save)
        save_file (m_file_name, false);
      else
        retval = false;
    }

  return retval;
}

// Toggle a breakpoint at the editor_linenr or, if this was called by
// a click with CTRL pressed, toggle a bookmark at that point.
void file_editor_tab::handle_margin_clicked (int margin, int editor_linenr,
                                             Qt::KeyboardModifiers state)
{
  if (margin == 1)
    {
      unsigned int markers_mask = m_edit_area->markersAtLine (editor_linenr);

      if (state & Qt::ControlModifier)
        {
          if (markers_mask & (1 << marker::bookmark))
            m_edit_area->markerDelete (editor_linenr, marker::bookmark);
          else
            m_edit_area->markerAdd (editor_linenr, marker::bookmark);
        }
      else
        {
          if (markers_mask & ((1 << marker::breakpoint)
                              | (1 << marker::cond_break)))
            handle_request_remove_breakpoint (editor_linenr + 1);
          else
            {
              if (unchanged_or_saved ())
                handle_request_add_breakpoint (editor_linenr + 1, "");
            }
        }
    }
}

void file_editor_tab::update_lexer (void)
{
  // Create a new lexer
  QsciLexer *lexer = nullptr;

  m_is_octave_file = false;

  // Find the required lexer from file extensions
  if (m_file_name.endsWith (".m")
      || m_file_name.endsWith ("octaverc")
      || m_file_name.endsWith (".cc-tst"))
    {
#if defined (HAVE_LEXER_OCTAVE)
      lexer = new QsciLexerOctave ();
#elif defined (HAVE_LEXER_MATLAB)
      lexer = new QsciLexerMatlab ();
#else
      lexer = new octave_txt_lexer ();
#endif
      m_is_octave_file = true;
    }

  if (! lexer)
    {
      if (m_file_name.endsWith (".c")
          || m_file_name.endsWith (".cc")
          || m_file_name.endsWith (".cpp")
          || m_file_name.endsWith (".cxx")
          || m_file_name.endsWith (".c++")
          || m_file_name.endsWith (".h")
          || m_file_name.endsWith (".hh")
          || m_file_name.endsWith (".hpp")
          || m_file_name.endsWith (".h++"))
        {
          lexer = new QsciLexerCPP ();
        }
      else if (m_file_name.endsWith (".java"))
        {
          lexer = new QsciLexerJava ();
        }
      else if (m_file_name.endsWith (".pl"))
        {
          lexer = new QsciLexerPerl ();
        }
      else if (m_file_name.endsWith (".bat"))
        {
          lexer = new QsciLexerBatch ();
        }
      else if (m_file_name.endsWith (".diff"))
        {
          lexer = new QsciLexerDiff ();
        }
      else if (m_file_name.endsWith (".sh"))
        {
          lexer = new QsciLexerBash ();
        }
      else if (! valid_file_name ())
        {
          // new, not yet named file: let us assume it is octave
#if defined (HAVE_LEXER_OCTAVE)
          lexer = new QsciLexerOctave ();
          m_is_octave_file = true;
#elif defined (HAVE_LEXER_MATLAB)
          lexer = new QsciLexerMatlab ();
          m_is_octave_file = true;
#else
          lexer = new octave_txt_lexer ();
#endif
        }
      else
        {
          // other or no extension
          lexer = new octave_txt_lexer ();
        }
    }

  // Get any existing lexer
  QsciLexer *old_lexer = m_edit_area->lexer ();

  // If new file, no lexer, or lexer has changed,
  // delete old one and set the newly created as current lexer
  if (! old_lexer || ! valid_file_name ()
      || QString(old_lexer->lexer ()) != QString(lexer->lexer ()))
    {
      // Delete and set new lexer
      if (old_lexer)
        delete old_lexer;
      m_edit_area->setLexer (lexer);

      // Build information for auto completion (APIs)
      m_lexer_apis = new QsciAPIs (lexer);

      connect (this, &file_editor_tab::request_add_octave_apis,
               this, &file_editor_tab::handle_add_octave_apis);

      // Get the settings for this new lexer
      update_lexer_settings ();
    }
  else
    {
      // Otherwise, delete the newly created lexer and
      // use the old, existing one.
      delete lexer;
    }
}

// Update settings, which are lexer related and have to be updated
// when
//    a) the lexer changes,
//    b) the settings have changed, or
//    c) a package was loaded/unloaded
void file_editor_tab::update_lexer_settings (bool update_apis_only)
{
  QsciLexer *lexer = m_edit_area->lexer ();

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (m_lexer_apis)
    {
      m_lexer_apis->cancelPreparation ();  // stop preparing if apis exists

      bool update_apis = false;  // flag, whether update of apis files

      // Get path to prepared api info (cache).  Temporarily set the
      // application name to 'octave' instead of 'GNU Octave' name for
      // not having blanks in the path.
      QString tmp_app_name = QCoreApplication::applicationName ();
      QCoreApplication::setApplicationName ("octave");  // Set new name

#if defined (HAVE_QSTANDARDPATHS)
      QString local_data_path
        = QStandardPaths::writableLocation (QStandardPaths::CacheLocation);
#else
      QString local_data_path
        = QDesktopServices::storageLocation (QDesktopServices::CacheLocation);
#endif

      QCoreApplication::setApplicationName ("octave");  // Set temp. name

      m_prep_apis_path
        = local_data_path + "/" + QString (OCTAVE_VERSION) + "/qsci/";

      // get settings which infos are used for octave
      bool octave_builtins
        = settings->value (ed_code_completion_octave_builtins).toBool ();
      bool octave_functions
        = settings->value (ed_code_completion_octave_functions).toBool ();

      QCoreApplication::setApplicationName (tmp_app_name);  // Restore name

      if (m_is_octave_file)
        {
          // Keywords and Builtins do not change, this information can be
          // stored in prepared form in a file. Information on function are
          // changing frequently, then if functions should also be auto-
          // completed, the date of any existing file is checked.

          // Keywords are always used
          m_prep_apis_file = m_prep_apis_path + lexer->lexer () + "_k";

          // Builtins are only used if the user settings say so
          if (octave_builtins)
            m_prep_apis_file += 'b';

          if (octave_functions)
            m_prep_apis_file += 'f';

          m_prep_apis_file += ".pap"; // final name of apis file

          // check whether the APIs info needs to be prepared and saved
          QFileInfo apis_file = QFileInfo (m_prep_apis_file);

          // flag whether apis file needs update
          update_apis = ! apis_file.exists ();

          if (octave_functions)
            {
              // Functions may change frequently.  Update the apis data
              // if the file is older than a few minutes preventing from
              // re-preparing data when the user opens several files.
              QDateTime apis_time = apis_file.lastModified ();
              if (update_apis_only
                  || QDateTime::currentDateTime () > apis_time.addSecs (180))
                update_apis = true;
            }

        }
      else
        {
          // No octave file, just add extension.
          m_prep_apis_file = m_prep_apis_path + lexer->lexer () + ".pap";
        }

      // Make sure the apis file is usable, otherwise the gui might crash,
      // e.g., in case of max. number of opened files
      QFile f (m_prep_apis_file);

      bool apis_usable = f.open (QIODevice::ReadOnly);
      if (! apis_usable)
        {
          QDir ().mkpath (QFileInfo (f).absolutePath ());
          apis_usable = f.open (QIODevice::WriteOnly);
        }
      if (apis_usable)
        f.close ();

      if (apis_usable
          && (update_apis || ! m_lexer_apis->loadPrepared (m_prep_apis_file)))
        {
          // either we have decided to update the apis file or
          // no prepared info was loaded, prepare and save if possible

          // create raw apis info

          m_lexer_apis->clear (); // Clear current contents

          if (m_is_octave_file)
            {
              // The interpreter_event callback function below emits a
              // signal.  Because we don't control when that happens,
              // use a guarded pointer so that the callback can abort if
              // this object is no longer valid.

              QPointer<file_editor_tab> this_fetab (this);

              emit interpreter_event
                ([=] (interpreter& interp)
                {
                  // INTERPRETER THREAD

                  // We can skip the entire callback function because it
                  // does not make any changes to the interpreter
                  // state.

                  if (this_fetab.isNull ())
                    return;

                  QStringList api_entries;

                  octave_value_list tmp = Fiskeyword ();
                  const Cell ctmp = tmp(0).cell_value ();
                  for (octave_idx_type i = 0; i < ctmp.numel (); i++)
                    {
                      std::string kw = ctmp(i).string_value ();
                      api_entries.append (QString::fromStdString (kw));
                    }

                  if (octave_builtins)
                    {
                      symbol_table& symtab = interp.get_symbol_table ();

                      string_vector bfl = symtab.built_in_function_names ();

                      for (octave_idx_type i = 0; i < bfl.numel (); i++)
                        api_entries.append (QString::fromStdString (bfl[i]));
                    }

                  if (octave_functions)
                    {
                      load_path& lp = interp.get_load_path ();

                      string_vector ffl = lp.fcn_names ();
                      string_vector afl = interp.autoloaded_functions ();

                      for (octave_idx_type i = 0; i < ffl.numel (); i++)
                        api_entries.append (QString::fromStdString (ffl[i]));

                      for (octave_idx_type i = 0; i < afl.numel (); i++)
                        api_entries.append (QString::fromStdString (afl[i]));
                    }

                  emit request_add_octave_apis (api_entries);
                });
            }
          else
            {
              for (int i = 1; i <= 3; i++)
                {
                  // Get list, split, and add to API.

                  QString keyword = QString (lexer->keywords (i));

                  QStringList keyword_list
                    = keyword.split (QRegExp (R"(\s+)"));

                  for (int j = 0; j < keyword_list.size (); j++)
                    m_lexer_apis->add (keyword_list.at (j));
                }

              emit api_entries_added ();
            }
        }
    }

  if (update_apis_only)
    return;   // We are done here

  int mode = settings->value (ed_color_mode).toInt ();
  rmgr.read_lexer_settings (lexer, settings, mode);

  m_edit_area->setCaretForegroundColor (lexer->color (0));
  m_edit_area->setIndentationGuidesForegroundColor (lexer->color (0));

  // set some colors depending on selected background color of the lexer
  QColor bg = lexer->paper (0);
  QColor fg = lexer->color (0);

  // margin and current line marker colors
  QColor bgm, fgm;

  bgm = interpolate_color (bg, fg, 0.5, 0.2);
  m_edit_area->setEdgeColor (bgm);

  m_edit_area->setMarkerForegroundColor (lexer->color (0));
  m_edit_area->setMarginsForegroundColor (lexer->color (0));

  bgm = interpolate_color (bg, fg, 0.5, 0.125);
  fgm = interpolate_color (bg, fg, 0.5, 0.25);
  m_edit_area->setMarginsBackgroundColor (bgm);
  m_edit_area->setFoldMarginColors (bgm, fgm);

  QColor current_line_bg
    = settings->color_value (ed_highlight_current_line_color, mode);
  if (current_line_bg == settings_color_no_change)
    bgm = interpolate_color (bg, fg, 0.5, 0.1);  // It is the "auto" color
  else
    bgm = current_line_bg;  // Specific color given

  m_edit_area->setCaretLineBackgroundColor (bgm);

  // color indicator for highlighting all occurrences:
  // applications highlight color with more transparency
  QColor hg = QApplication::palette ().color (QPalette::Highlight);
  m_edit_area->set_selection_marker_color (hg);

  // fix line number width with respect to the font size of the lexer and
  // set the line numbers font depending on the lexer's font
  if (settings->value (ed_show_line_numbers).toBool ())
    {
      // Line numbers width
      auto_margin_width ();

      // Line numbers font
      QFont line_numbers_font = lexer->defaultFont ();
      int font_size = line_numbers_font.pointSize ();
      font_size = font_size
        + settings->value (ed_line_numbers_size).toInt ();
      if (font_size < 4)
        font_size = 4;
      line_numbers_font.setPointSize (font_size);

      m_edit_area->setMarginsFont (line_numbers_font);
    }
  else
    m_edit_area->setMarginWidth (2, 0);
}

// function for adding entries to the octave lexer's APIs
void file_editor_tab::handle_add_octave_apis (const QStringList& api_entries)
{
  for (int idx = 0; idx < api_entries.size (); idx++)
    m_lexer_apis->add (api_entries.at (idx));

  emit api_entries_added ();
}

void file_editor_tab::handle_api_entries_added (void)
{
  // disconnect slot for saving prepared info if already connected
  disconnect (m_lexer_apis, &QsciAPIs::apiPreparationFinished,
              nullptr, nullptr);

  // check whether path for prepared info exists or can be created
  if (QDir ("/").mkpath (m_prep_apis_path))
    {
      // path exists, apis info can be saved there
      connect (m_lexer_apis, &QsciAPIs::apiPreparationFinished,
               this, &file_editor_tab::save_apis_info);
    }

  m_lexer_apis->prepare ();  // prepare apis info
}

void file_editor_tab::save_apis_info (void)
{
  m_lexer_apis->savePrepared (m_prep_apis_file);
}

// slot for fetab_set_focus: sets the focus to the current edit area
void file_editor_tab::set_focus (const QWidget *ID)
{
  if (ID != this)
    return;
  m_edit_area->setFocus ();
  emit edit_area_changed (m_edit_area); // update the edit area in find dlg
}

void file_editor_tab::context_help (const QWidget *ID, bool doc)
{
  if (ID != this)
    return;

  m_edit_area->context_help_doc (doc);
}

void file_editor_tab::context_edit (const QWidget *ID)
{
  if (ID != this)
    return;

  m_edit_area->context_edit ();
}

void file_editor_tab::save_file (const QWidget *ID)
{
  if (ID != this)
    return;

  save_file (m_file_name);
}

void file_editor_tab::save_file (const QWidget *ID, const QString& fileName,
                                 bool remove_on_success)
{
  if (ID != this)
    return;

  save_file (fileName, remove_on_success);
}

void file_editor_tab::save_file_as (const QWidget *ID)
{
  if (ID != this)
    return;

  save_file_as ();
}

void file_editor_tab::print_file (const QWidget *ID)
{
  if (ID != this)
    return;

  QsciPrinter *printer = new QsciPrinter (QPrinter::HighResolution);

  QPrintDialog printDlg (printer, this);

  if (printDlg.exec () == QDialog::Accepted)
    printer->printRange (m_edit_area);

  delete printer;
}

void file_editor_tab::run_file (const QWidget *ID, bool step_into)
{
  if (ID != this)
    return;

  if (m_edit_area->isModified () || ! valid_file_name ())
    {
      save_file (m_file_name);  // save file dialog

      // Running a file is disabled for non-octave files. But when saving
      // a new file, an octave file is assumed but might actually saved
      // as another file or with an invalid file name.
      if (! (m_is_octave_file && valid_file_name ()))
        return;
    }

  if (step_into)
    {
      // Get current first breakpoint and set breakpoint waiting for
      // the returned line number.  Store whether to remove this breakpoint
      // afterwards.
      int first_bp_line
        = m_edit_area->markerFindNext (0, (1 << marker::breakpoint)) + 1;

      // Set flag for storing the line number of the breakpoint
      m_breakpoint_info.remove_next = true;
      m_breakpoint_info.do_not_remove_line = first_bp_line;

      // Add breakpoint, storing its line number
      handle_request_add_breakpoint (1, QString ());
    }

  QFileInfo info (m_file_name);
  emit run_file_signal (info);
}

void file_editor_tab::context_run (const QWidget *ID)
{
  if (ID != this)
    return;

  m_edit_area->context_run ();
}

void file_editor_tab::toggle_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  m_edit_area->getCursorPosition (&line, &cur);

  if (m_edit_area->markersAtLine (line) & (1 << marker::bookmark))
    m_edit_area->markerDelete (line, marker::bookmark);
  else
    m_edit_area->markerAdd (line, marker::bookmark);
}

// Move the text cursor to the closest bookmark
// after the current line.
void file_editor_tab::next_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  m_edit_area->getCursorPosition (&line, &cur);

  line++; // Find bookmark strictly after the current line.

  int nextline = m_edit_area->markerFindNext (line, (1 << marker::bookmark));

  // Wrap.
  if (nextline == -1)
    nextline = m_edit_area->markerFindNext (1, (1 << marker::bookmark));

  m_edit_area->setCursorPosition (nextline, 0);
}

// Move the text cursor to the closest bookmark
// before the current line.
void file_editor_tab::previous_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  m_edit_area->getCursorPosition (&line, &cur);

  line--; // Find bookmark strictly before the current line.

  int prevline = m_edit_area->markerFindPrevious (line, (1 << marker::bookmark));

  // Wrap.  Should use the last line of the file, not 1<<15
  if (prevline == -1)
    prevline = m_edit_area->markerFindPrevious (m_edit_area->lines (),
                                                (1 << marker::bookmark));

  m_edit_area->setCursorPosition (prevline, 0);
}

QString file_editor_tab::get_all_bookmarks ()
{
  QString bmlist;
  int line = 0;

  while (line > -1)
    {
      line = m_edit_area->markerFindNext (line, (1 << marker::bookmark));
      if (line > -1)
        {
          if (! bmlist.isEmpty ())
            bmlist += ",";
          bmlist += QString::number (line);
          line++;   // search from next line, otherwise same line found again
        }
    }

  return bmlist;
}

void file_editor_tab::remove_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  m_edit_area->markerDeleteAll (marker::bookmark);
}

void
file_editor_tab::handle_request_add_breakpoint (int line,
                                                const QString& condition)
{
  if (! m_is_octave_file)
    return;

  add_breakpoint_event (line, condition);
}

void file_editor_tab::handle_request_remove_breakpoint (int line)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      tree_evaluator& tw = interp.get_evaluator ();
      bp_table& bptab = tw.get_bp_table ();

      bptab.remove_breakpoint_from_file (m_file_name.toStdString (), line);
    });
}

void file_editor_tab::toggle_breakpoint (const QWidget *ID)
{
  if (ID != this)
    return;

  int editor_linenr, cur;
  m_edit_area->getCursorPosition (&editor_linenr, &cur);

  if (m_edit_area->markersAtLine (editor_linenr) & (1 << marker::breakpoint))
    request_remove_breakpoint_via_editor_linenr (editor_linenr);
  else
    {
      if (unchanged_or_saved ())
        handle_request_add_breakpoint (editor_linenr + 1, "");
    }
}

// Move the text cursor to the closest breakpoint (conditional or unconditional)
// after the current line.
void file_editor_tab::next_breakpoint (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  m_edit_area->getCursorPosition (&line, &cur);

  line++; // Find breakpoint strictly after the current line.

  int nextline = m_edit_area->markerFindNext (line, (1 << marker::breakpoint));
  int nextcond = m_edit_area->markerFindNext (line, (1 << marker::cond_break));

  // Check if the next conditional breakpoint is before next unconditional one.
  if (nextcond != -1 && (nextcond < nextline || nextline == -1))
    nextline = nextcond;

  m_edit_area->setCursorPosition (nextline, 0);
}

// Move the text cursor to the closest breakpoint (conditional or unconditional)
// before the current line.
void file_editor_tab::previous_breakpoint (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur, prevline, prevcond;
  m_edit_area->getCursorPosition (&line, &cur);

  line--; // Find breakpoint strictly before the current line.

  prevline = m_edit_area->markerFindPrevious (line, (1 << marker::breakpoint));
  prevcond = m_edit_area->markerFindPrevious (line, (1 << marker::cond_break));

  // Check if the prev conditional breakpoint is closer than the unconditional.
  if (prevcond != -1 && prevcond > prevline)
    prevline = prevcond;

  m_edit_area->setCursorPosition (prevline, 0);
}

void file_editor_tab::remove_all_breakpoints (const QWidget *ID)
{
  if (ID != this)
    return;

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      tree_evaluator& tw = interp.get_evaluator ();
      bp_table& bptab = tw.get_bp_table ();

      bptab.remove_all_breakpoints_from_file (m_file_name.toStdString (),
                                              true);
    });
}

void file_editor_tab::scintilla_command (const QWidget *ID,
                                         unsigned int sci_msg)
{
  if (ID != this)
    return;

  m_edit_area->SendScintilla (sci_msg);
}

void file_editor_tab::comment_selected_text (const QWidget *ID,
                                             bool input_str)
{
  if (ID != this)
    return;

  do_comment_selected_text (true, input_str);
}

void file_editor_tab::uncomment_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_comment_selected_text (false);
}

void file_editor_tab::indent_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_indent_selected_text (true);
}

void file_editor_tab::unindent_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_indent_selected_text (false);
}

void file_editor_tab::smart_indent_line_or_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_smart_indent_line_or_selected_text ();
}

void file_editor_tab::convert_eol (const QWidget *ID,
                                   QsciScintilla::EolMode eol_mode)
{
  if (ID != this)
    return;

  m_edit_area->convertEols (eol_mode);
  m_edit_area->setEolMode (eol_mode);
  update_eol_indicator ();
}

void file_editor_tab::zoom_in (const QWidget *ID)
{
  if (ID != this)
    return;

  m_edit_area->zoomIn (1);
  auto_margin_width ();
}

void file_editor_tab::zoom_out (const QWidget *ID)
{
  if (ID != this)
    return;

  m_edit_area->zoomOut (1);
  auto_margin_width ();
}

void file_editor_tab::zoom_normal (const QWidget *ID)
{
  if (ID != this)
    return;

  m_edit_area->zoomTo (0);
  auto_margin_width ();
}

void file_editor_tab::add_breakpoint_event (int line, const QString& cond)
{
  // The interpreter_event callback function below emits a signal.
  // Because we don't control when that happens, use a guarded pointer
  // so that the callback can abort if this object is no longer valid.

  QPointer<file_editor_tab> this_fetab (this);

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // If THIS_FETAB is no longer valid, we still want to set the
      // breakpoint in the interpreter but we can't emit the signal
      // associated with THIS_FETAB.

      // FIXME: note duplication with the code in
      // handle_context_menu_break_condition.

      tree_evaluator& tw = interp.get_evaluator ();
      bp_table& bptab = tw.get_bp_table ();

      int lineno = bptab.add_breakpoint_in_file (m_file_name.toStdString (),
                                                 line, cond.toStdString ());
      if (this_fetab.isNull ())
        return;

      if (lineno)
        emit maybe_remove_next (lineno);
    });
}

void file_editor_tab::handle_remove_next (int remove_line)
{
  // Store some info breakpoint
  if (m_breakpoint_info.remove_next)
    {
      m_breakpoint_info.remove_line = remove_line;
      m_breakpoint_info.remove_next = false;
    }
}

void file_editor_tab::goto_line (const QWidget *ID, int line)
{
  if (ID != this)
    return;

  if (m_bp_restore_count > 0)
    {
      // This goto-line request is invoked by restoring a breakpoint during
      // saving the file, thus, do not go to the related line
      m_bp_restore_count--;
      return;
    }

  if (line <= 0)  // ask for desired line
    {
      bool ok = false;
      int index;
      m_edit_area->getCursorPosition (&line, &index);
      line = QInputDialog::getInt (m_edit_area, tr ("Goto line"),
                                   tr ("Line number"), line+1, 1,
                                   m_edit_area->lines (), 1, &ok);
      if (ok)
        m_edit_area->setCursorPosition (line-1, 0);
    }
  else  // go to given line without dialog
    m_edit_area->setCursorPosition (line-1, 0);

  center_current_line (false);  // only center line if at top or bottom
}

void file_editor_tab::move_match_brace (const QWidget *ID, bool select)
{
  if (ID != this)
    return;

  if (select)
    m_edit_area->selectToMatchingBrace ();
  else
    m_edit_area->moveToMatchingBrace ();
}

void file_editor_tab::show_auto_completion (const QWidget *ID)
{
  if (ID != this)
    return;

  m_autoc_active = true;

  QsciScintilla::AutoCompletionSource s = m_edit_area->autoCompletionSource ();
  switch (s)
    {
    case QsciScintilla::AcsAll:
      m_edit_area->autoCompleteFromAll ();
      break;

    case QsciScintilla::AcsAPIs:
      m_edit_area->autoCompleteFromAPIs ();
      break;

    case QsciScintilla::AcsDocument:
      m_edit_area->autoCompleteFromDocument ();
      break;

    case QsciScintilla::AcsNone:
      break;
    }
}

void file_editor_tab::do_indent_selected_text (bool indent)
{
  // FIXME:
  m_edit_area->beginUndoAction ();

  if (m_edit_area->hasSelectedText ())
    {
      int lineFrom, lineTo, colFrom, colTo;
      m_edit_area->getSelection (&lineFrom, &colFrom, &lineTo, &colTo);

      if (colTo == 0)  // the beginning of last line is not selected
        lineTo--;        // stop at line above

      for (int i = lineFrom; i <= lineTo; i++)
        {
          if (indent)
            m_edit_area->indent (i);
          else
            m_edit_area->unindent (i);
        }
      //set selection on (un)indented section
      m_edit_area->setSelection (lineFrom, 0, lineTo,
                                 m_edit_area->text (lineTo).length ()-1);
    }
  else
    {
      int cpline, col;
      m_edit_area->getCursorPosition (&cpline, &col);
      if (indent)
        m_edit_area->indent (cpline);
      else
        m_edit_area->unindent (cpline);
    }

  m_edit_area->endUndoAction ();
}

void file_editor_tab::do_smart_indent_line_or_selected_text (void)
{
  m_edit_area->beginUndoAction ();

  int lineFrom, lineTo;

  if (m_edit_area->hasSelectedText ())
    {
      int colFrom, colTo;
      m_edit_area->getSelection (&lineFrom, &colFrom, &lineTo, &colTo);

      if (colTo == 0)  // the beginning of last line is not selected
        lineTo--;        // stop at line above
    }
  else
    {
      int col;
      m_edit_area->getCursorPosition (&lineFrom, &col);

      lineTo = lineFrom;
    }

  m_edit_area->smart_indent_line_or_selected_text (lineFrom, lineTo);

  m_edit_area->endUndoAction ();
}

void file_editor_tab::do_comment_selected_text (bool comment, bool input_str)
{
  QRegExp rxc;
  QString ws = "^(?:[ \\t]*)";
  QStringList comment_str = m_edit_area->comment_string (comment);
  QString used_comment_str = comment_str.at (0);

  if (comment)
    {
      if (input_str)
        {
          bool ok;
          resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
          gui_settings *settings = rmgr.get_settings ();

          used_comment_str
            = QInputDialog::getText (this, tr ("Comment selected text"),
                                     tr ("Comment string to use:\n"),
                                     QLineEdit::Normal,
                                     settings->value (ed_last_comment_str, comment_str.at (0)).toString (),
                                     &ok);

          if ((! ok) || used_comment_str.isEmpty ())
            return;  // No input, do nothing
          else
            settings->setValue (ed_last_comment_str, used_comment_str);  // Store last
        }
    }
  else
    {
      // Uncommenting (several strings possible)

      // Sort strings according their length
      QStringList comment_str_sorted (comment_str.at (0));
      bool inserted;

      for (int i = 1; i < comment_str.length (); i++)
        {
          inserted = false;
          for (int j = 0; j < comment_str_sorted.length (); j++)
            {
              if (comment_str.at (i).length () > comment_str_sorted.at (j).length ())
                {
                  comment_str_sorted.insert (j, comment_str.at (i));
                  inserted = true;
                  break;
                }
            }
          if (! inserted)
            comment_str_sorted << comment_str.at (i);
        }

      // Create regular expression
      QString regexp;
      for (int i = 0; i < comment_str_sorted.length (); i++)
        {
          if (i > 0)
            regexp = regexp + QString ("|");
          regexp = regexp + comment_str_sorted.at (i);
        }
      rxc = QRegExp (ws + "(" + regexp + ")");
    }

  // Do the commenting/uncommenting
  int len = 0, lenc = 0;
  m_edit_area->beginUndoAction ();

  if (m_edit_area->hasSelectedText ())
    {
      int lineFrom, lineTo, colFrom, colTo;
      int change_col_from = 1;
      int change_col_to = 1;
      bool removed;

      m_edit_area->getSelection (&lineFrom, &colFrom, &lineTo, &colTo);

      if (colTo == 0)  // the beginning of last line is not selected
        lineTo--;        // stop at line above

      for (int i = lineFrom; i <= lineTo; i++)
        {
          if (comment)
            {
              m_edit_area->insertAt (used_comment_str, i, 0);
            }
          else
            {
              QString line (m_edit_area->text (i));
              if ((removed = line.contains (rxc)))
                {
                  len = rxc.matchedLength ();   // complete length
                  QString matched_text = rxc.capturedTexts ().at (0);
                  lenc = matched_text.remove (QRegExp (ws)).length ();  // only comment string
                  m_edit_area->setSelection (i, len-lenc, i, len);
                  m_edit_area->removeSelectedText ();
                }

              // handle case, where the selection remains unchanged
              if (i == lineFrom && (colFrom < len-lenc || ! removed))
                change_col_from = 0;  // do not change start of selection
              if (i == lineTo && (colTo < len-lenc || ! removed))
                change_col_to = 0;    // do not change end of selection
            }
        }

      // update the selection area
      if (comment)
        {
          colFrom = colFrom + lenc;   // shift start position by comment length
          if (colTo > 0)
            colTo = colTo + lenc;     // shift end position by comment length
          else
            lineTo++;                 // colTo == 0 , fully select previous line
        }
      else
        {
          if (colTo == 0)
            lineTo++;                 // colTo == 0 , fully select previous line
          colFrom = colFrom - change_col_from*lenc;
          colTo = colTo - change_col_to*lenc;
        }

      // set updated selection area
      m_edit_area->setSelection (lineFrom, colFrom, lineTo, colTo);
    }
  else
    {
      int cpline, col;
      m_edit_area->getCursorPosition (&cpline, &col);
      if (comment)
        m_edit_area->insertAt (used_comment_str, cpline, 0);
      else
        {
          QString line (m_edit_area->text (cpline));
          if (line.contains (rxc))
            {
              len = rxc.matchedLength ();   // complete length
              QString matched_text = rxc.capturedTexts ().at (0);
              lenc = matched_text.remove (QRegExp (ws)).length ();  // only comment string
              m_edit_area->setSelection (cpline, len-lenc, cpline, len);
              m_edit_area->removeSelectedText ();
            }
        }
    }
  m_edit_area->endUndoAction ();
}

void file_editor_tab::update_window_title (bool modified)
{
  QString title ("");
  QString tooltip ("");

  if (! valid_file_name ())
    title = tr ("<unnamed>");
  else
    {
      QFileInfo file (m_file_name);
      title = file.fileName ();
      tooltip = m_file_name;
    }

  emit file_name_changed (title, tooltip, modified);
}

void file_editor_tab::handle_copy_available (bool enableCopy)
{
  m_copy_available = enableCopy;
  emit editor_state_changed (m_copy_available, m_is_octave_file,
                             m_edit_area->isModified ());
}

// show_dialog: shows a modal or non modal dialog depending on input arg
void file_editor_tab::show_dialog (QDialog *dlg, bool modal)
{
  dlg->setAttribute (Qt::WA_DeleteOnClose);
  if (modal)
    dlg->exec ();
  else
    {
      dlg->setWindowModality (Qt::NonModal);
      dlg->show ();
    }
}

int file_editor_tab::check_file_modified (bool remove)
{
  int decision = QMessageBox::Yes;
  if (m_edit_area->isModified ())
    {
      // File is modified but not saved, ask user what to do.  The file
      // editor tab can't be made parent because it may be deleted depending
      // upon the response.  Instead, change the m_edit_area to read only.
      QMessageBox::StandardButtons buttons = QMessageBox::Save |
        QMessageBox::Discard |
        QMessageBox::Cancel;

      // For now, just a warning message about closing a tab that has been
      // modified seems sufficient.  Exit-condition-specific messages could
      // be achieved by making 'available_actions' a function input string.
      QString available_actions =
        tr ("Do you want to cancel closing, save or discard the changes?");

      QString file;
      if (valid_file_name ())
        file = m_file_name;
      else
        file = tr ("<unnamed>");

      QMessageBox *msgBox
        = new QMessageBox (QMessageBox::Warning, tr ("Octave Editor"),
                           tr ("The file\n\n"
                               "  %1\n\n"
                               "is about to be closed but has been modified.  "
                               "%2").
                           arg (file). arg (available_actions),
                           buttons, qobject_cast<QWidget *> (parent ()));

      msgBox->setDefaultButton (QMessageBox::Save);
      m_edit_area->setReadOnly (true);

      decision = msgBox->exec (); // show_dialog (msgBox, true);

      if (decision == QMessageBox::Cancel)
        m_edit_area->setReadOnly (false);
      else if (decision == QMessageBox::Save)
        save_file (m_file_name, remove, false);
      else
        emit tab_ready_to_close ();
    }
  else
    {
      emit tab_ready_to_close ();
    }

  return decision;
}

void file_editor_tab::set_modified (bool modified)
{
  m_edit_area->setModified (modified);
}

void file_editor_tab::recover_from_exit (void)
{
  // reset the possibly still existing read only state
  m_edit_area->setReadOnly (false);

  // if we are in this slot and the list of breakpoints is not empty,
  // then this tab was saved during an exit of the applications (not
  // restoring the breakpoints and not emptying the list) and the user
  // canceled this closing late on.
  check_restore_breakpoints ();
}

void file_editor_tab::check_restore_breakpoints (void)
{
  if (! m_bp_lines.isEmpty ())
    {
      // At least one breakpoint is present.
      // Get rid of breakpoints at old (now possibly invalid) linenumbers
      remove_all_breakpoints (this);

      // and set breakpoints at the new linenumbers
      m_bp_restore_count = m_bp_lines.length ();
      for (int i = 0; i < m_bp_lines.length (); i++)
        handle_request_add_breakpoint (m_bp_lines.value (i) + 1,
                                       m_bp_conditions.value (i));

      // Keep the list of breakpoints empty, except after explicit requests.
      m_bp_lines.clear ();
      m_bp_conditions.clear ();
    }
}

QString file_editor_tab::load_file (const QString& fileName)
{
  // get the absolute path
  QFileInfo file_info = QFileInfo (fileName);
  QString file_to_load;
  if (file_info.exists ())
    file_to_load = file_info.canonicalFilePath ();
  else
    file_to_load = fileName;
  QFile file (file_to_load);
  if (!file.open(QIODevice::ReadOnly))
    return file.errorString ();

  int col = 0, line = 0;
  if (fileName == m_file_name)
    {
      // We have to reload the current file, thus get current cursor position
      line = m_line;
      col = m_col;
    }

  QApplication::setOverrideCursor (Qt::WaitCursor);

  // read the file binary, decoding later
  QByteArray text_data = file.readAll ();

  // remove newline at end of file if we add one again when saving
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (settings->value (ed_force_newline).toBool ())
    {
      const QByteArray eol_lf = QByteArray (1, 0x0a);
      const QByteArray eol_cr = QByteArray (1, 0x0d);

      if (text_data.endsWith (eol_lf))
        text_data.chop (1);   // remove LF

      if (text_data.endsWith (eol_cr)) // remove CR (altogether CRLF, too)
        text_data.chop (1);
    }

  // decode
  QTextCodec::ConverterState st;
  QTextCodec *codec = QTextCodec::codecForName (m_encoding.toLatin1 ());
  if (codec == nullptr)
    codec = QTextCodec::codecForLocale ();

  const QString text = codec->toUnicode(text_data.constData(),
                                        text_data.size(), &st);

  // Decoding with invalid characters?
  if (st.invalidChars > 0)
    {
      // Set read only
      m_edit_area->setReadOnly (true);

      // Message box for user decision
      QString msg = tr ("There were problems reading the file\n"
                        "%1\n"
                        "with the selected encoding %2.\n\n"
                        "Modifying and saving the file might "
                        "cause data loss!")
        .arg (file_to_load).arg (m_encoding);
      QMessageBox *msg_box = new QMessageBox ();
      msg_box->setIcon (QMessageBox::Warning);
      msg_box->setText (msg);
      msg_box->setWindowTitle (tr ("Octave Editor"));
      msg_box->addButton (tr ("&Edit anyway"), QMessageBox::YesRole);
      msg_box->addButton (tr ("Chan&ge encoding"), QMessageBox::AcceptRole);
      msg_box->addButton (tr ("&Close"), QMessageBox::RejectRole);

      connect (msg_box, &QMessageBox::buttonClicked,
               this, &file_editor_tab::handle_decode_warning_answer);

      msg_box->setWindowModality (Qt::WindowModal);
      msg_box->setAttribute (Qt::WA_DeleteOnClose);
      msg_box->show ();
    }

  m_edit_area->setText (text);
  m_edit_area->setEolMode (detect_eol_mode ());

  QApplication::restoreOverrideCursor ();

  m_copy_available = false;     // no selection yet available
  m_edit_area->setModified (false); // loaded file is not modified yet
  set_file_name (file_to_load);

  update_eol_indicator ();

  m_edit_area->setCursorPosition (line, col);

  return QString ();
}

void file_editor_tab::handle_decode_warning_answer (QAbstractButton *btn)
{
  QString txt = btn->text ();

  if (txt == tr ("&Close"))
    {
      // Just close the file
      close ();
      return;
    }

  if (txt == tr ("Chan&ge encoding"))
    {
      // Dialog for reloading the file with another encoding
      QDialog dlg;
      dlg.setWindowTitle (tr ("Select new default encoding"));

      QLabel *text
        = new QLabel (tr ("Please select a new encoding\n"
                          "for reloading the current file.\n\n"
                          "This does not change the default encoding.\n"));

      QComboBox *enc_combo = new QComboBox ();
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      rmgr.combo_encoding (enc_combo);
      m_new_encoding = enc_combo->currentText ();
      connect (enc_combo, &QComboBox::currentTextChanged,
               this, &file_editor_tab::handle_current_enc_changed);

      QDialogButtonBox *buttons
        = new QDialogButtonBox (QDialogButtonBox::Ok | QDialogButtonBox::Cancel,
                                Qt::Horizontal);
      connect (buttons, &QDialogButtonBox::accepted, &dlg, &QDialog::accept);
      connect (buttons, &QDialogButtonBox::rejected, &dlg, &QDialog::reject);

      QGridLayout *main_layout = new QGridLayout;
      main_layout->setSizeConstraint (QLayout::SetFixedSize);
      main_layout->addWidget (text, 0, 0);
      main_layout->addWidget (enc_combo, 1, 0);
      main_layout->addWidget (buttons, 2, 0);
      dlg.setLayout (main_layout);

      int answer = dlg.exec ();

      if (answer == QDialog::Accepted)
        {
          // Reload the file with new encoding but using the same tab
          QString reload_file_name = m_file_name;  // store file name
          m_file_name = "";  // force reuse of this tab when opening a new file
          emit request_open_file (reload_file_name, m_new_encoding);
        }
    }

  // Continue editing, set writable again
  m_edit_area->setReadOnly (false);
}

void file_editor_tab::handle_current_enc_changed (const QString& enc)
{
  m_new_encoding = enc;
}

QsciScintilla::EolMode file_editor_tab::detect_eol_mode (void)
{
  QByteArray text = m_edit_area->text ().toLatin1 ();

  QByteArray eol_lf = QByteArray (1, 0x0a);
  QByteArray eol_cr = QByteArray (1, 0x0d);
  QByteArray eol_crlf = eol_cr;
  eol_crlf.append (eol_lf);

  int count_crlf = text.count (eol_crlf);
  int count_lf = text.count (eol_lf) - count_crlf;  // isolated lf
  int count_cr = text.count (eol_cr) - count_crlf;  // isolated cr

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  QsciScintilla::EolMode eol_mode
    = static_cast<QsciScintilla::EolMode> (settings->value (ed_default_eol_mode).toInt ());

  int count_max = 0;

  if (count_crlf > count_max)
    {
      eol_mode = QsciScintilla::EolWindows;
      count_max = count_crlf;
    }
  if (count_lf > count_max)
    {
      eol_mode = QsciScintilla::EolUnix;
      count_max = count_lf;
    }
  if (count_cr > count_max)
    {
      eol_mode = QsciScintilla::EolMac;
    }

  return eol_mode;
}

void file_editor_tab::update_eol_indicator (void)
{
  switch (m_edit_area->eolMode ())
    {
    case QsciScintilla::EolWindows:
      m_eol_indicator->setText ("CRLF");
      break;
    case QsciScintilla::EolMac:
      m_eol_indicator->setText ("CR");
      break;
    case QsciScintilla::EolUnix:
      m_eol_indicator->setText ("LF");
      break;
    }
}

void file_editor_tab::update_breakpoints ()
{
  if (m_file_name.isEmpty ())
    return;

  // Create and queue the command object.

  // The interpreter_event callback function below emits a signal.
  // Because we don't control when that happens, use a guarded pointer
  // so that the callback can abort if this object is no longer valid.

  QPointer<file_editor_tab> this_fetab (this);

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // We can skip the entire callback function because it does not
      // make any changes to the interpreter state.

      if (this_fetab.isNull ())
        return;

      octave_value_list argout = Fdbstatus (interp, ovl (), 1);

      connect (this, &file_editor_tab::update_breakpoints_signal,
               this, &file_editor_tab::update_breakpoints_handler,
               Qt::QueuedConnection);

      emit update_breakpoints_signal (argout);
    });
}

void file_editor_tab::update_breakpoints_handler (const octave_value_list& argout)
{
  octave_map dbg = argout(0).map_value ();
  octave_idx_type n_dbg = dbg.numel ();

  Cell file = dbg.contents ("file");
  Cell line = dbg.contents ("line");
  Cell cond = dbg.contents ("cond");

  for (octave_idx_type i = 0; i < n_dbg; i++)
    {
      if (file (i).string_value () == m_file_name.toStdString ())
        do_breakpoint_marker (true, this, line (i).int_value (),
                              QString::fromStdString (cond (i).string_value ()));
    }
}

void file_editor_tab::new_file (const QString& commands)
{
  update_window_title (false); // window title (no modification)

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  // set the eol mode from the settings or depending on the OS if the entry is
  // missing in the settings
  m_edit_area->setEolMode (static_cast<QsciScintilla::EolMode> (settings->value (ed_default_eol_mode).toInt ()));

  update_eol_indicator ();

  update_lexer ();

  m_edit_area->setText (commands);
  m_edit_area->setModified (!commands.isEmpty ());
}

void file_editor_tab::confirm_dbquit_and_save (const QString& file_to_save,
                                               const QString& base_name,
                                               bool remove_on_success,
                                               bool restore_breakpoints)
{
  int ans = QMessageBox::question (nullptr, tr ("Debug or Save"),
                                   tr ("This file is currently being executed.\n"
                                       "Quit debugging and save?"),
                                   QMessageBox::Save | QMessageBox::Cancel);

  if (ans == QMessageBox::Save)
    {
      // The interpreter_event callback function below emits a signal.
      // Because we don't control when that happens, use a guarded
      // pointer so that the callback can abort if this object is no
      // longer valid.

      QPointer<file_editor_tab> this_fetab (this);

      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          // If THIS_FETAB is no longer valid, we still want to perform
          // the actions in the interpreter but we can't emit the signal
          // associated with THIS_FETAB.

          tree_evaluator& tw = interp.get_evaluator ();

          tw.dbquit (true);

          command_editor::interrupt (true);

          std::string std_base_name = base_name.toStdString ();

          symbol_table& symtab = interp.get_symbol_table ();

          symtab.clear_user_function (std_base_name);

          if (this_fetab.isNull ())
            return;

          emit do_save_file_signal (file_to_save, remove_on_success,
                                    restore_breakpoints);
        });
    }
}

void file_editor_tab::save_file (const QString& saveFileName,
                                 bool remove_on_success,
                                 bool restore_breakpoints)
{
  // If it is a new file with no name, signal that saveFileAs
  // should be performed.
  if (! valid_file_name (saveFileName))
    {
      save_file_as (remove_on_success);
      return;
    }

  m_encoding = m_new_encoding;    // consider a possible new encoding

  // set the desired codec (if suitable for contents)
  QTextCodec *codec = check_valid_codec ();
  if (! codec)
    return;   // No valid codec

  // Get a list of breakpoint line numbers, before exiting debug mode
  // and clearing function in interpreter_event action below.

  emit report_marker_linenr (m_bp_lines, m_bp_conditions);

  // get the absolute path (if existing)
  QFileInfo file_info = QFileInfo (saveFileName);
  QString file_to_save;
  if (file_info.exists ())
    {
      file_to_save = file_info.canonicalFilePath ();
      QString base_name = file_info.baseName ();

      // The interpreter_event callback function below emits a signal.
      // Because we don't control when that happens, use a guarded
      // pointer so that the callback can abort if this object is no
      // longer valid.

      QPointer<file_editor_tab> this_fetab (this);

      emit interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          // We are intentionally skipping any side effects that may
          // occur in the callback function if THIS_FETAB is no longer
          // valid.  If the editor tab has disappeared, there is not
          // much point in reloading the function to restore breakpoint
          // info in the GUI.

          if (this_fetab.isNull ())
            return;

          // Force reloading of a file after it is saved.
          // This is needed to get the right line numbers for
          // breakpoints (bug #46632).

          tree_evaluator& tw = interp.get_evaluator ();

          symbol_table& symtab = interp.get_symbol_table ();

          std::string std_base_name = base_name.toStdString ();

          if (tw.in_debug_repl ())
            {
              octave_value sym;
              try
                {
                  sym = symtab.find_user_function (std_base_name);
                }
              catch (const execution_exception&)
                {
                  interp.recover_from_exception ();

                  // Ignore syntax error.  It was in the old file on disk;
                  // the user may have fixed it already.
                }

              // Return early if this file is not loaded in the symbol table
              if (! sym.is_defined () || ! sym.is_user_code ())
                {
                  emit do_save_file_signal (file_to_save, remove_on_success,
                                            restore_breakpoints);
                  return;
                }

              octave_user_code *fcn = sym.user_code_value ();

              std::string full_name = file_to_save.toStdString ();

              if (sys::canonicalize_file_name (full_name)
                  != sys::canonicalize_file_name (fcn->fcn_file_name ()))
                {
                  emit do_save_file_signal (file_to_save, remove_on_success,
                                            restore_breakpoints);
                  return;
                }

              // If this file is loaded, check that we aren't currently
              // running it.
              // FIXME: is there a better way to get this info?

              octave_idx_type curr_frame = -1;

              octave_map stk = tw.backtrace (curr_frame, false);

              Cell names = stk.contents ("name");

              for (octave_idx_type i = names.numel () - 1; i >= 0; i--)
                {
                  if (names(i).string_value () == std_base_name)
                    {
                      emit confirm_dbquit_and_save_signal
                        (file_to_save, base_name, remove_on_success,
                         restore_breakpoints);
                      return;
                    }
                }
            }

          symtab.clear_user_function (std_base_name);

          emit do_save_file_signal (file_to_save, remove_on_success,
                                    restore_breakpoints);
        });
    }
  else
    emit do_save_file_signal (saveFileName, remove_on_success,
                              restore_breakpoints);
}

void file_editor_tab::do_save_file (const QString& file_to_save,
                                    bool remove_on_success,
                                    bool restore_breakpoints)
{
  QSaveFile file (file_to_save);

  // stop watching file
  QStringList trackedFiles = m_file_system_watcher.files ();
  if (trackedFiles.contains (file_to_save))
    m_file_system_watcher.removePath (file_to_save);

  // Remove trailing white spaces if desired
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (settings->value (ed_rm_trailing_spaces).toBool ())
    {
      // Replace trailing spaces, make sure edit area is writable,
      // which is not the case when saving at exit or when closing
      // the modified file.
      bool ro = m_edit_area->isReadOnly ();
      m_edit_area->setReadOnly (false); // allow writing for replace_all
      m_edit_area->replace_all ("[ \\t]+$", "", true, false, false);
      m_edit_area->setReadOnly (ro);    // recover read only state
    }

  // open the file for writing (use QIODevice::ReadWrite for avoiding
  // truncating the previous file contents)
  if (! file.open (QIODevice::WriteOnly))
    {
      // Unsuccessful, begin watching file again if it was being
      // watched previously.
      if (trackedFiles.contains (file_to_save))
        m_file_system_watcher.addPath (file_to_save);

      // Create a NonModal message about error.
      QMessageBox *msgBox
        = new QMessageBox (QMessageBox::Critical,
                           tr ("Octave Editor"),
                           tr ("Could not open file %1 for write:\n%2.").
                           arg (file_to_save).arg (file.errorString ()),
                           QMessageBox::Ok, nullptr);
      show_dialog (msgBox, false);

      return;
    }

  // save the contents into the file

  // write the file
  QTextStream out (&file);

  // set the desired codec (if suitable for contents)
  QTextCodec *codec = check_valid_codec ();
  if (! codec)
    return;   // No valid codec

  // Save the file
  out.setCodec (codec);

  QApplication::setOverrideCursor (Qt::WaitCursor);

  out << m_edit_area->text ();
  if (settings->value (ed_force_newline).toBool ()
      && m_edit_area->text ().length ())
    out << m_edit_area->eol_string ();   // Add newline if desired

  out.flush ();
  QApplication::restoreOverrideCursor ();

  // Finish writing by committing the changes to disk,
  // where nothing is done when an error occurred while writing above
  bool writing_ok = file.commit ();

  if (writing_ok)
    {
      // Writing was successful: file exists now
      QFileInfo file_info = QFileInfo (file.fileName ());
      QString full_file_to_save = file_info.canonicalFilePath ();

      // file is save -> not modified, update encoding in statusbar
      m_edit_area->setModified (false);
      m_enc_indicator->setText (m_encoding);

      // save filename after closing file as set_file_name starts watching again
      set_file_name (full_file_to_save);   // make absolute

      emit tab_ready_to_close ();

      if (remove_on_success)
        {
          emit tab_remove_request ();
          return;  // Don't touch member variables after removal
        }

      // Attempt to restore the breakpoints if that is desired.
      // This is only allowed if the tab is not closing since changing
      // breakpoints would reopen the tab in this case.
      if (restore_breakpoints)
        check_restore_breakpoints ();
    }
  else
    {
      QMessageBox::critical (nullptr,
                             tr ("Octave Editor"),
                             tr ("The changes could not be saved to the file\n"
                                 "%1")
                             .arg (file.fileName ())
                             );
    }
}

void file_editor_tab::save_file_as (bool remove_on_success)
{
  // Simply put up the file chooser dialog box with a slot connection
  // then return control to the system waiting for a file selection.

  // reset m_new_encoding
  m_new_encoding = m_encoding;

  // If the tab is removed in response to a QFileDialog signal, the tab
  // can't be a parent.
  QFileDialog *fileDialog;
  if (remove_on_success)
    {
      // If tab is closed, "this" cannot be parent in which case modality
      // has no effect.  Disable editing instead.
      m_edit_area->setReadOnly (true);
      fileDialog = new QFileDialog ();
    }
  else
    fileDialog = new QFileDialog (this);

  // add the possible filters and the default suffix
  QStringList filters;
  filters << tr ("Octave Files (*.m)")
          << tr ("All Files (*)");
  fileDialog->setNameFilters (filters);
  fileDialog->setDefaultSuffix ("m");

  if (valid_file_name ())
    {
      fileDialog->selectFile (m_file_name);
      QFileInfo file_info (m_file_name);
      if (file_info.suffix () != "m")
        {
          // it is not an octave file
          fileDialog->selectNameFilter (filters.at (1));  // "All Files"
          fileDialog->setDefaultSuffix ("");              // no default suffix
        }
    }
  else
    {
      fileDialog->selectFile ("");
      fileDialog->setDirectory (m_ced);

      // propose a name corresponding to the function name
      // if the new file contains a function
      QString fname = get_function_name ();
      if (! fname.isEmpty ())
        fileDialog->selectFile (fname + ".m");
    }

  fileDialog->setAcceptMode (QFileDialog::AcceptSave);
  fileDialog->setViewMode (QFileDialog::Detail);
  fileDialog->setOption (QFileDialog::HideNameFilterDetails, false);

  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    {
      // Qt file dialogs
      fileDialog->setOption(QFileDialog::DontUseNativeDialog);
    }
  else
    {
      // Native file dialogs: Test for already existing files is done manually
      // since native file dialogs might not consider the automatically
      // appended default extension when checking if the file already exists
      fileDialog->setOption(QFileDialog::DontConfirmOverwrite);
    }

  connect (fileDialog, &QFileDialog::filterSelected,
           this, &file_editor_tab::handle_save_as_filter_selected);

  if (remove_on_success)
    {
      connect (fileDialog, &QFileDialog::fileSelected,
               this, &file_editor_tab::handle_save_file_as_answer_close);

      connect (fileDialog, &QFileDialog::rejected,
               this, &file_editor_tab::handle_save_file_as_answer_cancel);
    }
  else
    {
      connect (fileDialog, &QFileDialog::fileSelected,
               this, &file_editor_tab::handle_save_file_as_answer);
    }

  show_dialog (fileDialog, ! valid_file_name ());
}

void file_editor_tab::handle_save_as_filter_selected (const QString& filter)
{
  // On some systems, the filterSelected signal is emitted without user
  // action and with  an empty filter string when the file dialog is shown.
  // Just return in this case and do not remove the current default suffix.
  if (filter.isEmpty ())
    return;

  QFileDialog *file_dialog = qobject_cast<QFileDialog *> (sender ());

  QRegExp rx ("\\*\\.([^ ^\\)]*)[ \\)]");   // regexp for suffix in filter
  int index = rx.indexIn (filter, 0);       // get first suffix in filter

  if (index > -1)
    file_dialog->setDefaultSuffix (rx.cap (1)); // found a suffix, set default
  else
    file_dialog->setDefaultSuffix ("");         // not found, clear default
}

bool file_editor_tab::check_valid_identifier (QString file_name)
{
  QFileInfo file = QFileInfo (file_name);
  QString base_name = file.baseName ();

  if ((file.suffix () == "m")
      && (! valid_identifier (base_name.toStdString ())))
    {
      int ans = QMessageBox::question (nullptr, tr ("Octave Editor"),
                                       tr ("\"%1\"\n"
                                           "is not a valid identifier.\n\n"
                                           "If you keep this filename, you will not be able to\n"
                                           "call your script using its name as an Octave command.\n\n"
                                           "Do you want to choose another name?").arg (base_name),
                                       QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

      if (ans == QMessageBox::Yes)
        return true;
    }

  return false;
}

QTextCodec* file_editor_tab::check_valid_codec ()
{
  QTextCodec *codec = QTextCodec::codecForName (m_encoding.toLatin1 ());

  // "SYSTEM" is used as alias for the locale encoding.
  if ((! codec) && m_encoding.startsWith("SYSTEM"))
    codec = QTextCodec::codecForLocale ();

  if (! codec)
    {
      QMessageBox::critical (nullptr,
                             tr ("Octave Editor"),
                             tr ("The current encoding %1\n"
                                 "can not be applied.\n\n"
                                 "Please select another one!").arg (m_encoding));

      return nullptr;
    }

  QString editor_text = m_edit_area->text ();
  bool can_encode = codec->canEncode (editor_text);

  // We cannot rely on QTextCodec::canEncode because it uses the
  // ConverterState of convertFromUnicode which isn't updated by some
  // implementations.
  if (can_encode)
    {
      QVector<uint> u32_str = editor_text.toUcs4 ();
      const uint32_t *src = reinterpret_cast<const uint32_t *>
        (u32_str.data ());

      std::size_t length;
      const std::string encoding = m_encoding.toStdString ();
      char *res_str =
        octave_u32_conv_to_encoding_strict (encoding.c_str (), src,
                                            u32_str.size (), &length);
      if (! res_str)
        {
          if (errno == EILSEQ)
            can_encode = false;
        }
      else
        ::free (static_cast<void *> (res_str));
    }

  if (! can_encode)
    {
      QMessageBox::StandardButton pressed_button
        = QMessageBox::critical (nullptr,
                                 tr ("Octave Editor"),
                                 tr ("The current editor contents can not be encoded\n"
                                     "with the selected encoding %1.\n"
                                     "Using it would result in data loss!\n\n"
                                     "Please select another one!").arg (m_encoding),
                                 QMessageBox::Cancel | QMessageBox::Ignore,
                                 QMessageBox::Cancel);

      if (pressed_button == QMessageBox::Ignore)
        return codec;
      else
        return nullptr;
    }

  return codec;
}

void file_editor_tab::handle_save_file_as_answer (const QString& save_file_name)
{
  QString saveFileName = save_file_name;
  QFileInfo file (saveFileName);
  QFileDialog *file_dialog = qobject_cast<QFileDialog *> (sender ());

  // Test if the file dialog should have added a default file
  // suffix, but the selected file still has no suffix (see Qt bug
  // https://bugreports.qt.io/browse/QTBUG-59401)
  if ((! file_dialog->defaultSuffix ().isEmpty ()) && file.suffix ().isEmpty ())
    {
      saveFileName = saveFileName + "." + file_dialog->defaultSuffix ();
    }

  file.setFile (saveFileName);

  // If overwrite confirmation was not done by the file dialog (in case
  // of native file dialogs, see above), do it here
  if (file_dialog->testOption (QFileDialog::DontConfirmOverwrite) && file.exists ())
    {
      int ans = QMessageBox::question (file_dialog,
                                       tr ("Octave Editor"),
                                       tr ("%1\n already exists\n"
                                           "Do you want to overwrite it?").arg (saveFileName),
                                       QMessageBox::Yes | QMessageBox::No);
      if (ans != QMessageBox::Yes)
        {
          // Try again, if edit area is read only, remove on success
          save_file_as (m_edit_area->isReadOnly ());
          return;
        }
    }

  if (saveFileName == m_file_name)
    {
      save_file (saveFileName);
    }
  else
    {
      // Have editor check for conflict, do not delete tab after save.
      if (check_valid_identifier (saveFileName))
        save_file_as (false);
      else
        emit editor_check_conflict_save (saveFileName, false);
    }
}

void file_editor_tab::handle_save_file_as_answer_close (const QString& saveFileName)
{
  // saveFileName == m_file_name can not happen, because we only can get here
  // when we close a tab and m_file_name is not a valid filename yet

  // Have editor check for conflict, delete tab after save.
  if (check_valid_identifier (saveFileName))
    save_file_as (true);
  else
    emit editor_check_conflict_save (saveFileName, true);
}

void file_editor_tab::handle_save_file_as_answer_cancel (void)
{
  // User canceled, allow editing again.
  m_edit_area->setReadOnly (false);
}

void file_editor_tab::file_has_changed (const QString&, bool do_close)
{
  bool file_exists = QFile::exists (m_file_name);

  if (file_exists && ! do_close)
    {
      // Test if file is really modified or if just the timezone has
      // changed.  In the latter, just return without doing anything.
      QDateTime modified = QFileInfo (m_file_name).lastModified ().toUTC ();

      if (modified <= m_last_modified)
        return;

      m_last_modified = modified;
    }

  // Prevent popping up multiple message boxes when the file has
  // been changed multiple times by temporarily removing from the
  // file watcher.
  QStringList trackedFiles = m_file_system_watcher.files ();
  if (! trackedFiles.isEmpty ())
    m_file_system_watcher.removePath (m_file_name);

  if (file_exists && ! do_close)
    {

      // The file is modified
      if (m_always_reload_changed_files)
        load_file (m_file_name);

      else
        {
          // give editor and this tab the focus,
          // possibly making the editor visible if it is hidden
          emit set_focus_editor_signal (this);
          m_edit_area->setFocus ();

          // Create a WindowModal message that blocks the edit area
          // by making m_edit_area parent.
          QMessageBox *msgBox
            = new QMessageBox (QMessageBox::Warning,
                               tr ("Octave Editor"),
                               tr ("It seems that \'%1\' has been modified by another application. Do you want to reload it?").
                               arg (m_file_name),
                               QMessageBox::Yes | QMessageBox::No, this);

          connect (msgBox, &QMessageBox::finished,
                   this, &file_editor_tab::handle_file_reload_answer);

          msgBox->setWindowModality (Qt::WindowModal);
          msgBox->setAttribute (Qt::WA_DeleteOnClose);
          msgBox->show ();
        }
    }
  else
    {
      // If desired and if file is not modified,
      // close the file without any user interaction
      if (do_close && ! m_edit_area->isModified ())
        {
          handle_file_resave_answer (QMessageBox::Cancel);
          return;
        }

      // give editor and this tab the focus,
      // possibly making the editor visible if it is hidden
      emit set_focus_editor_signal (this);
      m_edit_area->setFocus ();

      QString modified = "";
      if (m_edit_area->isModified ())
        modified = tr ("\n\nWarning: The contents in the editor is modified!");

      // Create a WindowModal message.  The file editor tab can't be made
      // parent because it may be deleted depending upon the response.
      // Instead, change the m_edit_area to read only.
      QMessageBox *msgBox
        = new QMessageBox (QMessageBox::Warning, tr ("Octave Editor"),
                           tr ("It seems that the file\n"
                               "%1\n"
                               "has been deleted or renamed. Do you want to save it now?%2").
                           arg (m_file_name).arg (modified),
                           QMessageBox::Save | QMessageBox::Close, nullptr);

      m_edit_area->setReadOnly (true);

      connect (msgBox, &QMessageBox::finished,
               this, &file_editor_tab::handle_file_resave_answer);

      msgBox->setWindowModality (Qt::WindowModal);
      msgBox->setAttribute (Qt::WA_DeleteOnClose);
      msgBox->show ();
    }
}

void file_editor_tab::notice_settings (const gui_settings *settings, bool init)
{
  if (! settings)
    return;

  if (! init)
    update_lexer_settings ();

  // code folding
  if (settings->value (ed_code_folding).toBool ())
    {
      m_edit_area->setMarginType (3, QsciScintilla::SymbolMargin);
      m_edit_area->setFolding (QsciScintilla::BoxedTreeFoldStyle, 3);
    }
  else
    {
      m_edit_area->setFolding (QsciScintilla::NoFoldStyle, 3);
    }

  // status bar
  if (settings->value (ed_show_edit_status_bar).toBool ())
    m_status_bar->show ();
  else
    m_status_bar->hide ();

  //highlight current line color
  m_edit_area->setCaretLineVisible
    (settings->value (ed_highlight_current_line).toBool ());

  // auto completion
  bool match_keywords = settings->value
    (ed_code_completion_keywords).toBool ();
  bool match_document = settings->value
    (ed_code_completion_document).toBool ();

  QsciScintilla::AutoCompletionSource source = QsciScintilla::AcsNone;
  if (match_keywords)
    if (match_document)
      source = QsciScintilla::AcsAll;
    else
      source = QsciScintilla::AcsAPIs;
  else if (match_document)
    source = QsciScintilla::AcsDocument;
  m_edit_area->setAutoCompletionSource (source);

  m_edit_area->setAutoCompletionReplaceWord
    (settings->value (ed_code_completion_replace).toBool ());
  m_edit_area->setAutoCompletionCaseSensitivity
    (settings->value (ed_code_completion_case).toBool ());

  if (settings->value (ed_code_completion).toBool ())
    m_edit_area->setAutoCompletionThreshold
      (settings->value (ed_code_completion_threshold).toInt ());
  else
    m_edit_area->setAutoCompletionThreshold (-1);

  if (settings->value (ed_show_white_space).toBool ())
    if (settings->value (ed_show_white_space_indent).toBool ())
      m_edit_area->setWhitespaceVisibility (QsciScintilla::WsVisibleAfterIndent);
    else
      m_edit_area->setWhitespaceVisibility (QsciScintilla::WsVisible);
  else
    m_edit_area->setWhitespaceVisibility (QsciScintilla::WsInvisible);

  m_edit_area->setEolVisibility (settings->value (ed_show_eol_chars).toBool ());

  m_save_as_desired_eol = static_cast<QsciScintilla::EolMode>
    (settings->value (ed_default_eol_mode).toInt ());

  if (settings->value (ed_show_line_numbers).toBool ())
    {
      m_edit_area->setMarginLineNumbers (2, true);
      auto_margin_width ();
      connect (m_edit_area, SIGNAL (linesChanged ()),
               this, SLOT (auto_margin_width ()));
    }
  else
    {
      m_edit_area->setMarginLineNumbers (2, false);
      disconnect (m_edit_area, SIGNAL (linesChanged ()), nullptr, nullptr);
    }

  m_smart_indent = settings->value (ed_auto_indent).toBool ();
  m_edit_area->setAutoIndent (m_smart_indent);
  m_edit_area->setTabIndents
    (settings->value (ed_tab_indents_line).toBool ());
  m_edit_area->setBackspaceUnindents
    (settings->value (ed_backspace_unindents_line).toBool ());
  m_edit_area->setIndentationGuides
    (settings->value (ed_show_indent_guides).toBool ());
  m_edit_area->setIndentationsUseTabs
    (settings->value (ed_indent_uses_tabs).toBool ());
  m_edit_area->setIndentationWidth
    (settings->value (ed_indent_width).toInt ());

  m_edit_area->setTabWidth
    (settings->value (ed_tab_width).toInt ());

  m_ind_char_width = 1;
  if (m_edit_area->indentationsUseTabs ())
    m_ind_char_width = m_edit_area->tabWidth ();

  m_edit_area->SendScintilla (QsciScintillaBase::SCI_SETHSCROLLBAR,
                              settings->value (ed_show_hscroll_bar).toBool ());
  m_edit_area->SendScintilla (QsciScintillaBase::SCI_SETSCROLLWIDTH,-1);
  m_edit_area->SendScintilla (QsciScintillaBase::SCI_SETSCROLLWIDTHTRACKING,true);

  update_window_title (m_edit_area->isModified ());

  m_auto_endif = settings->value (ed_auto_endif).toInt ();

  // long line marker
  int line_length = settings->value (ed_long_line_column).toInt ();
  m_edit_area->setEdgeColumn (line_length);

  if (settings->value (ed_long_line_marker).toBool ())
    {
      if (settings->value (ed_long_line_marker_line).toBool ())
        m_edit_area->setEdgeMode (QsciScintilla::EdgeLine);
      else
        {
          if (settings->value (ed_long_line_marker_background)
              .toBool ())
            m_edit_area->setEdgeMode (QsciScintilla::EdgeBackground);
          else
            m_edit_area->setEdgeMode (QsciScintilla::EdgeLine);
        }
    }
  else
    m_edit_area->setEdgeMode (QsciScintilla::EdgeNone);

  // line wrapping and breaking
  m_edit_area->setWrapVisualFlags (QsciScintilla::WrapFlagByBorder);
  m_edit_area->setWrapIndentMode (QsciScintilla::WrapIndentSame);

  if (settings->value (ed_wrap_lines).toBool ())
    m_edit_area->setWrapMode (QsciScintilla::WrapWord);
  else
    m_edit_area->setWrapMode (QsciScintilla::WrapNone);

  if (settings->value (ed_break_lines).toBool ())
    m_line_break = line_length;
  else
    m_line_break = 0;

  m_line_break_comments =
    settings->value (ed_break_lines_comments).toBool ();

  // highlight all occurrences of a word selected by a double click
  m_highlight_all_occurrences =
    settings->value (ed_highlight_all_occurrences).toBool ();

  // reload changed files
  m_always_reload_changed_files =
    settings->value (ed_always_reload_changed_files).toBool ();

  // Set cursor blinking depending on the settings.
  // QScintilla ignores the application global settings, so some special
  // handling is required
  bool cursor_blinking;

  if (settings->contains (global_cursor_blinking.key))
    cursor_blinking = settings->value (global_cursor_blinking).toBool ();
  else
    cursor_blinking = settings->value (cs_cursor_blinking).toBool ();

  if (cursor_blinking)
    m_edit_area->SendScintilla (QsciScintillaBase::SCI_SETCARETPERIOD, 500);
  else
    m_edit_area->SendScintilla (QsciScintillaBase::SCI_SETCARETPERIOD, 0);

}

void file_editor_tab::auto_margin_width (void)
{
  m_edit_area->setMarginWidth (2, "1" + QString::number (m_edit_area->lines ()));
}

// the following close request was changed from a signal slot into a
// normal function because we need the return value from close whether
// the tab really was closed (for canceling exiting octave).
// When emitting a signal, only the return value from the last slot
// goes back to the sender
bool file_editor_tab::conditional_close (void)
{
  return close ();
}

void file_editor_tab::change_editor_state (const QWidget *ID)
{
  if (ID != this)
    return;

  emit editor_state_changed (m_copy_available, m_is_octave_file,
                             m_edit_area->isModified ());
}

void file_editor_tab::handle_file_reload_answer (int decision)
{
  if (decision == QMessageBox::Yes)
    {
      // reload: file is readded to the file watcher in set_file_name ()
      load_file (m_file_name);
    }
  else
    {
      // do not reload: readd to the file watcher
      m_file_system_watcher.addPath (m_file_name);
    }
}

void file_editor_tab::handle_file_resave_answer (int decision)
{
  // check decision of user in dialog
  if (decision == QMessageBox::Save)
    {
      save_file (m_file_name);  // readds file to watcher in set_file_name ()
      m_edit_area->setReadOnly (false);  // delete read only flag
    }
  else
    {
      // Definitely close the file.
      // Set modified to false to prevent the dialog box when the close event
      // is posted.  If the user cancels the close in this dialog the tab is
      // left open with a non-existing file.
      m_edit_area->setModified (false);
      close ();
    }
}

void file_editor_tab::insert_debugger_pointer (const QWidget *ID, int line)
{
  if (ID != this || ID == nullptr)
    return;

  emit remove_all_positions ();  // debugger_position, unsure_debugger_position

  if (line > 0)
    {
      marker *dp;

      if (m_edit_area->isModified ())
        {
          // The best that can be done if the editor contents have been
          // modified is to see if there is a match with the original
          // line number of any existing breakpoints.  We can put a normal
          // debugger pointer at that breakpoint position.  Otherwise, it
          // isn't certain whether the original line number and current line
          // number match.
          int editor_linenr = -1;
          marker *dummy;
          emit find_translated_line_number (line, editor_linenr, dummy);
          if (editor_linenr != -1)
            {
              // Match with an existing breakpoint.
              dp = new marker (m_edit_area, line,
                               marker::debugger_position, editor_linenr);
            }
          else
            {
              int original_linenr = -1;
              editor_linenr = -1;
              emit find_linenr_just_before (line, original_linenr, editor_linenr);
              if (original_linenr >= 0)
                {
                  // Make a guess by using an offset from the breakpoint.
                  int linenr_guess = editor_linenr + line - original_linenr;
                  dp = new marker (m_edit_area, line,
                                   marker::unsure_debugger_position,
                                   linenr_guess);
                }
              else
                {
                  // Can't make a very good guess, so just use the debugger
                  // line number.
                  dp = new marker (m_edit_area, line,
                                   marker::unsure_debugger_position);
                }
            }
        }
      else
        {
          dp = new marker (m_edit_area, line, marker::debugger_position);

          // In case of a not modified file we might have to remove
          // a breakpoint here if we have stepped into the file
          if (line == m_breakpoint_info.remove_line)
            {
              m_breakpoint_info.remove_line = -1;
              if (line != m_breakpoint_info.do_not_remove_line)
                handle_request_remove_breakpoint (line);
            }
        }

      connect (this, &file_editor_tab::remove_position_via_debugger_linenr,
               dp, &marker::handle_remove_via_original_linenr);

      connect (this, &file_editor_tab::remove_all_positions,
               dp, &marker::handle_remove);

      center_current_line (false);
    }
}

void file_editor_tab::delete_debugger_pointer (const QWidget *ID, int line)
{
  if (ID != this || ID == nullptr)
    return;

  if (line > 0)
    emit remove_position_via_debugger_linenr (line);
}

void file_editor_tab::do_breakpoint_marker (bool insert,
                                            const QWidget *ID, int line,
                                            const QString& cond)
{
  if (ID != this || ID == nullptr)
    return;

  if (line > 0)
    {
      if (insert)
        {
          int editor_linenr = -1;
          marker *bp = nullptr;

          // If comes back indicating a non-zero breakpoint marker,
          // reuse it if possible
          emit find_translated_line_number (line, editor_linenr, bp);
          if (bp != nullptr)
            {
              if ((cond == "") != (bp->get_cond () == ""))
                {
                  // can only reuse conditional bp as conditional
                  emit remove_breakpoint_via_debugger_linenr (line);
                  bp = nullptr;
                }
              else
                bp->set_cond (cond);
            }

          if (bp == nullptr)
            {
              bp = new marker (m_edit_area, line,
                               cond == "" ? marker::breakpoint
                               : marker::cond_break, cond);

              connect (this, &file_editor_tab::remove_breakpoint_via_debugger_linenr,
                       bp, &marker::handle_remove_via_original_linenr);
              connect (this, &file_editor_tab::request_remove_breakpoint_via_editor_linenr,
                       bp, &marker::handle_request_remove_via_editor_linenr);
              connect (this, &file_editor_tab::remove_all_breakpoints_signal,
                       bp, &marker::handle_remove);
              connect (this, &file_editor_tab::find_translated_line_number,
                       bp, &marker::handle_find_translation);
              connect (this, &file_editor_tab::find_linenr_just_before,
                       bp, &marker::handle_find_just_before);
              connect (this, &file_editor_tab::report_marker_linenr,
                       bp, &marker::handle_report_editor_linenr);
              connect (bp, &marker::request_remove,
                       this, &file_editor_tab::handle_request_remove_breakpoint);
            }
        }
      else
        emit remove_breakpoint_via_debugger_linenr (line);
    }
}

void file_editor_tab::center_current_line (bool always)
{
  long int visible_lines
    = m_edit_area->SendScintilla (QsciScintillaBase::SCI_LINESONSCREEN);

  if (visible_lines > 2)
    {
      int line, index;
      m_edit_area->getCursorPosition (&line, &index);
      // compensate for "folding":
      // step 1: expand the current line, if it was folded
      m_edit_area->SendScintilla (2232, line);   // SCI_ENSUREVISIBLE

      // step 2: map file line num to "visible" one // SCI_VISIBLEFROMDOCLINE
      int vis_line = m_edit_area->SendScintilla (2220, line);

      int first_line = m_edit_area->firstVisibleLine ();

      if (always || vis_line == first_line
          || vis_line > first_line + visible_lines - 2)
        {
          first_line += (vis_line - first_line - (visible_lines - 1) / 2);
          m_edit_area->SendScintilla (2613, first_line); // SCI_SETFIRSTVISIBLELINE
        }
    }
}

void file_editor_tab::handle_lines_changed (void)
{
  // the related signal is emitted before cursor-move-signal!
  m_lines_changed = true;
}

void file_editor_tab::handle_cursor_moved (int line, int col)
{
  // Cursor has moved, first check wether an autocompletion list
  // is active or if it was closed. Scintilla provides signals for
  // completed or cancelled lists, but not for list that where hidden
  // due to a new character not matching anymore with the list entries
  if (m_edit_area->SendScintilla (QsciScintillaBase::SCI_AUTOCACTIVE))
    m_autoc_active = true;
  else if (m_autoc_active)
    {
      m_autoc_active = false;
      emit autoc_closed (); // Tell editor about closed list
    }

  // Lines changed? Take care of indentation!
  bool do_smart_indent = m_lines_changed && m_is_octave_file
    && (line == m_line+1) && (col < m_col)
    && (m_smart_indent || m_auto_endif);
  m_lines_changed = false;

  // Update line and column indicator in the status bar
  int o_line = m_line;
  update_rowcol_indicator (line, col);

  // Do smart indent after update of line indicator for having
  // consistent indicator data
  if (do_smart_indent)
    m_edit_area->smart_indent (m_smart_indent, m_auto_endif,
                               o_line, m_ind_char_width);
}

void file_editor_tab::update_rowcol_indicator (int line, int col)
{
  m_line = line;
  m_col  = col;
  m_row_indicator->setNum (line+1);
  m_col_indicator->setNum (col+1);
}

// Slot that is entered each time a new character was typed.
// It is used for handling line breaking if this is desired.
// The related signal is emitted after the signal for a moved cursor
// such that m_col and m_line can not be used for current position.
void file_editor_tab::handle_char_added (int)
{
  if (m_line_break)
    {
      // If line breaking is desired, get the current line and column.
      // For taking the tab width into consideration, use own function
      int line, col, pos;
      m_edit_area->get_current_position (&pos, &line, &col);

      // immediately return if line has not reached the max. line length
      if (col <= m_line_break)
        return;

      // If line breaking is only desired in comments,
      // return if not in a comment
      int style_comment = octave_qscintilla::ST_NONE;
      if (m_line_break_comments)
        {
          // line breaking only in comments, check for comment style
          style_comment = m_edit_area->is_style_comment ();
          if (! style_comment)
            return;       // no comment, return
        }

      // Here we go for breaking the current line by inserting a newline.
      // For determining the position of a specific column, we have to get
      // the column from the QScintilla function without taking tab lengths
      // into account, since the calculation from line/col to position
      // ignores this, too.
      m_edit_area->getCursorPosition (&line, &col);
      int c = 0;
      int col_space = col;
      int indentation = m_edit_area->indentation (line);

      // Search the first occurrence of space or tab backwards starting from
      // the current column (col_space).
      while (c != ' ' && c != '\t' && col_space > indentation)
        {
          pos = m_edit_area->positionFromLineIndex (line, col_space--);
          c = m_edit_area->SendScintilla (QsciScintillaBase::SCI_GETCHARAT, pos);
        }

      // If a space or tab was found, break at this char,
      // otherwise break at cursor position
      int col_newline = col - 1;
      if (c == ' ' || c == '\t')
        col_newline = col_space + 1;

      // Insert a newline char for breaking the line possibly followed
      // by a line comment string
      QString newline = QString ("\n");
      style_comment = m_edit_area->is_style_comment ();
      if (style_comment == octave_qscintilla::ST_LINE_COMMENT)
        newline = newline + m_edit_area->comment_string ().at (0);
      m_edit_area->insertAt (newline, line, col_newline);

      // Automatically indent the new line to the indentation of previous line
      // and set the cursor position to the end of the indentation.
      m_edit_area->setIndentation (line + 1, indentation);
      m_edit_area->SendScintilla (QsciScintillaBase::SCI_LINEEND);
    }
}

// Slot handling a double click into the text area
void file_editor_tab::handle_double_click (int, int, int modifier)
{
  if (! modifier)
    {
      // double clicks without modifier
      // clear any existing indicators of this type
      m_edit_area->clear_selection_markers ();

      if (m_highlight_all_occurrences)
        {
          // Clear any previous selection.
          m_edit_area->set_word_selection ();

          // highlighting of all occurrences of the clicked word is enabled

          // get the resulting cursor position
          // (required if click was beyond a line ending)
          int line, col;
          m_edit_area->getCursorPosition (&line, &col);

          // get the word at the cursor (if any)
          QString word = m_edit_area->wordAtLineIndex (line, col);
          word = word.trimmed ();

          if (! word.isEmpty ())
            {
              // word is not empty, so find all occurrences of the word

              // remember first visible line and x-offset for restoring the view afterwards
              int first_line = m_edit_area->firstVisibleLine ();
              int x_offset = m_edit_area->SendScintilla (QsciScintillaBase::SCI_GETXOFFSET);

              // search for first occurrence of the detected word
              bool find_result_available
                = m_edit_area->findFirst (word,
                                          false,   // no regexp
                                          true,    // case sensitive
                                          true,    // whole words only
                                          false,   // do not wrap
                                          true,    // forward
                                          0, 0,    // from the beginning
                                          false
#if defined (HAVE_QSCI_VERSION_2_6_0)
                                          , true
#endif
                                          );

              // loop over all occurrences and set the related indicator
              int oline, ocol;
              int wlen = word.length ();

              while (find_result_available)
                {
                  // get cursor position after having found an occurrence
                  m_edit_area->getCursorPosition (&oline, &ocol);
                  // mark the selection
                  m_edit_area->show_selection_markers (oline, ocol-wlen, oline, ocol);

                  // find next occurrence
                  find_result_available = m_edit_area->findNext ();
                }

              // restore the visible area of the file, the cursor position,
              // and the selection
              m_edit_area->setFirstVisibleLine (first_line);
              m_edit_area->SendScintilla (QsciScintillaBase::SCI_SETXOFFSET, x_offset);
              m_edit_area->setCursorPosition (line, col);
              m_edit_area->setSelection (line, col - wlen, line, col);
              m_edit_area->set_word_selection (word);
            }
        }
    }
}

QString file_editor_tab::get_function_name (void)
{
  QRegExp rxfun1 ("^[\t ]*function[^=]+=([^\\(]+)\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun2 ("^[\t ]*function[\t ]+([^\\(]+)\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun3 ("^[\t ]*function[^=]+=[\t ]*([^\\s]+)[\t ]*$");
  QRegExp rxfun4 ("^[\t ]*function[\t ]+([^\\s]+)[\t ]*$");
  QRegExp rxfun5 ("^[\t ]*classdef[\t ]+([^\\s]+)[\t ]*$");

  QStringList lines = m_edit_area->text ().split ("\n");

  for (int i = 0; i < lines.count (); i++)
    {
      if (rxfun1.indexIn (lines.at (i)) != -1)
        return rxfun1.cap (1).remove (QRegExp ("[ \t]*"));
      else if (rxfun2.indexIn (lines.at (i)) != -1)
        return rxfun2.cap (1).remove (QRegExp ("[ \t]*"));
      else if (rxfun3.indexIn (lines.at (i)) != -1)
        return rxfun3.cap (1).remove (QRegExp ("[ \t]*"));
      else if (rxfun4.indexIn (lines.at (i)) != -1)
        return rxfun4.cap (1).remove (QRegExp ("[ \t]*"));
      else if (rxfun5.indexIn (lines.at (i)) != -1)
        return rxfun5.cap (1).remove (QRegExp ("[ \t]*"));
    }

  return QString ();
}

OCTAVE_END_NAMESPACE(octave)

#endif
