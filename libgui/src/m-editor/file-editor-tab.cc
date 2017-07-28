/*

Copyright (C) 2011-2017 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

/**
 @file A single GUI file tab.
 This interfaces QsciScintilla with the rest of Octave.
 */

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_QSCINTILLA)

#if defined (HAVE_QSCI_QSCILEXEROCTAVE_H)
#  define HAVE_LEXER_OCTAVE 1
#  include <Qsci/qscilexeroctave.h>
#elif defined (HAVE_QSCI_QSCILEXERMATLAB_H)
#  define HAVE_LEXER_MATLAB 1
#  include <Qsci/qscilexermatlab.h>
#endif
#include <Qsci/qscilexercpp.h>
#include <Qsci/qscilexerbash.h>
#include <Qsci/qscilexerperl.h>
#include <Qsci/qscilexerbatch.h>
#include <Qsci/qscilexerdiff.h>
#include <Qsci/qsciprinter.h>
#include <QApplication>
#include <QFileDialog>
#include <QMessageBox>
#include <QTextStream>
#include <QVBoxLayout>
#include <QInputDialog>
#include <QPrintDialog>
#include <QDateTime>
#include <QDesktopServices>
#include <QTextCodec>
#include <QStyle>
#include <QTextBlock>
#include <QLabel>
#include <QCheckBox>
#include <QDialogButtonBox>
#include <QPushButton>

#include "resource-manager.h"
#include "file-editor-tab.h"
#include "file-editor.h"
#include "octave-txt-lexer.h"
#include "marker.h"

#include "file-ops.h"

#include "bp-table.h"
#include "call-stack.h"
#include "defaults.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "oct-map.h"
#include "octave-qt-link.h"
#include "ov-usr-fcn.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "utils.h"
#include "version.h"

bool file_editor_tab::_cancelled = false;

/**
 A file_editor_tab object consists of a text area and three left margins.
 The first holds breakpoints, bookmarks, and the debug program counter.
 The second holds line numbers.
 The third holds "fold" marks, to hide sections of text.
 */
// Make parent null for the file editor tab so that warning
// WindowModal messages don't affect grandparents.
file_editor_tab::file_editor_tab (const QString& directory_arg)
{
  _lexer_apis = nullptr;
  _is_octave_file = true;
  _lines_changed = false;

  _ced = directory_arg;

  _file_name = "";
  _file_system_watcher.setObjectName ("_qt_autotest_force_engine_poller");

  _edit_area = new octave_qscintilla (this);
  _line = 0;
  _col  = 0;

  _bp_lines.clear ();      // start with empty lists of breakpoints
  _bp_conditions.clear ();

  // disable editor drag & drop so parent can handle
  _edit_area->setAcceptDrops (false);

  connect (_edit_area, SIGNAL (cursorPositionChanged (int, int)),
           this, SLOT (handle_cursor_moved (int,int)));

  connect (_edit_area, SIGNAL (SCN_CHARADDED (int)),
           this, SLOT (handle_char_added (int)));

  connect (_edit_area, SIGNAL (SCN_DOUBLECLICK (int, int, int)),
           this, SLOT (handle_double_click (int, int, int)));

  connect (_edit_area, SIGNAL (linesChanged ()),
           this, SLOT (handle_lines_changed ()));

  connect (_edit_area, SIGNAL (context_menu_edit_signal (const QString&)),
           this, SLOT (handle_context_menu_edit (const QString&)));

  // create statusbar for row/col indicator and eol mode
  _status_bar = new QStatusBar (this);

  // row- and col-indicator
  _row_indicator = new QLabel ("", this);
  QFontMetrics fm = _row_indicator->fontMetrics ();
  _row_indicator->setMinimumSize (4.5*fm.averageCharWidth (),0);
  QLabel *row_label = new QLabel (tr ("line:"), this);
  _col_indicator = new QLabel ("", this);
  _col_indicator->setMinimumSize (4*fm.averageCharWidth (),0);
  QLabel *col_label = new QLabel (tr ("col:"), this);
  _status_bar->addWidget (row_label, 0);
  _status_bar->addWidget (_row_indicator, 0);
  _status_bar->addWidget (col_label, 0);
  _status_bar->addWidget (_col_indicator, 0);

  // status bar: encoding
  QLabel *enc_label = new QLabel (tr ("encoding:"), this);
  _enc_indicator = new QLabel ("",this);
  _status_bar->addWidget (enc_label, 0);
  _status_bar->addWidget (_enc_indicator, 0);
  _status_bar->addWidget (new QLabel (" ", this), 0);

  // status bar: eol mode
  QLabel *eol_label = new QLabel (tr ("eol:"), this);
  _eol_indicator = new QLabel ("",this);
  _status_bar->addWidget (eol_label, 0);
  _status_bar->addWidget (_eol_indicator, 0);
  _status_bar->addWidget (new QLabel (" ", this), 0);

  // Leave the find dialog box out of memory until requested.
  _find_dialog = nullptr;
  _find_dialog_is_visible = false;

  // symbols
  _edit_area->setMarginType (1, QsciScintilla::SymbolMargin);
  _edit_area->setMarginSensitivity (1, true);
  _edit_area->markerDefine (QsciScintilla::RightTriangle, marker::bookmark);
  _edit_area->setMarkerBackgroundColor (QColor (0,0,232), marker::bookmark);
  _edit_area->markerDefine (QsciScintilla::Circle, marker::breakpoint);
  _edit_area->setMarkerBackgroundColor (QColor (192,0,0), marker::breakpoint);
  _edit_area->markerDefine (QsciScintilla::Circle, marker::cond_break);
  _edit_area->setMarkerBackgroundColor (QColor (255,127,0), marker::cond_break);
  _edit_area->markerDefine (QsciScintilla::RightArrow, marker::debugger_position);
  _edit_area->setMarkerBackgroundColor (QColor (255,255,0),
                                        marker::debugger_position);
  _edit_area->markerDefine (QsciScintilla::RightArrow,
                            marker::unsure_debugger_position);
  _edit_area->setMarkerBackgroundColor (QColor (192,192,192),
                                        marker::unsure_debugger_position);

  connect (_edit_area, SIGNAL (marginClicked (int, int,
                                              Qt::KeyboardModifiers)),
           this, SLOT (handle_margin_clicked (int, int,
                                              Qt::KeyboardModifiers)));

  connect (_edit_area, SIGNAL (context_menu_break_condition_signal (int)),
           this, SLOT (handle_context_menu_break_condition (int)));

  // line numbers
  _edit_area->setMarginsForegroundColor (QColor (96, 96, 96));
  _edit_area->setMarginsBackgroundColor (QColor (232, 232, 220));
  _edit_area->setMarginType (2, QsciScintilla::TextMargin);

  // other features
  _edit_area->setBraceMatching (QsciScintilla::StrictBraceMatch);
  _edit_area->setAutoIndent (true);
  _edit_area->setIndentationWidth (2);
  _edit_area->setIndentationsUseTabs (false);

  _edit_area->setUtf8 (true);

  // auto completion
  _edit_area->SendScintilla (QsciScintillaBase::SCI_AUTOCSETCANCELATSTART, false);

  QVBoxLayout *edit_area_layout = new QVBoxLayout ();
  edit_area_layout->addWidget (_edit_area);
  edit_area_layout->addWidget (_status_bar);
  edit_area_layout->setMargin (0);
  setLayout (edit_area_layout);

  // connect modified signal
  connect (_edit_area, SIGNAL (modificationChanged (bool)),
           this, SLOT (update_window_title (bool)));

  connect (_edit_area, SIGNAL (copyAvailable (bool)),
           this, SLOT (handle_copy_available (bool)));

  connect (&_file_system_watcher, SIGNAL (fileChanged (const QString&)),
           this, SLOT (file_has_changed (const QString&)));

  QSettings *settings = resource_manager::get_settings ();
  if (settings)
    notice_settings (settings, true);

  setFocusProxy (_edit_area);

  // encoding, not updated with the settings
#if defined (Q_OS_WIN32)
  _encoding = settings->value ("editor/default_encoding","SYSTEM")
                               .toString ();
#else
  _encoding = settings->value ("editor/default_encoding","UTF-8")
                               .toString ();
#endif
  _enc_indicator->setText (_encoding);
  // no changes in encoding yet
  _new_encoding = _encoding;

  // indicators
  _indicator_highlight_all
        = _edit_area->indicatorDefine (QsciScintilla::StraightBoxIndicator);
  if (_indicator_highlight_all == -1)
    _indicator_highlight_all = 1;

  _edit_area->setIndicatorDrawUnder (true, _indicator_highlight_all);
}

file_editor_tab::~file_editor_tab (void)
{
  // Tell all connected markers to self-destruct.
  emit remove_all_breakpoints ();
  emit remove_all_positions ();

  // Destroy items attached to _edit_area.
  QsciLexer *lexer = _edit_area->lexer ();
  if (lexer)
    {
      delete lexer;
      _edit_area->setLexer (nullptr);
    }
  if (_find_dialog)
    {
      delete _find_dialog;
      _find_dialog = nullptr;
    }

  // Destroy _edit_area.
  delete _edit_area;
}

void
file_editor_tab::set_encoding (const QString& new_encoding)
{
  if (new_encoding.isEmpty ())
    return;

  _encoding = new_encoding;
  _enc_indicator->setText (_encoding);
  if (! _edit_area->text ().isEmpty ())
    set_modified (true);
}

void
file_editor_tab::closeEvent (QCloseEvent *e)
{
  _cancelled = false;  // prevent unwanted interaction of previous
                       // exits of octave which were canceled by the user

  if (check_file_modified () == QMessageBox::Cancel)
    {
      // ignore close event if file is not saved and user cancels
      // closing this window
      e->ignore ();
    }
  else
    {
      e->accept ();
      emit tab_remove_request ();
    }
}

void
file_editor_tab::set_current_directory (const QString& dir)
{
  _ced = dir;
}

void
file_editor_tab::handle_context_menu_edit (const QString& word_at_cursor)
{
  // search for a subfunction in actual file (this is done at first because
  // octave finds this function before other with same name in the search path
  QRegExp rxfun1 ("^[\t ]*function[^=]+=[\t ]*"
                  + word_at_cursor + "[\t ]*\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun2 ("^[\t ]*function[\t ]+"
                  + word_at_cursor + "[\t ]*\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun3 ("^[\t ]*function[\t ]+"
                  + word_at_cursor + "[\t ]*$");
  QRegExp rxfun4 ("^[\t ]*function[^=]+=[\t ]*"
                  + word_at_cursor + "[\t ]*$");

  int pos_fct = -1;
  QStringList lines = _edit_area->text ().split ("\n");

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
      _edit_area->setCursorPosition (line, pos_fct);
      _edit_area->SendScintilla (2232, line);     // SCI_ENSUREVISIBLE
                                                  // SCI_VISIBLEFROMDOCLINE
      int vis_line = _edit_area->SendScintilla (2220, line);
      _edit_area->SendScintilla (2613, vis_line); // SCI_SETFIRSTVISIBLELINE
      return;
    }

  emit edit_mfile_request (word_at_cursor, _file_name, _ced, -1);
}

// If "dbstop if ..." selected from context menu, create a conditional
// breakpoint.  The default condition is (a) the existing condition if there
// is already a breakpoint (b) any selected text, or (c) empty
void
file_editor_tab::handle_context_menu_break_condition (int linenr)
{
  // Ensure editor line numbers match Octave core's line numbers.
  // Give users the option to save modifications if necessary.
  if (! unchanged_or_saved ())
    return;

  QString cond;
  bp_info info (_file_name, linenr+1); // Get function name & dir from filename.

  // Search for previous condition.  FIXME: is there a more direct way?
  if (_edit_area->markersAtLine (linenr) & (1 << marker::cond_break))
    {
      emit report_marker_linenr (_bp_lines, _bp_conditions);
      for (int i = 0; i < _bp_lines.length (); i++)
        if (_bp_lines.value (i) == linenr)
          {
            cond = _bp_conditions.value (i);
            break;
          }
      _bp_lines.clear ();
      _bp_conditions.clear ();
    }

  // If text selected by the mouse, default to that instead
  // If both present, use the OR of them, to avoid accidental overwriting
  // FIXME: If both are present, show old condition unselected and
  //        the selection (in edit area) selected (in the dialog).
  if (_edit_area->hasSelectedText ())
    {
      if (cond == "")
        cond = _edit_area->selectedText ();
      else
        cond = "(" + cond + ") || (" + _edit_area->selectedText () + ")";
    }

  bool valid = false;
  std::string prompt = "dbstop if";
  bool ok;
  while (! valid)
    {
      QString new_condition
        = QInputDialog::getText (this, tr ("Breakpoint condition"),
                                 tr (prompt.c_str ()), QLineEdit::Normal, cond,
                                 &ok);
      if (ok)     // If cancel, don't change breakpoint condition.
        {
          try
            {
              // Suppress error messages on the console.
              octave::unwind_protect frame;
              frame.protect_var (buffer_error_messages);
              buffer_error_messages++;

              bp_table::condition_valid (new_condition.toStdString ());
              valid = true;
            }
          catch (const octave::index_exception& e) { }
          catch (const octave::execution_exception& e) { }
          catch (const octave::interrupt_exception&)
            {
              ok = false;
              valid = true;
            }

          // In case we repeat, set new prompt.
          prompt = "ERROR: " + last_error_message () + "\n\ndbstop if";
          cond = new_condition;
        }
      else
        valid = true;
    }

  if (ok)       // If the user didn't cancel, actually set the breakpoint.
    {
      info.condition = cond.toStdString ();

      octave_link::post_event
        (this, &file_editor_tab::add_breakpoint_callback, info);
    }
}

void
file_editor_tab::set_file_name (const QString& fileName)
{
  // update tracked file if we really have a file on disk
  QStringList trackedFiles = _file_system_watcher.files ();
  if (! trackedFiles.isEmpty ())
    _file_system_watcher.removePath (_file_name);
  if (! fileName.isEmpty ())
    _file_system_watcher.addPath (fileName);

  // update lexer and file name variable if file name changes
  if (_file_name != fileName)
    {
      _file_name = fileName;
      update_lexer ();
    }

  // update the file editor with current editing directory
  emit editor_state_changed (_copy_available, _is_octave_file);

  // add the new file to the most-recently-used list
  emit mru_add_file (_file_name, _encoding);
}

// valid_file_name (file): checks whether "file" names a file.
// By default, "file" is empty; then _file_name is checked
bool
file_editor_tab::valid_file_name (const QString& file)
{
  if (file.isEmpty ())
    {
      if (_file_name.isEmpty ())
        return false;
      else
        return true;
    }

  return true;
}

// We cannot create a breakpoint when the file is modified
// because the line number the editor is providing might
// not match what Octave core is interpreting in the
// file on disk.  This function gives the user the option
// to save before creating the breakpoint.
bool
file_editor_tab::unchanged_or_saved (void)
{
  bool retval = true;
  if (_edit_area->isModified ())
    {
      int ans = QMessageBox::question (nullptr, tr ("Octave Editor"),
                  tr ("Cannot add breakpoint to modified file.\n"
                      "Save and add breakpoint, or cancel?"),
                  QMessageBox::Save | QMessageBox::Cancel, QMessageBox::Save);

      if (ans == QMessageBox::Save)
        save_file (_file_name, false);
      else
        retval = false;
    }

  return retval;
}

// Toggle a breakpoint at the editor_linenr or, if this was called by
// a click with CTRL pressed, toggle a bookmark at that point.
void
file_editor_tab::handle_margin_clicked (int margin, int editor_linenr,
                                        Qt::KeyboardModifiers state)
{
  if (margin == 1)
    {
      unsigned int markers_mask = _edit_area->markersAtLine (editor_linenr);

      if (state & Qt::ControlModifier)
        {
          if (markers_mask & (1 << marker::bookmark))
            _edit_area->markerDelete (editor_linenr, marker::bookmark);
          else
            _edit_area->markerAdd (editor_linenr, marker::bookmark);
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


void
file_editor_tab::update_lexer ()
{
  // Create a new lexer
  QsciLexer *lexer = nullptr;

  _is_octave_file = false;

  // Find the required lexer from file extensions
  if (_file_name.endsWith (".m")
      || _file_name.endsWith ("octaverc"))
    {
#if defined (HAVE_LEXER_OCTAVE)
      lexer = new QsciLexerOctave ();
#elif defined (HAVE_LEXER_MATLAB)
      lexer = new QsciLexerMatlab ();
#else
      lexer = new octave_txt_lexer ();
#endif
      _is_octave_file = true;
    }

  if (! lexer)
    {
      if (_file_name.endsWith (".c")
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
      else if (_file_name.endsWith (".sh"))
        {
          lexer = new QsciLexerBash ();
        }
      else if (! valid_file_name ())
        {
          // new, no yet named file: let us assume it is octave
#if defined (HAVE_LEXER_OCTAVE)
          lexer = new QsciLexerOctave ();
          _is_octave_file = true;
#elif defined (HAVE_LEXER_MATLAB)
          lexer = new QsciLexerMatlab ();
          _is_octave_file = true;
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
  QsciLexer *old_lexer = _edit_area->lexer ();

  // If new file, no lexer, or lexer has changed,
  // delete old one and set the newly created as current lexer
  if (! old_lexer || ! valid_file_name ()
      || QString(old_lexer->lexer ()) != QString(lexer->lexer ()))
    {
      // Delete and set new lexer
      if (old_lexer)
        delete old_lexer;
      _edit_area->setLexer (lexer);

      // build information for auto completion (APIs)
      _lexer_apis = new QsciAPIs (lexer);

      // Get the settings for this new lexer
      update_lexer_settings ();
    }
  else
    {
      // Otherwise, delete the newly created lexer and
      // use the old, exisiting one
      if (lexer)
        delete lexer;
    }
}


// Update settings, which are lexer related and have to be updated
// when a) the lexer changes or b) the settings have changed.
void
file_editor_tab::update_lexer_settings ()
{
  QsciLexer *lexer = _edit_area->lexer ();

  QSettings *settings = resource_manager::get_settings ();

  if (_lexer_apis)
    {
      _lexer_apis->cancelPreparation ();  // stop preparing if apis exists

      bool update_apis = false;  // flag, whether update of apis files

      // get path to prepared api info
#if defined (HAVE_QT4)
      QString prep_apis_path
        = QDesktopServices::storageLocation (QDesktopServices::HomeLocation)
          + "/.config/octave/" + QString (OCTAVE_VERSION) + "/qsci/";
#else
      QString prep_apis_path
        = QStandardPaths::writableLocation (QStandardPaths::HomeLocation)
          + "/.config/octave/" + QString (OCTAVE_VERSION) + "/qsci/";
#endif

      // get settings which infos are used for octave
      bool octave_builtins = settings->value (
                  "editor/codeCompletion_octave_builtins", true).toBool ();
      bool octave_functions = settings->value (
                  "editor/codeCompletion_octave_functions", true).toBool ();

      if (_is_octave_file)
        {
          // Keywords and Builtins do not change, these informations can be
          // stored in prepared form in a file. Information on function are
          // changing frequently, then if functions should also be auto-
          // completed, the date of any existing file is checked.

          // Keywords are always used
          _prep_apis_file = prep_apis_path + lexer->lexer () + "_k";

          // Buitlins are only used if the user settings say so
          if (octave_builtins)
            _prep_apis_file += 'b';

          if (octave_functions)
            _prep_apis_file += 'f';

         _prep_apis_file += ".pap"; // final name of apis file

          // check whether the APIs info needs to be prepared and saved
          QFileInfo apis_file = QFileInfo (_prep_apis_file);
          // flag whether apis file needs update
          update_apis = ! apis_file.exists ();

          if (octave_functions)
            {
              // Functions may change frequently. Update the apis data
              // if the file is older than a few minutes preventing from
              // re-preparing data when the user opens several files.
              QDateTime apis_time = apis_file.lastModified ();
              if (QDateTime::currentDateTime () > apis_time.addSecs (180))
                update_apis = true;
            }

          }
        else  // no octave file, just add extension
          {

            _prep_apis_file = prep_apis_path + lexer->lexer () + ".pap";

          }

      if (update_apis || ! _lexer_apis->loadPrepared (_prep_apis_file))
        {
          // either we have decided to update the apis file or
          // no prepared info was loaded, prepare and save if possible

          // create raw apis info
          QString keyword;
          QStringList keyword_list;
          int i,j;

          if (_is_octave_file)
            {
              // octave: get keywords from internal informations depending on
              //         user preferences

              // keywords are always used
              add_octave_apis (Fiskeyword ());            // add new entries

              octave::interpreter& interp
                = octave::__get_interpreter__ (
                                    "file_editor_tab::update_lexer_settings");

              if (octave_builtins)
                add_octave_apis (F__builtins__ (interp));       // add new entries

              if (octave_functions)
                add_octave_apis (F__list_functions__ (interp)); // add new entries

            }
          else
            {

              _prep_apis_file = prep_apis_path + lexer->lexer () + ".pap";

              for (i=1; i<=3; i++) // test the first 5 keyword sets
                {
                  keyword = QString (lexer->keywords (i));           // get list
                  keyword_list = keyword.split (QRegExp (R"(\s+)")); // split
                  for (j = 0; j < keyword_list.size (); j++)         // add to API
                    _lexer_apis->add (keyword_list.at (j));
                }
            }

          // disconnect slot for saving prepared info if already connected
          disconnect (_lexer_apis, SIGNAL (apiPreparationFinished ()), nullptr, nullptr);
          // check whether path for prepared info exists or can be created
          if (QDir ("/").mkpath (prep_apis_path))
            {
              // path exists, apis info can be saved there
              connect (_lexer_apis, SIGNAL (apiPreparationFinished ()),
                       this, SLOT (save_apis_info ()));
            }

          _lexer_apis->prepare ();  // prepare apis info

        }
    }

  lexer->readSettings (*settings);

  _edit_area->setCaretForegroundColor (lexer->color (0));
  _edit_area->setIndentationGuidesForegroundColor (lexer->color (0));

  // set some colors depending on selected background color of the lexer
  QColor bg = lexer->paper (0);
  QColor fg = lexer->color (0);

  int bh, bs, bv, fh, fs, fv, h, s, v;
  bg.getHsv (&bh,&bs,&bv);
  fg.getHsv (&fh,&fs,&fv);

  // margin colors
  h = bh;
  s = bs/2;
  v = bv + (fv - bv)/5;

  bg.setHsv (h,s,v);
  _edit_area->setEdgeColor (bg);

  v = bv + (fv - bv)/8;
  bg.setHsv (h,s,v);
  v = bv + (fv - bv)/4;
  fg.setHsv (h,s,v);

  _edit_area->setMarkerForegroundColor (lexer->color (0));
  _edit_area->setMarginsForegroundColor (lexer->color (0));
  _edit_area->setMarginsBackgroundColor (bg);
  _edit_area->setFoldMarginColors (bg,fg);

  // color indicator for highlighting all occurrences:
  // applications highlight color with more transparency
  QColor hg = QApplication::palette ().color (QPalette::Highlight);
  hg.setAlphaF (0.25);
  _edit_area->setIndicatorForegroundColor (hg, _indicator_highlight_all);
  _edit_area->setIndicatorOutlineColor (hg, _indicator_highlight_all);

  // fix line number width with respect to the font size of the lexer and
  // set the line numbers font depending on the lexers font
  if (settings->value ("editor/showLineNumbers", true).toBool ())
    {
      // Line numbers width
      auto_margin_width ();

      // Line numbers font
      QFont line_numbers_font = lexer->defaultFont ();
      int font_size = line_numbers_font.pointSize ();
      font_size = font_size
                  + settings->value ("editor/line_numbers_size", 0).toInt ();
      if (font_size < 4)
        font_size = 4;
      line_numbers_font.setPointSize (font_size);

      _edit_area->setMarginsFont (line_numbers_font);
    }
  else
    _edit_area->setMarginWidth (2,0);
}


// function for adding entries to the octave lexer's APIs
void
file_editor_tab::add_octave_apis (octave_value_list key_ovl)
{
  octave_value keys = key_ovl(0);
  Cell key_list = keys.cell_value ();

  for (int idx = 0; idx < key_list.numel (); idx++)
    _lexer_apis->add (QString (key_list.elem (idx).string_value ().data ()));
}

void
file_editor_tab::save_apis_info ()
{
  _lexer_apis->savePrepared (_prep_apis_file);
}

// slot for fetab_set_focus: sets the focus to the current edit area
void
file_editor_tab::set_focus (const QWidget *ID)
{
  if (ID != this)
    return;
  _edit_area->setFocus ();
}

void
file_editor_tab::context_help (const QWidget *ID, bool doc)
{
  if (ID != this)
    return;

  _edit_area->context_help_doc (doc);
}

void
file_editor_tab::context_edit (const QWidget *ID)
{
  if (ID != this)
    return;

  _edit_area->context_edit ();
}

void
file_editor_tab::check_modified_file (void)
{
  if (_cancelled)
    return;

  if (check_file_modified () == QMessageBox::Cancel)
    _cancelled = true;
}

void
file_editor_tab::save_file (const QWidget *ID)
{
  if (ID != this)
    return;

  save_file (_file_name);
}

void
file_editor_tab::save_file (const QWidget *ID, const QString& fileName,
                            bool remove_on_success)
{
  if (ID != this)
    return;

  save_file (fileName, remove_on_success);
}

void
file_editor_tab::save_file_as (const QWidget *ID)
{
  if (ID != this)
    return;

  save_file_as ();
}

void
file_editor_tab::print_file (const QWidget *ID)
{
  if (ID != this)
    return;

  QsciPrinter *printer = new QsciPrinter (QPrinter::HighResolution);

  QPrintDialog printDlg (printer, this);

  if (printDlg.exec () == QDialog::Accepted)
    printer->printRange (_edit_area);

  delete printer;
}

void
file_editor_tab::run_file (const QWidget *ID)
{
  if (ID != this)
    return;

  if (_edit_area->isModified () | ! valid_file_name ())
    {
      save_file (_file_name);  // save file dialog
      if (! valid_file_name ())
        return;   // still invalid filename: "save as" was cancelled
    }

  QFileInfo info (_file_name);
  emit run_file_signal (info);
}

void
file_editor_tab::context_run (const QWidget *ID)
{
  if (ID != this)
    return;

  _edit_area->context_run ();
}

void
file_editor_tab::toggle_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  _edit_area->getCursorPosition (&line, &cur);

  if (_edit_area->markersAtLine (line) & (1 << marker::bookmark))
    _edit_area->markerDelete (line, marker::bookmark);
  else
    _edit_area->markerAdd (line, marker::bookmark);
}

// Move the text cursor to the closest bookmark
// after the current line.
void
file_editor_tab::next_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  _edit_area->getCursorPosition (&line, &cur);

  line++; // Find bookmark strictly after the current line.

  int nextline = _edit_area->markerFindNext (line, (1 << marker::bookmark));

  // Wrap.
  if (nextline == -1)
    nextline = _edit_area->markerFindNext (1, (1 << marker::bookmark));

  _edit_area->setCursorPosition (nextline, 0);
}

// Move the text cursor to the closest bookmark
// before the current line.
void
file_editor_tab::previous_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  _edit_area->getCursorPosition (&line, &cur);

  line--; // Find bookmark strictly before the current line.

  int prevline = _edit_area->markerFindPrevious (line, (1 << marker::bookmark));

  // Wrap.  Should use the last line of the file, not 1<<15
  if (prevline == -1)
    prevline = _edit_area->markerFindPrevious (_edit_area->lines (),
                                               (1 << marker::bookmark));

  _edit_area->setCursorPosition (prevline, 0);
}

void
file_editor_tab::remove_bookmark (const QWidget *ID)
{
  if (ID != this)
    return;

  _edit_area->markerDeleteAll (marker::bookmark);
}

void
file_editor_tab::add_breakpoint_callback (const bp_info& info)
{
  bp_table::intmap line_info;
  line_info[0] = info.line;

  if (octave_qt_link::file_in_path (info.file, info.dir))
    bp_table::add_breakpoint (info.function_name, line_info, info.condition);
}

void
file_editor_tab::remove_breakpoint_callback (const bp_info& info)
{
  bp_table::intmap line_info;
  line_info[0] = info.line;

  if (octave_qt_link::file_in_path (info.file, info.dir))
    bp_table::remove_breakpoint (info.function_name, line_info);
}

void
file_editor_tab::remove_all_breakpoints_callback (const bp_info& info)
{
  if (octave_qt_link::file_in_path (info.file, info.dir))
    bp_table::remove_all_breakpoints_in_file (info.function_name, true);
}

file_editor_tab::bp_info::bp_info (const QString& fname, int l,
                                   const QString& cond)
  : line (l), file (fname.toStdString ()), condition (cond.toStdString ())
{
  QFileInfo file_info (fname);

  QString q_dir = file_info.absolutePath ();
  QString q_function_name = file_info.fileName ();

  // We have to cut off the suffix, because octave appends it.
  q_function_name.chop (file_info.suffix ().length () + 1);

  dir = q_dir.toStdString ();
  function_name = q_function_name.toStdString ();

  // Is the last component of DIR @foo?  If so, strip it and prepend it
  // to the name of the function.

  size_t pos = dir.rfind (octave::sys::file_ops::dir_sep_chars ());

  if (pos != std::string::npos && pos < dir.length () - 1)
    {
      if (dir[pos+1] == '@')
        {
          function_name = octave::sys::file_ops::concat (dir.substr (pos+1), function_name);

          dir = dir.substr (0, pos);
        }
    }
}

void
file_editor_tab::handle_request_add_breakpoint (int line,
                                                const QString& condition)
{
  bp_info info (_file_name, line, condition);

  octave_link::post_event
    (this, &file_editor_tab::add_breakpoint_callback, info);
}

void
file_editor_tab::handle_request_remove_breakpoint (int line)
{
  bp_info info (_file_name, line);

  octave_link::post_event
    (this, &file_editor_tab::remove_breakpoint_callback, info);
}

void
file_editor_tab::toggle_breakpoint (const QWidget *ID)
{
  if (ID != this)
    return;

  int editor_linenr, cur;
  _edit_area->getCursorPosition (&editor_linenr, &cur);

  if (_edit_area->markersAtLine (editor_linenr) & (1 << marker::breakpoint))
    request_remove_breakpoint_via_editor_linenr (editor_linenr);
  else
    {
      if (unchanged_or_saved ())
        handle_request_add_breakpoint (editor_linenr + 1, "");
    }
}

// Move the text cursor to the closest breakpoint (conditional or unconditional)
// after the current line.
void
file_editor_tab::next_breakpoint (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur;
  _edit_area->getCursorPosition (&line, &cur);

  line++; // Find breakpoint strictly after the current line.

  int nextline = _edit_area->markerFindNext (line, (1 << marker::breakpoint));
  int nextcond = _edit_area->markerFindNext (line, (1 << marker::cond_break));

  // Check if the next conditional breakpoint is before next unconditional one.
  if (nextcond != -1 && (nextcond < nextline || nextline == -1))
    nextline = nextcond;

  _edit_area->setCursorPosition (nextline, 0);
}

// Move the text cursor to the closest breakpoint (conditional or unconditional)
// before the current line.
void
file_editor_tab::previous_breakpoint (const QWidget *ID)
{
  if (ID != this)
    return;

  int line, cur, prevline, prevcond;
  _edit_area->getCursorPosition (&line, &cur);

  line--; // Find breakpoint strictly before the current line.

  prevline = _edit_area->markerFindPrevious (line, (1 << marker::breakpoint));
  prevcond = _edit_area->markerFindPrevious (line, (1 << marker::cond_break));

  // Check if the prev conditional breakpoint is closer than the unconditional.
  if (prevcond != -1 && prevcond > prevline)
    prevline = prevcond;

  _edit_area->setCursorPosition (prevline, 0);
}

void
file_editor_tab::remove_all_breakpoints (const QWidget *ID)
{
  if (ID != this)
    return;

  bp_info info (_file_name);

  octave_link::post_event
    (this, &file_editor_tab::remove_all_breakpoints_callback, info);
}

void
file_editor_tab::scintilla_command (const QWidget *ID, unsigned int sci_msg)
{
  if (ID != this)
    return;

  _edit_area->SendScintilla (sci_msg);
}

void
file_editor_tab::comment_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_comment_selected_text (true);
}

void
file_editor_tab::uncomment_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_comment_selected_text (false);
}

void
file_editor_tab::indent_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_indent_selected_text (true);
}

void
file_editor_tab::unindent_selected_text (const QWidget *ID)
{
  if (ID != this)
    return;

  do_indent_selected_text (false);
}

void
file_editor_tab::convert_eol (const QWidget *ID, QsciScintilla::EolMode eol_mode)
{
  if (ID != this)
    return;

  _edit_area->convertEols (eol_mode);
  _edit_area->setEolMode (eol_mode);
  update_eol_indicator ();
}

void
file_editor_tab::zoom_in (const QWidget *ID)
{
  if (ID != this)
    return;

  _edit_area->zoomIn (1);
  auto_margin_width ();
}

void
file_editor_tab::zoom_out (const QWidget *ID)
{
  if (ID != this)
    return;

  _edit_area->zoomOut (1);
  auto_margin_width ();
}

void
file_editor_tab::zoom_normal (const QWidget *ID)
{
  if (ID != this)
    return;

  _edit_area->zoomTo (0);
  auto_margin_width ();
}

void
file_editor_tab::handle_find_dialog_finished (int)
{
  // Find dialog is going to hide.  Save location of window for
  // when it is reshown.
  _find_dialog_geometry = _find_dialog->geometry ();
  _find_dialog_is_visible = false;
}

void
file_editor_tab::find (const QWidget *ID, QList<QAction *> fetab_actions)
{
  if (ID != this)
    return;

  // The find_dialog feature doesn't need a slot for return info.
  // Rather than Qt::DeleteOnClose, let the find feature hang about
  // in case it contains useful information like previous searches
  // and so on.  Perhaps one find dialog for the whole editor is
  // better, but individual find dialogs has the nice feature of
  // retaining position per file editor tabs, which can be undocked.

  if (! _find_dialog)
    {
      _find_dialog = new find_dialog (_edit_area,
                                      fetab_actions.mid (0,2),
                                      qobject_cast<QWidget *> (sender ()));
      connect (_find_dialog, SIGNAL (finished (int)),
               this, SLOT (handle_find_dialog_finished (int)));

      connect (this, SIGNAL (request_find_next ()),
               _find_dialog, SLOT (find_next ()));

      connect (this, SIGNAL (request_find_previous ()),
               _find_dialog, SLOT (find_prev ()));

      _find_dialog->setWindowModality (Qt::NonModal);
      _find_dialog_geometry = _find_dialog->geometry ();
    }
  else if (! _find_dialog->isVisible ())
    {
      _find_dialog->setGeometry (_find_dialog_geometry);
      QPoint p = _find_dialog->pos ();
      _find_dialog->move (p.x ()+10, p.y ()+10);
    }

  _find_dialog->show ();
  _find_dialog_is_visible = true;
  _find_dialog->activateWindow ();
  _find_dialog->init_search_text ();

}

void
file_editor_tab::find_next (const QWidget *ID)
{
  if (ID == this)
    emit request_find_next ();
}

void
file_editor_tab::find_previous (const QWidget *ID)
{
  if (ID == this)
    emit request_find_previous ();
}

void
file_editor_tab::goto_line (const QWidget *ID, int line)
{
  if (ID != this)
    return;

  if (line <= 0)  // ask for desired line
    {
      bool ok = false;
      int index;
      _edit_area->getCursorPosition (&line, &index);
      line = QInputDialog::getInt (_edit_area, tr ("Goto line"),
                                   tr ("Line number"), line+1, 1,
                                   _edit_area->lines (), 1, &ok);
      if (ok)
        _edit_area->setCursorPosition (line-1, 0);
    }
  else  // go to given line without dialog
    _edit_area->setCursorPosition (line-1, 0);

  center_current_line (false);  // only center line if at top or bottom
}

void
file_editor_tab::move_match_brace (const QWidget *ID, bool select)
{
  if (ID != this)
    return;

  if (select)
    _edit_area->selectToMatchingBrace ();
  else
    _edit_area->moveToMatchingBrace ();
}

void
file_editor_tab::show_auto_completion (const QWidget *ID)
{
  if (ID != this)
    return;

  QsciScintilla::AutoCompletionSource s = _edit_area->autoCompletionSource ();
  switch (s)
    {
      case QsciScintilla::AcsAll:
        _edit_area->autoCompleteFromAll ();
        break;

      case QsciScintilla::AcsAPIs:
        _edit_area->autoCompleteFromAPIs ();
        break;

      case QsciScintilla::AcsDocument:
        _edit_area->autoCompleteFromDocument ();
        break;

      case QsciScintilla::AcsNone:
        break;
    }
}

void
file_editor_tab::do_indent_selected_text (bool indent)
{
  // FIXME:
  _edit_area->beginUndoAction ();

  if (_edit_area->hasSelectedText ())
    {
      int lineFrom, lineTo, colFrom, colTo;
      _edit_area->getSelection (&lineFrom, &colFrom, &lineTo, &colTo);

      if (colTo == 0)  // the beginning of last line is not selected
        lineTo--;        // stop at line above

      for (int i = lineFrom; i <= lineTo; i++)
        {
          if (indent)
            _edit_area->indent (i);
          else
            _edit_area->unindent (i);
        }
      //set selection on (un)indented section
      _edit_area->setSelection (lineFrom, 0, lineTo,
                                _edit_area->text (lineTo).length ());
    }
  else
    {
      int cpline, col;
      _edit_area->getCursorPosition (&cpline, &col);
      if (indent)
        _edit_area->indent (cpline);
      else
        _edit_area->unindent (cpline);
    }

  _edit_area->endUndoAction ();
}

void
file_editor_tab::do_comment_selected_text (bool comment)
{
  QString comment_str = _edit_area->comment_string ();
  _edit_area->beginUndoAction ();

  if (_edit_area->hasSelectedText ())
    {
      int lineFrom, lineTo, colFrom, colTo;
      _edit_area->getSelection (&lineFrom, &colFrom, &lineTo, &colTo);

      if (colTo == 0)  // the beginning of last line is not selected
        lineTo--;        // stop at line above

      for (int i = lineFrom; i <= lineTo; i++)
        {
          if (comment)
            _edit_area->insertAt (comment_str, i, 0);
          else
            {
              QString line (_edit_area->text (i));
              if (line.startsWith (comment_str))
                {
                  _edit_area->setSelection (i, 0, i, comment_str.length ());
                  _edit_area->removeSelectedText ();
                }
            }
        }
      //set selection on (un)commented section
      _edit_area->setSelection (lineFrom, 0, lineTo,
                                _edit_area->text (lineTo).length ());
    }
  else
    {
      int cpline, col;
      _edit_area->getCursorPosition (&cpline, &col);
      if (comment)
        _edit_area->insertAt (comment_str, cpline, 0);
      else
        {
          QString line (_edit_area->text (cpline));
          if (line.startsWith (comment_str))
            {
              _edit_area->setSelection (cpline, 0, cpline, comment_str.length ());
              _edit_area->removeSelectedText ();
            }
        }
    }
  _edit_area->endUndoAction ();
}

void
file_editor_tab::update_window_title (bool modified)
{
  QString title ("");
  QString tooltip ("");

  if (! valid_file_name ())
    title = tr ("<unnamed>");
  else
    {
      if (_long_title)
        title = _file_name;
      else
        {
          QFileInfo file (_file_name);
          title = file.fileName ();
          tooltip = _file_name;
        }
    }

  if (modified)
    emit file_name_changed (title.prepend ("* "), tooltip);
  else
    emit file_name_changed (title, tooltip);
}

void
file_editor_tab::handle_copy_available (bool enableCopy)
{
  _copy_available = enableCopy;
  emit editor_state_changed (_copy_available, _is_octave_file);
}

// show_dialog: shows a modal or non modal dialog depending on input arg
void
file_editor_tab::show_dialog (QDialog *dlg, bool modal)
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

int
file_editor_tab::check_file_modified ()
{
  int decision = QMessageBox::Yes;
  if (_edit_area->isModified ())
    {
      // File is modified but not saved, ask user what to do.  The file
      // editor tab can't be made parent because it may be deleted depending
      // upon the response.  Instead, change the _edit_area to read only.
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
          file = _file_name;
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
      _edit_area->setReadOnly (true);
      connect (msgBox, SIGNAL (finished (int)),
               this, SLOT (handle_file_modified_answer (int)));

      show_dialog (msgBox, true);

      if (_cancelled)
        return QMessageBox::Cancel;
      else
        return decision;
    }
  else
    {
      // Nothing was modified.  Leave tab present in case user
      // decides to cancel some point further along.
    }

  return decision;
}

void
file_editor_tab::handle_file_modified_answer (int decision)
{
  if (decision == QMessageBox::Save)
    {
      // Save file, but do not remove from editor.
      save_file (_file_name, false, false);
    }
  else if (decision == QMessageBox::Discard)
    {
      // User doesn't want to save, leave tab and remove subsequently.
    }
  else
    {
      // User canceled, allow editing again.
      _edit_area->setReadOnly (false);
      _cancelled = true;
    }
}

void
file_editor_tab::set_modified (bool modified)
{
  _edit_area->setModified (modified);
}

void
file_editor_tab::recover_from_exit ()
{
  // reset the possibly still existing read only state
  _edit_area->setReadOnly (false);

  // if we are in this slot and the list of breakpoint is not empty,
  // then this tab was saved during an exit of the applications (not
  // restoring the breakpoints and not emptying the list) and the user
  // canceled this closing late on.
  check_restore_breakpoints ();
}

void
file_editor_tab::check_restore_breakpoints ()
{
  if (! _bp_lines.isEmpty ())
    {
      // At least one breakpoint is present.
      // Get rid of breakpoints at old (now possibly invalid) linenumbers
      remove_all_breakpoints (this);

      // and set breakpoints at the new linenumbers
      for (int i = 0; i < _bp_lines.length (); i++)
        handle_request_add_breakpoint (_bp_lines.value (i) + 1,
                                       _bp_conditions.value (i));

     // Keep the list of breakpoints empty, except after explicit requests.
      _bp_lines.clear ();
      _bp_conditions.clear ();
    }
}

QString
file_editor_tab::load_file (const QString& fileName)
{
  // get the absolute path
  QFileInfo file_info = QFileInfo (fileName);
  QString file_to_load;
  if (file_info.exists ())
    file_to_load = file_info.canonicalFilePath ();
  else
    file_to_load = fileName;
  QFile file (file_to_load);
  if (! file.open (QFile::ReadOnly))
    return file.errorString ();

  // read the file
  QTextStream in (&file);
  // set the desired codec
  QTextCodec *codec = QTextCodec::codecForName (_encoding.toLatin1 ());
  in.setCodec (codec);

  QApplication::setOverrideCursor (Qt::WaitCursor);
  _edit_area->setText (in.readAll ());
  _edit_area->setEolMode (detect_eol_mode ());
  QApplication::restoreOverrideCursor ();

  _copy_available = false;     // no selection yet available
  set_file_name (file_to_load);
  update_window_title (false); // window title (no modification)
  _edit_area->setModified (false); // loaded file is not modified yet

  update_eol_indicator ();

  // FIXME: (BREAKPOINTS) At this point it would be nice to put any set
  // breakpoints on the margin.  In order to do this, somehow the
  // "dbstatus" command needs to be accessed.  All it would require is a
  // routine that does "res = feval ("dbstatus") and signals that result
  // to some slot.
  //
  // See patch #8016 for a general way to get Octave results from
  // commands processed in the background.

/*
  connect (octave_link, SIGNAL (fileSelected (QObject *, const QString&, const octave_value_list&)),
           this, SLOT (handle_feval_result (QObject *, const QString&, const octave_value_list&)));
  connect (this, SIGNAL (evaluate_octave_command (const QString&)),
           octave_link, SLOT (queue_octave_command (const QString&)));

  emit evaluate_octave_command ("dbstatus");
*/

  return QString ();
}

QsciScintilla::EolMode
file_editor_tab::detect_eol_mode ()
{
  QByteArray text = _edit_area->text ().toLatin1 ();

  QByteArray eol_lf = QByteArray (1,0x0a);
  QByteArray eol_cr = QByteArray (1,0x0d);
  QByteArray eol_crlf = eol_cr;
  eol_crlf.append (eol_lf);

  int count_crlf = text.count (eol_crlf);
  int count_lf = text.count (eol_lf) - count_crlf;  // isolated lf
  int count_cr = text.count (eol_cr) - count_crlf;  // isolated cr;

  // get default from OS or from settings
#if defined (Q_OS_WIN32)
  int os_eol_mode = QsciScintilla::EolWindows;
#elif defined (Q_OS_MAC)
  int os_eol_mode = QsciScintilla::EolMac;
#else
  int os_eol_mode = QsciScintilla::EolUnix;
#endif
  QSettings *settings = resource_manager::get_settings ();
  QsciScintilla::EolMode eol_mode = static_cast<QsciScintilla::EolMode> (
        settings->value ("editor/default_eol_mode",os_eol_mode).toInt ());

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
      count_max = count_cr;
    }

  return eol_mode;
}

void
file_editor_tab::update_eol_indicator ()
{
  switch (_edit_area->eolMode ())
    {
      case QsciScintilla::EolWindows:
        _eol_indicator->setText ("CRLF");
        break;
      case QsciScintilla::EolMac:
        _eol_indicator->setText ("CR");
        break;
      case QsciScintilla::EolUnix:
        _eol_indicator->setText ("LF");
        break;
    }
}

// FIXME: See patch #8016 for a general way to get Octave results from
// commands processed in the background, e.g., dbstatus.
void
file_editor_tab::handle_octave_result (QObject *requester, QString& command,
                                       octave_value_list&)
{
  // Check if this object initiated the command.
  if (requester == this)
    {
      if (command == "dbstatus")
        {
          // Should be installing breakpoints in this file
/*
octave:1> result = dbstatus
result =

  0x1 struct array containing the fields:

    name
    file
    line
*/
          // Check for results that match "file".
        }
    }
}

void
file_editor_tab::new_file (const QString& commands)
{
  update_window_title (false); // window title (no modification)

  QSettings *settings = resource_manager::get_settings ();

  // set the eol mode from the settings or depending on the OS if the entry is
  // missing in the settings
#if defined (Q_OS_WIN32)
  int eol_mode = QsciScintilla::EolWindows;
#elif defined (Q_OS_MAC)
  int eol_mode = QsciScintilla::EolMac;
#else
  int eol_mode = QsciScintilla::EolUnix;
#endif
  _edit_area->setEolMode (
    static_cast<QsciScintilla::EolMode> (
      settings->value ("editor/default_eol_mode",eol_mode).toInt ()));

  update_eol_indicator ();

  update_lexer ();

  _edit_area->setText (commands);
  _edit_area->setModified (false); // new file is not modified yet
}

// Force reloading of a file after it is saved.
// This is needed to get the right line numbers for breakpoints (bug #46632).
bool
file_editor_tab::exit_debug_and_clear (const QString& full_name_q,
                                       const QString& base_name_q)
{
  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("file_editor_tab::exit_debug_and_clear");

  std::string base_name = base_name_q.toStdString ();
  octave_value sym;
  try
    {
      sym = symtab.find (base_name);
    }
  catch (const octave::execution_exception& e)
    {
      // Ignore syntax error.
      // It was in the old file on disk; the user may have fixed it already.
    }

  // Return early if this file is not loaded in the symbol table
  if (! sym.is_defined () || ! sym.is_user_code ())
    return true;

  octave_user_code *fcn = sym.user_code_value ();

  std::string full_name = full_name_q.toStdString ();
  if (octave::sys::canonicalize_file_name (full_name.c_str ())
      != octave::sys::canonicalize_file_name (fcn->fcn_file_name ().c_str ()))
    return true;

  // If this file is loaded, check that we aren't currently running it
  bool retval = true;
  octave_idx_type curr_frame = -1;
  size_t nskip = 0;
  octave::call_stack& cs
    = octave::__get_call_stack__ ("file_editor_tab::exit_debug_and_clear");
  octave_map stk = cs.backtrace (nskip, curr_frame, false);
  Cell names = stk.contents ("name");
  for (octave_idx_type i = names.numel () - 1; i >= 0; i--)
    {
      if (names(i).string_value () == base_name)
        {
          int ans = QMessageBox::question (nullptr, tr ("Debug or Save"),
             tr ("This file is currently being executed.\n"
                          "Quit debugging and save?"),
              QMessageBox::Save | QMessageBox::Cancel);

          if (ans == QMessageBox::Save)
            {
              emit execute_command_in_terminal_signal ("dbquit");
              // Wait until dbquit has actually occurred
              while (names.numel () > i)
                {
                  octave_sleep (0.01);
                  stk = cs.backtrace (nskip, curr_frame, false);
                  names = stk.contents ("name");
                }
            }
          else
            retval = false;
          break;
        }
    }

  // If we aren't currently running it, or have quit above, force a reload.
  if (retval == true)
    symtab.clear_user_function (base_name);

  return retval;
}

void
file_editor_tab::save_file (const QString& saveFileName,
                            bool remove_on_success, bool restore_breakpoints)
{
  // If it is a new file with no name, signal that saveFileAs
  // should be performed.
  if (! valid_file_name (saveFileName))
    {
      save_file_as (remove_on_success);
      return;
    }

  // Get a list of breakpoint line numbers, before  exit_debug_and_clear().
  emit report_marker_linenr (_bp_lines, _bp_conditions);

  // get the absolute path (if existing)
  QFileInfo file_info = QFileInfo (saveFileName);
  QString file_to_save;
  if (file_info.exists ())
    {
      file_to_save = file_info.canonicalFilePath ();
      // Force reparse of this function next time it is used (bug #46632)
      if ((Fisdebugmode ())(0).is_true ()
          && ! exit_debug_and_clear (file_to_save, file_info.baseName ()))
        return;
    }
  else
    file_to_save = saveFileName;
  QFile file (file_to_save);

  // stop watching file
  QStringList trackedFiles = _file_system_watcher.files ();
  if (trackedFiles.contains (file_to_save))
    _file_system_watcher.removePath (file_to_save);

  // open the file for writing
  if (! file.open (QIODevice::WriteOnly))
    {
      // Unsuccessful, begin watching file again if it was being
      // watched previously.
      if (trackedFiles.contains (file_to_save))
        _file_system_watcher.addPath (file_to_save);

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

  _encoding = _new_encoding;    // consider a possible new encoding

  // set the desired codec (if suitable for contents)
  QTextCodec *codec = QTextCodec::codecForName (_encoding.toLatin1 ());

  if (check_valid_codec (codec))
    {
      save_file_as (remove_on_success);
      return;
    }

  // write the file
  QTextStream out (&file);
  out.setCodec (codec);

  QApplication::setOverrideCursor (Qt::WaitCursor);
  out << _edit_area->text ();
  out.flush ();
  QApplication::restoreOverrideCursor ();
  file.flush ();
  file.close ();

  // file exists now
  file_info = QFileInfo (file);
  file_to_save = file_info.canonicalFilePath ();

  // save filename after closing file as set_file_name starts watching again
  set_file_name (file_to_save);   // make absolute

  // set the window title to actual filename (not modified)
  update_window_title (false);

  // files is save -> not modified, update encoding in statusbar
  _edit_area->setModified (false);
  _enc_indicator->setText (_encoding);

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

void
file_editor_tab::save_file_as (bool remove_on_success)
{
  // Simply put up the file chooser dialog box with a slot connection
  // then return control to the system waiting for a file selection.

  // reset _new_encoding
  _new_encoding = _encoding;

  // If the tab is removed in response to a QFileDialog signal, the tab
  // can't be a parent.
  QFileDialog *fileDialog;
  if (remove_on_success)
    {
      // If tab is closed, "this" cannot be parent in which case modality
      // has no effect.  Disable editing instead.
      _edit_area->setReadOnly (true);
      fileDialog = new QFileDialog ();
    }
  else
    fileDialog = new QFileDialog (this);

  // Giving trouble under KDE (problem is related to Qt signal handling on unix,
  // see https://bugs.kde.org/show_bug.cgi?id=260719 ,
  // it had/has no effect on Windows, though)
  fileDialog->setOption (QFileDialog::DontUseNativeDialog, true);

  // define a new grid layout with the extra elements
  QGridLayout *extra = new QGridLayout (fileDialog);
  QFrame *separator = new QFrame (fileDialog);
  separator->setFrameShape (QFrame::HLine);   // horizontal line as separator
  separator->setFrameStyle (QFrame::Sunken);

  // combo box for choosing new line ending chars
  QLabel *label_eol = new QLabel (tr ("Line Endings:"));
  QComboBox *combo_eol = new QComboBox ();
  combo_eol->addItem ("Windows (CRLF)");  // ensure the same order as in
  combo_eol->addItem ("Mac (CR)");        // the settings dialog
  combo_eol->addItem ("Unix (LF)");
  _save_as_desired_eol = _edit_area->eolMode ();      // init with current eol
  combo_eol->setCurrentIndex (_save_as_desired_eol);

  // combo box for encoding
  QLabel *label_enc = new QLabel (tr ("File Encoding:"));
  QComboBox *combo_enc = new QComboBox ();
  resource_manager::combo_encoding (combo_enc, _encoding);

  // track changes in the combo boxes
  connect (combo_eol, SIGNAL (currentIndexChanged (int)),
           this, SLOT (handle_combo_eol_current_index (int)));
  connect (combo_enc, SIGNAL (currentIndexChanged (QString)),
           this, SLOT (handle_combo_enc_current_index (QString)));

  // build the extra grid layout
  extra->addWidget (separator,0,0,1,6);
  extra->addWidget (label_eol,1,0);
  extra->addWidget (combo_eol,1,1);
  extra->addItem   (new QSpacerItem (1,20,QSizePolicy::Fixed,
                                          QSizePolicy::Fixed), 1,2);
  extra->addWidget (label_enc,1,3);
  extra->addWidget (combo_enc,1,4);
  extra->addItem   (new QSpacerItem (1,20,QSizePolicy::Expanding,
                                          QSizePolicy::Fixed), 1,5);

  // and add the extra grid layout to the dialog's layout
  QGridLayout *dialog_layout = dynamic_cast<QGridLayout *> (fileDialog->layout ());
  dialog_layout->addLayout (extra,dialog_layout->rowCount (),0,
                                  1,dialog_layout->columnCount ());

  // add the possible filters and the default suffix
  QStringList filters;
  filters << tr ("Octave Files (*.m)")
          << tr ("All Files (*)");
  fileDialog->setNameFilters (filters);
  fileDialog->setDefaultSuffix ("m");

  if (valid_file_name ())
    {
      fileDialog->selectFile (_file_name);
      QFileInfo file_info (_file_name);
      if (file_info.suffix () != "m")
        { // it is not an octave file
          fileDialog->selectNameFilter (filters.at (1));  // "All Files"
          fileDialog->setDefaultSuffix ("");              // no default suffix
        }
    }
  else
    {
      fileDialog->selectFile ("");
      fileDialog->setDirectory (_ced);

      // propose a name corresponding to the function name
      QString fname = get_function_name ();
      if (! fname.isEmpty ())
        fileDialog->selectFile (fname + ".m");
    }

  fileDialog->setAcceptMode (QFileDialog::AcceptSave);
  fileDialog->setViewMode (QFileDialog::Detail);

  connect (fileDialog, SIGNAL (filterSelected (const QString&)),
           this, SLOT (handle_save_as_filter_selected (const QString&)));

  if (remove_on_success)
    {
      connect (fileDialog, SIGNAL (fileSelected (const QString&)),
               this, SLOT (handle_save_file_as_answer_close (const QString&)));

      connect (fileDialog, SIGNAL (rejected ()),
               this, SLOT (handle_save_file_as_answer_cancel ()));
    }
  else
    {
      connect (fileDialog, SIGNAL (fileSelected (const QString&)),
               this, SLOT (handle_save_file_as_answer (const QString&)));
    }

  show_dialog (fileDialog, ! valid_file_name ());
}

void
file_editor_tab::handle_combo_eol_current_index (int index)
{
  _save_as_desired_eol = static_cast<QsciScintilla::EolMode> (index);
}

void
file_editor_tab::handle_combo_enc_current_index (QString text)
{
  _new_encoding = text;
}

void
file_editor_tab::handle_save_as_filter_selected (const QString& filter)
{
  QFileDialog *file_dialog = qobject_cast<QFileDialog *> (sender ());

  QRegExp rx ("\\*\\.([^ ^\\)]*)[ \\)]");   // regexp for suffix in filter
  int index = rx.indexIn (filter,0);        // get first suffix in filter

  if (index > -1)
    file_dialog->setDefaultSuffix (rx.cap (1)); // found a suffix, set default
  else
    file_dialog->setDefaultSuffix ("");         // not found, clear default
}

bool
file_editor_tab::check_valid_identifier (QString file_name)
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

bool
file_editor_tab::check_valid_codec (QTextCodec *codec)
{
  if (! codec->canEncode (_edit_area->text ()))
    {
      int ans = QMessageBox::warning (nullptr,
            tr ("Octave Editor"),
            tr ("The current editor contents can not be encoded\n"
                "with the selected codec %1.\n"
                "Using it will result in data loss!\n\n"
                "Do you want to chose another codec?").arg (_encoding),
            QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

      if (ans == QMessageBox::Yes)
        return true;
    }

  return false;
}

void
file_editor_tab::handle_save_file_as_answer (const QString& saveFileName)
{
  if (_save_as_desired_eol != _edit_area->eolMode ())
    convert_eol (this,_save_as_desired_eol);

  if (saveFileName == _file_name)
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

void
file_editor_tab::handle_save_file_as_answer_close (const QString& saveFileName)
{
  if (_save_as_desired_eol != _edit_area->eolMode ())
    {
      _edit_area->setReadOnly (false);  // was set to read-only in save_file_as
      convert_eol (this,_save_as_desired_eol);
      _edit_area->setReadOnly (true);   // restore read-only mode
    }

  // saveFileName == _file_name can not happen, because we only can get here
  // when we close a tab and _file_name is not a valid filename yet

  // Have editor check for conflict, delete tab after save.
  if (check_valid_identifier (saveFileName))
    save_file_as (true);
  else
    emit editor_check_conflict_save (saveFileName, true);
}

void
file_editor_tab::handle_save_file_as_answer_cancel ()
{
  // User canceled, allow editing again.
  _edit_area->setReadOnly (false);
}

void
file_editor_tab::file_has_changed (const QString&)
{
  // Prevent popping up multiple message boxes when the file has
  // been changed multiple times by temporarily removing from the
  // file watcher.
  QStringList trackedFiles = _file_system_watcher.files ();
  if (! trackedFiles.isEmpty ())
    _file_system_watcher.removePath (_file_name);

  if (QFile::exists (_file_name))
    {
      if (_always_reload_changed_files)

              load_file (_file_name);

      else
        {
          // give editor and this tab the focus,
          // possibly making the editor visible if it is hidden
          emit set_focus_editor_signal (this);
          _edit_area->setFocus ();

          // Create a WindowModal message that blocks the edit area
          // by making _edit_area parent.
          QMessageBox *msgBox
            = new QMessageBox (QMessageBox::Warning,
                               tr ("Octave Editor"),
                               tr ("It seems that \'%1\' has been modified by another application. Do you want to reload it?").
                               arg (_file_name),
                               QMessageBox::Yes | QMessageBox::No, this);

          connect (msgBox, SIGNAL (finished (int)),
                   this, SLOT (handle_file_reload_answer (int)));

          msgBox->setWindowModality (Qt::WindowModal);
          msgBox->setAttribute (Qt::WA_DeleteOnClose);
          msgBox->show ();
        }
    }
  else
    {
      // give editor and this tab the focus,
      // possibly making the editor visible  if it is hidden
      emit set_focus_editor_signal (this);
      _edit_area->setFocus ();

      QString modified = "";
      if (_edit_area->isModified ())
        modified = tr ("\n\nWarning: The contents in the editor is modified!");

      // Create a WindowModal message. The file editor tab can't be made
      // parent because it may be deleted depending upon the response.
      // Instead, change the _edit_area to read only.
      QMessageBox *msgBox
        = new QMessageBox (QMessageBox::Warning, tr ("Octave Editor"),
                           tr ("It seems that the file\n"
                               "%1\n"
                               "has been deleted or renamed. Do you want to save it now?%2").
                           arg (_file_name).arg (modified),
                           QMessageBox::Save | QMessageBox::Close, nullptr);

      _edit_area->setReadOnly (true);

      connect (msgBox, SIGNAL (finished (int)),
               this, SLOT (handle_file_resave_answer (int)));

      msgBox->setWindowModality (Qt::WindowModal);
      msgBox->setAttribute (Qt::WA_DeleteOnClose);
      msgBox->show ();
    }
}

void
file_editor_tab::notice_settings (const QSettings *settings, bool init)
{
  // QSettings pointer is checked before emitting.

  if (! init)
    update_lexer_settings ();

  // code folding
  if (settings->value ("editor/code_folding",true).toBool ())
    {
      _edit_area->setMarginType (3, QsciScintilla::SymbolMargin);
      _edit_area->setFolding (QsciScintilla::BoxedTreeFoldStyle , 3);
    }
  else
    {
      _edit_area->setFolding (QsciScintilla::NoFoldStyle, 3);
    }

  // status bar
  if (settings->value ("editor/show_edit_status_bar",true).toBool ())
    _status_bar->show ();
  else
    _status_bar->hide ();

  //highlight current line color
  QVariant default_var = QColor (240, 240, 240);
  QColor setting_color = settings->value ("editor/highlight_current_line_color",
                                          default_var).value<QColor> ();
  _edit_area->setCaretLineBackgroundColor (setting_color);
  _edit_area->setCaretLineVisible
    (settings->value ("editor/highlightCurrentLine", true).toBool ());

  bool match_keywords = settings->value
                            ("editor/codeCompletion_keywords",true).toBool ();
  bool match_document = settings->value
                            ("editor/codeCompletion_document",true).toBool ();

  QsciScintilla::AutoCompletionSource source = QsciScintilla::AcsNone;
  if (match_keywords)
    if (match_document)
      source = QsciScintilla::AcsAll;
    else
      source = QsciScintilla::AcsAPIs;
  else if (match_document)
    source = QsciScintilla::AcsDocument;
  _edit_area->setAutoCompletionSource (source);

  _edit_area->setAutoCompletionReplaceWord
      (settings->value ("editor/codeCompletion_replace",false).toBool ());
  _edit_area->setAutoCompletionCaseSensitivity
      (settings->value ("editor/codeCompletion_case",true).toBool ());

  if (settings->value ("editor/codeCompletion", true).toBool ())
    _edit_area->setAutoCompletionThreshold
      (settings->value ("editor/codeCompletion_threshold",2).toInt ());
  else
    _edit_area->setAutoCompletionThreshold (-1);

  if (settings->value ("editor/show_white_space",false).toBool ())
    if (settings->value ("editor/show_white_space_indent",false).toBool ())
      _edit_area->setWhitespaceVisibility (QsciScintilla::WsVisibleAfterIndent);
    else
      _edit_area->setWhitespaceVisibility (QsciScintilla::WsVisible);
  else
    _edit_area->setWhitespaceVisibility (QsciScintilla::WsInvisible);

  _edit_area->setEolVisibility (
              settings->value ("editor/show_eol_chars",false).toBool ());

  if (settings->value ("editor/showLineNumbers", true).toBool ())
    {
      _edit_area->setMarginLineNumbers (2, true);
      auto_margin_width ();
      connect (_edit_area, SIGNAL (linesChanged ()),
               this, SLOT (auto_margin_width ()));
    }
  else
    {
      _edit_area->setMarginLineNumbers (2, false);
      disconnect (_edit_area, SIGNAL (linesChanged ()), nullptr, nullptr);
    }

  _smart_indent = settings->value ("editor/auto_indent",true).toBool ();
  _edit_area->setAutoIndent (_smart_indent);
  _edit_area->setTabIndents
        (settings->value ("editor/tab_indents_line",false).toBool ());
  _edit_area->setBackspaceUnindents
        (settings->value ("editor/backspace_unindents_line",false).toBool ());
  _edit_area->setIndentationGuides
        (settings->value ("editor/show_indent_guides",false).toBool ());
  _edit_area->setIndentationsUseTabs
        (settings->value ("editor/indent_uses_tabs",false).toBool ());
  _edit_area->setIndentationWidth
        (settings->value ("editor/indent_width",2).toInt ());

  _edit_area->setTabWidth
        (settings->value ("editor/tab_width",2).toInt ());

  _edit_area->SendScintilla (QsciScintillaBase::SCI_SETHSCROLLBAR,
        settings->value ("editor/show_hscroll_bar",true).toBool ());
  _edit_area->SendScintilla (QsciScintillaBase::SCI_SETSCROLLWIDTH,-1);
  _edit_area->SendScintilla (QsciScintillaBase::SCI_SETSCROLLWIDTHTRACKING,true);

  _long_title = settings->value ("editor/longWindowTitle", false).toBool ();
  update_window_title (_edit_area->isModified ());

  _auto_endif = settings->value ("editor/auto_endif",1).toInt ();

  // long line marker
  int line_length = settings->value ("editor/long_line_column",80).toInt ();
  _edit_area->setEdgeColumn (line_length);
  _edit_area->setEdgeMode (QsciScintilla::EdgeNone);
  if (settings->value ("editor/long_line_marker",true).toBool ())
    _edit_area->setEdgeMode (QsciScintilla::EdgeLine);
  if (settings->value ("editor/long_line_marker_background",true).toBool ())
    _edit_area->setEdgeMode (QsciScintilla::EdgeBackground);

  // line wrapping and breaking
  _edit_area->setWrapVisualFlags (QsciScintilla::WrapFlagByBorder);
  _edit_area->setWrapIndentMode (QsciScintilla::WrapIndentSame);

  if (settings->value ("editor/wrap_lines",false).toBool ())
    _edit_area->setWrapMode (QsciScintilla::WrapWord);
  else
    _edit_area->setWrapMode (QsciScintilla::WrapNone);

  if (settings->value ("editor/break_lines",false).toBool ())
    _line_break = line_length;
  else
    _line_break = 0;

  _line_break_comments =
        settings->value ("editor/break_lines_comments",false).toBool ();

  // highlight all occurrences of a word selected by a double click
  _highlight_all_occurrences =
        settings->value ("editor/highlight_all_occurrences", true).toBool ();

  // reload changed files
  _always_reload_changed_files =
        settings->value ("editor/always_reload_changed_files",false).toBool ();
}

void
file_editor_tab::auto_margin_width ()
{
  _edit_area->setMarginWidth (2, "1"+QString::number (_edit_area->lines ()));
}

// the following close request was changed from a signal slot into a
// normal function because we need the return value from close whether
// the tab really was closed (for canceling exiting octave).
// When emitting a signal, only the return value from the last slot
// goes back to the sender
bool
file_editor_tab::conditional_close (void)
{
  return close ();
}

void
file_editor_tab::change_editor_state (const QWidget *ID)
{
  if (ID != this)
    {
      // Widget may be going out of focus.  If so, record location.
      if (_find_dialog)
        {
          if (_find_dialog->isVisible ())
            {
              _find_dialog_geometry = _find_dialog->geometry ();
              _find_dialog->hide ();
            }
        }
      return;
    }

  if (_find_dialog && _find_dialog_is_visible)
    {
      _find_dialog->setGeometry (_find_dialog_geometry);
      QPoint p = _find_dialog->pos ();
      _find_dialog->move (p.x ()+10, p.y ()+10);
      _find_dialog->show ();
    }

  emit editor_state_changed (_copy_available, _is_octave_file);
}

void
file_editor_tab::file_name_query (const QWidget *ID)
{
  // A zero (null pointer) means that all file editor tabs
  // should respond, otherwise just the desired file editor tab.
  if (ID != this && ID != nullptr)
    return;

  // This list also includes windows with name ""
  emit add_filename_to_list (_file_name, _encoding, this);
}

void
file_editor_tab::handle_file_reload_answer (int decision)
{
  if (decision == QMessageBox::Yes)
    {
      // reload: file is readded to the file watcher in set_file_name ()
      load_file (_file_name);
    }
  else
    {
      // do not reload: readd to the file watche
      _file_system_watcher.addPath (_file_name);
    }
}

void
file_editor_tab::handle_file_resave_answer (int decision)
{
  // check decision of user in dialog
  if (decision == QMessageBox::Save)
    {
      save_file (_file_name);  // readds file to watcher in set_file_name ()
      _edit_area->setReadOnly (false);  // delete read only flag
    }
  else
    {
      // Definitely close the file.
      // Set modified to false to prevent the dialog box when the close event
      // is posted. If the user cancels the close in this dialog the tab is
      // left open with a non-existing file.
      _edit_area->setModified (false);
      close ();
    }
}

void
file_editor_tab::insert_debugger_pointer (const QWidget *ID, int line)
{
  if (ID != this || ID == nullptr)
    return;

  emit remove_all_positions ();  // debugger_position, unsure_debugger_position

  if (line > 0)
    {
      marker *dp;

      if (_edit_area->isModified ())
        {
          // The best that can be done if the editor contents has been
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
              dp = new marker (_edit_area, line,
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
                  dp = new marker (_edit_area, line,
                                   marker::unsure_debugger_position,
                                   linenr_guess);
                }
              else
                {
                  // Can't make a very good guess, so just use the debugger
                  // line number.
                  dp = new marker (_edit_area, line,
                                   marker::unsure_debugger_position);
                }
            }
        }
      else
        dp = new marker (_edit_area, line, marker::debugger_position);

      connect (this, SIGNAL (remove_position_via_debugger_linenr (int)),
               dp,   SLOT (handle_remove_via_original_linenr (int)));
      connect (this, SIGNAL (remove_all_positions (void)),
               dp,   SLOT (handle_remove (void)));

      center_current_line (false);
    }
}

void
file_editor_tab::delete_debugger_pointer (const QWidget *ID, int line)
{
  if (ID != this || ID == nullptr)
    return;

  if (line > 0)
    emit remove_position_via_debugger_linenr (line);
}

void
file_editor_tab::do_breakpoint_marker (bool insert, const QWidget *ID, int line,
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
                {       // can only reuse conditional bp as conditional
                  emit remove_breakpoint_via_debugger_linenr (line);
                  bp = nullptr;
                }
              else
                bp->set_cond (cond);
            }

          if (bp == nullptr)
            {
              bp = new marker (_edit_area, line,
                               cond == "" ? marker::breakpoint
                                          : marker::cond_break, cond);

              connect (this, SIGNAL (remove_breakpoint_via_debugger_linenr
                                     (int)),
                       bp,   SLOT (handle_remove_via_original_linenr (int)));
              connect (this, SIGNAL (request_remove_breakpoint_via_editor_linenr
                                     (int)),
                       bp,   SLOT (handle_request_remove_via_editor_linenr
                                     (int)));
              connect (this, SIGNAL (remove_all_breakpoints (void)),
                       bp,   SLOT (handle_remove (void)));
              connect (this, SIGNAL (find_translated_line_number (int, int&,
                                                                  marker*&)),
                       bp,   SLOT (handle_find_translation (int, int&,
                                                            marker*&)));
              connect (this, SIGNAL (find_linenr_just_before (int, int&, int&)),
                       bp,   SLOT (handle_find_just_before (int, int&, int&)));
              connect (this, SIGNAL (report_marker_linenr (QIntList&,
                                                           QStringList&)),
                       bp,   SLOT (handle_report_editor_linenr (QIntList&,
                                                                QStringList&)));
              connect (bp,   SIGNAL (request_remove (int)),
                       this, SLOT (handle_request_remove_breakpoint (int)));
            }
        }
      else
        emit remove_breakpoint_via_debugger_linenr (line);
    }
}

void
file_editor_tab::center_current_line (bool always)
{
  long int visible_lines
    = _edit_area->SendScintilla (QsciScintillaBase::SCI_LINESONSCREEN);

  if (visible_lines > 2)
    {
      int line, index;
      _edit_area->getCursorPosition (&line, &index);
      // compensate for "folding":
      // step 1: expand the current line, if it was folded
      _edit_area->SendScintilla (2232, line);   // SCI_ENSUREVISIBLE

      // step 2: map file line num to "visible" one // SCI_VISIBLEFROMDOCLINE
      int vis_line = _edit_area->SendScintilla (2220, line);

      int first_line = _edit_area->firstVisibleLine ();

      if (always || vis_line == first_line
          || vis_line > first_line + visible_lines - 2)
        {
          first_line += (vis_line - first_line - (visible_lines - 1) / 2);
          _edit_area->SendScintilla (2613, first_line); // SCI_SETFIRSTVISIBLELINE
        }
    }
}

void
file_editor_tab::handle_lines_changed ()
{
  // the related signal is emitted before cursor-move-signal!
  _lines_changed = true;
}

void
file_editor_tab::handle_cursor_moved (int line, int col)
{
  if (_edit_area->SendScintilla (QsciScintillaBase::SCI_AUTOCACTIVE))
    show_auto_completion (this);

  if (_lines_changed)  // cursor moved and lines have changed
    {
      _lines_changed = false;
      if (_is_octave_file && line == _line+1 && col < _col)
        {
          // Obviously, we have a newline here
          if (_smart_indent || _auto_endif)
            _edit_area->smart_indent (_smart_indent, _auto_endif, _line);
        }
    }

  _line = line;
  _col  = col;

  _row_indicator->setNum (line+1);
  _col_indicator->setNum (col+1);
}

// Slot that is entered each time a new character was typed.
// It is used for handling line breaking if this is desired.
// The related signal is emitted after the signal for a moved cursor
// such that _col and _line can not be used for current position.
void
file_editor_tab::handle_char_added (int)
{
  if (_line_break)
  {
    // If line breaking is desired, get the current line and column.
    // For taking the tab width into consideration, use own function
    int line, col, pos;
    _edit_area->get_current_position (&pos, &line, &col);

    // immediately return if line has not reached the max. line length
    if (col <= _line_break)
      return;

    // If line breaking is only desired in comments,
    // return if not in a comment
    int style_comment = octave_qscintilla::ST_NONE;
    if (_line_break_comments)
      {
        // line breaking only in comments, check for comment style
        style_comment = _edit_area->is_style_comment ();
        if (! style_comment)
          return;       // no comment, return
      }

    // Here we go for breaking the current line by inserting a newline.
    // For determining the position of a specific column, we have to get
    // the column from the QScintila function without taking tab lengths
    // into account, since the calculation from line/col to position ignores
    // this, too
    _edit_area->getCursorPosition (&line, &col);
    int c = 0;
    int col_space = col;
    int indentation = _edit_area->indentation (line);

    // Search the first occurence of space or tab backwards starting from
    // the current column (col_space).
    while (c != ' ' && c != '\t' && col_space > indentation)
      {
        pos = _edit_area->positionFromLineIndex (line, col_space--);
        c = _edit_area->SendScintilla (QsciScintillaBase::SCI_GETCHARAT, pos);
      }

    // If a space or tab was found, break at this char,
    // otherwise break at cursor position
    int col_newline = col - 1;
    if (c == ' ' || c == '\t')
      col_newline = col_space + 1;

    // Insert a newline char for breaking the line possibly followed
    // by a line comment string
    QString newline = QString ("\n");
    style_comment = _edit_area->is_style_comment ();
    if (style_comment == octave_qscintilla::ST_LINE_COMMENT)
      newline = newline + _edit_area->comment_string ();
    _edit_area->insertAt (newline, line, col_newline);

    // Automatically indent the new line to the indentation of previous line
    // and set the cursor position to the end of the indentation.
    _edit_area->setIndentation (line + 1, indentation);
    _edit_area->SendScintilla (QsciScintillaBase::SCI_LINEEND);
  }
}

// Slot handling a double click into the text area
void
file_editor_tab::handle_double_click (int, int, int modifier)
{
  if (! modifier)
    {
      // double clicks without modifier
      // clear any existing indicators of this type
      _edit_area->clear_indicator (_indicator_highlight_all);

      if (_highlight_all_occurrences)
        {
          // highlighting of all occurrences of the clicked word is enabled

          // get the resulting cursor position
          // (required if click was beyond a line ending)
          int line, col;
          _edit_area->getCursorPosition (&line, &col);

          // get the word at the cursor (if any)
          QString word = _edit_area->wordAtLineIndex (line, col);
          word = word.trimmed ();

          if (! word.isEmpty ())
            {
              // word is not empty, so find all occurrences of the word

              // remember first visible line for restoring the view afterwards
              int first_line = _edit_area->firstVisibleLine ();

              // search for first occurrence of the detected word
              bool find_result_available
                      = _edit_area->findFirst (word,
                                               false,   // no regexp
                                               true,    // case sensitive
                                               true,    // whole words only
                                               false,   // do not wrap
                                               true,    // forward
                                               0,0,     // from the beginning
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
                  _edit_area->getCursorPosition (&oline, &ocol);
                  // set the indicator
                  _edit_area->fillIndicatorRange (oline, ocol - wlen,
                                                  oline, ocol,
                                                  _indicator_highlight_all);
                  // find next occurrence
                  find_result_available = _edit_area->findNext ();
                }

              // restore the visible area of the file, the cursor position,
              // and the selection
              _edit_area->setFirstVisibleLine (first_line);
              _edit_area->setCursorPosition (line, col);
              _edit_area->setSelection (line, col - wlen, line, col);
            }
        }
    }
}


QString
file_editor_tab::get_function_name ()
{
  QRegExp rxfun1 ("^[\t ]*function[^=]+=([^\\(]+)\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun2 ("^[\t ]*function[\t ]+([^\\(]+)\\([^\\)]*\\)[\t ]*$");
  QRegExp rxfun3 ("^[\t ]*function[^=]+=[\t ]*([^\\s]+)[\t ]*$");
  QRegExp rxfun4 ("^[\t ]*function[\t ]+([^\\s]+)[\t ]*$");

  QStringList lines = _edit_area->text ().split ("\n");

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
    }

  return QString ();
}

#endif
