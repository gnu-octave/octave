/*

Copyright (C) 2013-2017 Torsten

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

// Author: Torsten <ttl@justmail.de>

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_QSCINTILLA)

#include <Qsci/qscilexer.h>

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

#include <Qsci/qscicommandset.h>
#include <QShortcut>
#include <QMessageBox>

#include "octave-qscintilla.h"
#include "file-editor-tab.h"
#include "shortcut-manager.h"
#include "resource-manager.h"

octave_qscintilla::octave_qscintilla (QWidget *p)
  : QsciScintilla (p)
{
  connect (this, SIGNAL (textChanged ()), this, SLOT (text_changed ()));

  // clear scintilla edit shortcuts that are handled by the editor
  QsciCommandSet *cmd_set = standardCommands ();

#if defined (HAVE_QSCI_VERSION_2_6_0)
  // find () was added in QScintilla 2.6
  cmd_set->find (QsciCommand::SelectionCopy)->setKey (0);
  cmd_set->find (QsciCommand::SelectionCut)->setKey (0);
  cmd_set->find (QsciCommand::Paste)->setKey (0);
  cmd_set->find (QsciCommand::SelectAll)->setKey (0);
  cmd_set->find (QsciCommand::SelectionDuplicate)->setKey (0);
  cmd_set->find (QsciCommand::LineTranspose)->setKey (0);
  cmd_set->find (QsciCommand::Undo)->setKey (0);
  cmd_set->find (QsciCommand::Redo)->setKey (0);
  cmd_set->find (QsciCommand::SelectionUpperCase)->setKey (0);
  cmd_set->find (QsciCommand::SelectionLowerCase)->setKey (0);
  cmd_set->find (QsciCommand::ZoomIn)->setKey (0);
  cmd_set->find (QsciCommand::ZoomOut)->setKey (0);
  cmd_set->find (QsciCommand::DeleteWordLeft)->setKey (0);
  cmd_set->find (QsciCommand::DeleteWordRight)->setKey (0);
  cmd_set->find (QsciCommand::DeleteLineLeft)->setKey (0);
  cmd_set->find (QsciCommand::DeleteLineRight)->setKey (0);
  cmd_set->find (QsciCommand::LineDelete)->setKey (0);
  cmd_set->find (QsciCommand::LineCut)->setKey (0);
  cmd_set->find (QsciCommand::LineCopy)->setKey (0);
#else
  // find commands via its default key (tricky way without find ())
  QList< QsciCommand * > cmd_list = cmd_set->commands ();
  for (int i = 0; i < cmd_list.length (); i++)
    {
      int cmd_key = cmd_list.at (i)->key ();
      switch (cmd_key)
        {
        case Qt::Key_C | Qt::CTRL :               // SelectionCopy
        case Qt::Key_X | Qt::CTRL :               // SelectionCut
        case Qt::Key_V | Qt::CTRL :               // Paste
        case Qt::Key_A | Qt::CTRL :               // SelectAll
        case Qt::Key_D | Qt::CTRL :               // SelectionDuplicate
        case Qt::Key_T | Qt::CTRL :               // LineTranspose
        case Qt::Key_Z | Qt::CTRL :               // Undo
        case Qt::Key_Y | Qt::CTRL :               // Redo
        case Qt::Key_Z | Qt::CTRL | Qt::SHIFT :   // Redo
        case Qt::Key_U | Qt::CTRL :               // SelectionLowerCase
        case Qt::Key_U | Qt::CTRL | Qt::SHIFT :   // SelectionUpperCase
        case Qt::Key_Plus | Qt::CTRL :            // ZoomIn
        case Qt::Key_Minus | Qt::CTRL :           // ZoomOut
        case Qt::Key_Backspace | Qt::CTRL | Qt::SHIFT :   // DeleteLineLeft
        case Qt::Key_Delete | Qt::CTRL | Qt::SHIFT :      // DeleteLineRight
        case Qt::Key_K | Qt::META :                       // DeleteLineRight
        case Qt::Key_Backspace | Qt::CTRL :       // DeleteWordLeft
        case Qt::Key_Delete | Qt::CTRL :          // DeleteWordRight
        case Qt::Key_L | Qt::CTRL | Qt::SHIFT :   // LineDelete
        case Qt::Key_L | Qt::CTRL :               // LineCut
        case Qt::Key_T | Qt::CTRL | Qt::SHIFT :   // LineCopy
          cmd_list.at (i)->setKey (0);
        }
    }
#endif

#if defined (Q_OS_MAC)
  // Octave interprets Cmd key as Meta whereas Qscintilla interprets it
  // as Ctrl. We thus invert Meta/Ctrl in Qscintilla's shortcuts list.
  QList< QsciCommand * > cmd_list_mac = cmd_set->commands ();
  for (int i = 0; i < cmd_list_mac.length (); i++)
    {
      // Primary key
      int key = cmd_list_mac.at (i)->key ();

      if (static_cast<int> (key | Qt::META) == key &&
          static_cast<int> (key | Qt::CTRL) != key)
        key = (key ^ Qt::META) | Qt::CTRL;
      else if (static_cast<int> (key | Qt::CTRL) == key)
        key = (key ^ Qt::CTRL) | Qt::META;

      cmd_list_mac.at (i)->setKey (key);

      // Alternate key
      key = cmd_list_mac.at (i)->alternateKey ();

      if (static_cast<int> (key | Qt::META) == key &&
          static_cast<int> (key | Qt::CTRL) != key)
        key = (key ^ Qt::META) | Qt::CTRL;
      else if (static_cast<int> (key | Qt::CTRL) == key)
        key = (key ^ Qt::CTRL) | Qt::META;

      cmd_list_mac.at (i)->setAlternateKey (key);
    }
#endif

  // init state of undo/redo action for this tab
  emit status_update (isUndoAvailable (), isRedoAvailable ());
}

octave_qscintilla::~octave_qscintilla ()
{ }

void
octave_qscintilla::get_global_textcursor_pos (QPoint *global_pos,
                                              QPoint *local_pos)
{
  long position = SendScintilla (SCI_GETCURRENTPOS);
  long point_x  = SendScintilla
                    (SCI_POINTXFROMPOSITION,0,position);
  long point_y  = SendScintilla
                    (SCI_POINTYFROMPOSITION,0,position);
  *local_pos = QPoint (point_x,point_y);  // local cursor position
  *global_pos = mapToGlobal (*local_pos); // global position of cursor
}

// determine the actual word and whether we are in an octave or matlab script
bool
octave_qscintilla::get_actual_word ()
{
  QPoint global_pos, local_pos;
  get_global_textcursor_pos (&global_pos, &local_pos);
  _word_at_cursor = wordAtPoint (local_pos);
  QString lexer_name = lexer ()->lexer ();
  return ((lexer_name == "octave" || lexer_name == "matlab")
          && ! _word_at_cursor.isEmpty ());
}

// call documentation or help on the current word
void
octave_qscintilla::context_help_doc (bool documentation)
{
  if (get_actual_word ())
    contextmenu_help_doc (documentation);
}

// call edit the function related to the current word
void
octave_qscintilla::context_edit ()
{
  if (get_actual_word ())
    contextmenu_edit (true);
}

// call edit the function related to the current word
void
octave_qscintilla::context_run ()
{
  if (hasSelectedText ())
    contextmenu_run (true);
}

// context menu requested
void
octave_qscintilla::contextMenuEvent (QContextMenuEvent *e)
{
#if defined (HAVE_QSCI_VERSION_2_6_0)
  QPoint global_pos, local_pos;                         // the menu's position
  QMenu *context_menu = createStandardContextMenu ();  // standard menu

  bool in_left_margin = false;

  // determine position depending on mouse or keyboard event
  if (e->reason () == QContextMenuEvent::Mouse)
    {
      // context menu by mouse
      global_pos = e->globalPos ();            // global mouse position
      local_pos  = e->pos ();                  // local mouse position
      if (e->x () < marginWidth (1) + marginWidth (2))
        in_left_margin = true;
    }
  else
    {
      // context menu by keyboard or other: get point of text cursor
      get_global_textcursor_pos (&global_pos, &local_pos);
      QRect editor_rect = geometry ();      // editor rect mapped to global
      editor_rect.moveTopLeft
      (parentWidget ()->mapToGlobal (editor_rect.topLeft ()));
      if (! editor_rect.contains (global_pos))  // is cursor outside editor?
        global_pos = editor_rect.topLeft ();   // yes, take top left corner
    }

#if defined (HAVE_QSCI_VERSION_2_6_0)
  if (! in_left_margin)
#endif
    {
      // fill context menu with editor's standard actions
      emit create_context_menu_signal (context_menu);

      // additional custom entries of the context menu
      context_menu->addSeparator ();   // separator before custom entries

      // help menu: get the position of the mouse or the text cursor
      // (only for octave files)
      QString lexer_name = lexer ()->lexer ();
      if (lexer_name == "octave" || lexer_name == "matlab")
        {
          _word_at_cursor = wordAtPoint (local_pos);
          if (! _word_at_cursor.isEmpty ())
            {
              context_menu->addAction (tr ("Help on") + " " + _word_at_cursor,
                                       this, SLOT (contextmenu_help (bool)));
              context_menu->addAction (tr ("Documentation on")
                                       + " " + _word_at_cursor,
                                       this, SLOT (contextmenu_doc (bool)));
              context_menu->addAction (tr ("Edit") + " " + _word_at_cursor,
                                       this, SLOT (contextmenu_edit (bool)));
            }
        }
    }
#if defined (HAVE_QSCI_VERSION_2_6_0)
  else
    {
      // remove all standard actions from scintilla
      QList<QAction *> all_actions = context_menu->actions ();
      QAction *a;

      foreach (a, all_actions)
        context_menu->removeAction (a);

      a = context_menu->addAction (tr ("dbstop if ..."), this,
                                   SLOT (contextmenu_break_condition (bool)));
      a->setData (local_pos);
    }
#endif

  // finaly show the menu
  context_menu->exec (global_pos);
#endif
}

// handle the menu entry for calling help or doc
void
octave_qscintilla::contextmenu_doc (bool)
{
  contextmenu_help_doc (true);
}
void
octave_qscintilla::contextmenu_help (bool)
{
  contextmenu_help_doc (false);
}

// common function with flag for documentation
void
octave_qscintilla::contextmenu_help_doc (bool documentation)
{
  if (documentation)
    emit show_doc_signal (_word_at_cursor);
  else
    emit execute_command_in_terminal_signal ("help " + _word_at_cursor);
}

void
octave_qscintilla::contextmenu_edit (bool)
{
  emit context_menu_edit_signal (_word_at_cursor);
}

void
octave_qscintilla::contextmenu_run (bool)
{
  QStringList commands = selectedText ().split (QRegExp ("[\r\n]"),
                                                QString::SkipEmptyParts);
  for (int i = 0; i < commands.size (); i++)
    emit execute_command_in_terminal_signal (commands.at (i));
}

// wrappers for dbstop related context menu items

// FIXME: Why can't the data be sent as the argument to the function???
void
octave_qscintilla::contextmenu_break_condition (bool)
{
#if defined (HAVE_QSCI_VERSION_2_6_0)
  QAction *action = qobject_cast<QAction *>(sender ());
  QPoint local_pos = action->data ().value<QPoint> ();

  // pick point just right of margins, so lineAt doesn't give -1
  int margins = marginWidth (1) + marginWidth (2) + marginWidth (3);
  local_pos = QPoint (margins + 1, local_pos.y ());

  emit context_menu_break_condition_signal (lineAt (local_pos));
#endif
}

void
octave_qscintilla::contextmenu_break_once (const QPoint& local_pos)
{
#if defined (HAVE_QSCI_VERSION_2_6_0)
  emit context_menu_break_once (lineAt (local_pos));
#endif
}

void
octave_qscintilla::text_changed ()
{
  emit status_update (isUndoAvailable (), isRedoAvailable ());
}

// when edit area gets focus update information on undo/redo actions
void octave_qscintilla::focusInEvent (QFocusEvent *focusEvent)
{
  emit status_update (isUndoAvailable (), isRedoAvailable ());

  QsciScintilla::focusInEvent (focusEvent);
}

// Function returning the comment string of the current lexer
QString
octave_qscintilla::comment_string ()
{
  int lexer = SendScintilla (SCI_GETLEXER);

  switch (lexer)
    {
#if defined (HAVE_LEXER_MATLAB)
      case SCLEX_MATLAB:
#if defined (HAVE_LEXER_OCTAVE)
      case SCLEX_OCTAVE:
#endif
       {
          QSettings *settings = resource_manager::get_settings ();
          int comment_index
                = settings->value ("editor/octave_comment_string", 0).toInt ();
          if (comment_index == 1)
            return QString ("#");
          else if (comment_index == 2)
            return QString ("%");
          else
            return QString ("##");  // default and for index 0
        }
#endif

      case SCLEX_PERL:
      case SCLEX_BASH:
      case SCLEX_DIFF:
        return QString ("#");

      case SCLEX_CPP:
        return QString ("//");

      case SCLEX_BATCH:
        return QString ("REM ");
    }

    return QString ("%");  // should never happen
}

// helper function for clearing all indicators of a specific style
void
octave_qscintilla::clear_indicator (int indicator_style)
{
  int end_pos = text ().length ();
  int end_line, end_col;
  lineIndexFromPosition (end_pos, &end_line, &end_col);
  clearIndicatorRange (0, 0, end_line, end_col, indicator_style);
}

// provide the style at a specific position
int
octave_qscintilla::get_style (int pos)
{
  int position;
  if (pos < 0)
    // The positition has to be reduced by 2 for getting the real style (?)
    position = SendScintilla (QsciScintillaBase::SCI_GETCURRENTPOS) - 2;
  else
    position = pos;

  return SendScintilla (QsciScintillaBase::SCI_GETSTYLEAT, position);
}

// Return true if CANDIDATE is a "closing" that matches OPENING,
// such as "end" or "endif" for "if", or "catch" for "try".
// Used for testing the last word of an "if" etc. line,
// or the first word of the following line.
static bool
is_end (const QString& candidate, const QString& opening)
{
  bool retval = false;

  if (opening == "do")          // The only one that can't be ended by "end"
    {
      if (candidate == "until")
        retval = true;
    }
  else
    {
      if (candidate == "end")
        retval =  true;
      else
        {
          if (opening == "try")
            {
              if (candidate == "catch" || candidate == "end_try_catch")
                retval = true;
            }
          else if (opening == "unwind_protect")
            {
              if (candidate == "unwind_protect_cleanup"
                  || candidate == "end_unwind_protect")
                retval = true;
            }
          else if (candidate == "end" + opening)
            retval = true;
          else if (opening == "if" && candidate == "else")
            retval = true;
        }
    }

  return retval;
}

void
octave_qscintilla::auto_close (int auto_endif, int linenr,
                               const QString& line, QString& first_word)
{
  // Insert and "end" for an "if" etc., if needed.
  // (Use of "while" allows "return" to skip the rest.
  // It may be clearer to use "if" and "goto",
  // but that violates the coding standards.)

  bool autofill_simple_end = (auto_endif == 2);

  size_t start = line.toStdString ().find_first_not_of (" \t");

  // Check if following line has the same or less indentation
  // Check if the following line does not start with
  //       end* (until) (catch)
  if (linenr < lines () - 1)
    {
      int offset = 1;
      size_t next_start;
      QString next_line;
      do                            // find next non-blank line
        {
          next_line = text (linenr + offset++);
          next_start = next_line.toStdString ().find_first_not_of (" \t\n");
        }
      while (linenr + offset < lines ()
             && next_start == std::string::npos);
      if (next_start == std::string::npos)
        next_start = 0;
      if (next_start > start)       // more indented => don't add "end"
        return;
      if (next_start == start)      // same => check if already is "end"
        {
          QRegExp rx_start = QRegExp ("(\\w+)");
          int tmp = rx_start.indexIn (next_line, start);
           if (tmp != -1 && is_end (rx_start.cap(1), first_word))
             return;
        }
    }

    // If all of the above, insert a new line, with matching indent
    // and either 'end' or 'end...', depending on a flag.

    // If we insert directly after the last line, the "end" is autoindented,
    // so add a dummy line.
    if (linenr + 2 == lines ())
      insertAt (QString ("\n"), linenr + 2, 0);

    // For try/catch/end, fill "end" first, so "catch" is top of undo stack
    if (first_word == "try")
      insertAt (QString (start, ' ')
                + (autofill_simple_end ? "end\n" : "end_try_catch\n"),
                linenr + 2, 0);
    else if (first_word == "unwind_protect")
      insertAt (QString (start, ' ')
                + (autofill_simple_end ? "end\n" : "end_unwind_protect\n"),
                linenr + 2, 0);

    QString next_line;
    if (first_word == "do")
      next_line = "until\n";
    else if (first_word == "try")
      next_line = "catch\n";
    else if (first_word == "unwind_protect")
      next_line = "unwind_protect_cleanup\n";
    else if (autofill_simple_end)
      next_line = "end\n";
    else
      {
        if (first_word == "unwind_protect")
          first_word = "_" + first_word;
        next_line = "end" + first_word + "\n";
      }

    insertAt (QString (start, ' ') + next_line, linenr + 2, 0);
}


// Do smart indendation after if, for, ...
void
octave_qscintilla::smart_indent (bool do_smart_indent,
                                 int do_auto_close, int line)
{
  QString prevline = text (line);

  QRegExp bkey = QRegExp ("^[\t ]*(if|for|while|switch|case|do|function"
                          "|properties|events|classdef|unwind_protect"
                          "|unwind_protect_cleanup|try)"
                          "[\r]?[\n\t #%]");
  // last word except for comments, assuming no ' or " in comment.
  // rx_end = QRegExp ("(\\w+)[ \t;\r\n]*([%#][^\"']*)?$");

  // last word except for comments,
  // allowing % and # in single or double quoted strings
  // FIXME This will get confused by transpose.
  QRegExp ekey = QRegExp ("(?:(?:['\"][^'\"]*['\"])?[^%#]*)*"
                          "(\\w+)[ \t;\r\n]*([%#].*)?$");

  int bpos = bkey.indexIn (prevline, 0);
  int epos;

  if (bpos > -1)
    {
      // Found keyword after that indentation should be added

      // Check for existing end statement in the same line
      epos = ekey.indexIn (prevline, bpos);
      QString first_word = bkey.cap(1);
      bool inline_end = (epos > -1) && is_end (ekey.cap(1), first_word);

      if (do_smart_indent && ! inline_end)
        {
          // Do smart indent in the current line (line+1)
          indent (line+1);
          setCursorPosition (line+1, indentation (line) + indentationWidth ());
        }

      if (do_auto_close && ! inline_end)
        {
          // Do auto close
          auto_close (do_auto_close, line, prevline, first_word);
        }

      return;
    }

  QRegExp mkey = QRegExp ("^[\t ]*(else|elseif|catch)[\r]?[\t #%\n]");
  if (prevline.contains (mkey))
    {
      int prev_ind = indentation (line-1);
      int act_ind = indentation (line);

      if (prev_ind == act_ind)
        unindent (line);
      else if (prev_ind > act_ind)
        {
          setIndentation (line+1, prev_ind);
          setCursorPosition (line+1, prev_ind);
        }
      return;
    }

  ekey = QRegExp ("^[\t ]*(end|endif|endfor|endwhile|until|endfunction"
                  "|end_try_catch|end_unwind_protext)[\r]?[\t #%\n(;]");
  if (prevline.contains (ekey))
    {
      if (indentation (line-1) <= indentation (line))
        {
          unindent (line+1);
          unindent (line);
          setCursorPosition (line+1,
                                         indentation (line));
        }
      return;
    }

}

// Is a specific cursor position in a line or block comment?
int
octave_qscintilla::is_style_comment (int pos)
{
  int lexer = SendScintilla (QsciScintillaBase::SCI_GETLEXER);
  int style = get_style (pos);

  switch (lexer)
    {
      case SCLEX_CPP:
        return (ST_LINE_COMMENT * (
                          style == QsciLexerCPP::CommentLine
                       || style == QsciLexerCPP::CommentLineDoc)
              + ST_BLOCK_COMMENT * (
                           style == QsciLexerCPP::Comment
                        || style == QsciLexerCPP::CommentDoc
                        || style == QsciLexerCPP::CommentDocKeyword
                        || style == QsciLexerCPP::CommentDocKeywordError)
                );

#if defined (HAVE_LEXER_MATLAB)
      case SCLEX_MATLAB:
        return (ST_LINE_COMMENT * (style == QsciLexerMatlab::Comment));
#endif
#if  defined (HAVE_LEXER_OCTAVE)
      case SCLEX_OCTAVE:
        return (ST_LINE_COMMENT * (style == QsciLexerOctave::Comment));
#endif

      case SCLEX_PERL:
        return (ST_LINE_COMMENT * (style == QsciLexerPerl::Comment));

      case SCLEX_BATCH:
        return (ST_LINE_COMMENT * (style == QsciLexerBatch::Comment));

      case SCLEX_DIFF:
        return (ST_LINE_COMMENT * (style == QsciLexerDiff::Comment));

      case SCLEX_BASH:
        return (ST_LINE_COMMENT * (style == QsciLexerBash::Comment));

    }

  return ST_NONE;
}

// Function returning the true cursor position where the tab length
// is taken into account.
void
octave_qscintilla::get_current_position (int *pos, int *line, int *col)
{
  *pos = SendScintilla (QsciScintillaBase::SCI_GETCURRENTPOS);
  *line = SendScintilla (QsciScintillaBase::SCI_LINEFROMPOSITION, *pos);
  *col = SendScintilla (QsciScintillaBase::SCI_GETCOLUMN, *pos);
}
#endif
