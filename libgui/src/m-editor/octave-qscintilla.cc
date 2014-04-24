/*

Copyright (C) 2013 Torsten

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

// Author: Torsten <ttl@justmail.de>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_QSCINTILLA

#include <Qsci/qscilexer.h>
#include <Qsci/qscicommandset.h>
#include <QShortcut>

#include "octave-qscintilla.h"
#include "file-editor-tab.h"
#include "shortcut-manager.h"

octave_qscintilla::octave_qscintilla (QWidget *p)
  : QsciScintilla (p)
{
  // clear scintilla edit shortcuts that are handled by the editor
  QsciCommandSet  *cmd_set =  standardCommands ();
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
          && !_word_at_cursor.isEmpty ());
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

#ifdef HAVE_QSCI_VERSION_2_6_0
// context menu requested
void
octave_qscintilla::contextMenuEvent (QContextMenuEvent *e)
{
  QMenu *context_menu = createStandardContextMenu ( );  // standard menu

  // the menu's position
  QPoint global_pos, local_pos;

  if (e->reason () == QContextMenuEvent::Mouse)
    {
      // context menu by mouse
      global_pos = e->globalPos ();            // global mouse position
      local_pos  = e->pos ();                  // local mouse position
    }
  else
    {
      // context menu by keyboard or other: get point of text cursor
      get_global_textcursor_pos (&global_pos, &local_pos);
      QRect editor_rect = geometry ();      // editor rect mapped to global
      editor_rect.moveTopLeft
      (parentWidget ()->mapToGlobal (editor_rect.topLeft ()));
      if (!editor_rect.contains (global_pos))  // is cursor outside editor?
        global_pos = editor_rect.topLeft ();   // yes, take top left corner
    }

  // additional custom entries of the context menu
  context_menu->addSeparator ();   // separator before custom entries

  // help menu: get the position of the mouse or the text cursor
  // (only for octave files)
  QString lexer_name = lexer ()->lexer ();
  if (lexer_name == "octave" || lexer_name == "matlab")
    {
      _word_at_cursor = wordAtPoint (local_pos);
      if (!_word_at_cursor.isEmpty ())
        {
          context_menu->addAction (tr ("Help on") + " " + _word_at_cursor,
                                   this, SLOT (contextmenu_help (bool)));
          context_menu->addAction (tr ("Documentation on")
                                   + " " + _word_at_cursor,
                                   this, SLOT (contextmenu_doc (bool)));
          context_menu->addAction (tr ("Edit") + " " + _word_at_cursor,
                                   this, SLOT (contextmenu_edit (bool)));
        }
      context_menu->addSeparator ();   // separator before custom entries
      if (hasSelectedText ())
        context_menu->addAction (tr ("&Run Selection"),
                                 this, SLOT (contextmenu_run (bool)));
    }

  // finaly show the menu
  context_menu->exec (global_pos);
}
#endif


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
  QString command;
  if (documentation)
    command = "doc ";
  else
    command = "help ";
  emit execute_command_in_terminal_signal (command + _word_at_cursor);
}

void
octave_qscintilla::contextmenu_edit (bool)
{
  emit execute_command_in_terminal_signal (QString("edit ") + _word_at_cursor);
}

void
octave_qscintilla::contextmenu_run (bool)
{
  QStringList commands = selectedText ().split (QRegExp("[\r\n]"),
                                                QString::SkipEmptyParts);
  for (int i = 0; i < commands.size (); i++ )
    emit execute_command_in_terminal_signal (commands.at (i));
}

#endif
