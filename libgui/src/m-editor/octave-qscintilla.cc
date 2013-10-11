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

#include "octave-qscintilla.h"
#include "file-editor-tab.h"

octave_qscintilla::octave_qscintilla (QWidget *p)
    : QsciScintilla (p)
{ }

octave_qscintilla::~octave_qscintilla ()
{ }

void
octave_qscintilla::get_global_textcursor_pos (QPoint *global_pos, QPoint *local_pos)
{
  long position = SendScintilla (QsciScintillaBase::SCI_GETCURRENTPOS);
  long point_x  = SendScintilla
                    (QsciScintillaBase::SCI_POINTXFROMPOSITION,0,position);
  long point_y  = SendScintilla
                    (QsciScintillaBase::SCI_POINTYFROMPOSITION,0,position);
  *local_pos = QPoint (point_x,point_y);  // local cursor position
  *global_pos = mapToGlobal (*local_pos); // global position of cursor
}

// call documentation or help on the current word
void
octave_qscintilla::context_help_doc (bool documentation)
{
  QPoint global_pos, local_pos;
  get_global_textcursor_pos (&global_pos, &local_pos);
  _word_at_cursor = wordAtPoint (local_pos);
  QString lexer_name = lexer ()->lexer ();
  if ((lexer_name == "octave" || lexer_name == "matlab")
                              && !_word_at_cursor.isEmpty ())
    contextmenu_help_doc (documentation);
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
    { // context menu by mouse
      global_pos = e->globalPos ();            // global mouse position
      local_pos  = e->pos ();                  // local mouse position
    }
  else
    { // context menu by keyboard or other: get point of text cursor
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
        context_menu->addAction (tr ("Help on") + " " + _word_at_cursor,
                                this, SLOT (contextmenu_help (bool)));
        context_menu->addAction (tr ("Documentation on") + " " + _word_at_cursor,
                                this, SLOT (contextmenu_doc (bool)));
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

#endif
