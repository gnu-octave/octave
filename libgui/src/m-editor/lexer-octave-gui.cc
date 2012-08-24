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

#include "lexer-octave-gui.h"
#include <qcolor.h>
#include <qfont.h>

// -----------------------------------------------------
// Some basic functions
// -----------------------------------------------------
lexer_octave_gui::lexer_octave_gui(QObject *parent)
    : QsciLexer(parent)  // inherit from base lexer
{
}

lexer_octave_gui::~lexer_octave_gui()
{
}

const char *lexer_octave_gui::language() const
{
  return "Octave";  // return the name of the language
}

const char *lexer_octave_gui::lexer() const
{
  return "octave";  // return the name of the lexer
}

// -----------------------------------------------------
// The colors for syntax highlighting
// -----------------------------------------------------
QColor lexer_octave_gui::defaultColor(int style) const
{
    switch (style)
      {
        case Default:  // black
          return QColor(0x00,0x00,0x00);
        case Operator: // red
          return QColor(0xef,0x00,0x00);
        case Comment:  // gray
          return QColor(0x7f,0x7f,0x7f);
        case Command:  // blue-green
          return QColor(0x00,0x7f,0x7f);
        case Number:   // orange
          return QColor(0x7f,0x7f,0x00);
        case Keyword:  // blue
          return QColor(0x00,0x00,0xbf);
        case SingleQuotedString: // green
          return QColor(0x00,0x7f,0x00);
        case DoubleQuotedString: // green-yellow
          return QColor(0x4f,0x7f,0x00);
      }
    return QsciLexer::defaultColor(style);
}


// -----------------------------------------------------
// The font decorations for highlighting
// -----------------------------------------------------
QFont lexer_octave_gui::defaultFont(int style) const
{
    QFont f;

    switch (style)
      {
        case Comment: // default but italic
          f = QsciLexer::defaultFont(style);
          f.setItalic(true);
          break;
        case Keyword: // default
          f = QsciLexer::defaultFont(style);
          break;
        case Operator:  // default
          f = QsciLexer::defaultFont(style);
          break;
        default:        // default
          f = QsciLexer::defaultFont(style);
          break;
      }
    return f;   // return the selected font
}


// -----------------------------------------------------
// Style names
// -----------------------------------------------------
QString lexer_octave_gui::description(int style) const
{
    switch (style)
      {
        case Default:
          return tr("Default");
        case Comment:
          return tr("Comment");
        case Command:
          return tr("Command");
        case Number:
          return tr("Number");
        case Keyword:
          return tr("Keyword");
        case SingleQuotedString:
          return tr("Single-quoted string");
        case Operator:
          return tr("Operator");
        case Identifier:
          return tr("Identifier");
        case DoubleQuotedString:
          return tr("Double-quoted string");
      }
    return QString();
}


// -----------------------------------------------------
// The set of keywords for highlighting
// TODO: How to define a second set?
// -----------------------------------------------------
const char *lexer_octave_gui::keywords(int set) const
{
    if (set == 1)
      {
        return resource_manager::octave_keywords ();
      }
    return 0;
}

