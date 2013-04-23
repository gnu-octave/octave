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

#ifdef HAVE_QSCINTILLA

#include "lexer-octave-gui.h"
#include <qcolor.h>
#include <qfont.h>

// -----------------------------------------------------
// Some basic functions
// -----------------------------------------------------
lexer_octave_gui::lexer_octave_gui (QObject *p)
  : QsciLexer (p)
{
  // The API info that is used for auto completion
  // TODO: Where to store a file with API info (raw or prepared?)?
  // TODO: Also provide infos on octave-forge functions?
  // TODO: Also provide infos on function parameters?
  // By now, use the keywords-list from syntax highlighting
  QString keyword;
  QStringList keywordList;
  keyword = this->keywords (1);           // get whole string with all keywords
  keywordList = keyword.split (QRegExp ("\\s+"));  // split into single strings
  lexer_api = new QsciAPIs (this);
  if (lexer_api)
    {
      for (int i = 0; i < keywordList.size (); i++)  // add all keywords to API
        lexer_api->add (keywordList.at (i));
      lexer_api->prepare ();   // prepare API info ... this may take some time
    }
}

lexer_octave_gui::~lexer_octave_gui()
{
  if (lexer_api)
    delete lexer_api;
}

// -----------------------------------------------------------------------------
// Redefined functions to make an octave lexer from the abtract class Qscilexer.
//   Scintilla has an octave/matlab-lexer but the interface in Qscintilla is
//   only available in version 2.5.1. Redefining the following purely virtual
//   functions of the class QsciLexer () and the enum of available styles (see
//   lexer-octave-gui.h provides the functionality of the octave lexer.
// -----------------------------------------------------------------------------
const char *
lexer_octave_gui::language() const
{
  return "Octave";  // return the name of the language
}

const char *
lexer_octave_gui::lexer() const
{
  return "octave";  // return the name of the lexer
}

QString
lexer_octave_gui::description(int style) const
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
    return QString();  // no valid style, return empty string
}


// -----------------------------------------------------
// The set of default colors
// -----------------------------------------------------
QColor
lexer_octave_gui::defaultColor (int style) const
{
  switch (style)
    {
      case Default:
      case Operator:
        return QColor (0x00,0x00,0x00);

      case Comment:
        return QColor (0x00,0x7f,0x00);

      case Command:
        return QColor (0x7f,0x7f,0x00);

      case Number:
        return QColor (0x00,0x7f,0x7f);

      case Keyword:
        return QColor (0x00,0x00,0x7f);

      case SingleQuotedString:
      case DoubleQuotedString:
        return QColor (0x7f,0x00,0x7f);
    }

    return QsciLexer::defaultColor (style);
}

// -----------------------------------------------------
// The defaulot fonts
// -----------------------------------------------------
QFont
lexer_octave_gui::defaultFont (int style) const
{
  QFont font;

  switch (style)
    {
      case Keyword:
        font = QsciLexer::defaultFont (style);
        font.setBold(true);
        break;

      default:
        font = QsciLexer::defaultFont (style);
    }

  return font;
}

// -----------------------------------------------------
// The set of keywords for highlighting
// -----------------------------------------------------
const char *
lexer_octave_gui::keywords(int set) const
{
  if (set == 1)
      return resource_manager::octave_keywords ();

  return 0;
}

#endif
