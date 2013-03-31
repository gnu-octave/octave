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
  : QsciLexerOctave (p)
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

  // get the settings from the settings file
  QSettings *settings = resource_manager::get_settings ();
}

lexer_octave_gui::~lexer_octave_gui()
{
  if (lexer_api)
    delete lexer_api;
}

// -----------------------------------------------------
// The set of keywords for highlighting
// -----------------------------------------------------
const char *lexer_octave_gui::keywords(int set) const
{
  if (set == 1)
      return resource_manager::octave_keywords ();

  return 0;
}

#endif
