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

#ifndef LEXEROCTAVE_H
#define LEXEROCTAVE_H

#ifdef HAVE_QSCINTILLA

#include "resource-manager.h"
#include <QObject>
#include <Qsci/qsciglobal.h>
#include <Qsci/qscilexer.h>
#include <Qsci/qsciapis.h>

class lexer_octave_gui : public QsciLexer
{
  Q_OBJECT

  public:
  // the used styles
  enum
    {
      Default = 0,
      Comment = 1,
      Command = 2,
      Number = 3,
      Keyword = 4,
      SingleQuotedString = 5,
      Operator = 6,
      Identifier = 7,
      DoubleQuotedString = 8
    };

  lexer_octave_gui (QObject *parent = 0);
  ~lexer_octave_gui ();
  const char *language () const;
  const char *lexer () const;
  QColor defaultColor (int style) const;
  QFont defaultFont (int style) const;
  const char *keywords (int set) const;
  QString description (int style) const;

private:
  lexer_octave_gui (const lexer_octave_gui &);
  lexer_octave_gui &operator= (const lexer_octave_gui &);
  QsciAPIs *lexer_api;
};

#endif  // HAVE_QSCINTILLA

#endif  // LEXEROCTAVE_H
