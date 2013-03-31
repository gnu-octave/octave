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

#include "resource-manager.h"
#include <QObject>
#include <Qsci/qsciglobal.h>
#include <Qsci/qscilexeroctave.h>
#include <Qsci/qsciapis.h>

class lexer_octave_gui : public QsciLexerOctave
{
  Q_OBJECT

public:

  lexer_octave_gui (QObject *parent = 0);
  ~lexer_octave_gui ();
  virtual const char *keywords (int set) const;

private:
  lexer_octave_gui (const lexer_octave_gui &);
  lexer_octave_gui &operator= (const lexer_octave_gui &);
  QsciAPIs *lexer_api;
};

#endif
