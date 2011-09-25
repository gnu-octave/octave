/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef LEXEROCTAVE_H
#define LEXEROCTAVE_H

#include "ResourceManager.h"
#include <QObject>

#include <Qsci/qsciglobal.h>
#include <Qsci/qscilexer.h>


class QSCINTILLA_EXPORT LexerOctaveGui : public QsciLexer
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

    LexerOctaveGui(QObject *parent = 0);
    virtual ~LexerOctaveGui();
    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    QFont defaultFont(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;

private:
    LexerOctaveGui(const LexerOctaveGui &);
    LexerOctaveGui &operator=(const LexerOctaveGui &);
};

#endif
