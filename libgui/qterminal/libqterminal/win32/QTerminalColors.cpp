/*

Copyright (C) 2011-2019 Michael Goffioul

This file is part of QConsole.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not,
see <https://www.gnu.org/licenses/>.

*/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "QTerminalColors.h"

//////////////////////////////////////////////////////////////////////////////

QConsoleColors::QConsoleColors (void)
    : QMap<int, QColor> ()
{
  (*this)[0]  = Qt::white;
  (*this)[1]  = Qt::blue;
  (*this)[2]  = Qt::green;
  (*this)[3]  = Qt::cyan;
  (*this)[4]  = Qt::red;
  (*this)[5]  = Qt::magenta;
  (*this)[6]  = Qt::yellow;
  (*this)[7]  = Qt::black;
  (*this)[8]  = Qt::lightGray;
  (*this)[9]  = Qt::darkBlue;
  (*this)[10] = Qt::darkGreen;
  (*this)[11] = Qt::darkCyan;
  (*this)[12] = Qt::darkRed;
  (*this)[13] = Qt::darkMagenta;
  (*this)[14] = Qt::darkYellow;
  (*this)[15] = Qt::darkGray;
}
