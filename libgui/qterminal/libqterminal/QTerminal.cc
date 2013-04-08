/*

Copyright (C) 2012 Michael Goffioul.
Copyright (C) 2012 Jacob Dawid.

This file is part of QTerminal.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

QTerminal is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "QTerminal.h"

void
QTerminal::notice_settings (const QSettings *settings)
{
  // QSettings pointer is checked before emitting.

  // Set terminal font:
  QFont term_font = QFont();
  term_font.setFamily(settings->value("terminal/fontName","Courier New").toString());
  term_font.setPointSize(settings->value("terminal/fontSize",10).toInt ());
  setTerminalFont (term_font);

  QString cursorType = settings->value ("terminal/cursorType","ibeam").toString ();
  bool cursorBlinking = settings->value ("terminal/cursorBlinking",true).toBool ();
  if (cursorType == "ibeam")
    setCursorType(QTerminalInterface::IBeamCursor, cursorBlinking);
  else if (cursorType == "block")
    setCursorType(QTerminalInterface::BlockCursor, cursorBlinking);
  else if (cursorType == "underline")
    setCursorType(QTerminalInterface::UnderlineCursor, cursorBlinking);
}

void
QTerminal::relay_command (const QString& command)
{
  sendText (command);
}
