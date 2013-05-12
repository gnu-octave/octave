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

#if defined (Q_OS_WIN32)
# include "win32/QWinTerminalImpl.h"
#else
# include "unix/QUnixTerminalImpl.h"
#endif

QTerminal *
QTerminal::create (QWidget *xparent)
{
#if defined (Q_OS_WIN32)
  return new QWinTerminalImpl (xparent);
#else
  return new QUnixTerminalImpl (xparent);
#endif
}

void
QTerminal::notice_settings (const QSettings *settings)
{
  // QSettings pointer is checked before emitting.

  // Set terminal font:
  QFont term_font = QFont ();
  term_font.setFamily
    (settings->value ("terminal/fontName", "Courier New").toString ());

  term_font.setPointSize (settings->value ("terminal/fontSize", 10).toInt ());

  setTerminalFont (term_font);

  QString cursorType
    = settings->value ("terminal/cursorType", "ibeam").toString ();

  bool cursorBlinking
    = settings->value ("terminal/cursorBlinking", true).toBool ();

  if (cursorType == "ibeam")
    setCursorType (QTerminal::IBeamCursor, cursorBlinking);
  else if (cursorType == "block")
    setCursorType (QTerminal::BlockCursor, cursorBlinking);
  else if (cursorType == "underline")
    setCursorType (QTerminal::UnderlineCursor, cursorBlinking);

  bool cursorUseForegroundColor
    = settings->value ("terminal/cursorUseForegroundColor", true).toBool ();

  // FIXME -- we shouldn't duplicate this information here and in the
  // resource manager.
  QList<QColor> default_colors;

  default_colors << QColor(0,0,0)
                 << QColor(255,255,255)
                 << QColor(192,192,192)
                 << QColor(128,128,128);

  setForegroundColor
    (settings->value ("terminal/color_f",
                      QVariant (default_colors.at (0))).value<QColor> ());

  setBackgroundColor
    (settings->value ("terminal/color_b",
                      QVariant (default_colors.at (1))).value<QColor> ());

  setSelectionColor
    (settings->value ("terminal/color_s",
                      QVariant (default_colors.at (2))).value<QColor> ());

  setCursorColor
    (cursorUseForegroundColor,
     settings->value ("terminal/color_c",
                      QVariant (default_colors.at (3))).value<QColor> ());
}
