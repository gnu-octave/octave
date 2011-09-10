/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 John P. Swensen, Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef TERMINALEMULATION_H
#define TERMINALEMULATION_H

#include <QObject>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QTextCursor>

class Terminal
{
  public:
    virtual QTextCursor textCursor () = 0;
    virtual void setTextCursor (const QTextCursor& cursor) = 0;

    virtual void bell () = 0;
};

class TerminalEmulation : public QObject
{
  Q_OBJECT
public:
  static TerminalEmulation *newTerminalEmulation (Terminal *terminal);
  TerminalEmulation ();
  virtual ~TerminalEmulation () {}

  virtual void processKeyEvent (QKeyEvent *keyEvent) = 0;
  virtual void transmitText (const QString& text) = 0;
protected:
  Terminal *m_terminal;
};

#endif // TERMINALEMULATION_H
