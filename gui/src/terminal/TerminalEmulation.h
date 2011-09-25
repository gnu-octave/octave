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

#ifndef TERMINALEMULATION_H
#define TERMINALEMULATION_H

#include <QObject>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QTextCursor>

/**
  * @class Terminal
  * Interface for a terminal. This is the counterpart for the terminal
  * emulation and must be implemented by the view part. It defines a basic set
  * of actions that must be supported by the view part.
  */
class Terminal
{
  public:
    /** Queries the current text cursor.
      * @return The current text cursor. */
    virtual QTextCursor textCursor () = 0;

    /** Sets the given text cursor.
      * @param cursor New text cursor to set. */
    virtual void setTextCursor (const QTextCursor& cursor) = 0;

    /** Called whenever the bell has been activated. */
    virtual void bell () = 0;
};

/**
  * @class TerminalEmulation
  * Interface for any terminal emulation. It's the counterpart to the Terminal.
  */
class TerminalEmulation : public QObject
{
  Q_OBJECT
public:
  /** Produces a new terminal emulation for a specific terminal.
    * @param terminal The terminal associated with the emulation
    * @return Terminal emulation. */
  static TerminalEmulation *newTerminalEmulation (Terminal *terminal);

  TerminalEmulation ();
  virtual ~TerminalEmulation () {}

  /** Processes an incoming key event.
    * @param keyEvent Key event to process. */
  virtual void processKeyEvent (QKeyEvent *keyEvent) = 0;

  /** Transmit a block of text straight to the terminal emulation.
    * @param text Text to be transmitted. */
  virtual void transmitText (const QString& text) = 0;
protected:
  /** The terminal this terminal emulation is connected to. */
  Terminal *m_terminal;
};

#endif // TERMINALEMULATION_H
