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

#ifndef LINUXTERMINALEMULATION_H
#define LINUXTERMINALEMULATION_H

#include "TerminalEmulation.h"
#include "KPtyDevice.h"

/**
  * @class LinuxTerminalEmulation
  * Implements the terminal emulation on Linux and Unix as well as Unix-like
  * systems, like MacOSX.
  */
class LinuxTerminalEmulation : public TerminalEmulation
{
  Q_OBJECT
public:
  LinuxTerminalEmulation ();
  ~LinuxTerminalEmulation ();

  /** @reimp */
  void processKeyEvent (QKeyEvent *keyEvent);

  /** @reimp */
  void transmitText (const QString &text);

private slots:
  void handleReadyRead ();

private:
  /** KPtyDevice from the Konsole sources. */
  KPtyDevice *m_pty;
};

#endif // LINUXTERMINALEMULATION_H
