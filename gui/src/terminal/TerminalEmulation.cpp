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

#include "TerminalEmulation.h"

#ifdef Q_OS_UNIX
  #include "LinuxTerminalEmulation.h"
#endif

TerminalEmulation *TerminalEmulation::newTerminalEmulation (Terminal *terminal)
{
#ifdef Q_OS_UNIX
  TerminalEmulation *terminalEmulation = new LinuxTerminalEmulation ();
  terminalEmulation->m_terminal = terminal;
  return terminalEmulation;
#endif
}

TerminalEmulation::TerminalEmulation ()
  : QObject ()
{
}
