/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Michael Goffioul (michael.goffioul@gmail.com)
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

#include "AbstractTerminalView.h"
#if defined (Q_OS_UNIX)
#include "TerminalView.h"
#elif defined (Q_OS_WIN)
#include "WinTerminalView.h"
#endif

AbstractTerminalView* AbstractTerminalView::create (QWidget* parent)
{
#if defined (Q_OS_UNIX)
  return new TerminalView (parent);
#elif defined (Q_OS_WIN)
  return new WinTerminalView (parent);
#else
  qFatal ("No terminal widget available for this platform.");
  return 0
#endif
}
