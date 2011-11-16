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

#ifndef WINTERMINALVIEW_H
#define WINTERMINALVIEW_H

#include "QConsole.h"
#include "AbstractTerminalView.h"

/**
  * @class WinTerminalView
  * A Windows terminal widget, based on QConsole.
  */
class WinTerminalView : public QConsole, public AbstractTerminalView
{
public:
  WinTerminalView (QWidget* parent = 0);
  ~WinTerminalView (void);

  // AbstractTerminalView interface
  void sendText (const QString&);
  QWidget* widget (void) { return this; }
};

#endif // WINTERMINALVIEW_H
