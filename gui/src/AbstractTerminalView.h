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

#ifndef ABSTRACTTERMINALVIEW_H
#define ABSTRACTTERMINALVIEW_H

#include <QString>

class QWidget;

/**
  * @class AbstractTerminalView
  * Abstract class defining the interface for any terminal view widget.
  */
class AbstractTerminalView
{
public:
  /** Sends text to the terminal. */
  virtual void sendText (const QString&) = 0;

  /** Gets the terminal widget. */
  virtual QWidget* widget (void) = 0;

  /** Creates a terminal view for the current platform. */
  static AbstractTerminalView* create (QWidget* parent = 0);
};

#endif // ABSTRACTTERMINALVIEW_H
