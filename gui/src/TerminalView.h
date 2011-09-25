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

#ifndef OCTAVETERMINAL_H
#define OCTAVETERMINAL_H
#include <QPlainTextEdit>
#include "TerminalEmulation.h"

class TerminalView:public QPlainTextEdit, Terminal
{
Q_OBJECT
public:
  TerminalView (QWidget * parent = 0);
  ~TerminalView ();

  TerminalEmulation *terminalEmulation ()
  {
    return m_terminalEmulation;
  }

  void sendText (QString text) { m_terminalEmulation->transmitText(text); }

  // Terminal Interface
  QTextCursor textCursor();
  void setTextCursor (const QTextCursor &cursor);
  void bell ();

protected:
  void keyPressEvent (QKeyEvent *keyEvent);
  void mousePressEvent (QMouseEvent *mouseEvent);
  void mouseDoubleClickEvent (QMouseEvent *mouseEvent);
  void wheelEvent (QWheelEvent *wheelEvent);

private:
  TerminalEmulation *m_terminalEmulation;
};
#endif // OCTAVETERMINAL_H
