/*

Copyright (C) 2011 Michael Goffioul.

This file is part of QConsole.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

QConsole is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef __QConsole_h__
#define __QConsole_h__ 1

#include <QWidget>
#include "QTerminalInterface.h"
class QFocusEvent;
class QKeyEvent;
class QPainter;
class QPaintEvent;
class QResizeEvent;
class QWheelEvent;
class QPoint;

class QConsolePrivate;
class QConsoleThread;
class QConsoleView;

//////////////////////////////////////////////////////////////////////////////

class QWinTerminalImpl : public QTerminalInterface
{
  Q_OBJECT
  friend class QConsolePrivate;
  friend class QConsoleThread;
  friend class QConsoleView;

public:
  QWinTerminalImpl (QWidget* parent = 0);
  QWinTerminalImpl (const QString& cmd, QWidget* parent = 0);
  ~QWinTerminalImpl (void);

  void setTerminalFont (const QFont& font);
  void setSize (int columns, int lines);
  void sendText (const QString& s);
  void setCursorType (CursorType type, bool blinking);

public slots:
  void copyClipboard (void);
  void pasteClipboard (void);
  void blinkCursorEvent (void);

signals:
  void terminated (void);
  void titleChanged (const QString&);

protected:
  void viewPaintEvent (QConsoleView*, QPaintEvent*);
  void setBlinkingCursor (bool blink);
  void setBlinkingCursorState (bool blink);
  void viewResizeEvent (QConsoleView*, QResizeEvent*);
  void wheelEvent (QWheelEvent*);
  void focusInEvent (QFocusEvent*);
  void focusOutEvent (QFocusEvent*);
  void keyPressEvent (QKeyEvent*);
  bool winEvent (MSG*, long*);
  virtual void start (void);
  void mouseMoveEvent (QMouseEvent *event);
  void mousePressEvent (QMouseEvent *event);
  void mouseReleaseEvent (QMouseEvent *event);

private slots:
  void scrollValueChanged (int value);
  void monitorConsole (void);
  void updateSelection (void);

private:
  QConsolePrivate* d;
};

//////////////////////////////////////////////////////////////////////////////

#endif // __QConsole_h__
