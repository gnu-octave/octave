/*

Copyright (C) 2011-2024 The Octave Project Developers

This file is part of QConsole.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not,
see <https://www.gnu.org/licenses/>.

*/

#ifndef __QConsole_h__
#define __QConsole_h__ 1

#include <QWidget>
#include "QTerminal.h"
class QFocusEvent;
class QKeyEvent;
class QPainter;
class QPaintEvent;
class QResizeEvent;
class QWheelEvent;
class QPoint;
class QDragEnterEvent;
class QDropEvent;

class QConsolePrivate;
class QConsoleThread;
class QConsoleView;

//////////////////////////////////////////////////////////////////////////////

class QWinTerminalImpl : public QTerminal
{
  Q_OBJECT
  friend class QConsolePrivate;
  friend class QConsoleThread;
  friend class QConsoleView;

public:
  QWinTerminalImpl (QWidget *parent);
  ~QWinTerminalImpl ();

  void setTerminalFont (const QFont& font);
  void setSize (int columns, int lines);
  void sendText (const QString& s);
  void setCursorType (CursorType type, bool blinking);

  void setBackgroundColor (const QColor& color);
  void setForegroundColor (const QColor& color);
  void setSelectionColor (const QColor& color);
  void setCursorColor (bool useForegoundColor, const QColor& color);
  void setScrollBufferSize (int value);

  QString selectedText ();

  void has_extra_interrupt (bool);

public slots:
  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();
  void blinkCursorEvent ();
  void init_terminal_size ();

signals:
  void terminated ();
  void titleChanged (const QString&);

protected:
  void viewPaintEvent (QConsoleView *, QPaintEvent *);
  void setBlinkingCursor (bool blink);
  void setBlinkingCursorState (bool blink);
  void viewResizeEvent (QConsoleView *, QResizeEvent *);
  void wheelEvent (QWheelEvent *);
  void focusInEvent (QFocusEvent *);
  void focusOutEvent (QFocusEvent *);
  void keyPressEvent (QKeyEvent *);
  virtual void start ();
  void mouseMoveEvent (QMouseEvent *event);
  void mousePressEvent (QMouseEvent *event);
  void mouseReleaseEvent (QMouseEvent *event);
  void mouseDoubleClickEvent (QMouseEvent *event);
  void mouseTripleClickEvent (QMouseEvent *event);

  bool eventFilter (QObject *obj, QEvent *ev);

  void dragEnterEvent (QDragEnterEvent *event);
  void dropEvent (QDropEvent *event);

private slots:
  void horizontalScrollValueChanged (int value);
  void verticalScrollValueChanged (int value);
  void monitorConsole ();
  void updateSelection ();
  void tripleClickTimeout ();

private:
  QConsolePrivate *d;
  bool allowTripleClick;
  bool _extra_interrupt;
};

//////////////////////////////////////////////////////////////////////////////

#endif // __QConsole_h__
