/*  Copyright (C) 2008 e_k (e_k@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.
*/

#ifndef QTERMINALWIDGET_H
#define QTERMINALWIDGET_H

#include <QtGui>

struct TermWidgetImpl;
/**
  * \class QTerminalWidget
  * This class forms a widget class that can be inserted into other widgets.
  */
class QTerminalWidget:public QWidget
{
  Q_OBJECT
public:
  QTerminalWidget (int startnow = 1, QWidget * parent = 0);
  ~QTerminalWidget ();

  void openTeletype (int fd);

  /** Text codec, default is UTF-8. */
  void setTextCodec (QTextCodec * codec);

  /** Resize terminal widget. */
  void setSize (int h, int v);

  /** History size for scrolling, values below zero mean infinite. */
  void setHistorySize (int lines);

  /** Send some text to the terminal. */
  void sendText (const QString & text);

signals:
  /** Emitted, when the current program has finished. */
  void finished ();

  void scrollToBottomRequest ();

protected:
  virtual void resizeEvent (QResizeEvent *);

protected slots:
  void sessionFinished ();
  void terminalKeyPressed (QKeyEvent *keyEvent);

private:
  /** Performs initial operations on this widget. */
  void initialize ();
  TermWidgetImpl *m_impl;
};

#endif // QTERMINALWIDGET_H
