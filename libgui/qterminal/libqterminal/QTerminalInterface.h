/*

Copyright (C) 2012 Michael Goffioul.
Copyright (C) 2012 Jacob Dawid.

This file is part of QTerminal.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

QTerminal is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef QTERMINALINTERFACE_H
#define QTERMINALINTERFACE_H

#include <QWidget>
#include <QMenu>

class QTerminalInterface : public QWidget
{
    Q_OBJECT
public:
    QTerminalInterface(QWidget *xparent = 0) : QWidget(xparent) {
      connect (this, SIGNAL(customContextMenuRequested(QPoint)),
               this, SLOT(handleCustomContextMenuRequested(QPoint)));

      setContextMenuPolicy (Qt::CustomContextMenu);

      _contextMenu = new QMenu (this);
      QAction *copyAction  = _contextMenu->addAction ("Copy");
      QAction *pasteAction = _contextMenu->addAction ("Paste");

      connect (copyAction, SIGNAL (triggered()), this, SLOT (copyClipboard()));
      connect (pasteAction, SIGNAL (triggered()), this, SLOT (pasteClipboard()));
    }
    virtual ~QTerminalInterface() { }

    virtual void setTerminalFont(const QFont& font) = 0;
    virtual void setSize(int h, int v) = 0;
    virtual void sendText(const QString& text) = 0;

    enum CursorType {
      UnderlineCursor,
      BlockCursor,
      IBeamCursor
    };

    virtual void setCursorType(CursorType type, bool blinking) {
        // Provide empty default impl in order to avoid conflicts with the win impl.
        Q_UNUSED(type);
        Q_UNUSED(blinking);
    }

public slots:
    virtual void copyClipboard() = 0;
    virtual void pasteClipboard() = 0;

    virtual void handleCustomContextMenuRequested(QPoint at) {
      _contextMenu->move (mapToGlobal(at));
      _contextMenu->show ();
    }

private:
    QMenu *_contextMenu;
};

#endif // QTERMINALINTERFACE_H
