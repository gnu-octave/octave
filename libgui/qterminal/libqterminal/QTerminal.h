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

#ifndef QTERMINAL_H
#define QTERMINAL_H

#include <QSettings>
#include <QtGlobal>

#ifdef Q_OS_WIN32
    #include "win32/QWinTerminalImpl.h"
    class QTerminal : public QWinTerminalImpl
    {
        Q_OBJECT
    public:
        QTerminal(QWidget *xparent = 0)
            : QWinTerminalImpl(xparent) { }
        ~QTerminal() { }

    public slots:
        void notice_settings (const QSettings *settings);
        void relay_command (const QString& text);
    };
#else
    #include "unix/QUnixTerminalImpl.h"
    class QTerminal : public QUnixTerminalImpl
    {
        Q_OBJECT
    public:
        QTerminal(QWidget *xparent = 0)
            : QUnixTerminalImpl(xparent) { }
        ~QTerminal() { }

    public slots:
        void notice_settings (const QSettings *settings);
        void relay_command (const QString& command);
    };
#endif

#endif // QTERMINAL_H
