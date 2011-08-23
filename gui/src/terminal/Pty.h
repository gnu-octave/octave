/*
    This file is part of Konsole, KDE's terminal emulator. 
    
    Copyright 2007-2008 by Robert Knight <robertknight@gmail.com>
    Copyright 1997,1998 by Lars Doelle <lars.doelle@on-line.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA.
*/

#ifndef PTY_H
#define PTY_H

// Qt
#include <QtCore/QStringList>
#include <QtCore/QVector>
#include <QtCore/QList>
#include <QtCore/QSize>

// KDE
#include "kptyprocess.h"

/**
 * The Pty class is used to start the terminal process, 
 * send data to it, receive data from it and manipulate 
 * various properties of the pseudo-teletype interface
 * used to communicate with the process.
 *
 * To use this class, construct an instance and connect
 * to the sendData slot and receivedData signal to
 * send data to or receive data from the process.
 *
 * To start the terminal process, call the start() method
 * with the program name and appropriate arguments. 
 */
class Pty:public KPtyProcess
{
Q_OBJECT public:

    /** 
     * Construct a process using an open pty master.
     * See KPtyProcess::KPtyProcess()
     */
  explicit Pty (int ptyMasterFd, QObject * parent = 0);
   ~Pty ();

  public slots:
    /** 
     * Sends data to the process currently controlling the 
     * teletype ( whose id is returned by foregroundProcessGroup() )
     *
     */
  void sendData (const QByteArray& data);

    signals:
    /**
     * Emitted when a new block of data is received from
     * the teletype.
     *
     */
  void receivedData (const QByteArray& data);

private slots:
  // called when data is received from the terminal process
  void dataReceived ();

};

#endif // PTY_H
