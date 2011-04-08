/* This file is part of the KDE libraries

    Copyright (C) 2007 Oswald Buddenhagen <ossi@kde.org>

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

#ifndef kptydev_h
#define kptydev_h

#include "kpty.h"

#include <QtCore/QIODevice>

struct KPtyDevicePrivate;

#define Q_DECLARE_PRIVATE_MI(Class, SuperClass) \
    inline Class##Private* d_func() { return reinterpret_cast<Class##Private *>(SuperClass::d_ptr); } \
    inline const Class##Private* d_func() const { return reinterpret_cast<const Class##Private *>(SuperClass::d_ptr); } \
    friend class Class##Private;

/**
 * Encapsulates KPty into a QIODevice, so it can be used with Q*Stream, etc.
 */
class KPtyDevice : public QIODevice, public KPty { //krazy:exclude=dpointer (via macro)
    Q_OBJECT
    Q_DECLARE_PRIVATE_MI(KPtyDevice, KPty)

public:

    /**
     * Constructor
     */
    KPtyDevice(QObject *parent = 0);

    /**
     * Destructor:
     *
     *  If the pty is still open, it will be closed. Note, however, that
     *  an utmp registration is @em not undone.
     */
    virtual ~KPtyDevice();

    /**
     * Create a pty master/slave pair.
     *
     * @return true if a pty pair was successfully opened
     */
    virtual bool open(OpenMode mode = ReadWrite | Unbuffered);

    /**
     * Open using an existing pty master. The ownership of the fd
     * remains with the caller, i.e., close() will not close the fd.
     *
     * This is useful if you wish to attach a secondary "controller" to an
     * existing pty device such as a terminal widget.
     * Note that you will need to use setSuspended() on both devices to
     * control which one gets the incoming data from the pty.
     *
     * @param fd an open pty master file descriptor.
     * @param mode the device mode to open the pty with.
     * @return true if a pty pair was successfully opened
     */
    bool open(int fd, OpenMode mode = ReadWrite | Unbuffered);

    /**
     * Close the pty master/slave pair.
     */
    virtual void close();

    /**
     * Sets whether the KPtyDevice monitors the pty for incoming data.
     *
     * When the KPtyDevice is suspended, it will no longer attempt to buffer
     * data that becomes available from the pty and it will not emit any
     * signals.
     *
     * Do not use on closed ptys.
     * After a call to open(), the pty is not suspended. If you need to
     * ensure that no data is read, call this function before the main loop
     * is entered again (i.e., immediately after opening the pty).
     */
    void setSuspended(bool suspended);

    /**
     * Returns true if the KPtyDevice is not monitoring the pty for incoming
     * data.
     *
     * Do not use on closed ptys.
     *
     * See setSuspended()
     */
    bool isSuspended() const;

    /**
     * @return always true
     */
    virtual bool isSequential() const;

    /**
     * @reimp
     */
    bool canReadLine() const;

    /**
     * @reimp
     */
    bool atEnd() const;

    /**
     * @reimp
     */
    qint64 bytesAvailable() const;

    /**
     * @reimp
     */
    qint64 bytesToWrite() const;

    bool waitForBytesWritten(int msecs = -1);
    bool waitForReadyRead(int msecs = -1);


Q_SIGNALS:
    /**
     * Emitted when EOF is read from the PTY.
     *
     * Data may still remain in the buffers.
     */
    void readEof();

protected:
    virtual qint64 readData(char *data, qint64 maxSize);
    virtual qint64 readLineData(char *data, qint64 maxSize);
    virtual qint64 writeData(const char *data, qint64 maxSize);

private:
    Q_PRIVATE_SLOT(d_func(), bool _k_canRead())
    Q_PRIVATE_SLOT(d_func(), bool _k_canWrite())
};

#endif

