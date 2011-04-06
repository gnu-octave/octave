/* Quint - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef CLIENT_H
#define CLIENT_H

#include "clientmanager.h"
#include <QProcess>
#include <QObject>
#include <QThread>
#include <QMutex>

/**
  * \class Client
  *
  * A client is a representation of another external unit Quint is communicating with.
  * Usually this will be the octave process. A client is launched by providing a command
  * that will be executed. It will handle the process itself.
  */
class Client : public QObject {
    Q_OBJECT
    friend class ClientManager;

public:
    QMutex *accessMutex();

public slots:
    void send(QString content);

signals:
    void dataAvailable(QString data);
    void errorAvailable(QString error);
    void lostConnection();

protected:
    /** Clients may only be created by the client manager. */
    Client(QString command);

private slots:
    void reemitDataAvailable();
    void reemitErrorAvailable();
    void handleProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

private:
    QString m_command;
    QProcess m_process;
    QThread m_thread;
    QMutex m_access;
};

/**
  * \class PendingRequest
  *
  * The PendingRequest class helps to communicate with clients. It automatically locks
  * the client until the PendingRequest is deleted. As soon as the request is being
  * answered, it will send out the answered()-Signal. It buffers all input and delivers
  * it by offering the fetchData() and fetchError()-methods. A request is considered
  * as answered, when the termination string is found in the buffer.
  */
class PendingRequest : public QObject {
    Q_OBJECT
public:
    PendingRequest(Client *client)
        : QObject(),
          m_client(client),
          m_dataBuffer(""),
          m_errorBuffer("") {
        client->accessMutex()->lock();
        connect(client, SIGNAL(dataAvailable(QString)), this, SLOT(receiveData(QString)));
        connect(client, SIGNAL(errorAvailable(QString)), this, SLOT(receiveError(QString)));
    }

    virtual ~PendingRequest() {
        m_client->accessMutex()->unlock();
    }

    QString fetchData() {
        QString content = m_dataBuffer, m_dataBuffer = "";
        return content;
    }

    QString fetchError() {
        QString content = m_errorBuffer, m_errorBuffer = "";
        return content;
    }

    void query(QString request, QRegExp terminator = QRegExp("(\r\n | \n\r | \n)+")) {
        m_terminator = terminator;
        QMetaObject::invokeMethod(m_client, "send", Q_ARG(QString, request));
    }

signals:
    void answered();

private slots:
    void receiveData(QString data) {
        m_dataBuffer += data;
        if(m_terminator.indexIn(m_dataBuffer) != -1)
            emit answered();
    }

    void receiveError(QString error) {
        m_errorBuffer += error;
        if(m_terminator.indexIn(m_dataBuffer) != -1)
            emit answered();
    }

private:
    Client *m_client;
    QString m_dataBuffer;
    QString m_errorBuffer;
    QString m_request;
    QRegExp m_terminator;
};

#endif // CLIENT_H
