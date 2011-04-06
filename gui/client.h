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

    void query(QString request) {
        QMetaObject::invokeMethod(m_client, "send", Q_ARG(QString, request));
    }

signals:
    void dataIncome();

private slots:
    void receiveData(QString data) {
        QRegExp octavePrompt("octave:[0-9]+>");
        m_dataBuffer += data;
        if(octavePrompt.indexIn(m_dataBuffer) != -1)
            emit dataIncome();
    }

    void receiveError(QString error) {
        QRegExp octavePrompt("octave:[0-9]+>");
        m_errorBuffer += error;
        if(octavePrompt.indexIn(m_dataBuffer) != -1)
            emit dataIncome();
    }

private:
    Client *m_client;
    QString m_dataBuffer;
    QString m_errorBuffer;
    QString m_request;
};

#endif // CLIENT_H
