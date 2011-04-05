#ifndef CLIENT_H
#define CLIENT_H

#include "clientmanager.h"
#include <QProcess>
#include <QObject>
#include <QThread>

class Client : public QObject {
    Q_OBJECT
    friend class ClientManager;

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
    void handleProcessStatusChange(QProcess::ProcessState processState);

private:
    QString m_command;
    QProcess m_process;
    QThread m_thread;
};

#endif // CLIENT_H
