#ifndef CLIENT_H
#define CLIENT_H

#include "clientmanager.h"
#include <QProcess>
#include <QObject>
#include <QMutex>

class Client : public QObject {
    Q_OBJECT
    friend class ClientManager;

public:
    void send(QString content);
    QString fetch();
    QString errorMessage();

signals:
    void dataAvailable();
    void errorAvailable();
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
    QMutex m_clientInRequest;
};

#endif // CLIENT_H
