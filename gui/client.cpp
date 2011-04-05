#include "client.h"

Client::Client(QString command)
    : QObject(),
      m_command(command) {
    m_thread.start();
    moveToThread(&m_thread);

    m_process.start(m_command, QProcess::ReadWrite);
    connect(&m_process, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(handleProcessFinished(int,QProcess::ExitStatus)));
    connect(&m_process, SIGNAL(readyReadStandardOutput()), this, SLOT(reemitDataAvailable()));
    connect(&m_process, SIGNAL(readyReadStandardError()), this, SLOT(reemitErrorAvailable()));
}

void Client::send(QString content) {
    m_process.write(content.toLocal8Bit());
}

void Client::reemitDataAvailable() {
    emit dataAvailable(m_process.readAllStandardOutput());
}

void Client::reemitErrorAvailable() {
    emit errorAvailable(m_process.readAllStandardError());
}

void Client::handleProcessFinished(int exitCode, QProcess::ExitStatus exitStatus) {
    m_process.start(m_command, QProcess::ReadWrite);
}
