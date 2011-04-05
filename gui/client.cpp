#include "client.h"

Client::Client(QString command)
    : m_command(command) {
    m_process.start(m_command, QProcess::ReadWrite);
    connect(&m_process, SIGNAL(readyRead()), this, SLOT(reemitDataAvailable()));
    connect(&m_process, SIGNAL(stateChanged(QProcess::ProcessState)), this, SLOT(handleProcessStatusChange(QProcess::ProcessState)));
    connect(&m_process, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(handleProcessFinished(int,QProcess::ExitStatus)));
}

void Client::send(QString content) {
    m_process.write(content.toLocal8Bit());
}

QString Client::fetch() {
    QString fetchedInput(m_process.readAllStandardOutput());
    return fetchedInput;
}

QString Client::errorMessage() {
    QString error(m_process.readAllStandardError());
    return error;
}

void Client::reemitDataAvailable() {
    emit dataAvailable();
}

void Client::reemitErrorAvailable() {
    emit errorAvailable();
}

void Client::handleProcessFinished(int exitCode, QProcess::ExitStatus exitStatus) {
    m_process.start(m_command, QProcess::ReadWrite);
}

void Client::handleProcessStatusChange(QProcess::ProcessState processState) {
    switch(processState) {
        case QProcess::NotRunning:
            break;
        case QProcess::Starting:
            break;
        case QProcess::Running:
            break;
    };
}
