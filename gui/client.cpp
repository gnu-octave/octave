#include "client.h"

Client::Client(QString command) {
    m_process.start(command, QProcess::ReadWrite);
    connect(&m_process, SIGNAL(readyRead()), this, SLOT(reemitDataAvailable()));
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

