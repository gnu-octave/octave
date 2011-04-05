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
