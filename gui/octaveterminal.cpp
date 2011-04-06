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

#include "octaveterminal.h"
#include <QVBoxLayout>
#include <QPushButton>

OctaveTerminal::OctaveTerminal(QWidget *parent) :
    QMdiSubWindow(parent),
    m_client(0) {
    setWindowTitle("Octave Terminal");

    setWidget(new QWidget(this));
    m_mainToolBar = new QToolBar(widget());
    m_octaveOutput = new QTextBrowser(widget());
    m_commandLine = new TerminalCommandLine(widget());

    QVBoxLayout *layout = new QVBoxLayout();
    layout->addWidget(m_mainToolBar);
    layout->addWidget(m_octaveOutput);
    layout->addWidget(m_commandLine);
    widget()->setLayout(layout);

    QPushButton *showEnvironmentButton = new QPushButton("Show Environment (who)");
    m_mainToolBar->addWidget(showEnvironmentButton);

    m_octaveOutput->setFontFamily("Monospace");
    m_octaveOutput->setReadOnly(true);

    blockUserInput();
    connect(m_commandLine, SIGNAL(claimCommand(QString)), this, SLOT(sendCommand(QString)));
    connect(showEnvironmentButton, SIGNAL(clicked()), this, SLOT(showEnvironment()));

    m_terminalHighlighter = new TerminalHighlighter(m_octaveOutput->document());
}

void OctaveTerminal::sendCommand(QString command) {
    addRequest(command + "\n");
}

void OctaveTerminal::blockUserInput() {
    m_commandLine->setEnabled(false);
}

void OctaveTerminal::allowUserInput() {
    m_commandLine->setEnabled(true);
    m_commandLine->setFocus();
}

void OctaveTerminal::assignClient(Client *client) {
    m_client = client;
    allowUserInput();
    addRequest("info\n");
}

void OctaveTerminal::showEnvironment() {
    addRequest("who\n");
}

void OctaveTerminal::handleAnsweredRequest() {
    allowUserInput();
    QString data = m_pendingRequest->fetchData();
    QString error = m_pendingRequest->fetchError();
    m_octaveOutput->append(data);
    m_octaveOutput->append(error);
    delete m_pendingRequest;
}

void OctaveTerminal::addRequest(QString command) {
    blockUserInput();
    m_pendingRequest = new PendingRequest(m_client);
    connect(m_pendingRequest, SIGNAL(dataIncome()), this, SLOT(handleAnsweredRequest()));
    m_pendingRequest->query(command);
}
