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

#ifndef OCTAVETERMINAL_H
#define OCTAVETERMINAL_H

#include <QMdiSubWindow>
#include <QLineEdit>
#include <QTextBrowser>
#include <QAction>
#include <QMenuBar>
#include <QMenu>
#include <QToolBar>
#include <QKeyEvent>

#include "client.h"
#include "terminalhighlighter.h"

class TerminalCommandLine : public QLineEdit {
    Q_OBJECT
public:
    TerminalCommandLine(QWidget *parent = 0)
        : QLineEdit(parent),
          m_commandHistoryIndex(0) {
    }

signals:
    void claimCommand(QString command);

protected:
    void keyPressEvent(QKeyEvent *keyEvent) {
        QString command;
        switch(keyEvent->key()) {
            case Qt::Key_Return:
                command = text();
                emit claimCommand(command);
                m_commandHistory.append(command);
                m_commandHistoryIndex = m_commandHistory.size();
                m_currentlyEditedCommand = "";
                setText("");
                break;

            case Qt::Key_Up:
                if(!m_commandHistory.empty())
                {
                    if(m_commandHistoryIndex == m_commandHistory.size())
                        m_currentlyEditedCommand = text();

                    m_commandHistoryIndex--;
                    if(m_commandHistoryIndex < 0)
                        m_commandHistoryIndex = m_commandHistory.size();

                    if(m_commandHistoryIndex == m_commandHistory.size())
                        setText(m_currentlyEditedCommand);
                    else
                        setText(m_commandHistory.at(m_commandHistoryIndex));
                }
                break;

            case Qt::Key_Down:
                if(!m_commandHistory.empty())
                {
                    if(m_commandHistoryIndex == m_commandHistory.size())
                        m_currentlyEditedCommand = text();

                    m_commandHistoryIndex++;
                    if(m_commandHistoryIndex > m_commandHistory.size())
                        m_commandHistoryIndex = 0;

                    if(m_commandHistoryIndex == m_commandHistory.size())
                        setText(m_currentlyEditedCommand);
                    else
                        setText(m_commandHistory.at(m_commandHistoryIndex));
                }
                break;

            default:
                QLineEdit::keyPressEvent(keyEvent);
                break;
        }
    }

private:
    QList<QString> m_commandHistory;
    QString m_currentlyEditedCommand;
    int m_commandHistoryIndex;
};

class OctaveTerminal : public QMdiSubWindow {
    Q_OBJECT
public:
    explicit OctaveTerminal(QWidget *parent = 0);

signals:

public slots:
    void sendCommand(QString command);
    void blockUserInput();
    void allowUserInput();

    void assignClient(Client* client);
    void showEnvironment();

protected slots:
    void handleDataFromClient(QString data);
    void handleErrorFromClient(QString error);

private:
    QToolBar *m_mainToolBar;
    QTextBrowser *m_octaveOutput;
    TerminalCommandLine *m_commandLine;
    Client *m_client;
    TerminalHighlighter *m_terminalHighlighter;
};

#endif // OCTAVETERMINAL_H
