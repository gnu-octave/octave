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

#ifndef IRCWIDGET_H
#define IRCWIDGET_H

#include <QWidget>
#include <QTextEdit>
#include <QPushButton>
#include <QLineEdit>
#include <QCompleter>
#include "IRCClient.h"

class IRCWidget : public QWidget
{
    Q_OBJECT
public:
    explicit IRCWidget(QWidget *parent, QString settingsFile);
    void connectToServer();

signals:

public slots:
    void showStatusMessage(const char*);
    void joinOctaveChannel(const char*);
    void loginSuccessful(const char*);
    void showPrivateMessage(const char*, const char*, const char*);
    void showNotice(const char*, const char*, const char*);
    void showTopic(const char*, const char*, const char*);
    void showJoin(const char*, const char*);
    void showQuit(const char*, const char*);
    void showNickChange(const char*, const char*);
    void nickPopup();
    void sendMessage(QString);
    void sendInputLine();

    void handleNickInUseChanged();
    void handleReplyCode(IRCEvent *event);

    void updateNickCompleter();
private:
    IRCClient *m_ircClient;
    QTextEdit *m_chatWindow;
    QPushButton *m_nickButton;
    QLineEdit *m_inputLine;
    bool m_alternatingColor;

    QString getAlternatingColor() {
        m_alternatingColor = !m_alternatingColor;
        if(m_alternatingColor)
            return "#000077";
        return "#005533";
    }

    QString m_initialNick;
    bool m_autoIdentification;
    QString m_nickServPassword;
    QString m_settingsFile;
    QStringList m_nickList;
};

#endif // IRCWIDGET_H
