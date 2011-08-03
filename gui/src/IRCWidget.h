/* OctaveGUI - A graphical user interface for Octave
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
#include "IRCClientInterface.h"

class IRCWidget:public QWidget
{
Q_OBJECT public:
  explicit IRCWidget (QWidget * parent);
  void connectToServer ();

public slots:
  void showStatusMessage (const QString&);
  void showErrorMessage (const QString&);
  void showMessage (const QString& channel, const QString& sender, const QString& message);
  void showNotification (const QString& sender, const QString& message);

  void handleConnected (const QString& host);
  void joinOctaveChannel (const QString& nick);

  void handleLoggedIn (const QString& nick);
  void handleNickChange (const QString& oldNick, const QString& newNick);
  void handleUserJoined (const QString& nick, const QString& channel);
  void handleUserQuit (const QString& nick, const QString& reason);

  void nickPopup ();
  void sendMessage (QString);
  void sendInputLine ();
  void updateNickCompleter ();

private:
  IRCClientInterface *m_ircClientInterface;
  IRCChannelProxyInterface *m_octaveChannel;
  QTextEdit *m_chatWindow;
  QPushButton *m_nickButton;
  QLineEdit *m_inputLine;

  QString m_initialNick;
  bool m_autoIdentification;
  QString m_nickServPassword;
  QString m_settingsFile;
  QStringList m_nickList;
};

#endif // IRCWIDGET_H
