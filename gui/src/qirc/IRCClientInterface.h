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

#ifndef IRCCLIENTINTERFACE_H
#define IRCCLIENTINTERFACE_H

#include <QString>
#include <QObject>
#include <QHostAddress>

#include "IRCCodes.h"

/**
  * \class IRCClientInterface
  * IRC Clients need to implement this interface.
  */
class IRCClientInterface : public QObject
{
  Q_OBJECT
public:
  IRCClientInterface () { }
  virtual ~IRCClientInterface () { }

public slots:
  // Connection state:
  virtual void connectToServer (const QHostAddress& host, int port) = 0;
  virtual void disconnect () = 0;
  virtual void reconnect () = 0;

  virtual bool isConnected () = 0;
  virtual const QHostAddress& host() = 0;
  virtual int port() = 0;

  virtual void enterChannel (const QString& channel) = 0;
  virtual void leaveChannel (const QString& channel, const QString& reason) = 0;

  // Messaging:
  virtual void focusChannel (const QString& channel) = 0;
  virtual void sendNicknameChangeRequest (const QString& nickname) = 0;
  virtual void sendMessage (const QString& message) = 0;

  virtual const QString& nickname () = 0;

signals:
  void newMessage (const QString& channel, const QString& sender, const QString& message);
  void connected (const QString& server);
  void disconnected ();

};

#endif // IRCCLIENTINTERFACE_H
