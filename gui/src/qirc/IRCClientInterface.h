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
#include <QTextDocument>
#include <QStringListModel>

class IRCClientInterface;
/**
  * \class IRCChannelProxyInterface
  * Interface for a handle to an IRC channel.
  */
class IRCChannelProxyInterface : public QObject
{
  Q_OBJECT
public:
  IRCChannelProxyInterface (IRCClientInterface *, const QString&, QObject *parent = 0) : QObject (parent) { }
  virtual ~IRCChannelProxyInterface () { }

  /** Returns the conversation model part. */
  virtual QTextDocument *conversationModel () = 0;

  /** Returns a string list model for the user list. */
  virtual QStringListModel *userListModel () = 0;

  /** Returns the name of this channel. */
  virtual QString channelName () = 0;

  /**
    * Overwrites the current nick list by settings a new nick list.
    * \arg nickList The new nick list to set.
    */
  virtual void setNickList (const QStringList& nickList) = 0;

  /**
    * Sends a public message onto this channel.
    * \arg message The message that should be sent.
    */
  virtual void sendMessage (const QString& message) = 0;

  /** Requests to join this channel. */
  virtual void sendJoinRequest () = 0;

  /**
    * Requests to leave this channel.
    * \arg reason Reason for leaving the channel.
    */
  virtual void leave (const QString& reason) = 0;

public slots:
  virtual void handleNickChange (const QString& oldNick, const QString& newNick) = 0;
  virtual void handleJoin (const QString& nick) = 0;
};

/**
  * \class IRCClientInterface
  * IRC Clients need to implement this interface.
  */
class IRCClientInterface : public QObject
{
  Q_OBJECT
public:
  IRCClientInterface (QObject *parent = 0) : QObject (parent) { }
  virtual ~IRCClientInterface () { }

  /** Returns the current nickname of this client. */
  virtual const QString& nickname () = 0;

  /** Returns true if connected to the server. */
  virtual bool isConnected () = 0;

  /**
    * Returns true if logged in to the server.
    * Note: There is a small difference between isConnected and isLoggedIn.
    * isConnected returns true if there is a physical connection to the server.
    * isLoggedIn only returns true if the server has already accepted you
    * and you are ready to log into channels.
    */
  virtual bool isLoggedIn () = 0;

  /** Returns the current host address. */
  virtual const QHostAddress& host() = 0;

  /** Returns the current port. */
  virtual int port() = 0;

  /**
    * Returns a handle to an IRC channel.
    * Note: Retrieving a handle does not mean you have joined this channel.
    * \arg channel The channel to retrieve a handle for.
    */
  virtual IRCChannelProxyInterface *ircChannelProxy(const QString& channel) = 0;

  /**
    * Send an IRC command to the server.
    * \arg command Command to send.
    * \arg arguments Arguments to send.
    */
  virtual void sendIRCCommand (const QString& command, const QStringList& arguments) = 0;

public slots:
  /**
    * Connects to a host.
    * \arg host The host to connect tp.
    * \arg port The port on which to connect to the host.
    * \arg initialNick The initial nick to use when attempting to login.
    */
  virtual void connectToHost (const QHostAddress& host, int port, const QString& initialNick) = 0;

  /** Disconnects from the host. */
  virtual void disconnect () = 0;

  /** Reconnects to the host. */
  virtual void reconnect () = 0;

  /**
    * Sends a request to change the nickname.
    * \arg nickname The new nickname to be requested.
    */
  virtual void sendNicknameChangeRequest (const QString& nickname) = 0;

  /**
    * Sends a private message.
    * \arg recipient The nickname or channel that message should be sent to.
    * \arg message The message that should be sent.
    */
  virtual void sendPrivateMessage (const QString& recipient, const QString& message) = 0;

signals:
  /**
    * Sent upon the arrival of a new message.
    * \arg channel The channel this message was sent from.
    * \arg sender The nickname of the sender.
    * \arg message The message that has been sent.
    */
  void newMessage (const QString& channel, const QString& sender, const QString& message);
  void message (const QString& channel, const QString& sender, const QString& message);

  /**
    * Sent when the connection to a server has been established.
    * \arg server The name of the server that the connection has been established to.
    */
  void connected (const QString& server);

  /** Sent when the connection to the server has been interrupted. */
  void disconnected ();

  /**
    * Sent when an error occurs.
    * \arg message A descriptive message of the error that occured.
    */
  void error (const QString& message);

  /**
    * Sent when a notification arrives.
    * \arg sender The source of the notification.
    * \arg message The notification.
    */
  void notification (const QString& sender, const QString& message);

  /**
    * Sent when a nickname changed.
    * \arg oldNick The previous nickname.
    * \arg newNick The new nickname.
    */
  void nicknameChanged (const QString& oldNick, const QString& newNick);

  /**
    * Sent when the nickname of this client changed.
    * \arg nick The new nickname of this client.
    */
  void userNicknameChanged (const QString& nick);

  /**
    * Sent when a user has joined a channel.
    * \arg nick Nickname of the user that joined the channel.
    * \arg channel Channel that this user joined.
    */
  void userJoined (const QString& nick, const QString& channel);

  /**
    * Sent when a user quits.
    * \arg nick Nickname of the user that quit.
    * \arg reason Reason of the user to quit.
    */
  void userQuit (const QString& nick, const QString& reason);

  /**
    * Sent when a user logged in.
    * \arg nick The nickname of the user that logged in.
    */
  void loggedIn (const QString& nick);

  /**
    * Sent when the server provides a userlist for a channel.
    * \arg channel The channel that userlist applies to.
    * \arg list The actual userlist.
    */
  void userList (const QString& channel, const QStringList& list);
};

#endif // IRCCLIENTINTERFACE_H
