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

#include "IRCClientImpl.h"

IRCServerMessage::IRCServerMessage (const QString& serverMessage)
{
  const char *message = serverMessage.toStdString().c_str();
  char prefix[512];
  int index = 0;

  m_nick = "";
  m_user = "";
  m_host = "";
  m_parameters.reserve (16);

  if (message[0] == CHR_COLON)
    {
      index++;
      strcpy (prefix,
              getStringToken (message, index).toStdString ().c_str ());

      int etapa = 0;
      for (unsigned int i = 0; i < strlen (prefix); i++)
        {
          switch (prefix[i])
            {
            case '!':
              etapa = 1;
              break;
            case '@':
              etapa = 2;
              break;
            default:
              switch (etapa)
                {
                case 0:
                  m_nick += prefix[i];
                  break;
                case 1:
                  m_user += prefix[i];
                  break;
                default:
                  m_host += prefix[i];
                  break;
                }
            }
        }
    }

  m_command = getStringToken (message, index);
  m_command = m_command.toUpper ();

  while (message[index] != 0)
    {
      if ((message[index] == CHR_COLON))
        {

          if (message[index] == CHR_COLON)
            {
              index++;
            }

          m_parameters.append ( (const char *) (message + index));
          index += strlen (message + index);
        }
      else
        {
          m_parameters.append (getStringToken (message, index));
        }
    }

  if (strlen (m_command.toStdString ().c_str ()) ==
      strspn (m_command.toStdString ().c_str (), DIGITS))
    {
      m_isNumeric = true;
      m_codeNumber = atoi (m_command.toStdString ().c_str ());
    }
  else
    {
      m_isNumeric = false;
    }
}

int
IRCServerMessage::numericValue ()
{
  if (m_isNumeric)
    return m_codeNumber;
  return -1;
}

QString
IRCServerMessage::parameter (int index)
{
  if (index >= 0 && index < m_parameters.size ())
    return m_parameters.at (index);
  return "";
}

int
IRCServerMessage::skipSpaces (const char *line, int &index)
{
  while (line[index] == CHR_SPACE)
    {
      index++;
    }
  return index;
}

QString
IRCServerMessage::getStringToken (const char *line, int &index)
{
  QString token ("");
  skipSpaces (line, index);
  while ((line[index] != CHR_SPACE) && (line[index] != CHR_ZERO))
    {
      token += line[index];
      index++;
    }

  skipSpaces (line, index);
  return token;
}

QString
IRCServerMessage::getStringToken (QString line, int &index)
{
  return getStringToken (line.toStdString ().c_str (), index);
}

IRCChannelProxy::IRCChannelProxy ()
  : IRCChannelProxyInterface ()
{

}

QTextDocument *
IRCChannelProxy::conversationModel ()
{
  return &m_conversationModel;
}

QStringListModel *
IRCChannelProxy::userListModel ()
{
  return &m_userListModel;
}

IRCClientImpl::IRCClientImpl ()
  : IRCClientInterface ()
{
  connect (&m_tcpSocket, SIGNAL (connected ()), this, SLOT (handleConnected ()));
  connect (&m_tcpSocket, SIGNAL (disconnected ()), this, SLOT (handleDisconnected ()));
  connect (&m_tcpSocket, SIGNAL (readyRead ()), this, SLOT (handleReadyRead ()));
}

void
IRCClientImpl::connectToHost (const QHostAddress& host, int port, const QString& initialNick)
{
  m_host = host;
  m_nickname = initialNick;
  m_tcpSocket.connectToHost(host, port);
}

void
IRCClientImpl::disconnect ()
{
  m_tcpSocket.disconnect ();
}

void
IRCClientImpl::reconnect ()
{
  disconnect ();
  connectToHost (m_host, m_port, m_nickname);
}

bool
IRCClientImpl::isConnected ()
{
  return m_connected;
}

const QHostAddress&
IRCClientImpl::host()
{
  return m_host;
}

int
IRCClientImpl::port()
{
  return m_port;
}

IRCChannelProxyInterface *
IRCClientImpl::ircChannelProxy (const QString &channel)
{
  if (m_channels.contains (channel))
    return m_channels[channel];
  return 0;
}

void
IRCClientImpl::sendJoinRequest (const QString& channel)
{
  sendIRCCommand (IRCCommand::Join, QStringList (channel));
}

void
IRCClientImpl::leaveChannel (const QString& channel, const QString& reason)
{
  Q_UNUSED (channel);
  Q_UNUSED (reason);
}

void
IRCClientImpl::focusChannel (const QString& channel)
{
  m_focussedChannel = channel;
}

void
IRCClientImpl::sendNicknameChangeRequest (const QString &nickname)
{
  sendIRCCommand (IRCCommand::Nick, QStringList (nickname));
}

void
IRCClientImpl::sendPublicMessage (const QString& message)
{
  QStringList arguments;
  arguments << m_focussedChannel;
  arguments << message;
  sendIRCCommand (IRCCommand::PrivateMessage, arguments);
}

void
IRCClientImpl::sendPrivateMessage (const QString &recipient, const QString &message)
{
  QStringList arguments;
  arguments << recipient;
  arguments << message;
  sendIRCCommand (IRCCommand::PrivateMessage, arguments);
}

const QString&
IRCClientImpl::nickname ()
{
  return m_nickname;
}

void
IRCClientImpl::handleConnected ()
{
  m_connected = true;
  QStringList arguments;
  arguments << "na" << "0" << "0" << "na";
  sendIRCCommand (IRCCommand::User, arguments);
  sendNicknameChangeRequest (m_nickname);
  emit connected (m_host.toString ());
}

void
IRCClientImpl::handleDisconnected ()
{
  m_connected = false;
  emit disconnected ();
}

void
IRCClientImpl::handleReadyRead ()
{
  QByteArray line;
  do
    {
      line = m_tcpSocket.readLine();
      if (line.size ())
        handleIncomingLine(QString(line));
      else
        break;
    }
  while (true);
}

void
IRCClientImpl::handleNicknameChanged (const QString &oldNick, const QString &newNick)
{
  emit nicknameChanged (oldNick, newNick);
}

void
IRCClientImpl::handleUserJoined (const QString &nick, const QString &channel)
{
  emit userJoined (nick, channel);
}

void
IRCClientImpl::handleUserQuit (const QString &nick, const QString &reason)
{
  emit userQuit (nick, reason);
}

void
IRCClientImpl::handleIncomingLine (const QString &line)
{
  if (m_connected && !line.isEmpty())
    {
      IRCServerMessage ircEvent(line);
      if (ircEvent.isNumericValue () == true)
        {
          switch (ircEvent.numericValue ())
            {
              case IRCReply::Welcome:
                emit loggedIn (nickname ());
                break;
              case IRCError::NicknameInUse:
              case IRCError::NickCollision:
                emit debugMessage ("FIXME: Received nickname in use reply.");
                break;
              case IRCError::PasswordMismatch:
                emit debugMessage ("FIXME: Received password mismatch reply.");
                break;
              case IRCReply::MessageOfTheDayStart:
              case IRCReply::MessageOfTheDay:
              case IRCReply::MessageOfTheDayEnd:
              case IRCError::NoMessageOfTheDay:
                break;
              case IRCReply::NoTopic:
              case IRCReply::Topic:
                break;
              case IRCReply::NameReply:

                //m_nickList = event->getParam (3).split (QRegExp ("\\s+"), QString::SkipEmptyParts);
                break;
            }
        }
      else
        {
          QString command = ircEvent.command ();
          if (command == IRCCommand::Nick)
            {
              handleNicknameChanged (ircEvent.parameter (0), ircEvent.parameter (1));
            }
          else if (command == IRCCommand::Quit)
            {
              handleUserQuit (ircEvent.nick (), ircEvent.parameter (0));
            }
          else if (command == IRCCommand::Join)
            {
              handleUserJoined(ircEvent.nick (), ircEvent.parameter (0));
            }
          else if (command == IRCCommand::Part)
            {
              emit debugMessage ("WRITEME: Received part.");
              //emit part (ircEvent.getNick ().toStdString ().c_str (),
              //           ircEvent.getParam (0).toStdString ().c_str (),
              //           ircEvent.getParam (1).toStdString ().c_str ());
            }
          else if (command == IRCCommand::Mode)
            {
              emit debugMessage ("WRITEME: Received mode.");
              //emit mode (&ircEvent);
            }
          else if (command == IRCCommand::Topic)
            {
              emit debugMessage ("WRITEME: Received topic.");
              //emit topic (ircEvent.getNick ().toStdString ().c_str (),
              //            ircEvent.getParam (0).toStdString ().c_str (),
              //            ircEvent.getParam (1).toStdString ().c_str ());
            }
          else if (command == IRCCommand::Kick)
            {
              emit debugMessage ("WRITEME: Received kick.");
              //emit kick (ircEvent.getNick ().toStdString ().c_str (),
              //           ircEvent.getParam (0).toStdString ().c_str (),
              //           ircEvent.getParam (1).toStdString ().c_str (),
              //           ircEvent.getParam (2).toStdString ().c_str ());
            }
          else if (command == IRCCommand::Invite)
            {
              emit debugMessage ("WRITEME: Received invite.");
              //emit invite (ircEvent.getNick ().toStdString ().c_str (),
              //             ircEvent.getParam (1).toStdString ().c_str ());
            }
          else if (command == IRCCommand::PrivateMessage)
            {
              emit message (ircEvent.parameter (0), ircEvent.nick (), ircEvent.parameter (1));
            }
          else if (command == IRCCommand::Notice)
            {
              emit notification (ircEvent.nick ().toStdString ().c_str (),
                                 ircEvent.parameter (1).toStdString ().c_str ());
            }
          else if (command == IRCCommand::Ping)
            {
              sendIRCCommand (IRCCommand::Pong, QStringList (m_nickname));
            }
          else if (command == IRCCommand::Error)
            {
              emit error (ircEvent.parameter (0));
            }
          else
            {
              emit debugMessage (QString("FIXME: Received unknown reply: %1").arg(command));
              // not recognized.
            }
        }
    }
}

void
IRCClientImpl::sendLine (const QString &line)
{
  if (m_connected)
    m_tcpSocket.write ((line + "\r\n").toStdString ().c_str ());
}

void
IRCClientImpl::sendIRCCommand (const QString &command, const QStringList &arguments)
{
  QString line = command;
  for (int i = 0; i < arguments.size (); i++)
    {
      bool applyColon = false;
      // Usually all parameters are separated by spaces.
      // The last parameter of the message may contain spaces, it is usually used
      // to transmit messages. In order to parse it correctly, if needs to be prefixed
      // with a colon, so the server knows to ignore all forthcoming spaces and has to treat
      // all remaining characters as a single parameter. If we detect any whitespace in the
      // last argument, prefix it with a colon:
      if ((i == arguments.size () - 1) && arguments.at (i).contains (QRegExp("\\s")))
        applyColon = true;
      line += QString (" %1%2").arg (applyColon ? ":" : "").arg (arguments.at (i));
    }
  sendLine (line);
}
