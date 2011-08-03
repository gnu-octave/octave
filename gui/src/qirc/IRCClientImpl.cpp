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

IRCServerMessage::IRCServerMessage (const char *serverMessage)
{
  char prefix[MAX_LINE_LEN];
  int index = 0;

  n_nick = "";
  m_user = "";
  m_host = "";
  for (int i = 0; i < 15; i++)
    {
      m_parameter[i] = "";
    }

  if (serverMessage[0] == CHR_COLON)
    {
      index++;
      strcpy (prefix,
              getStringToken (serverMessage, index).toStdString ().c_str ());

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
                  n_nick += prefix[i];
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

  m_command = getStringToken (serverMessage, index);
  m_command = m_command.toUpper ();

  m_parameterCount = 0;
  while (serverMessage[index] != 0)
    {
      if ((serverMessage[index] == CHR_COLON) || (m_parameterCount == 14))
        {

          if (serverMessage[index] == CHR_COLON)
            {
              index++;
            }

          m_parameter[m_parameterCount] = (const char *) (serverMessage + index);
          index += strlen (serverMessage + index);
        }
      else
        {
          m_parameter[m_parameterCount] = getStringToken (serverMessage, index);
        }
      m_parameterCount++;
    }

  if (strlen (m_command.toStdString ().c_str ()) ==
      strspn (m_command.toStdString ().c_str (), DIGITS))
    {
      n_numeric = true;
      m_codeNumber = atoi (m_command.toStdString ().c_str ());
    }
  else
    {
      n_numeric = false;
    }
}

int
IRCServerMessage::numericValue ()
{
  if (!n_numeric)
    {
      return -1;
    }
  else
    {
      return m_codeNumber;
    }
}

QString
IRCServerMessage::parameter (int index)
{
  if ((index < 0) || (index > 14))
    {
      return QString ();
    }
  else
    {
      return m_parameter[index];
    }
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

}

void
IRCClientImpl::reconnect ()
{

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
  sendCommand (1, IRCCommand::Join, channel.toStdString ().c_str ());
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
  sendCommand (1, IRCCommand::Nick, nickname.toStdString ().c_str ());
}

void
IRCClientImpl::sendPublicMessage (const QString& message)
{
  sendCommand (2, IRCCommand::PrivateMessage, m_focussedChannel.toStdString ().c_str (),
                message.toStdString ().c_str ());
}

void
IRCClientImpl::sendPrivateMessage (const QString &recipient, const QString &message)
{
  sendCommand (2, IRCCommand::PrivateMessage,
                  recipient.toStdString ().c_str (),
                  message.toStdString ().c_str ());
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
  sendCommand (4, IRCCommand::User, "na", "0", "0", "na");
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
      IRCServerMessage ircEvent(line.toStdString().c_str());
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
              sendCommand (1, IRCCommand::Pong, m_nickname.toStdString ().c_str ());
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
IRCClientImpl::sendCommand (int numberOfCommands, const QString& command, ...)
{
  // TODO: Change this. We are coding C++, not C ;)
  char linea[513];
  char *parametro;
  const char *cmd = command.toStdString().c_str();
  va_list lp;

  strncpy (linea, cmd, 512);
  linea[512] = 0;
  va_start (lp, cmd);
  for (int i = 0; i < numberOfCommands; i++)
    {
      if (i == 15)
        break;
      parametro = va_arg (lp, char *);
      if (i == numberOfCommands - 1)
        {
          if (strlen (linea) + strlen (parametro) + 2 > 512)
            break;
          if (strchr (parametro, ' ') != NULL)
            {
              strcat (linea, " :");
            }
          else
            {
              strcat (linea, " ");
            }
          strcat (linea, parametro);
        }
      else
        {
          if (strlen (linea) + strlen (parametro) + 1 > 512)
            break;
          strcat (linea, " ");
          strcat (linea, parametro);
        }
    }
  va_end (lp);
  sendLine (linea);
}
