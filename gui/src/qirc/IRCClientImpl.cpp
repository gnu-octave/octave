#include "IRCClientImpl.h"

IRCEvent::IRCEvent (const char *serverMessage)
{
  char prefix[MAX_LINE_LEN];
  int index = 0;

  nick = "";
  user = "";
  host = "";
  for (int i = 0; i < 15; i++)
    {
      param[i] = "";
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
                  nick += prefix[i];
                  break;
                case 1:
                  user += prefix[i];
                  break;
                default:
                  host += prefix[i];
                  break;
                }
            }
        }
    }

  command = getStringToken (serverMessage, index);
  command = command.toUpper ();

  paramCount = 0;
  while (serverMessage[index] != 0)
    {
      if ((serverMessage[index] == CHR_COLON) || (paramCount == 14))
        {

          if (serverMessage[index] == CHR_COLON)
            {
              index++;
            }

          param[paramCount] = (const char *) (serverMessage + index);
          index += strlen (serverMessage + index);
        }
      else
        {
          param[paramCount] = getStringToken (serverMessage, index);
        }
      paramCount++;
    }

  if (strlen (command.toStdString ().c_str ()) ==
      strspn (command.toStdString ().c_str (), DIGITS))
    {
      numeric = true;
      codeNumber = atoi (command.toStdString ().c_str ());
    }
  else
    {
      numeric = false;
    }
}

int
IRCEvent::getNumeric ()
{
  if (!numeric)
    {
      return -1;
    }
  else
    {
      return codeNumber;
    }
}

QString
IRCEvent::getParam (int index)
{
  if ((index < 0) || (index > 14))
    {
      return QString ();
    }
  else
    {
      return param[index];
    }
}

int
IRCEvent::skipSpaces (const char *line, int &index)
{
  while (line[index] == CHR_SPACE)
    {
      index++;
    }
  return index;
}

QString
IRCEvent::getStringToken (const char *line, int &index)
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
IRCEvent::getStringToken (QString line, int &index)
{
  return getStringToken (line.toStdString ().c_str (), index);
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

void
IRCClientImpl::sendJoinRequest (const QString& channel)
{
  sendCommand (1, COMMAND_JOIN, channel.toStdString ().c_str ());
  focusChannel (channel);
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
  sendCommand (1, COMMAND_NICK, nickname.toStdString ().c_str ());
}

void
IRCClientImpl::sendPublicMessage (const QString& message)
{
  sendCommand (2, COMMAND_PRIVMSG, m_focussedChannel.toStdString ().c_str (),
                message.toStdString ().c_str ());
}

void
IRCClientImpl::sendPrivateMessage (const QString &recipient, const QString &message)
{
  sendCommand (2, COMMAND_PRIVMSG,
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
  sendCommand (4, COMMAND_USER, "na", "0", "0", "na");
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
IRCClientImpl::handleIncomingLine (const QString &line)
{
  if (m_connected && !line.isEmpty())
    {
      IRCEvent ircEvent(line.toStdString().c_str());
      if (ircEvent.isNumeric () == true)
        {
          switch (ircEvent.getNumeric ())
            {
              case RPL_WELCOME:
                emit loggedIn (nickname ());
                break;
              case ERR_NICKNAMEINUSE:
              case ERR_NICKCOLLISION:
                emit debugMessage ("FIXME: Received nickname in use reply.");
                break;
              case ERR_PASSWDMISMATCH:
                emit debugMessage ("FIXME: Received password mismatch reply.");
                break;
              case RPL_MOTDSTART:
              case RPL_MOTD:
              case ERR_NOMOTD:
              case RPL_ENDOFMOTD:
                break;
              case RPL_NOTOPIC:
              case RPL_TOPIC:
                break;
              case RPL_NAMREPLY:
                /*
                m_nickList =
                  event->getParam (3).split (QRegExp ("\\s+"), QString::SkipEmptyParts);
                updateNickCompleter ();*/
                break;
            }
        }
      else
        {
          QString command = ircEvent.getCommand ();
          if (command == COMMAND_NICK)
            {
              emit nicknameChanged (ircEvent.getParam(0), ircEvent.getParam(1));
            }
          else if (command == COMMAND_QUIT)
            {
              emit userQuit (ircEvent.getNick (), ircEvent.getParam (0));
            }
          else if (command == COMMAND_JOIN)
            {
              emit userJoined (ircEvent.getNick (), ircEvent.getParam (0));
            }
          else if (command == COMMAND_PART)
            {
              emit debugMessage ("WRITEME: Received part.");
              //emit part (ircEvent.getNick ().toStdString ().c_str (),
              //           ircEvent.getParam (0).toStdString ().c_str (),
              //           ircEvent.getParam (1).toStdString ().c_str ());
            }
          else if (command == COMMAND_MODE)
            {
              emit debugMessage ("WRITEME: Received mode.");
              //emit mode (&ircEvent);
            }
          else if (command == COMMAND_TOPIC)
            {
              emit debugMessage ("WRITEME: Received topic.");
              //emit topic (ircEvent.getNick ().toStdString ().c_str (),
              //            ircEvent.getParam (0).toStdString ().c_str (),
              //            ircEvent.getParam (1).toStdString ().c_str ());
            }
          else if (command == COMMAND_KICK)
            {
              emit debugMessage ("WRITEME: Received kick.");
              //emit kick (ircEvent.getNick ().toStdString ().c_str (),
              //           ircEvent.getParam (0).toStdString ().c_str (),
              //           ircEvent.getParam (1).toStdString ().c_str (),
              //           ircEvent.getParam (2).toStdString ().c_str ());
            }
          else if (command == COMMAND_INVITE)
            {
              emit debugMessage ("WRITEME: Received invite.");
              //emit invite (ircEvent.getNick ().toStdString ().c_str (),
              //             ircEvent.getParam (1).toStdString ().c_str ());
            }
          else if (command == COMMAND_PRIVMSG)
            {
              emit message (ircEvent.getParam (0), ircEvent.getNick (), ircEvent.getParam (1));
            }
          else if (command == COMMAND_NOTICE)
            {
              emit notification (ircEvent.getNick ().toStdString ().c_str (),
                                 ircEvent.getParam (1).toStdString ().c_str ());
            }
          else if (command == COMMAND_PING)
            {
              sendCommand (1, COMMAND_PONG, m_nickname.toStdString ().c_str ());
            }
          else if (command == COMMAND_ERROR)
            {
              emit error (ircEvent.getParam (0));
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
IRCClientImpl::sendCommand (int numberOfCommands, const char *command, ...)
{
  char linea[513];
  char *parametro;
  va_list lp;

  strncpy (linea, command, 512);
  linea[512] = 0;
  va_start (lp, command);
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
