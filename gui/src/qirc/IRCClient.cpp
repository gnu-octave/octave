/***************************************************************************
                          IRCClient.cpp  -  description
                             -------------------
    begin                : Sun Sep 17 2000
    copyright            : (C) 2000 by gerardo Puga
    email                : gere@mailroom.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "IRCClient.h"

#include <string.h>
#include <stdlib.h>

void
IRCClient::handleIncomingLine ()
{
  IRCEvent evento (lines);
  emit event (&evento);
  if (evento.isNumeric () == true)
    {
      if (evento.getNumeric () == RPL_WELCOME)
	{
	  m_loggedIn = true;
	  emit completedLogin (m_nickInUse.toStdString ().c_str ());
	}
      else if ((evento.getNumeric () == ERR_NICKNAMEINUSE)
	       && (m_loggedIn == false))
	{
	  if (testedNicks == 50)
	    {
	      emit
		connectionStatus ("Nicknames in use, aborting connection.");
	      disconnectFromServer ();
	    }
	  else
	    {
	      emit connectionStatus (QString ("Nickname %1 is in use.").
				     arg (m_nickInUse).toStdString ().
				     c_str ());
	      setNickInUse (QString ("%1%2").arg (m_nick1).arg (testedNicks));
	      sendCommand (1, COMMAND_NICK,
			   m_nickInUse.toStdString ().c_str ());
	      testedNicks++;
	    }
	}
      else if (evento.getNumeric () == ERR_PASSWDMISMATCH)
	{
	  emit
	    connectionStatus ("The password you provided seems to be wrong.");
	}
      else
	{
	  emit replyCode (&evento);
	}
      return;
    }

  QString command = evento.getCommand ();

  if (command == COMMAND_NICK)
    {
      emit nick (evento.getNick ().toStdString ().c_str (),
		 evento.getParam (0).toStdString ().c_str ());

      if (evento.getNick () == nickInUse ())
	setNickInUse (evento.getParam (0));

      return;
    }
  else if (command == COMMAND_QUIT)
    {
      emit quit (evento.getNick ().toStdString ().c_str (),
		 evento.getParam (0).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_JOIN)
    {
      emit join (evento.getNick ().toStdString ().c_str (),
		 evento.getParam (0).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_PART)
    {
      emit part (evento.getNick ().toStdString ().c_str (),
		 evento.getParam (0).toStdString ().c_str (),
		 evento.getParam (1).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_MODE)
    {
      emit mode (&evento);
      return;
    }
  else if (command == COMMAND_TOPIC)
    {
      emit topic (evento.getNick ().toStdString ().c_str (),
		  evento.getParam (0).toStdString ().c_str (),
		  evento.getParam (1).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_KICK)
    {
      emit kick (evento.getNick ().toStdString ().c_str (),
		 evento.getParam (0).toStdString ().c_str (),
		 evento.getParam (1).toStdString ().c_str (),
		 evento.getParam (2).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_INVITE)
    {
      emit invite (evento.getNick ().toStdString ().c_str (),
		   evento.getParam (1).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_PRIVMSG)
    {
      emit privateMessage (evento.getNick ().toStdString ().c_str (),
			   evento.getParam (0).toStdString ().c_str (),
			   evento.getParam (1).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_NOTICE)
    {
      emit notice (evento.getNick ().toStdString ().c_str (),
		   evento.getParam (0).toStdString ().c_str (),
		   evento.getParam (1).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_PING)
    {
      if (m_automaticPong == true)
	{
	  sendCommand (1, COMMAND_PONG, m_nickInUse.toStdString ().c_str ());
	}
      emit ping (evento.getParam (0).toStdString ().c_str ());
      return;
    }
  else if (command == COMMAND_ERROR)
    {
      emit error (evento.getParam (0).toStdString ().c_str ());
      terminateConnection ();
      return;
    }
  else
    {
      emit notRecognized (&evento);
    }
}

IRCClient::IRCClient (bool autoPong)
{
  m_clientSocket = new IClientSocket ();
  m_connected = false;
  m_loggedIn = false;
  lines[0] = 0;
  longitud = 0;

  m_readSocketNotifier = m_writeSocketNotifier = NULL;
  m_automaticPong = autoPong;
}

IRCClient::~IRCClient ()
{
  disconnectFromServer ();
  delete m_clientSocket;
}

int
IRCClient::getSocket ()
{
  return m_clientSocket->getSocket ();
}

bool
IRCClient::setAutoPong (bool aP)
{
  return (m_automaticPong = aP);
}

void
IRCClient::initializeReadingSocket (int socketHandler)
{
  Q_UNUSED (socketHandler);
  int error = 0;
  bool moreThanOne = false;
  char c;

  while ((m_connected == true)
	 && ((error = m_clientSocket->read (&c, 1)) > 0))
    {
      if ((longitud < 510) && (c != 10) && (c != 13))
	{
	  lines[longitud] = c;
	  lines[longitud + 1] = 0;
	  longitud++;
	}

      if (((c == 10) || (c == 13)) && (longitud > 0))
	{
	  handleIncomingLine ();
	  lines[0] = 0;
	  longitud = 0;
	}
      moreThanOne = true;
    }

  if ((error == 0) && (moreThanOne == false))
    {
      terminateConnection ();
    }

  if ((error < 0) && (m_connected == true))
    {
      terminateConnection ();
    }
}

void
IRCClient::initializeWritingSocket (int socketHandler)
{
  Q_UNUSED (socketHandler);
  m_writeSocketNotifier->setEnabled (false);
  int resultado = 0;
  socklen_t l = sizeof (resultado);
  getsockopt (m_clientSocket->getSocket (), SOL_SOCKET, SO_ERROR, &resultado,
	      &l);
  if (resultado != 0)
    {
      emit
	connectionStatus
	("<font color=\"#990000\"><b>Connection failed.</b></font>");
      terminateConnection ();
    }
  else
    {
      emit
	connectionStatus
	("<font color=\"#00AA00\"><b>Connection to server established.</b></font>");
      m_connected = true;
      m_readSocketNotifier->setEnabled (true);
      emit establishedConnection ();
      setNickInUse (m_nick1);
      m_loggedIn = false;

      emit connectionStatus (QString ("Attempting to login as %1.").
			     arg (m_nickInUse).toStdString ().c_str ());
      if (m_password.isNull () == false)
	{
	  sendCommand (1, COMMAND_PASS, m_password.toStdString ().c_str ());
	}
      sendCommand (4, COMMAND_USER, m_userName.toStdString ().c_str (),
		   "0", "0", m_realName.toStdString ().c_str ());
      sendCommand (1, COMMAND_NICK, m_nickInUse.toStdString ().c_str ());

      testedNicks = 1;
    }
}

void
IRCClient::sendNickChange (QString nick)
{
  sendCommand (1, COMMAND_NICK, nick.toStdString ().c_str ());
}

void
IRCClient::joinChannel (QString channel)
{
  emit connectionStatus (QString ("Joining channel %1.").arg (channel).
			 toStdString ().c_str ());
  sendCommand (1, COMMAND_JOIN, channel.toStdString ().c_str ());
  m_recentChannel = channel;
}

void
IRCClient::sendPublicMessage (QString message)
{
  sendCommand (2, COMMAND_PRIVMSG, m_recentChannel.toStdString ().c_str (),
	       message.toStdString ().c_str ());
}

void
IRCClient::connectToServer (const char *server,
			    int puerto,
			    const char *nick1,
			    const char *nick2,
			    const char *user,
			    const char *realName, const char *pass, int flags)
{
  Q_UNUSED (flags);
  if (m_readSocketNotifier != NULL)
    {
      if (m_connected == true)
	{
	  disconnectFromServer ();
	}
      else
	{
	  terminateConnection ();
	}
    }

  m_connected = false;
  IRCClient::m_nick1 = nick1;
  IRCClient::m_nick2 = nick2;
  IRCClient::m_userName = user;
  IRCClient::m_password = pass;
  IRCClient::m_realName = realName;

  if (m_clientSocket->connect (server, puerto, false) < 0)
    {
      emit
	connectionStatus
	("Can't resolve the server name. Connection aborted.");
      return;
    }

  m_writeSocketNotifier =
    new QSocketNotifier (m_clientSocket->getSocket (), QSocketNotifier::Write,
			 this);
  m_writeSocketNotifier->setEnabled (true);
  connect (m_writeSocketNotifier, SIGNAL (activated (int)), this,
	   SLOT (initializeWritingSocket (int)));

  m_readSocketNotifier =
    new QSocketNotifier (m_clientSocket->getSocket (), QSocketNotifier::Read,
			 this);
  m_readSocketNotifier->setEnabled (false);
  connect (m_readSocketNotifier, SIGNAL (activated (int)), this,
	   SLOT (initializeReadingSocket (int)));
  emit connectionStatus ("Trying to connect to IRC server, please wait..");
}

void
IRCClient::disconnectFromServer (const char *razon)
{
  lines[0] = 0;
  longitud = 0;

  if (m_readSocketNotifier == NULL)
    return;

  if (m_connected)
    {
      if (razon == 0)
	{
	  sendCommand (0, COMMAND_QUIT);
	}
      else
	{
	  sendCommand (1, COMMAND_QUIT, razon);
	}
    }

  delete m_readSocketNotifier;
  delete m_writeSocketNotifier;

  m_readSocketNotifier = m_writeSocketNotifier = NULL;
  m_clientSocket->close ();
  m_connected = m_loggedIn = false;
  emit disconnected ();
}

void
IRCClient::terminateConnection ()
{
  if (m_readSocketNotifier == NULL)
    return;

  delete m_readSocketNotifier;
  delete m_writeSocketNotifier;
  m_readSocketNotifier = m_writeSocketNotifier = NULL;
  m_clientSocket->close ();
  m_connected = m_loggedIn = false;
  emit disconnected ();
}


void
IRCClient::sendLine (const char *line)
{
  QString msg = QString ("%1%2").arg (line).arg (CRLF);
  m_clientSocket->write (msg.toStdString ().c_str (), msg.length ());
}

void
IRCClient::sendCommand (int numberOfCommands, const char *command, ...)
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


// ************************** IRCEvent **********************

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
