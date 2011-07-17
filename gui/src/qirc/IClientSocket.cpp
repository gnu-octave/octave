/***************************************************************************
                          IClientSocket.cpp  -  description
                             -------------------
    begin                : Fri Sep 15 2000
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


#include <IClientSocket.h>

#define ERROR_ICLIENTSOCKET_NOERROR		0
#define ERROR_ICLIENTSOCKET_NOSOCKET            1
#define ERROR_ICLIENTSOCKET_NOCONNECT           2
#define ERROR_ICLIENTSOCKET_NORESOLV            3
#define ERROR_ICLIENTSOCKET_NOREAD		4
#define ERROR_ICLIENTSOCKET_NOWRITE		5
#define ERROR_ICLIENTSOCKET_NOFCNTL		6

const char *ICLIENTSOCKET_MENSAJES[] = { "Nothing has happened, boss",
  "Can't create the socket to initiate the connection",
  "Can't connect to the destination",
  "Can't resolv the server name",
  "Problems during read operation",
  "Problems during write operation",
  "Can't make the socket non-blocking"
};

#define ICLIENT_MAX_MENSAJE_ERROR 300

char ICLIENTSOCKETmensajeError[ICLIENT_MAX_MENSAJE_ERROR + 1];

void
IClientSocket::inicializar ()
{
  handler = 0;
  puerto = 0;
  bzero ((void *) &ip, sizeof (in_addr));

  conectado = false;
  socketCreado = false;
  blocking = true;
  lastError = ERROR_ICLIENTSOCKET_NOERROR;
}

IClientSocket::IClientSocket ()
{
  inicializar ();
}


IClientSocket::~IClientSocket ()
{
  if (conectado == true)
    {
      close ();
    }
}

int
IClientSocket::getSocket ()
{
  return handler;
}

const char *
IClientSocket::strError ()
{
  switch (lastError)
    {
    case ERROR_ICLIENTSOCKET_NORESOLV:
      snprintf (ICLIENTSOCKETmensajeError, ICLIENT_MAX_MENSAJE_ERROR, "%s",
		ICLIENTSOCKET_MENSAJES[lastError]);
      break;
    default:
      snprintf (ICLIENTSOCKETmensajeError, ICLIENT_MAX_MENSAJE_ERROR,
		"%s - %s",
		ICLIENTSOCKET_MENSAJES[lastError],::
		strerror (lastErrnoValue));
      break;
    }
  return ICLIENTSOCKETmensajeError;
}

int
IClientSocket::connect ()
{
  if (conectado == true)
    {
      close ();
    }

  if ((handler = socket (AF_INET, SOCK_STREAM, 0)) < 0)
    {
      lastError = ERROR_ICLIENTSOCKET_NOSOCKET;
      lastErrnoValue = errno;
      return -1;
    }
  socketCreado = true;

  if (blocking == false)
    {
      if (fcntl (handler, F_SETFL, O_NONBLOCK) < 0)
	{
	  lastError = ERROR_ICLIENTSOCKET_NOFCNTL;
	  lastErrnoValue = errno;
	}
    }

  sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons (puerto);
  addr.sin_addr = ip;
  bzero (&(addr.sin_zero), 8);
  int respuesta =::connect (handler, (sockaddr *) & addr,
			    sizeof (sockaddr_in));

  if ((respuesta == 0)
      || ((respuesta < 0) && (errno == EINPROGRESS) && (blocking == false)))
    {
      conectado = true;
    }
  else
    {
      lastError = ERROR_ICLIENTSOCKET_NOCONNECT;
      lastErrnoValue = errno;
      return -1;
    }

  return 0;
}


int
IClientSocket::connect (int socket, bool block)
{
  if (socketCreado)
    {
      close ();
    }

  handler = socket;
  bzero ((void *) &ip, sizeof (in_addr));

  conectado = true;
  socketCreado = true;
  blocking = block;
  lastError = 0;

  if (block == false)
    {
      fcntl (handler, F_SETFL, O_NONBLOCK);
    }

  return 0;
}

int
IClientSocket::connect (in_addr addr, unsigned int p, bool block)
{
  if (socketCreado == true)
    {
      close ();
    }

  ip = addr;
  puerto = p;
  blocking = block;

  return connect ();
}

int
IClientSocket::connect (const char *server, unsigned int p, bool block)
{
  if (socketCreado == true)
    {
      close ();
    }

  puerto = p;
  blocking = block;

  if (resolv (server, &ip) < 0)
    {
      lastError = ERROR_ICLIENTSOCKET_NORESOLV;
      return -1;
    }
  return connect ();
}

long int
IClientSocket::read (char *data, long count)
{
  long done = 0;
  long res = 0;

  do
    {
      res = recv (handler, data + done, count - done, 0);

      if (res > 0)
	{
	  done += res;
	}
    }
  while ((((done < count) && (res > 0)) && (blocking == true))
	 || ((res < 0) && (errno == EINTR)));

  if (res < 0)
    {
      if (errno == EWOULDBLOCK)
	{
	  return 0;
	}
      else
	{
	  lastError = ERROR_ICLIENTSOCKET_NOREAD;
	  lastErrnoValue = errno;
	  return -1;
	}
    }
  else
    {
      return done;
    }
}

long
IClientSocket::write (const char *data, long count)
{
  long sent = 0;
  long response = 0;

  do
    {
      response = send (handler, data + sent, count - sent, 0);

      if (response > 0)
	{
	  sent += response;
	}
    }
  while (((sent < count) && (response >= 0))
	 || ((response < 0) && (errno == EWOULDBLOCK)));

  if (response < 0)
    {
      lastError = ERROR_ICLIENTSOCKET_NOWRITE;
      lastErrnoValue = errno;
      return -1;
    }
  return sent;
}

void
IClientSocket::close ()
{
  if (socketCreado == true)
    {
      ::close (handler);
      conectado = false;
      socketCreado = false;
      blocking = true;
    }
}

int
IClientSocket::resolv (const char *fqdn, in_addr * ip)
{
  hostent *host;
  host = gethostbyname (fqdn);
  if (host == NULL)
    {
      return -1;
    }

  bcopy ((void *) host->h_addr, (void *) ip, sizeof (in_addr));
  return 0;
}
