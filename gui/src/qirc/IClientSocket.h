/***************************************************************************
                          IClientSocket.h  -  description
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

#ifndef  ICLIENTSOCKET_H
#define ICLIENTSOCKET_H

#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>

class IClientSocket
{
private:
    int handler;
    in_addr ip;
    unsigned int puerto;

    int lastError, lastErrnoValue;

    bool conectado;
    bool socketCreado;
    bool blocking;

    void inicializar();
    int connect();

public:
    IClientSocket();
    ~IClientSocket();

    int getSocket();
    const char *strError();

    int connect(int socket, bool block = true);
    int connect(in_addr addr, unsigned int puerto, bool blocking = true);
    int connect(const char * server, unsigned int puerto, bool blocking = true);

    long int read(char *, long count);
    long int write(const char *, long count);

    void close();

    int resolv(const char * fqdn, in_addr * ip);
};

#endif
