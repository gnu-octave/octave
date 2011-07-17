/***************************************************************************
                          IRCClient.h  -  description
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

#ifndef IRCCLIENT_H
#define IRCCLIENT_H

#include <QObject>
#include <QSocketNotifier>
#include "IClientSocket.h"
#include "IRCCodes.h"
#include <stdarg.h>

#define MAX_LINE_LEN 512
#define PARAM_MAX_COUNT 15
#define CHR_COLON ':'
#define CHR_SPACE ' '
#define CHR_ZERO '\0'
#ifdef Q_OS_LINUX
 #define CRLF "\n"
#else
 #define CRLF "\r\n"
#endif
#define DIGITS	"0123456789"

class IRCEvent
{
private:
    int codeNumber;
    bool numeric;

    QString nick, user, host;
    QString command;
    int  paramCount;
    QString param[PARAM_MAX_COUNT];

protected:
    int skipSpaces(const char *linea, int &index);
    QString getStringToken(const char *linea, int &index);
    QString getStringToken(QString linea, int &index);

public:
    IRCEvent(const char * serverMessage);

    bool isNumeric() { return numeric; }

    QString getNick() { return nick; }
    QString getUser() { return user; }
    QString getHost() { return host; }
    QString getCommand() { return command; }
    int getNumeric();

    int getParamCount() { return paramCount; }
    QString getParam(int index);
};

class IRCClient : public QObject
{
    Q_OBJECT
public:
    IRCClient(bool autoPong = true);
    ~IRCClient();

    int getSocket();
    bool setAutoPong(bool aP);

    void connectToServer(const char *server, int puerto, const char *m_nick1, const char * m_nick2, const char * m_userName, const char * m_realName, const char *m_password, int flags);
    void disconnectFromServer(const char *razon = 0);
    void terminateConnection();

    void sendLine(const char *lines);
    void sendCommand(int numberOfCommands, const char *command, ...);

    QString nickInUse() { return m_nickInUse; }
    void sendNickChange(QString nick);
    void joinChannel(QString channel);
    void sendPublicMessage(QString message);

protected slots:
    void initializeReadingSocket(int);
    void initializeWritingSocket(int);

signals:
    void establishedConnection();
    void connectionStatus(const char *mensaje);
    void completedLogin(const char * nick);
    void disconnected();

    void event(IRCEvent *ircEvent);
    void notRecognized(IRCEvent *e);
    void replyCode(IRCEvent *);

    void nick(const char *nick, const char *newNick) ;
    void quit(const char *nick, const char *razon);
    void join(const char *nick, const char *canal);
    void part(const char *nick, const char *canal, const char *mensaje);
    void mode(IRCEvent *evento);
    void topic(const char *nick, const char *canal, const char *topic);
    void invite(const char *nick, const char *canal);
    void kick(const char *nick, const char *canal, const char *echado, const char *razon);

    void privateMessage(const char *nick, const char *destino, const char *mensaje);
    void notice(const char *nick, const char *destino, const char *mensaje);

    void ping(const char *server);
    void error(const char *mensaje);

    void nickInUseChanged();

private:
    void setNickInUse(QString nick) { m_nickInUse = nick; emit nickInUseChanged(); }
    void handleIncomingLine();

    bool m_connected;
    bool m_loggedIn;
    int testedNicks;

    char lines[MAX_LINE_LEN+1]; // 513 ( para darle espacio al 0 )
    int longitud;
    IClientSocket *m_clientSocket;

    QSocketNotifier *m_readSocketNotifier, *m_writeSocketNotifier;

    QString m_nick1, m_nick2, m_password;
    QString m_nickInUse, m_userName, m_realName;
    QString m_recentChannel;
    bool m_automaticPong;
};

#endif
