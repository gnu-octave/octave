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

#ifndef IRCCLIENTIMPL_H
#define IRCCLIENTIMPL_H

#include <QTcpSocket>
#include <QHostInfo>
#include <QStringList>
#include <QTextDocument>
#include <QStringListModel>
#include "IRCClientInterface.h"

/**
  * \namespace IRCCommand
  * This namespace includes all IRC commands.
  */
namespace IRCCommand
{
  const QString Password = "PASS";
  const QString Nick = "NICK";
  const QString User = "USER";
  const QString Operation = "OPER";
  const QString Service = "SERVICE";
  const QString Quit = "QUIT";
  const QString ServerQuit = "SQUIT";

  const QString Join = "JOIN";
  const QString Part = "PART";
  const QString Mode = "MODE";
  const QString Topic = "TOPIC";
  const QString Names = "NAMES";
  const QString List = "LIST";
  const QString Invite = "INVITE";
  const QString Kick = "KICK";

  const QString PrivateMessage = "PRIVMSG";
  const QString Notice = "NOTICE";

  const QString MessageOfTheDay = "MOTD";
  const QString ListUsers = "LUSERS";
  const QString Version = "VERSION";
  const QString Stats = "STATS";
  const QString Links = "LINKS";
  const QString Time = "TIME";
  const QString Command = "CONNECT";
  const QString Trace = "TRACE";
  const QString Admin = "ADMIN";
  const QString Info = "INFO";

  const QString ServerList = "SERVLIST";
  const QString ServerQuery = "SQUERY";

  const QString Who = "WHO";
  const QString WhoIs = "WHOIS";
  const QString WhoWas = "WHOWAS";

  const QString Kill = "KILL";
  const QString Ping = "PING";
  const QString Pong = "PONG";
  const QString Error = "ERROR";

  const QString Away = "AWAY";
  const QString Rehash = "REHASH";
  const QString Die = "DIE";
  const QString Restart = "RESTART";
  const QString Summon = "SUMMON";
  const QString Users = "USERS";
  const QString OperatorWall = "OPERWALL";
  const QString UserHost = "USERHOST";
  const QString IsOn = "ISON";
};

/**
  * \namespace IRCReply
  * This namespace includes all numeric IRC replies.
  */
namespace IRCReply
{
  const int Welcome = 1;
  const int YourHost = 2;
  const int Created = 3;
  const int MyInfo = 4;
  const int ReplyBounce = 5;
  const int UserHost = 302;
  const int IsOn = 303;
  const int Away = 301;
  const int UnAway = 305;
  const int NoAway = 306;
  const int WhoIsUser = 311;
  const int WhoIsServer = 312;
  const int WhoIsOperator = 313;
  const int WhoIsIdle = 317;
  const int EndOfWhoIs = 318;
  const int WhoIsChannels = 319;
  const int WhoWasUser = 314;
  const int EndOfWhoWas = 369;
  const int ListStart = 321;
  const int List = 322;
  const int ListEnd = 323;
  const int UniqueOpIs = 325;
  const int ChannelModeIs = 324;
  const int NoTopic = 331;
  const int Topic = 332;
  const int Inviting = 341;
  const int Summoning = 342;
  const int InviteList = 346;
  const int EndOfInviteList = 347;
  const int ExceptList = 348;
  const int EndOfExceptList = 349;
  const int Version = 351;
  const int WhoReply = 352;
  const int EndOfWho = 315;
  const int NameReply = 353;
  const int EndOfNames = 366;
  const int Links = 364;
  const int EndOfLinks = 367;
  const int BanList = 368;
  const int Info = 371;
  const int EndOfInfo = 374;
  const int MessageOfTheDayStart = 375;
  const int MessageOfTheDay = 372;
  const int MessageOfTheDayEnd = 376;
  const int YouAreOperator = 381;
  const int Rehashing = 382;
  const int YouAreService = 383;
  const int Time = 391;
  const int UserStart = 392;
  const int Users = 393;
  const int EndOfUsers = 394;
  const int NoUsers = 395;
  const int TraceLink = 200;
  const int TraceConnecting = 201;
  const int TraceHandshake = 202;
  const int TraceUnknown = 203;
  const int TraceOperator = 204;
  const int TraceUser = 205;
  const int TraceServer = 206;
  const int TraceService = 207;
  const int TraceNewType = 208;
  const int TraceClass = 209;
  const int TraceConnect = 210;
  const int TraceLog = 261;
  const int TraceEnd = 262;
  const int StatsLinkInfo = 211;
  const int StatsCommands = 212;
  const int EndOfStats = 219;
  const int StatsUptime = 242;
  const int StatsOnline = 243;
  const int UModeIs = 221;
  const int ServerList = 234;
  const int ServerListEnd = 235;
  const int ListUserClient = 251;
  const int ListUserOperator = 252;
  const int ListUserUnknown = 253;
  const int ListUserChannels = 254;
  const int ListUserMe = 255;
  const int AdminMe = 256;
  const int AdminLoc1 = 257;
  const int AdminLoc2 = 258;
  const int AdminEmail = 259;
  const int TryAgain = 263;
};

/**
  * \namespace IRCError
  * This namespace includes all numeric IRC errors.
  */
namespace IRCError
{
  const int NoSuchNick = 401;
  const int NoSuchServer = 402;
  const int NoSuchChannel = 403;
  const int CannotSendToChannel = 404;
  const int TooManyChannels = 405;
  const int WasNoSuchNick = 406;
  const int TooManyTargets = 407;
  const int NoSuchService = 408;
  const int NoOrigin = 409;
  const int NoRecipient = 411;
  const int NoTextToSend = 412;
  const int NoTopLevel = 413;
  const int WildTopLevel = 414;
  const int BasMask = 415;
  const int UnknownCommand = 421;
  const int NoMessageOfTheDay = 422;
  const int NoAdminInfo = 423;
  const int FileError = 424;
  const int NoNickNameGiven = 431;
  const int ErroneusNick = 432;
  const int NicknameInUse = 433;
  const int NickCollision = 436;
  const int UnavailResource = 437;
  const int UserNotInChannel = 441;
  const int NotOnChannel = 442;
  const int UserOnChannel = 443;
  const int NoLogin = 444;
  const int SummonDisabled = 445;
  const int UsersDisabled = 446;
  const int NotRegistered = 451;
  const int NeedMoreParams = 461;
  const int AlreadyRegistered = 462;
  const int NoPermissionForHost = 463;
  const int PasswordMismatch = 464;
  const int YouAreBannedCreep = 465;
  const int YouWillBeBanned = 466;
  const int KeySet = 467;
  const int ChannelIsFull = 471;
  const int UnknownMode = 472;
  const int InviteOnlyChannel = 473;
  const int BannedFromChannel = 474;
  const int BadChannelKey = 475;
  const int BadChannelMask = 476;
  const int NoChannelModes = 477;
  const int BanListFull = 478;
  const int NoPrivileges = 481;
  const int ChannelOperatorPrivilegesNeeded = 482;
  const int CannotKillServer = 483;
  const int Restricted = 484;
  const int UniqueOperatorPrivilegesNeeded = 485;
  const int NoOperatorHost = 491;
  const int YourModeListUnknownFlag = 501;
  const int UsersDontMatch = 502;
};

/**
  * \class IRCServerMessage
  * The IRCServerMessage class is a wrapper for server messages.
  * It parses the server message into its single bits and makes these
  * available through Getter-methods.
  */
class IRCServerMessage
{
public:
  IRCServerMessage (const QString& serverMessage);

  bool isNumeric ()
  { return m_isNumeric; }

  QString nick ()
  { return m_nick; }

  QString command ()
  { return m_command; }

  int numericValue ();
  QString parameter (int index);

private:
  int         m_codeNumber;
  bool        m_isNumeric;
  QString     m_nick;
  QString     m_user;
  QString     m_host;
  QString     m_command;
  QStringList m_parameters;
};

/**
  * \class IRCChannelProxyImpl
  * Implements a handle to an IRC channel. This is usually provided by the
  * the IRC client class.
  */
class IRCChannelProxyImpl : public IRCChannelProxyInterface
{
  Q_OBJECT
public:
  IRCChannelProxyImpl (IRCClientInterface *clientInterface, const QString& channelName, QObject *parent = 0);
  QTextDocument *conversationModel ();
  QStringListModel *userListModel ();
  QString channelName ();

  void setNickList (const QStringList &nickList);
  void sendMessage (const QString& message);
  void sendJoinRequest ();
  void leave (const QString &reason);
public slots:
  void handleNickChange (const QString& oldNick, const QString& newNick);
  void handleJoin (const QString& nick);
private:
  QString             m_channelName;
  QStringList         m_userList;
  QStringListModel    m_userListModel;
  QTextDocument       m_conversationModel;
  IRCClientInterface *m_clientInterface;
};

/**
  * \class IRCClientImpl
  * Implements an IRC client. This class can maintain a connection to one server.
  * In order to interface an IRC channel, use the ircChannelProxy-method to retrieve
  * a handle.
  */
class IRCClientImpl : public IRCClientInterface
{
  Q_OBJECT
public:
  IRCClientImpl (QObject *parent = 0);
  ~IRCClientImpl ();

  const QString& nickname ();
  bool isConnected ();
  bool isLoggedIn ();
  const QHostAddress& host();
  int port();
  IRCChannelProxyInterface *ircChannelProxy(const QString& channel);
  void sendIRCCommand (const QString& command, const QStringList& arguments);

public slots:
  void connectToHost (const QHostAddress& host, int port, const QString& initialNick);
  void disconnect ();
  void reconnect ();

  void sendNicknameChangeRequest (const QString &nickname);
  void sendPrivateMessage (const QString &recipient, const QString &message);

signals:
  void debugMessage (const QString& message);

private slots:
  void handleConnected ();
  void handleDisconnected ();
  void handleReadyRead ();

private:
  void handleNicknameChanged (const QString& oldNick, const QString& newNick);
  void handleUserJoined (const QString& nick, const QString& channel);
  void handleUserQuit (const QString& nick, const QString& reason);
  void handleIncomingLine (const QString& line);
  void sendLine (const QString& line);

  QHostAddress                              m_host;
  int                                       m_port;
  QString                                   m_nickname;
  bool                                      m_connected;
  bool                                      m_loggedIn;
  QTcpSocket                                m_tcpSocket;
  QMap<QString, IRCChannelProxyInterface*>  m_channels;
};

#endif // IRCCLIENTIMPL_H
