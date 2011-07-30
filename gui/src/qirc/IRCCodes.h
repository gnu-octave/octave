/***************************************************************************
                          IRCCodes.h  -  description
                             -------------------
    begin                : Mon Sep 18 2000
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

#ifndef IRCCODES_H
#define IRCCODES_H

// *********                                                     ***********
// ************************  as seen on the RFC 2812 ***********************
// *********                                                     ***********

// COMMAND STRINGS
// - connection registration
#define COMMAND_PASS		 "PASS"
#define COMMAND_NICK 		 "NICK"
#define COMMAND_USER		 "USER"
#define COMMAND_OPER		 "OPER"
#define COMMAND_SERVICE  "SERVICE"
#define COMMAND_QUIT		 "QUIT"
#define COMMAND_SQUIT		 "SQUIT"
// - channel operations
#define COMMAND_JOIN		 "JOIN"
#define COMMAND_PART 		 "PART"
#define COMMAND_MODE		 "MODE"
#define COMMAND_TOPIC		 "TOPIC"
#define COMMAND_NAMES		 "NAMES"
#define COMMAND_LIST		 "LIST"
#define COMMAND_INVITE	 "INVITE"
#define COMMAND_KICK		 "KICK"
// - sending messages
#define COMMAND_PRIVMSG  "PRIVMSG"
#define COMMAND_NOTICE   "NOTICE"
// - server queries and commands
#define COMMAND_MOTD		 "MOTD"
#define COMMAND_LUSERS	 "LUSERS"
#define COMMAND_VERSION	 "VERSION"
#define COMMAND_STATS		 "STATS"
#define COMMAND_LINKS		 "LINKS"
#define COMMAND_TIME		 "TIME"
#define COMMAND_CONNECT  "CONNECT"
#define COMMAND_TRACE    "TRACE"
#define COMMAND_ADMIN    "ADMIN"
#define COMMAND_INFO		 "INFO"
// - service query and commands
#define COMMAND_SERVLIST "SERVLIST"
#define COMMAND_SQUERY	 "SQUERY"
// - user based queries
#define COMMAND_WHO			 "WHO"
#define COMMAND_WHOIS		 "WHOIS"
#define COMMAND_WHOWAS	 "WHOWAS"
// - miscellaneous messages
#define COMMAND_KILL		 "KILL"
#define COMMAND_PING		 "PING"
#define COMMAND_PONG		 "PONG"
#define COMMAND_ERROR		 "ERROR"
// - optional features
#define COMMAND_AWAY		 "AWAY"
#define COMMAND_REHASH	 "REHASH"
#define COMMAND_DIE			 "DIE"
#define COMMAND_RESTART  "RESTART"
#define COMMAND_SUMMON	 "SUMMON"
#define COMMAND_USERS		 "USERS"
#define COMMAND_OPERWALL "OPERWALL"
#define COMMAND_USERHOST "USERHOST"
#define COMMAND_ISON		 "ISON"

// COMMAND REPLIES

#define RPL_WELCOME				1
#define RPL_YOURHOST			2
#define RPL_CREATED				3
#define RPL_MYINFO        4
#define RPL_RPLBOUNCE     5
#define RPL_USERHOST      302
#define RPL_ISON          303
#define RPL_AWAY          301
#define RPL_UNAWAY        305
#define RPL_NOWAWAY       306
#define RPL_HWOISUSER     311
#define RPL_WHOISSERVER   312
#define RPL_WHOISOPERATOR 313
#define RPL_WHOISIDLE     317
#define RPL_ENOFWHOIS     318
#define RPL_WHOISCHANNELS 319
#define RPL_WHOWASUSER    314
#define RPL_ENDOFWHOWAS   369
#define RPL_LISTSTART     321
#define RPL_LIST          322
#define RPL_LISTEND       323
#define RPL_UNIQOPIS      325
#define RPL_CHANNELMODEIS 324
#define RPL_NOTOPIC       331
#define RPL_TOPIC         332
#define RPL_INVITING      341
#define RPL_SUMMONING     342
#define RPL_INVITELIST    346
#define RPL_ENDOFINVITELIST 347
#define RPL_EXCEPTLIST    348
#define RPL_ENDOFEXCEPTLIST 349
#define RPL_VERSION       351
#define RPL_WHOREPLY      352
#define RPL_ENDOFWHO      315
#define RPL_NAMREPLY      353
#define RPL_ENDOFNAMES    366
#define RPL_LINKS         364
#define RPL_ENDOFLINKS    367
#define RPL_BANLIST       368
#define RPL_INFO          371
#define RPL_ENDOFINFO     374
#define RPL_MOTDSTART     375
#define RPL_MOTD          372
#define RPL_ENDOFMOTD     376
#define RPL_YOUREOPER     381
#define RPL_REHASHING     382
#define RPL_YOURESERVICE  383
#define RPL_TIME          391
#define RPL_USERSSTART    392
#define RPL_USERS         393
#define RPL_ENDOFUSERS    394
#define RPL_NOUSERS       395
#define RPL_TRACELINK				200
#define RPL_TRACECONNECTING 201
#define RPL_TRACEHANDSHAKE	202
#define RPL_TRACEUNKNOWN    203
#define RPL_TRACEOPERATOR   204
#define RPL_TRACEUSER       205
#define RPL_TRACESERVER     206
#define RPL_TRACESERVICE    207
#define RPL_TRACENEWTYPE    208
#define RPL_TRACECLASS      209
#define RPL_TRACECONNECT    210
#define RPL_TRACELOG        261
#define RPL_TRACEEND        262
#define RPL_STATSLINKINFO   211
#define RPL_STATSCOMMANDS   212
#define RPL_ENDOFSTATS      219
#define RPL_STATSUPTIME     242
#define RPL_STATSONLINE     243
#define RPL_UMODEIS         221
#define RPL_SERVLIST        234
#define RPL_SERVLISTEND     235
#define RPL_LUSERCLIENT     251
#define RPL_LUSEROP         252
#define RPL_LUSERUNKNOWN    253
#define RPL_LUSERCHANNELS   254
#define RPL_LUSERME         255
#define RPL_ADMINME         256
#define RPL_ADMINLOC1       257
#define RPL_ADMINLOC2       258
#define RPL_ADMINEMAIL      259
#define RPL_TRYAGAIN        263

// ERRROR REPLIES

#define ERR_NOSUCHNICK      401
#define ERR_NOSUCHSERVER    402
#define ERR_NOSUCHCHANNEL   403
#define ERR_CANNOTSENDTOCHAN 404
#define ERR_TOOMANYCHANNELS 405
#define ERR_WASNOSUCHNICK   406
#define ERR_TOOMANYTARGETS  407
#define ERR_NOSUCHSERVICE   408
#define ERR_NOORIGIN        409
#define ERR_NORECPIENT      411
#define ERR_NOTEXTTOSEND    412
#define ERR_NOTOPLEVEL      413
#define ERR_WILDTOPLEVEL    414
#define ERR_BADMASK         415
#define ERR_UNKNOWNCOMMAND  421
#define ERR_NOMOTD          422
#define ERR_NOADMININFO     423
#define ERR_FILEERROR       424
#define ERR_NONICKNAMEGIVEN 431
#define ERR_ERRONEUSNICKNAME 432
#define ERR_NICKNAMEINUSE   433
#define ERR_NICKCOLLISION   436
#define ERR_UNAVAILRESOURCE 437
#define ERR_USERNOTINCHANNEL 441
#define ERR_NOTONCHANNEL    442
#define ERR_USERONCHANNEL   443
#define ERR_NOLOGIN         444
#define ERR_SUMMONDISABLED  445
#define ERR_USERSDISABLED   446
#define ERR_NOTREGISTERED   451
#define ERR_NEEDMOREPARAMS  461
#define ERR_ALREADYREGISTRED 462
#define ERR_NOPERMFORHOST   463
#define ERR_PASSWDMISMATCH  464
#define ERR_YOUREBANNEDCREEP 465
#define ERR_YOUWILLBEBANNED 466
#define ERR_KEYSET          467
#define ERR_CHANNELISFULL   471
#define ERR_UNKNOWNMODE     472
#define ERR_INVITEONLYCHANNEL 473
#define ERR_BANNEDFROMCHAN  474
#define ERR_BADCHANNELKEY   475
#define ERR_BADCHANMASK     476
#define ERR_NOCHANMODES     477
#define ERR_BANLISTFULL     478
#define ERR_NOPRIVILEGES    481
#define ERR_CHANOPPRIVNEEDED 482
#define ERR_CANTKILLSERVER  483
#define ERR_RESTRICTED      484
#define ERR_UNIQOPPRIVSNEEDED 485
#define ERR_NOOPERHOST      491
#define ERR_UMODELUNKNOWNFLAG 501
#define ERR_USERSDONTMATCH  502

#endif
