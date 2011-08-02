#ifndef IRCCLIENTIMPL_H
#define IRCCLIENTIMPL_H

#include <QTcpSocket>
#include <QHostInfo>
#include "IRCClientInterface.h"

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
  int paramCount;
  QString param[PARAM_MAX_COUNT];

protected:
  int skipSpaces (const char *linea, int &index);
  QString getStringToken (const char *linea, int &index);
  QString getStringToken (QString linea, int &index);

public:
    IRCEvent (const char *serverMessage);

  bool isNumeric ()
  {
    return numeric;
  }

  QString getNick ()
  {
    return nick;
  }
  QString getUser ()
  {
    return user;
  }
  QString getHost ()
  {
    return host;
  }
  QString getCommand ()
  {
    return command;
  }
  int getNumeric ();

  int getParamCount ()
  {
    return paramCount;
  }
  QString getParam (int index);
};

class IRCClientImpl : public IRCClientInterface
{
  Q_OBJECT
public:
  IRCClientImpl();

public slots:
  void connectToHost (const QHostAddress& host, int port, const QString& initialNick);
  void disconnect ();
  void reconnect ();

  bool isConnected ();
  const QHostAddress& host();
  int port();

  void sendJoinRequest (const QString& channel);
  void leaveChannel (const QString& channel, const QString& reason);

  void focusChannel (const QString& channel);
  void sendNicknameChangeRequest (const QString &nickname);
  void sendPublicMessage (const QString& message);
  void sendPrivateMessage (const QString &recipient, const QString &message);

  const QString& nickname ();

signals:
  void debugMessage (const QString& message);

private slots:
  void handleConnected ();
  void handleDisconnected ();
  void handleReadyRead ();

private:
  void handleIncomingLine (const QString& line);
  void sendLine (const QString& line);
  void sendCommand (int numberOfCommands, const char *command, ...);

  QHostAddress  m_host;
  int           m_port;
  QString       m_nickname;
  bool          m_connected;
  QString       m_focussedChannel;

  QTcpSocket    m_tcpSocket;
};

#endif // IRCCLIENTIMPL_H
