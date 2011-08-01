#include "IRCClientImpl.h"

IRCClientImpl::IRCClientImpl ()
  : IRCClientInterface ()
{
  connect (&m_tcpSocket, SIGNAL (connected ()), this, SLOT (handleConnected ()));
  connect (&m_tcpSocket, SIGNAL (disconnected ()), this, SLOT (handleDisconnected ()));
  connect (&m_tcpSocket, SIGNAL (readyRead ()), this, SLOT (handleReadyRead ()));
}

void
IRCClientImpl::connectToServer (const QHostAddress& server, int port)
{
  m_tcpSocket.connectToHost(server, port);
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
IRCClientImpl::enterChannel (const QString& channel)
{
  Q_UNUSED (channel);
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
  Q_UNUSED (channel);
}

void
IRCClientImpl::sendNicknameChangeRequest (const QString &nickname)
{
  Q_UNUSED (nickname);
}

void
IRCClientImpl::sendMessage (const QString& message)
{
  Q_UNUSED (message);
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
}

void
IRCClientImpl::handleDisconnected ()
{
  m_connected = false;
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
  if (line.isEmpty())
    return;
}
