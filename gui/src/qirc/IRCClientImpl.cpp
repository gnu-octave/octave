#include "IRCClientImpl.h"

IRCClientImpl::IRCClientImpl ()
  : IRCClientInterface ()
{
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

}

const QHostAddress&
IRCClientImpl::host()
{

}

int
IRCClientImpl::port()
{

}

void
IRCClientImpl::enterChannel (const QString& channel)
{

}

void
IRCClientImpl::leaveChannel (const QString& channel, const QString& reason)
{
}

void
IRCClientImpl::focusChannel (const QString& channel)
{

}

void
IRCClientImpl::sendNicknameChangeRequest (const QString &nickname)
{

}

void
IRCClientImpl::sendMessage (const QString& message)
{

}

const QString&
IRCClientImpl::nickname ()
{

}
