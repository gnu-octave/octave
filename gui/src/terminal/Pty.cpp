/*
    This file is part of Konsole, an X terminal.
    Copyright 1997,1998 by Lars Doelle <lars.doelle@on-line.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA.
*/

// Own
#include "kptyprocess.h"
#include "Pty.h"

// System
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <termios.h>
#include <signal.h>

// Qt
#include <QtCore/QStringList>

#include "kpty.h"
#include "kptydevice.h"

int
Pty::start (const QString & program,
            const QStringList & programArguments)
{
  clearProgram ();
  setProgram (program.toLatin1 (), programArguments.mid (1));

  struct::termios ttmode;
  pty ()->tcGetAttr (&ttmode);
  if (!_xonXoff)
    ttmode.c_iflag &= ~(IXOFF | IXON);
  else
    ttmode.c_iflag |= (IXOFF | IXON);


  pty ()->tcSetAttr(&ttmode);

  KProcess::start ();

  if (!waitForStarted ())
    return -1;
  return 0;
}

Pty::Pty (int masterFd, QObject * parent):
KPtyProcess (masterFd, parent)
{
  init ();
}

Pty::Pty (QObject * parent):KPtyProcess (parent)
{
  init ();
}

void
Pty::init ()
{
  _xonXoff = true;

  connect (pty (), SIGNAL (readyRead ()), this, SLOT (dataReceived ()));
  setPtyChannels (KPtyProcess::AllChannels);
}

Pty::~Pty ()
{
}

void
Pty::sendData (const QByteArray& data)
{
  if (!data.length ())
    return;
  pty ()->write (data);
}

void
Pty::dataReceived ()
{
  emit receivedData (pty ()->readAll ());
}

void
Pty::setupChildProcess ()
{
  KPtyProcess::setupChildProcess ();

  // reset all signal handlers
  // this ensures that terminal applications respond to 
  // signals generated via key sequences such as Ctrl+C
  // (which sends SIGINT)
  struct sigaction action;
  sigemptyset (&action.sa_mask);
  action.sa_handler = SIG_DFL;
  action.sa_flags = 0;
  for (int signal = 1; signal < NSIG; signal++)
    sigaction (signal, &action, 0L);
}
