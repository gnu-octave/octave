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
#include "kprocess_p.h"
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


void
Pty::setWindowSize (int lines, int cols)
{
  _windowColumns = cols;
  _windowLines = lines;

  if (pty ()->masterFd () >= 0)
    pty ()->setWinSize (lines, cols);
}

QSize
Pty::windowSize () const
{
  return QSize (_windowColumns, _windowLines);
}

void
Pty::setFlowControlEnabled (bool enable)
{
  _xonXoff = enable;

  if (pty ()->masterFd () >= 0)
    {
      struct::termios ttmode;
      pty ()->tcGetAttr (&ttmode);
      if (!enable)
	ttmode.c_iflag &= ~(IXOFF | IXON);
      else
	ttmode.c_iflag |= (IXOFF | IXON);
      pty()->tcSetAttr(&ttmode);
    }
}

bool
Pty::flowControlEnabled () const
{
  if (pty ()->masterFd () >= 0)
    {
      struct::termios ttmode;
      pty ()->tcGetAttr (&ttmode);
      return ttmode.c_iflag & IXOFF && ttmode.c_iflag & IXON;
    }
  return false;
}

void
Pty::setUtf8Mode (bool enable)
{
#ifdef IUTF8
  _utf8 = enable;

  if (pty ()->masterFd () >= 0)
    {
      struct::termios ttmode;
      pty ()->tcGetAttr (&ttmode);
      if (!enable)
	ttmode.c_iflag &= ~IUTF8;
      else
	ttmode.c_iflag |= IUTF8;
      pty()->tcSetAttr(&ttmode);
    }
#endif
}

void
Pty::setErase (char erase)
{
  _eraseChar = erase;

  if (pty ()->masterFd () >= 0)
    {
      struct::termios ttmode;
      pty ()->tcGetAttr (&ttmode);
      ttmode.c_cc[VERASE] = erase;
      pty()->tcSetAttr(&ttmode);
    }
}

char
Pty::erase () const
{
  if (pty ()->masterFd () >= 0)
    {
      struct::termios ttyAttributes;
      pty ()->tcGetAttr (&ttyAttributes);
      return ttyAttributes.c_cc[VERASE];
    }

  return _eraseChar;
}

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
#ifdef IUTF8			// XXX not a reasonable place to check it.
  if (!_utf8)
    ttmode.c_iflag &= ~IUTF8;
  else
    ttmode.c_iflag |= IUTF8;
#endif

  if (_eraseChar != 0)
    ttmode.c_cc[VERASE] = _eraseChar;

  pty ()->tcSetAttr(&ttmode);
  pty ()->setWinSize (_windowLines, _windowColumns);

  KProcess::start ();

  if (!waitForStarted ())
    return -1;
  return 0;
}

void
Pty::setWriteable (bool writeable)
{
  struct stat sbuf;
  ::stat (pty ()->ttyName (), &sbuf);
  if (writeable)
    chmod (pty ()->ttyName (), sbuf.st_mode | S_IWGRP);
  else
    chmod (pty ()->ttyName (), sbuf.st_mode & ~(S_IWGRP | S_IWOTH));
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
  _windowColumns = 0;
  _windowLines = 0;
  _eraseChar = 0;
  _xonXoff = true;
  _utf8 = true;

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
