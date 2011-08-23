/*

   This file is part of the KDE libraries
   Copyright (C) 2002 Waldo Bastian <bastian@kde.org>
   Copyright (C) 2002-2003,2007-2008 Oswald Buddenhagen <ossi@kde.org>
   Copyright (C) 2010 KDE e.V. <kde-ev-board@kde.org>
     Author Adriaan de Groot <groot@kde.org>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; see the file COPYING.LIB.  If not, write to
   the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
*/

#include "kpty_p.h"

#include <sys/types.h>
#include <sys/ioctl.h>#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/param.h>

#include <errno.h>
#include <fcntl.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <grp.h>

#include <pty.h>
#define _UTMPX_COMPAT
#include <utmp.h>
#include <utmpx.h>

/* for HP-UX (some versions) the extern C is needed, and for other
   platforms it doesn't hurt */
extern "C"
{
#include <termios.h>
}

#if defined (__FreeBSD__) || defined (__NetBSD__) || defined (__OpenBSD__) || defined (__bsdi__) || defined(__APPLE__) || defined (__DragonFly__)
#define _tcgetattr(fd, ttmode) ioctl(fd, TIOCGETA, (char *)ttmode)
#else
#if defined(_HPUX_SOURCE) || defined(__Lynx__) || defined (__CYGWIN__) || defined(__sun)
#define _tcgetattr(fd, ttmode) tcgetattr(fd, ttmode)
#else
#define _tcgetattr(fd, ttmode) ioctl(fd, TCGETS, (char *)ttmode)
#endif
#endif

#if defined (__FreeBSD__) || defined (__NetBSD__) || defined (__OpenBSD__) || defined (__bsdi__) || defined(__APPLE__) || defined (__DragonFly__)
#define _tcsetattr(fd, ttmode) ioctl(fd, TIOCSETA, (char *)ttmode)
#else
#if defined(_HPUX_SOURCE) || defined(__CYGWIN__) || defined(__sun)
#define _tcsetattr(fd, ttmode) tcsetattr(fd, TCSANOW, ttmode)
#else
#define _tcsetattr(fd, ttmode) ioctl(fd, TCSETS, (char *)ttmode)
#endif
#endif

#include <QtCore/Q_PID>

#define TTY_GROUP "tty"

KPtyPrivate::KPtyPrivate (KPty * parent):
masterFd (-1),
slaveFd (-1),
ownMaster (true),
q_ptr (parent)
{
}

KPtyPrivate::~KPtyPrivate ()
{
}

KPty::KPty ():
d_ptr (new KPtyPrivate (this))
{
}

KPty::KPty (KPtyPrivate * d):
d_ptr (d)
{
  d_ptr->q_ptr = this;
}

KPty::~KPty ()
{
  close ();
  delete d_ptr;
}

bool
KPty::open ()
{
  Q_D (KPty);

  if (d->masterFd >= 0)
    return true;

  d->ownMaster = true;

  QByteArray ptyName;

  // Find a master pty that we can open ////////////////////////////////

  // Because not all the pty animals are created equal, they want to
  // be opened by several different methods.

  // We try, as we know them, one by one.

  char ptsn[PATH_MAX];
  if (::openpty (&d->masterFd, &d->slaveFd, ptsn, 0, 0))
    {
      d->masterFd = -1;
      d->slaveFd = -1;
      return false;
    }
  d->ttyName = ptsn;

  fcntl (d->masterFd, F_SETFD, FD_CLOEXEC);
  fcntl (d->slaveFd, F_SETFD, FD_CLOEXEC);

  return true;
}

bool
KPty::open (int fd)
{
  Q_D (KPty);

  if (d->masterFd >= 0)
    {
      return false;
    }

  d->ownMaster = false;

  int ptyno;
  if (!ioctl (fd, TIOCGPTN, &ptyno))
    {
      char buf[32];
      sprintf (buf, "/dev/pts/%d", ptyno);
      d->ttyName = buf;
    }
  else
    {
       return false;
    }

  d->masterFd = fd;
  if (!openSlave ())
    {
      d->masterFd = -1;
      return false;
    }

  return true;
}

void
KPty::closeSlave ()
{
  Q_D (KPty);

  if (d->slaveFd < 0)
    return;
  ::close (d->slaveFd);
  d->slaveFd = -1;
}

bool
KPty::openSlave ()
{
  Q_D (KPty);

  if (d->slaveFd >= 0)
    return true;
  if (d->masterFd < 0)
    {
      return false;
    }
  d->slaveFd =::open (d->ttyName.data (), O_RDWR | O_NOCTTY);
  if (d->slaveFd < 0)
    {
      return false;
    }
  fcntl (d->slaveFd, F_SETFD, FD_CLOEXEC);
  return true;
}

void
KPty::close ()
{
  Q_D (KPty);

  if (d->masterFd < 0)
    return;
  closeSlave ();
  if (d->ownMaster)
    {
      ::close (d->masterFd);
    }
  d->masterFd = -1;
}

void
KPty::setCTty ()
{
  Q_D (KPty);

  // Setup job control //////////////////////////////////

  // Become session leader, process group leader,
  // and get rid of the old controlling terminal.
  setsid ();

  // make our slave pty the new controlling terminal.
  ioctl (d->slaveFd, TIOCSCTTY, 0);

  // make our new process group the foreground group on the pty
  int pgrp = getpid ();
  tcsetpgrp (d->slaveFd, pgrp);
}

void
KPty::login (const char *user, const char *remotehost)
{
  struct utmp l_struct;

  memset (&l_struct, 0, sizeof (l_struct));
  // note: strncpy without terminators _is_ correct here. man 4 utmp

  if (user)
    strncpy (l_struct.ut_name, user, sizeof (l_struct.ut_name));

  if (remotehost)
    {
      strncpy (l_struct.ut_host, remotehost, sizeof (l_struct.ut_host));
    }

  l_struct.ut_time = time (0);
  utmpname (_PATH_UTMP);
  setutent ();
  pututline (&l_struct);
  endutent ();
  updwtmp (_PATH_WTMP, &l_struct);
}

void
KPty::logout ()
{
  Q_D (KPty);

  const char *str_ptr = d->ttyName.data ();
  if (!memcmp (str_ptr, "/dev/", 5))
    str_ptr += 5;
  else
    {
      const char *sl_ptr = strrchr (str_ptr, '/');
      if (sl_ptr)
	str_ptr = sl_ptr + 1;
    }

  struct utmp l_struct, *ut;

  memset (&l_struct, 0, sizeof (l_struct));
  strncpy (l_struct.ut_line, str_ptr, sizeof (l_struct.ut_line));
  utmpname (_PATH_UTMP);
  setutent ();
  if ((ut = getutline (&l_struct)))
    {
      memset (ut->ut_name, 0, sizeof (*ut->ut_name));
      memset (ut->ut_host, 0, sizeof (*ut->ut_host));
      ut->ut_time = time (0);
      pututline (ut);
    }
  endutent ();
}

bool
KPty::tcGetAttr (struct::termios * ttmode) const
{
  Q_D (const KPty);
  return _tcgetattr (d->masterFd, ttmode) == 0;
}

bool
KPty::tcSetAttr (struct::termios * ttmode)
{
  Q_D (KPty);
  return _tcsetattr (d->masterFd, ttmode) == 0;
}

bool
KPty::setWinSize (int lines, int columns)
{
  Q_D (KPty);

  struct winsize winSize;
  memset (&winSize, 0, sizeof (winSize));
  winSize.ws_row = (unsigned short) lines;
  winSize.ws_col = (unsigned short) columns;
  return ioctl (d->masterFd, TIOCSWINSZ, (char *) &winSize) == 0;
}

bool
KPty::setEcho (bool echo)
{
  struct::termios ttmode;
  if (!tcGetAttr (&ttmode))
    return false;
  if (!echo)
    ttmode.c_lflag &= ~ECHO;
  else
    ttmode.c_lflag |= ECHO;
  return tcSetAttr (&ttmode);
}

const char *
KPty::ttyName () const
{
  Q_D (const KPty);

  return d->ttyName.data ();
}

int
KPty::masterFd () const
{
  Q_D (const KPty);

  return d->masterFd;
}

int
KPty::slaveFd () const
{
  Q_D (const KPty);

  return d->slaveFd;
}
