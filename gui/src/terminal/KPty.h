/* This file is part of the KDE libraries

    Copyright (C) 2003,2007 Oswald Buddenhagen <ossi@kde.org>

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

#ifndef kpty_h
#define kpty_h

#ifdef __APPLE__
#ifndef Q_OS_MAC
#define Q_OS_MAC
#endif
#endif

#if defined(Q_OS_MAC)
#define HAVE_UTIL_H
#define HAVE_UTMPX
#define _UTMPX_COMPAT
#define HAVE_PTSNAME
#define HAVE_SYS_TIME_H
#else
#define HAVE_PTY_H
#endif

#define HAVE_OPENPTY

#include <QtCore/QByteArray>

#ifdef __sgi
#define __svr4__
#endif

#ifdef __osf__
#define _OSF_SOURCE
#include <float.h>
#endif

#ifdef _AIX
#define _ALL_SOURCE
#endif

// __USE_XOPEN isn't defined by default in ICC
// (needed for ptsname(), grantpt() and unlockpt())
#ifdef __INTEL_COMPILER
#ifndef __USE_XOPEN
#define __USE_XOPEN
#endif
#endif

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
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

#if defined(HAVE_PTY_H)
#include <pty.h>
#endif

#ifdef HAVE_LIBUTIL_H
#include <libutil.h>
#elif defined(HAVE_UTIL_H)
#include <util.h>
#endif

#define HAVE_UTMPX
#define _UTMPX_COMPAT

#ifdef HAVE_UTEMPTER
extern "C"
{
#include <utempter.h>
}
#else
#include <utmp.h>
#ifdef HAVE_UTMPX
#include <utmpx.h>
#endif
#if !defined(_PATH_UTMPX) && defined(_UTMPX_FILE)
#define _PATH_UTMPX _UTMPX_FILE
#endif
#if !defined(_PATH_WTMPX) && defined(_WTMPX_FILE)
#define _PATH_WTMPX _WTMPX_FILE
#endif
#endif

/* for HP-UX (some versions) the extern C is needed, and for other
   platforms it doesn't hurt */
extern "C"
{
#include <termios.h>
#if defined(HAVE_TERMIO_H)
#include <termio.h>		// struct winsize on some systems
#endif
}

#if defined (_HPUX_SOURCE)
#define _TERMIOS_INCLUDED
#include <bsdtty.h>
#endif

#ifdef HAVE_SYS_STROPTS_H
#include <sys/stropts.h>	// Defines I_PUSH
#define _NEW_TTY_CTRL
#endif

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

#include <QtCore/qglobal.h>
#include <QByteArray>

class KPty;
struct KPtyPrivate
{
  Q_DECLARE_PUBLIC (KPty) KPtyPrivate (KPty * parent);
  virtual ~ KPtyPrivate ();
  bool chownpty (bool grant);

  int masterFd;
  int slaveFd;
  bool ownMaster:1;

  QByteArray ttyName;

  KPty *q_ptr;
};

struct termios;

/**
 * Provides primitives for opening & closing a pseudo TTY pair, assigning the
 * controlling TTY, utmp registration and setting various terminal attributes.
 */
class KPty
{
Q_DECLARE_PRIVATE (KPty) public:

  /**
   * Constructor
   */
  KPty ();

  /**
   * Destructor:
   *
   *  If the pty is still open, it will be closed. Note, however, that
   *  an utmp registration is @em not undone.
  */
  ~KPty ();

  /**
   * Create a pty master/slave pair.
   *
   * @return true if a pty pair was successfully opened
   */
  bool open ();

  /**
   * Open using an existing pty master.
   *
   * @param fd an open pty master file descriptor.
   *   The ownership of the fd remains with the caller;
   *   it will not be automatically closed at any point.
   * @return true if a pty pair was successfully opened
   */
  bool open (int fd);

  /**
   * Close the pty master/slave pair.
   */
  void close ();

  /**
   * Close the pty slave descriptor.
   *
   * When creating the pty, KPty also opens the slave and keeps it open.
   * Consequently the master will never receive an EOF notification.
   * Usually this is the desired behavior, as a closed pty slave can be
   * reopened any time - unlike a pipe or socket. However, in some cases
   * pipe-alike behavior might be desired.
   *
   * After this function was called, slaveFd() and setCTty() cannot be
   * used.
   */
  void closeSlave ();

  /**
   * Open the pty slave descriptor.
   *
   * This undoes the effect of closeSlave().
   *
   * @return true if the pty slave was successfully opened
   */
  bool openSlave ();

  /**
   * Creates a new session and process group and makes this pty the
   * controlling tty.
   */
  void setCTty ();

  /**
   * Creates an utmp entry for the tty.
   * This function must be called after calling setCTty and
   * making this pty the stdin.
   * @param user the user to be logged on
   * @param remotehost the host from which the login is coming. This is
   *  @em not the local host. For remote logins it should be the hostname
   *  of the client. For local logins from inside an X session it should
   *  be the name of the X display. Otherwise it should be empty.
   */
  void login (const char *user = 0, const char *remotehost = 0);

  /**
   * Removes the utmp entry for this tty.
   */
  void logout ();

  /**
   * Wrapper around tcgetattr(3).
   *
   * This function can be used only while the PTY is open.
   * You will need an #include &lt;termios.h&gt; to do anything useful
   * with it.
   *
   * @param ttmode a pointer to a termios structure.
   *  Note: when declaring ttmode, @c struct @c ::termios must be used -
   *  without the '::' some version of HP-UX thinks, this declares
   *  the struct in your class, in your method.
   * @return @c true on success, false otherwise
   */
  bool tcGetAttr (struct::termios * ttmode) const;

  /**
   * Wrapper around tcsetattr(3) with mode TCSANOW.
   *
   * This function can be used only while the PTY is open.
   *
   * @param ttmode a pointer to a termios structure.
   * @return @c true on success, false otherwise. Note that success means
   *  that @em at @em least @em one attribute could be set.
   */
  bool tcSetAttr (struct::termios * ttmode);

  /**
   * Change the logical (screen) size of the pty.
   * The default is 24 lines by 80 columns.
   *
   * This function can be used only while the PTY is open.
   *
   * @param lines the number of rows
   * @param columns the number of columns
   * @return @c true on success, false otherwise
   */
  bool setWinSize (int lines, int columns);

  /**
   * Set whether the pty should echo input.
   *
   * Echo is on by default.
   * If the output of automatically fed (non-interactive) PTY clients
   * needs to be parsed, disabling echo often makes it much simpler.
   *
   * This function can be used only while the PTY is open.
   *
   * @param echo true if input should be echoed.
   * @return @c true on success, false otherwise
   */
  bool setEcho (bool echo);

  /**
   * @return the name of the slave pty device.
   *
   * This function should be called only while the pty is open.
   */
  const char *ttyName () const;

  /**
   * @return the file descriptor of the master pty
   *
   * This function should be called only while the pty is open.
   */
  int masterFd () const;

  /**
   * @return the file descriptor of the slave pty
   *
   * This function should be called only while the pty slave is open.
   */
  int slaveFd () const;

protected:
  /**
   * @internal
   */
    KPty (KPtyPrivate * d);

  /**
   * @internal
   */
  KPtyPrivate *const d_ptr;
};

#endif
