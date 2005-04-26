/*

Copyright (C) 1999 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)

#include <windows.h>

#else

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "systime.h"

#ifdef HAVE_POLL_H
#include <poll.h>
#elif HAVE_SYS_POLL_H
#include <sys/poll.h>
#endif

#endif

void
octave_sleep (unsigned int seconds)
{
#if defined (__WIN32__) && ! defined (_POSIX_VERSION)
  Sleep (1000 * seconds);
#else
  sleep (seconds);
#endif
}

void
octave_usleep (unsigned int useconds)
{
  unsigned int sec = useconds / 1000000;
  unsigned int usec = useconds % 1000000;

  if (sec > 0)
    octave_sleep (sec);

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)

  /* Round to the nearest millisecond, with a minimum of 1 millisecond
     if usleep was called with a a non-zero value.  */

  if (usec > 500)
    Sleep ((usec+500)/1000);
  else if (usec > 0)
    Sleep (1);
  else
    Sleep (0);

#elif defined (HAVE_USLEEP)

  usleep (usec);

#elif defined (HAVE_SELECT)

  {
    struct timeval delay;

    delay.tv_sec = 0;
    delay.tv_usec = usec;

    select (0, 0, 0, 0, &delay);
  }

#elif defined (HAVE_POLL)

  {
    struct pollfd pfd;

    int delay = usec / 1000;

    if (delay > 0)
      poll (&pfd, 0, delay);
  }

#endif
}

int
octave_strcasecmp (const char *s1, const char *s2)
{
  return strcasecmp (s1, s2);
}

int
octave_strncasecmp (const char *s1, const char *s2, size_t n)
{
  return strncasecmp (s1, s2, n);
}

int
octave_raw_vsnprintf (char *buf, size_t n, const char *fmt, va_list args)
{
  return vsnprintf (buf, n, fmt, args);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
