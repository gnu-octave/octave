/*

Copyright (C) 1999, 2000, 2002, 2003, 2005, 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

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

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

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
octave_raw_vsnprintf (char *buf, size_t n, const char *fmt, va_list args)
{
  return vsnprintf (buf, n, fmt, args);
}
