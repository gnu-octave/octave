////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

// These functions may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sys/resource.h>
#include <sys/time.h>
#include <sys/times.h>

#include "time-wrappers.h"

int
octave_gettimeofday_wrapper (time_t *sec, long *usec)
{
  struct timeval tv;

  int status = gettimeofday (&tv, 0);

  if (status < 0)
    {
      *sec = 0;
      *usec = 0;
    }
  else
    {
      *sec = tv.tv_sec;
      *usec = tv.tv_usec;
    }

  return status;
}

int
octave_cpu_time (time_t *usr_sec, time_t *sys_sec,
                 long *usr_usec, long *sys_usec)
{
  struct rusage ru;

  int status = getrusage (RUSAGE_SELF, &ru);

  if (status < 0)
    {
      *usr_sec = 0;
      *sys_sec = 0;

      *usr_usec = 0;
      *sys_usec = 0;
    }
  else
    {
      *usr_sec = ru.ru_utime.tv_sec;
      *usr_usec = ru.ru_utime.tv_usec;

      *sys_sec = ru.ru_stime.tv_sec;
      *sys_usec = ru.ru_stime.tv_usec;
    }

  return status;
}

int
octave_getrusage_wrapper (time_t *usr_sec, time_t *sys_sec,
                          long *usr_usec, long *sys_usec,
                          long *maxrss, long *ixrss, long *idrss,
                          long *isrss, long *minflt, long *majflt,
                          long *nswap, long *inblock, long *oublock,
                          long *msgsnd, long *msgrcv, long *nsignals,
                          long *nvcsw, long *nivcsw)
{
  struct rusage ru;

  int status = getrusage (RUSAGE_SELF, &ru);

  if (status < 0)
    {
      *usr_sec = 0;
      *usr_usec = 0;

      *sys_sec = 0;
      *sys_usec = 0;

      *maxrss = 0;
      *ixrss = 0;
      *idrss = 0;
      *isrss = 0;
      *minflt = 0;
      *majflt = 0;
      *nswap = 0;
      *inblock = 0;
      *oublock = 0;
      *msgsnd = 0;
      *msgrcv = 0;
      *nsignals = 0;
      *nvcsw = 0;
      *nivcsw = 0;
    }
  else
    {
      *usr_sec = ru.ru_utime.tv_sec;
      *usr_usec = ru.ru_utime.tv_usec;

      *sys_sec = ru.ru_stime.tv_sec;
      *sys_usec = ru.ru_stime.tv_usec;

      *maxrss = ru.ru_maxrss;
      *ixrss = ru.ru_ixrss;
      *idrss = ru.ru_idrss;
      *isrss = ru.ru_isrss;
      *minflt = ru.ru_minflt;
      *majflt = ru.ru_majflt;
      *nswap = ru.ru_nswap;
      *inblock = ru.ru_inblock;
      *oublock = ru.ru_oublock;
      *msgsnd = ru.ru_msgsnd;
      *msgrcv = ru.ru_msgrcv;
      *nsignals = ru.ru_nsignals;
      *nvcsw = ru.ru_nvcsw;
      *nivcsw = ru.ru_nivcsw;
    }

  return status;
}

time_t
octave_mktime_wrapper (struct tm *tp)
{
  return mktime (tp);
}
