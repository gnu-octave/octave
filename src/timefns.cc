// timefns.cc                                            -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "dMatrix.h"

#include "tree-const.h"
#include "oct-obj.h"
#include "systime.h"
#include "defun.h"

#include <sys/resource.h>

#ifndef RUSAGE_SELF
#define RUSAGE_SELF 0
#endif

#if defined (HAVE_GETRUSAGE)
extern "C"
{
  int getrusage ();
}
#endif


DEFUN ("clock", Fclock, Sclock, 1, 0,
  "clock (): return current date and time in vector with elements\n\
\n\
  [ year, month, day-of-month, hour, minute, second ]")
{
  time_t now;
  double fraction = 0.0;

#ifdef HAVE_GETTIMEOFDAY

  struct timeval tp;

  gettimeofday (&tp, 0);

  now = tp.tv_sec;

  fraction = tp.tv_usec / 1e6;

#else

  time (&now);

#endif

  struct tm *tm = localtime (&now);

  Matrix m (1, 6);
  m.elem (0, 0) = tm->tm_year + 1900;
  m.elem (0, 1) = tm->tm_mon + 1;
  m.elem (0, 2) = tm->tm_mday;
  m.elem (0, 3) = tm->tm_hour;
  m.elem (0, 4) = tm->tm_min;
  m.elem (0, 5) = tm->tm_sec + fraction;

  return m;
}

DEFUN ("cputime", Fcputime, Scputime, 0, 0,
  "[total, user, system] = cputime ()\n\
\n\
Return CPU time statistics.")
{
  Octave_object retval (3, Matrix (1, 1, 0.0));

#if defined (HAVE_GETRUSAGE)

  struct rusage resource_stats;

  getrusage (RUSAGE_SELF, &resource_stats);

  struct timeval usr = resource_stats.ru_utime;
  struct timeval sys = resource_stats.ru_stime;

  double usr_time = usr.tv_sec + usr.tv_usec / 1e6;
  double sys_time = sys.tv_sec + sys.tv_usec / 1e6;

  retval (2) = sys_time;
  retval (1) = usr_time;
  retval (0) = usr_time + sys_time;

#endif

  return retval;
}

DEFUN ("date", Fdate, Sdate, 1, 0,
  "date (): return current date in a string, in the form `18-Jul-94'")
{
  Octave_object retval;

  time_t now;
  struct tm *tm;

  time (&now);
  tm = localtime (&now);
  char date[32];
  int len = strftime (date, 31, "%d-%b-%y", tm);
  if (len > 0)
    retval = date;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
