/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "systime.h"

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#if defined (HAVE_TIMES) && defined (HAVE_SYS_TIMES_H)

#if defined (HAVE_SYS_PARAM_H)
#include <sys/param.h>
#endif
#include <sys/times.h>

#if !defined (HZ)
#if defined (CLK_TCK)
#define HZ CLK_TCK
#elif defined (USG)
#define HZ 100
#else
#define HZ 60
#endif
#endif

#endif

#include "defun-dld.h"
#include "help.h"
#include "oct-map.h"
#include "sysdep.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

#ifndef RUSAGE_SELF
#define RUSAGE_SELF 0
#endif

// System resource functions.

DEFUN_DLD (getrusage, , ,
  "getrusage ()\n\
\n\
Return system resource statistics.")
{
  Octave_map m;
  Octave_map tv_tmp;

#if defined (HAVE_GETRUSAGE)

  struct rusage ru;

  getrusage (RUSAGE_SELF, &ru);

  tv_tmp ["sec"] = static_cast<double> (ru.ru_utime.tv_sec);
  tv_tmp ["usec"] = static_cast<double> (ru.ru_utime.tv_usec);
  m ["utime"] = octave_value (tv_tmp);

  tv_tmp ["sec"] = static_cast<double> (ru.ru_stime.tv_sec);
  tv_tmp ["usec"] = static_cast<double> (ru.ru_stime.tv_usec);
  m ["stime"] = octave_value (tv_tmp);

#if ! defined (RUSAGE_TIMES_ONLY)
  m ["maxrss"] = static_cast<double> (ru.ru_maxrss);
  m ["ixrss"] = static_cast<double> (ru.ru_ixrss);
  m ["idrss"] = static_cast<double> (ru.ru_idrss);
  m ["isrss"] = static_cast<double> (ru.ru_isrss);
  m ["minflt"] = static_cast<double> (ru.ru_minflt);
  m ["majflt"] = static_cast<double> (ru.ru_majflt);
  m ["nswap"] = static_cast<double> (ru.ru_nswap);
  m ["inblock"] = static_cast<double> (ru.ru_inblock);
  m ["oublock"] = static_cast<double> (ru.ru_oublock);
  m ["msgsnd"] = static_cast<double> (ru.ru_msgsnd);
  m ["msgrcv"] = static_cast<double> (ru.ru_msgrcv);
  m ["nsignals"] = static_cast<double> (ru.ru_nsignals);
  m ["nvcsw"] = static_cast<double> (ru.ru_nvcsw);
  m ["nivcsw"] = static_cast<double> (ru.ru_nivcsw);
#endif

#else
#if defined (HAVE_TIMES) && defined (HAVE_SYS_TIMES_H)

  struct tms t;

  times (&t);

  unsigned long ticks;
  unsigned long seconds;
  unsigned long fraction;

  ticks = t.tms_utime + t.tms_cutime;
  fraction = ticks % HZ;
  seconds = ticks / HZ;

  tv_tmp ["sec"] = static_cast<double> (seconds);
  tv_tmp ["usec"] = static_cast<double> (fraction * 1e6 / HZ);
  m ["utime"] = octave_value (tv_tmp);

  ticks = t.tms_stime + t.tms_cstime;
  fraction = ticks % HZ;
  seconds = ticks / HZ;

  tv_tmp ["sec"] = static_cast<double> (seconds);
  tv_tmp ["usec"] = static_cast<double> (fraction * 1e6 / HZ);
  m ["stime"] = octave_value (tv_tmp);

#else

  tv_tmp ["sec"] = 0.0;
  tv_tmp ["usec"] = 0.0;
  m ["utime"] = octave_value (tv_tmp);

  tv_tmp ["sec"] = 0.0;
  tv_tmp ["usec"] = 0.0;
  m ["stime"] = octave_value (tv_tmp);

#endif

  m ["maxrss"] = octave_NaN;
  m ["ixrss"] = octave_NaN;
  m ["idrss"] = octave_NaN;
  m ["isrss"] = octave_NaN;
  m ["minflt"] = octave_NaN;
  m ["majflt"] = octave_NaN;
  m ["nswap"] = octave_NaN;
  m ["inblock"] = octave_NaN;
  m ["oublock"] = octave_NaN;
  m ["msgsnd"] = octave_NaN;
  m ["msgrcv"] = octave_NaN;
  m ["nsignals"] = octave_NaN;
  m ["nvcsw"] = octave_NaN;
  m ["nivcsw"] = octave_NaN;

#endif

  return octave_value (m);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
