// resource.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
#include <config.h>
#endif

#include "tree-const.h"
#include "systime.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "defun.h"
#include "utils.h"
#include "help.h"

#ifdef HAVE_SYS_RESOURCE_H
extern "C"
{
#include <sys/resource.h>
}
#endif

#ifndef RUSAGE_SELF
#define RUSAGE_SELF 0
#endif

// System resource functions.

static Octave_map
mk_ru_map (struct rusage *ru)
{
  Octave_map m;
  Octave_map tv_tmp;

#if defined (HAVE_GETRUSAGE)
  tv_tmp ["tv_sec"] = (double) ru->ru_utime.tv_sec;
  tv_tmp ["tv_usec"] = (double) ru->ru_utime.tv_usec;
  m ["ru_utime"] = tree_constant (tv_tmp);

  tv_tmp ["tv_sec"] = (double) ru->ru_stime.tv_sec;
  tv_tmp ["tv_usec"] = (double) ru->ru_stime.tv_usec;
  m ["ru_stime"] = tree_constant (tv_tmp);

  m ["ru_maxrss"] = (double) ru->ru_maxrss;
  m ["ru_ixrss"] = (double) ru->ru_ixrss;
  m ["ru_idrss"] = (double) ru->ru_idrss;
  m ["ru_isrss"] = (double) ru->ru_isrss;
  m ["ru_minflt"] = (double) ru->ru_minflt;
  m ["ru_majflt"] = (double) ru->ru_majflt;
  m ["ru_nswap"] = (double) ru->ru_nswap;
  m ["ru_inblock"] = (double) ru->ru_inblock;
  m ["ru_oublock"] = (double) ru->ru_oublock;
  m ["ru_msgsnd"] = (double) ru->ru_msgsnd;
  m ["ru_msgrcv"] = (double) ru->ru_msgrcv;
  m ["ru_nsignals"] = (double) ru->ru_nsignals;
  m ["ru_nvcsw"] = (double) ru->ru_nvcsw;
  m ["ru_nivcsw"] = (double) ru->ru_nivcsw;
#else
  tv_tmp ["tv_sec"] = octave_NaN;
  tv_tmp ["tv_usec"] = octave_NaN;
  m ["ru_utime"] = tree_constant (tv_tmp);

  tv_tmp ["tv_sec"] = octave_NaN;
  tv_tmp ["tv_usec"] = octave_NaN;
  m ["ru_stime"] = tree_constant (tv_tmp);

  m ["ru_maxrss"] = octave_NaN;
  m ["ru_ixrss"] = octave_NaN;
  m ["ru_idrss"] = octave_NaN;
  m ["ru_isrss"] = octave_NaN;
  m ["ru_minflt"] = octave_NaN;
  m ["ru_majflt"] = octave_NaN;
  m ["ru_nswap"] = octave_NaN;
  m ["ru_inblock"] = octave_NaN;
  m ["ru_oublock"] = octave_NaN;
  m ["ru_msgsnd"] = octave_NaN;
  m ["ru_msgrcv"] = octave_NaN;
  m ["ru_nsignals"] = octave_NaN;
  m ["ru_nvcsw"] = octave_NaN;
  m ["ru_nivcsw"] = octave_NaN;
#endif

  return m;
}

DEFUN ("getrusage", Fgetrusage, Sgetrusage, 0, 0,
  "getrusage ()\n\
\n\
Return system resource statistics.")
{
  Octave_object retval;

#if defined (HAVE_GETRUSAGE)

  struct rusage resource_stats;

  getrusage (RUSAGE_SELF, &resource_stats);

  retval = mk_ru_map (&resource_stats);

#endif

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
