// resource.cc                                           -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#include "defun.h"
#include "help.h"
#include "oct-map.h"
#include "sysdep.h"
#include "pt-const.h"
#include "oct-obj.h"
#include "utils.h"

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
  tv_tmp ["sec"] = (double) ru->ru_utime.tv_sec;
  tv_tmp ["usec"] = (double) ru->ru_utime.tv_usec;
  m ["utime"] = tree_constant (tv_tmp);

  tv_tmp ["sec"] = (double) ru->ru_stime.tv_sec;
  tv_tmp ["usec"] = (double) ru->ru_stime.tv_usec;
  m ["stime"] = tree_constant (tv_tmp);

  m ["maxrss"] = (double) ru->ru_maxrss;
  m ["ixrss"] = (double) ru->ru_ixrss;
  m ["idrss"] = (double) ru->ru_idrss;
  m ["isrss"] = (double) ru->ru_isrss;
  m ["minflt"] = (double) ru->ru_minflt;
  m ["majflt"] = (double) ru->ru_majflt;
  m ["nswap"] = (double) ru->ru_nswap;
  m ["inblock"] = (double) ru->ru_inblock;
  m ["oublock"] = (double) ru->ru_oublock;
  m ["msgsnd"] = (double) ru->ru_msgsnd;
  m ["msgrcv"] = (double) ru->ru_msgrcv;
  m ["nsignals"] = (double) ru->ru_nsignals;
  m ["nvcsw"] = (double) ru->ru_nvcsw;
  m ["nivcsw"] = (double) ru->ru_nivcsw;
#else
  tv_tmp ["sec"] = octave_NaN;
  tv_tmp ["usec"] = octave_NaN;
  m ["utime"] = tree_constant (tv_tmp);

  tv_tmp ["sec"] = octave_NaN;
  tv_tmp ["usec"] = octave_NaN;
  m ["stime"] = tree_constant (tv_tmp);

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

  return m;
}

DEFUN (getrusage, , ,
  "getrusage ()\n\
\n\
Return system resource statistics.")
{
  Octave_object retval;

#if defined (HAVE_GETRUSAGE)

  struct rusage resource_stats;

  getrusage (RUSAGE_SELF, &resource_stats);

  retval = tree_constant (mk_ru_map (&resource_stats));

#endif

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
