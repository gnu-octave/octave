// utils.cc                                             -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <csignal>
#include <cstdlib>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include "f77-uscore.h"

// All the STOP statements in the Fortran routines have been replaced
// with a call to XSTOPX, defined in the file libcruft/misc/xstopx.f.
//
// The XSTOPX function calls this function, which will send a SIGINT
// signal to the program that invoked it.
//
// Octave's SIGINT signal handler calls jump_to_top_level(), and the
// user will end up at the top level instead of the shell prompt.
//
// Programs that don't handle SIGINT will be interrupted.

extern "C"
{

  volatile void
#if defined (F77_APPEND_UNDERSCORE)
  do_stop_ (void)
#else
  do_stop (void)
#endif
    {
      kill (getpid (), SIGINT);
      abort ();
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
