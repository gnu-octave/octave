// error.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "error.h"
#include "pager.h"

// Current error state.
int error_state;

static void
verror (const char *name, const char *fmt, va_list args)
{
  if (name != (char *) NULL)
    fprintf (stderr, "%s: ", name);

  vfprintf (stderr, fmt, args);
  fprintf (stderr, "\n");
  fflush (stderr);
}

void
message (const char *name, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror (name, fmt, args);
  va_end (args);
}

void
usage (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror ("usage", fmt, args);
  va_end (args);
}

void
warning (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror ("warning", fmt, args);
  va_end (args);
}

void
error (const char *fmt, ...)
{
  if (! error_state)
    error_state = 1;

  flush_output_to_pager ();

  va_list args;
  va_start (args, fmt);
  verror ("error", fmt, args);
  va_end (args);
}

void volatile
panic (const char *fmt, ...)
{
  flush_output_to_pager ();

  va_list args;
  va_start (args, fmt);
  verror ("panic", fmt, args);
  va_end (args);
  abort ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
