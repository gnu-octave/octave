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

#include "liboctave-error.h"

static
liboctave_error_handler default_liboctave_error_handler = liboctave_fatal;

static
liboctave_error_handler current_liboctave_error_handler = liboctave_fatal;

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
set_liboctave_error_handler (liboctave_error_handler f)
{
  if (f)
    current_liboctave_error_handler = f;
  else
    current_liboctave_error_handler = default_liboctave_error_handler;
}

void
liboctave_fatal (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror ("fatal", fmt, args);
  va_end (args);

  exit (1);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
