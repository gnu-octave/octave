// error.cc                                             -*- C++ -*-
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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "utils.h"
#include "error.h"
#include "pager.h"
#include "oct-obj.h"
#include "tree-const.h"
#include "defun.h"

// Current error state.
int error_state;

static void
verror (const char *name, const char *fmt, va_list args)
{
  if (name)
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
  if (error_state == -2)
    return;

  if (! error_state)
    error_state = 1;

  flush_output_to_pager ();

  va_list args;
  va_start (args, fmt);

  int len;
  if (fmt && *fmt && fmt[(len = strlen (fmt)) - 1] == '\n')
    {
      error_state = -2;
      char *tmp_fmt = strsave (fmt);
      tmp_fmt[len - 1] = '\0';
      verror ("error", tmp_fmt, args);
      delete [] tmp_fmt;
    }
  else
    verror ("error", fmt, args);

  va_end (args);
}

void
panic (const char *fmt, ...)
{
  flush_output_to_pager ();

  va_list args;
  va_start (args, fmt);
  verror ("panic", fmt, args);
  va_end (args);
  abort ();
}

DEFUN ("error", Ferror, Serror, 2, 1,
  "error (MESSAGE): print MESSAGE and set the error state.\n\
This should eventually take us up to the top level, possibly\n\
printing traceback messages as we go.\n\
\n\
If MESSAGE ends in a newline character, traceback messages are not\n\
printed.") 
{
  Octave_object retval;

  char *msg = "unspecified_error";

  int nargin = args.length ();

  if (nargin == 2 && args(1).is_defined ())
    {
      if (args(1).is_string_type ())
	{
	  msg = args(1).string_value ();

	  if (! msg || ! *msg)
	    return retval;
	}
      else if (args(1).is_empty ())
	return retval;
    }

  error (msg);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
