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

#include <strstream.h>
#include <stdarg.h>

#include "utils.h"
#include "error.h"
#include "pager.h"
#include "oct-obj.h"
#include "tree-const.h"
#include "defun.h"

// Current error state.
int error_state = 0;

// XXX FIXME XXX
int suppress_octave_error_messages = 0;

static void
verror (const char *name, const char *fmt, va_list args)
{
  if (name)
    cerr << name << ": ";
  cerr.vform (fmt, args);
  cerr << endl;

  ostrstream output_buf;

  if (name)
    output_buf << name << ": ";
  output_buf.vform (fmt, args);
  output_buf << endl;

  char *msg = output_buf.str ();

  maybe_write_to_diary_file (msg);

  delete [] msg;
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

  if (suppress_octave_error_messages)
    return;

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

DEFUN ("error", Ferror, Serror, 1, 1,
  "error (MESSAGE): print MESSAGE and set the error state.\n\
This should eventually take us up to the top level, possibly\n\
printing traceback messages as we go.\n\
\n\
If MESSAGE ends in a newline character, traceback messages are not\n\
printed.") 
{
  Octave_object retval;

  char *msg = "unspecified error";

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    {
      if (args(0).is_string ())
	{
	  msg = args(0).string_value ();

	  if (! msg || ! *msg)
	    return retval;
	}
      else if (args(0).is_empty ())
	return retval;
    }

  error (msg);

  return retval;
}

DEFUN ("warning", Fwarning, Swarning, 1, 1,
  "warning (MESSAGE): print a warning MESSAGE.\n\
\n\
See also: error")
{
  Octave_object retval;

  char *msg = "unspecified warning";

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    {
      if (args(0).is_string ())
	{
	  msg = args(0).string_value ();

	  if (! msg || ! *msg)
	    return retval;
	}
      else if (args(0).is_empty ())
	return retval;
    }

  warning (msg);

  return retval;
}

DEFUN ("usage", Fusage, Susage, 1, 1,
  "usage (MESSAGE): print a usage MESSAGE.\n\
\n\
See also: error")
{
  Octave_object retval;

  char *msg = "unknown";

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    {
      if (args(0).is_string ())
	{
	  msg = args(0).string_value ();

	  if (! msg || ! *msg)
	    return retval;
	}
      else if (args(0).is_empty ())
	return retval;
    }

  usage (msg);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
