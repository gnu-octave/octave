// error.cc                                             -*- C++ -*-
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

#include <cstdarg>

#include <strstream.h>

#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "pager.h"
#include "tree-const.h"
#include "utils.h"

// Current error state.
int error_state = 0;

// XXX FIXME XXX
int suppress_octave_error_messages = 0;

static void
verror (const char *name, const char *fmt, va_list args)
{
  flush_output_to_pager ();

  if (name)
    cerr << name << ": ";
  cerr.vform (fmt, args);
  cerr << endl;

  ostrstream output_buf;

  if (name)
    output_buf << name << ": ";
  output_buf.vform (fmt, args);
  output_buf << endl << ends;

  char *msg = output_buf.str ();

  maybe_write_to_diary_file (msg);

  delete [] msg;
}

// Note that we don't actually print any message if the error string
// is just "" or "\n".  This allows error ("") and error ("\n") to
// just set the error state.

static void
error_1 (const char *name, const char *fmt, va_list args)
{
  if (error_state != -2)
    {
      if (! error_state)
	error_state = 1;

      if (! suppress_octave_error_messages)
	{
	  if (fmt)
	    {
	      if (*fmt)
		{
		  int len = strlen (fmt);
		  if (fmt[len - 1] == '\n')
		    {
		      error_state = -2;

		      if (len > 1)
			{
			  char *tmp_fmt = strsave (fmt);
			  tmp_fmt[len - 1] = '\0';
			  verror (name, tmp_fmt, args);
			  delete [] tmp_fmt;
			}
		    }
		  else
		    verror (name, fmt, args);
		}
	    }
	  else
	    panic ("error_1: invalid format");
	}
    }
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
  error_state = -1;
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
  va_list args;
  va_start (args, fmt);
  error_1 ("error", fmt, args);
  va_end (args);
}

void
parse_error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 (0, fmt, args);
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

  const char *msg = "unspecified error";

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    {
      if (args(0).is_string ())
	{
	  msg = args(0).string_value ();

	  if (! msg)
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

  const char *msg = "unspecified warning";

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

  const char *msg = "unknown";

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
