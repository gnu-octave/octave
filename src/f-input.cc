// f-input.cc                                           -*- C++ -*-
/*

Copyright (C) 1994 John W. Eaton

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

#include <string.h>

#include "tree-const.h"
#include "octave-hist.h"
#include "pager.h"
#include "input.h"
#include "f-eval.h"
#include "f-input.h"

static int
match_sans_spaces (const char *standard, const char *test)
{
  const char *tp = test;
  while (*tp == ' ' || *tp == '\t')
    tp++;

  const char *ep = test + strlen (test) - 1;
  while (*ep == ' ' || *ep == '\t')
    ep--;

  int len = ep - tp + 1;

  return (strncmp (standard, tp, len) == 0);
}

tree_constant
get_user_input (const Octave_object& args, int nargout, int debug = 0)
{
  tree_constant retval;

  int nargin = args.length ();

  int read_as_string = 0;

  if (nargin == 3)
    {
      if (args(2).is_string_type ()
	  && strcmp ("s", args(2).string_value ()) == 0)
	read_as_string++;
      else
	{
	  error ("input: unrecognized second argument");
	  return retval;
	}
    }

  char *prompt = "debug> ";
  if (nargin > 1)
   {
      if (args(1).is_string_type ())
	prompt = args(1).string_value ();
      else
	{
	  error ("input: unrecognized argument");
	  return retval;
	}
    }

 again:

  flush_output_to_pager ();

  char *input_buf = gnu_readline (prompt);

  if (input_buf != (char *) NULL)
    {
      if (input_buf)
	maybe_save_history (input_buf);

      int len = strlen (input_buf);

      if (len < 1)
	{
	  if (debug)
	    goto again;
	  else
	    return retval;
	}

      if (match_sans_spaces ("exit", input_buf)
	  || match_sans_spaces ("quit", input_buf)
	  || match_sans_spaces ("return", input_buf))
	return tree_constant ();
      else if (read_as_string)
	retval = input_buf;
      else
	{
	  int parse_status = 0;
	  retval = eval_string (input_buf, 0, 0, parse_status);
	  if (debug && retval.is_defined ())
	    retval.eval (1);
	}
    }
  else
    error ("input: reading user-input failed!");

  if (debug)
    goto again;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
