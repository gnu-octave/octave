// pager.cc                                               -*- C++ -*-
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

#include <iostream.h>
#include <strstream.h>
#include <stdlib.h>

#include "procstream.h"

#include "user-prefs.h"
#include "input.h"
#include "pager.h"

// Where we stash output headed for the screen.
static ostrstream *pager_buf = 0;

static int
line_count (char *s)
{
  int count = 0;
  if (s)
    {
      char c;
      while ((c = *s++) != '\0')
	if (c == '\n')
	  count++;
    }
  return count;
}

/*
 * For now, use the variables from readline.  It already handles
 * SIGWINCH, so these values have a good chance of being correct even
 * if the window changes size (they will be wrong if, for example, the
 * luser changes the window size while the pager is running, and the
 * signal is handled by the pager instead of us.
 */
int
terminal_columns (void)
{
  extern int screenwidth;
  return screenwidth > 0 ? screenwidth : 80;
}

int
terminal_rows (void)
{
  extern int screenheight;
  return screenheight > 0 ? screenheight : 24;
}

void
initialize_pager (void)
{
  delete pager_buf;
  pager_buf = new ostrstream ();
}

void
maybe_page_output (ostrstream& msg_buf)
{
  msg_buf << ends;

  char *message = msg_buf.str ();

  if (interactive
      && user_pref.page_screen_output
      && user_pref.pager_binary)
    {
      *pager_buf << message;
      delete [] message;
    }
  else
    {
      cout << message;
      cout.flush ();
      delete [] message;
    }
}

void
flush_output_to_pager (void)
{
  *pager_buf << ends;

  char *message = pager_buf->str ();

  if (! message || ! *message)
    {
      delete [] message;
      initialize_pager ();
      return;
    }

  int nlines = line_count (message);

  if (nlines > terminal_rows () - 2)
    {
      char *pgr = user_pref.pager_binary;
      if (pgr)
	{
	  oprocstream pager_stream (pgr);
	  if (pager_stream)
	    {
	      pager_stream << message;
	      pager_stream.flush ();

	      delete [] message;
	      initialize_pager ();
	      return;
	    }
	}
    }

  cout << message;
  cout.flush ();
  delete [] message;
  initialize_pager ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
