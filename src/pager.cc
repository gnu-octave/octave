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

#include <signal.h>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>
#include <stdlib.h>

#include "procstream.h"

#include "sighandlers.h"
#include "user-prefs.h"
#include "oct-obj.h"
#include "error.h"
#include "defun.h"
#include "input.h"
#include "pager.h"
#include "utils.h"
#include "help.h"

// Where we stash output headed for the screen.
static ostrstream *pager_buf = 0;

// Nonzero means we write to the diary file.
static int write_to_diary_file = 0;

// The name of the current diary file.
static char *diary_file = "diary";

// The diary file.
static ofstream diary_stream;

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

// For now, use the variables from readline.  It already handles
// SIGWINCH, so these values have a good chance of being correct even
// if the window changes size (they will be wrong if, for example, the
// luser changes the window size while the pager is running, and the
// signal is handled by the pager instead of us.

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
 // Extract message from buffer, then delete the buffer so that any
 // new messages get sent separately.

  *pager_buf << ends;
  char *message = pager_buf->str ();
  initialize_pager ();

  if (! message || ! *message)
    {
      delete [] message;
      return;
    }

  maybe_write_to_diary_file (message);

  int nlines = line_count (message);

  if (nlines > terminal_rows () - 2)
    {
      char *pgr = user_pref.pager_binary;
      if (pgr)
	{
	  oprocstream pager_stream (pgr);
	  if (pager_stream)
	    {
	      volatile sig_handler *old_sigint_handler;
	      old_sigint_handler = signal (SIGINT, SIG_IGN);

	      pager_stream << message;
	      delete [] message;
	      pager_stream.flush ();

	      signal (SIGINT, old_sigint_handler);

	      return;
	    }
	}
    }

  cout << message;
  delete [] message;
  cout.flush ();
}

static void
open_diary_file (void)
{
  if (diary_stream.is_open ())
    diary_stream.close ();

  diary_stream.open (diary_file, ios::app);

  if (! diary_stream)
    error ("diary: can't open diary file `%s'", diary_file);
}

void
close_diary_file (void)
{
  if (diary_stream)
    diary_stream.close ();
}

void
maybe_write_to_diary_file (const char *s)
{
  if (write_to_diary_file && diary_stream)
    diary_stream << s;
}

DEFUN_TEXT ("diary", Fdiary, Sdiary, -1, 1,
  "diary [on|off]\n\
diary [file]\n\
\n\
redirect all input and screen output to a file.")
{
  Octave_object retval;

  DEFINE_ARGV("diary");

  switch (argc)
    {
    case 1:
      write_to_diary_file = ! write_to_diary_file;
      open_diary_file ();
      break;

    case 2:
      {
	char *arg = argv[1];
	if (strcmp (arg, "on") == 0)
	  {
	    write_to_diary_file = 1;
	    open_diary_file ();
	  }	
	else if (strcmp (arg, "off") == 0)
	  write_to_diary_file = 0;
	else
	  {
	    delete [] diary_file;
	    diary_file = strsave (arg);
	    open_diary_file ();
	  }
      }
      break;

    default:
      print_usage ("diary");
      break;
    }

  DELETE_ARGV;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
