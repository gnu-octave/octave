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

#include <csignal>
#include <cstdlib>

#include <string>

#include <iostream.h>
#include <strstream.h>
#include <fstream.h>

#include "procstream.h"

#include "oct-term.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "help.h"
#include "input.h"
#include "oct-obj.h"
#include "pager.h"
#include "pt-const.h"
#include "sighandlers.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"

// Where we stash output headed for the screen.
static ostrstream *pager_buf = 0;

// Nonzero means we write to the diary file.
static int write_to_diary_file = 0;

// The name of the current diary file.
static string diary_file;

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

  if (message)
    {
      maybe_write_to_diary_file (message);

      if (interactive
	  && user_pref.page_screen_output
	  && ! user_pref.pager_binary.empty ())
	{
	  *pager_buf << message;
	}
      else
	{
	  cout << message;
	  cout.flush ();
	}

      delete [] message;
    }
}

static void
cleanup_oprocstream (void *p)
{
  delete (oprocstream *) p;
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

  int nlines = line_count (message);

  if (nlines > terminal_rows () - 2)
    {
      string pgr = user_pref.pager_binary;

      if (! pgr.empty ())
	{
	  volatile sig_handler *old_sigint_handler;
	  old_sigint_handler = octave_set_signal_handler (SIGINT, SIG_IGN);

	  oprocstream *pager_stream = new oprocstream (pgr.c_str ());

	  add_unwind_protect (cleanup_oprocstream, pager_stream);

	  int output_paged = 0;
	  if (pager_stream && *pager_stream)
	    {
	      output_paged = 1;
	      *pager_stream << message;
	      delete [] message;
	      pager_stream->flush ();
	      pager_stream->close ();
	    }

	  run_unwind_protect ();

	  octave_set_signal_handler (SIGINT, old_sigint_handler);

	  if (output_paged)
	    return;
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

  diary_stream.open (diary_file.c_str (), ios::app);

  if (! diary_stream)
    error ("diary: can't open diary file `%s'", diary_file.c_str ());
}

void
close_diary_file (void)
{
  if (diary_stream)
    diary_stream.close ();
}

void
maybe_write_to_diary_file (const string& s)
{
  if (write_to_diary_file && diary_stream)
    diary_stream << s;
}

DEFUN_TEXT (diary, args, ,
  "diary [on|off]\n\
diary [file]\n\
\n\
redirect all input and screen output to a file.")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("diary");

  if (error_state)
    return retval;

  if (diary_file.empty ())
    diary_file = "diary";

  switch (argc)
    {
    case 1:
      write_to_diary_file = ! write_to_diary_file;
      open_diary_file ();
      break;

    case 2:
      {
	string arg = argv[1];

	if (arg == "on")
	  {
	    write_to_diary_file = 1;
	    open_diary_file ();
	  }	
	else if (arg == "off")
	  write_to_diary_file = 0;
	else
	  {
	    diary_file = arg;
	    open_diary_file ();
	  }
      }
      break;

    default:
      print_usage ("diary");
      break;
    }

  return retval;
}

DEFUN_TEXT (more, args, ,
  "more on\n\
more off\n\
\n\
Turn output pagination on or off.")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("more");

  if (error_state)
    return retval;

  if (argc == 2)
    {
      string arg = argv[1];

      if (arg == "on")
	bind_builtin_variable ("page_screen_output", "true");
      else if (arg == "off")
	bind_builtin_variable ("page_screen_output", "false");
      else
	error ("more: unrecognized argument `%s'", arg.c_str ());
    }
  else
    print_usage ("more");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
