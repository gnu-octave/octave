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

#include <string>
#include <fstream.h>

#include "procstream.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "help.h"
#include "oct-obj.h"
#include "pager.h"
#include "sighandlers.h"
#include "user-prefs.h"

pid_t octave_pager_pid = -1;

// Our actual connection to the external pager.
static oprocstream *external_pager = 0;

// Nonzero means we write to the diary file.
static int write_to_diary_file = 0;

// The name of the current diary file.
static string diary_file;

// The diary file.
static ofstream external_diary_file;

static sig_handler *saved_sigint_handler = 0;

static void
do_sync (const char *msg)
{
  if (! error_state)
    {
      if (msg && *msg)
	{
	  if (! external_pager)
	    {
	      string pgr = user_pref.pager_binary;

	      if (! pgr.empty ())
		{
		  saved_sigint_handler
		    = octave_set_signal_handler (SIGINT, SIG_IGN);

		  external_pager = new oprocstream (pgr.c_str ());

		  if (external_pager)
		    octave_pager_pid = external_pager->pid ();
		}
	    }

	  if (external_pager)
	    {
	      *external_pager << msg;

	      if (external_pager->fail ())
		{
		  octave_pager_pid = -1;

		  delete external_pager;
		  external_pager = 0;

		  if (saved_sigint_handler)
		    {
		      octave_set_signal_handler (SIGINT, saved_sigint_handler);
		      saved_sigint_handler = 0;
		    }
		}
	      else
		external_pager->flush ();
	    }
	  else
	    cout << msg;
	}
    }
}

int
octave_pager_buf::sync (void)
{
  sputc ('\0');

  char *buf = eback ();

  do_sync (buf);

  octave_diary << buf;

  seekoff (0, ios::beg);

  return 0;
}

int
octave_diary_buf::sync (void)
{
  sputc ('\0');

  if (write_to_diary_file && external_diary_file)
    external_diary_file << eback ();

  seekoff (0, ios::beg);

  return 0;
}

octave_pager_stream *octave_pager_stream::instance = 0;

octave_pager_stream::octave_pager_stream (void) : ostream (), pb (0)
{
  pb = new octave_pager_buf;
  rdbuf (pb);
  setf (unitbuf);
}

octave_pager_stream::~octave_pager_stream (void)
{
  flush ();
  delete pb;
}

octave_pager_stream&
octave_pager_stream::stream (void)
{
  if (! instance)
    instance = new octave_pager_stream ();
      
  return *instance;
}

octave_diary_stream *octave_diary_stream::instance = 0;

octave_diary_stream::octave_diary_stream (void) : ostream (), db (0)
{
  db = new octave_diary_buf;
  rdbuf (db);
  setf (unitbuf);
}

octave_diary_stream::~octave_diary_stream (void)
{
  flush ();
  delete db;
}

octave_diary_stream&
octave_diary_stream::stream (void)
{
  if (! instance)
    instance = new octave_diary_stream ();

  return *instance;
}

void
flush_octave_stdout (void)
{
  octave_stdout.flush ();

  if (external_pager)
    {
      octave_pager_pid = -1;

      delete external_pager;
      external_pager = 0;

      if (saved_sigint_handler)
	{
	  octave_set_signal_handler (SIGINT, saved_sigint_handler);
	  saved_sigint_handler = 0;
	}
    }
}

static void
close_diary_file (void)
{
  if (external_diary_file.is_open ())
    {
      octave_diary.flush ();
      external_diary_file.close ();
    }
}

static void
open_diary_file (void)
{
  close_diary_file ();

  external_diary_file.open (diary_file.c_str (), ios::app);

  if (! external_diary_file)
    error ("diary: can't open diary file `%s'", diary_file.c_str ());
}

DEFUN_TEXT (diary, args, ,
  "diary [on|off]\n\
diary [file]\n\
\n\
redirect all input and screen output to a file.")
{
  octave_value_list retval;

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
	  {
	    close_diary_file ();
	    write_to_diary_file = 0;
	  }
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
  octave_value_list retval;

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

static string
default_pager (void)
{
  string pager_binary;

  char *pgr = getenv ("PAGER");

  if (pgr)
    pager_binary = string (pgr);
#ifdef DEFAULT_PAGER
  else
    {
      pager_binary = string (DEFAULT_PAGER);

      if (pager_binary == "less")
	{
	  pager_binary.append (" -e");

	  if (! getenv ("LESS"))
	    pager_binary.append
	      (" -P'-- less ?pB(%pB\\%):--. (f)orward, (b)ack, (q)uit$'");
	}
    }
#endif

  return pager_binary;
}

void
symbols_of_pager (void)
{
  DEFVAR (PAGER, default_pager (), 0, sv_pager_binary,
    "path to pager binary");

  DEFVAR (page_screen_output, 1.0, 0, page_screen_output,
    "if possible, send output intended for the screen through the pager");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
