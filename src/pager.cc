/*

Copyright (C) 1996, 1997 John W. Eaton

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

#include "cmd-edit.h"
#include "oct-env.h"

#include "procstream.h"

#include <defaults.h>
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-obj.h"
#include "pager.h"
#include "sighandlers.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

static pid_t octave_pager_pid = -1;

// Our actual connection to the external pager.
static oprocstream *external_pager = 0;

// TRUE means we write to the diary file.
static bool write_to_diary_file = false;

// The name of the current diary file.
static string diary_file;

// The diary file.
static ofstream external_diary_file;

// The shell command to run as the pager.
static string Vpager_binary;

// TRUE means that if output is going to the pager, it is sent as soon
// as it is available.  Otherwise, it is buffered and only sent to the
// pager when it is time to print another prompt.
static bool Vpage_output_immediately;

// TRUE means all output intended for the screen should be passed
// through the pager.
static bool Vpage_screen_output;

// Only one pager can be active at once, so having these at file
// scope should be ok.
static octave_interrupt_handler saved_interrupt_handler;
static bool interrupt_handler_saved = false;

static bool really_flush_to_pager = false;

static bool flushing_output_to_pager = false;

static void
clear_external_pager (void)
{
  octave_child_list::remove (octave_pager_pid);

  octave_pager_pid = -1;

  delete external_pager;
  external_pager = 0;

  if (interrupt_handler_saved)
    {
      octave_set_interrupt_handler (saved_interrupt_handler);
      interrupt_handler_saved = false;
    }
}

static void
pager_death_handler (pid_t pid, int status)
{
  if (pid > 0)
    {
      if (WIFEXITED (status) || WIFSIGNALLED (status))
	{
	  // Avoid warning() or error(), since that will put us back in
	  // the pager, which would be bad news.

	  cerr << "warning: connection to external pager (pid = "
	       << pid << ") lost --\n"
	       << "warning: attempting to finish pending computations...\n";
	}
    }
}

static void
do_sync (const char *msg, bool bypass_pager)
{
  if (msg && *msg)
    {
      if (bypass_pager)
	{
	  cout << msg;
	  cout.flush ();
	}
      else
	{
	  if (! external_pager)
	    {
	      string pgr = Vpager_binary;

	      if (! pgr.empty ())
		{
		  saved_interrupt_handler = octave_ignore_interrupts ();
		  interrupt_handler_saved = true;

		  external_pager = new oprocstream (pgr.c_str ());

		  if (external_pager)
		    {
		      octave_pager_pid = external_pager->pid ();

		      octave_child_list::insert (octave_pager_pid,
						 pager_death_handler);
		    }
		}
	    }

	  if (external_pager)
	    {
	      if (octave_pager_pid > 0 && external_pager->good ())
		{
		  *external_pager << msg;

		  // These checks are needed if a signal handler
		  // invoked since the last set of checks attempts
		  // to flush output and then returns

		  if (octave_pager_pid > 0
		      && external_pager
		      && external_pager->good ())
		    external_pager->flush ();
		}
	      else
		{
		  // We had a pager, but it must have died.  Restore
		  // the interrupt state so we can escape back to the
		  // prompt if there are lots of computations pending.

		  if (interrupt_handler_saved)
		    {
		      octave_set_interrupt_handler (saved_interrupt_handler);
		      interrupt_handler_saved = false;
		    }
		}
	    }
	  else
	    {
	      cout << msg;
	      cout.flush ();
	    }
	}
    }
}

static bool
more_than_a_screenful (const char *s)
{
  if (s)
    {
      int available_rows = command_editor::terminal_rows () - 2;

      int count = 0;

      char c;

      while ((c = *s++) != '\0')
	if (c == '\n')
	  {
	    count++;

	    if (count > available_rows)
	      return true;
	  }
    }

  return false;
}

int
octave_pager_buf::sync (void)
{
  if (! interactive
      || really_flush_to_pager
      || (Vpage_screen_output && Vpage_output_immediately)
      || ! Vpage_screen_output)
    {
      sputc ('\0');

      char *buf = eback ();

      bool bypass_pager = (! interactive
			   || ! Vpage_screen_output
			   || (really_flush_to_pager
			       && Vpage_screen_output
			       && ! Vpage_output_immediately
			       && ! more_than_a_screenful (buf)));

      seekoff (0, ios::beg);

      do_sync (buf, bypass_pager);

      octave_diary << buf;
    }

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
  if (! flushing_output_to_pager)
    {
      unwind_protect::begin_frame ("flush_octave_stdout");

      unwind_protect_bool (really_flush_to_pager);
      unwind_protect_bool (flushing_output_to_pager);

      really_flush_to_pager = true;
      flushing_output_to_pager = true;

      octave_stdout.flush ();

      if (external_pager)
	clear_external_pager ();

      unwind_protect::run_frame ("flush_octave_stdout");
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
	    write_to_diary_file = true;
	    open_diary_file ();
	  }	
	else if (arg == "off")
	  {
	    close_diary_file ();
	    write_to_diary_file = false;
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
	bind_builtin_variable ("page_screen_output", 1.0);
      else if (arg == "off")
	bind_builtin_variable ("page_screen_output", 0.0);
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
  string pager_binary = octave_env::getenv ("PAGER");

#ifdef DEFAULT_PAGER
  if (pager_binary.empty ())
    {
      pager_binary = string (DEFAULT_PAGER);

      if (pager_binary == "less")
	{
	  pager_binary.append (" -e");

	  string lessflags = octave_env::getenv ("LESS");
	  if (lessflags.empty ())
	    pager_binary.append
	      (" -P'-- less ?pB(%pB\\%):--. (f)orward, (b)ack, (q)uit$'");
	}
    }
#endif

  return pager_binary;
}

static int
pager_binary (void)
{
  int status = 0;

  string s = builtin_string_variable ("PAGER");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("PAGER");
      status = -1;
    }
  else
    Vpager_binary = s;

  return status;
}

static int
page_output_immediately (void)
{
  Vpage_output_immediately = check_preference ("page_output_immediately");

  return 0;
}

static int
page_screen_output (void)
{
  Vpage_screen_output = check_preference ("page_screen_output");

  return 0;
}

void
symbols_of_pager (void)
{
  DEFVAR (PAGER, default_pager (), 0, pager_binary,
    "path to pager binary");

  DEFVAR (page_output_immediately, 0.0, 0, page_output_immediately,
    "if paging output, start sending it as soon as it is available");

  DEFVAR (page_screen_output, 1.0, 0, page_screen_output,
    "if possible, send output intended for the screen through the pager");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
