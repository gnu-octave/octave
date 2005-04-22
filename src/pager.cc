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

#include <fstream>
#include <string>

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

// Our actual connection to the external pager.
static oprocstream *external_pager = 0;

// TRUE means we write to the diary file.
static bool write_to_diary_file = false;

// The name of the current diary file.
static std::string diary_file;

// The diary file.
static std::ofstream external_diary_file;

// The shell command to run as the pager.
static std::string Vpager_binary;

// TRUE means that if output is going to the pager, it is sent as soon
// as it is available.  Otherwise, it is buffered and only sent to the
// pager when it is time to print another prompt.
static bool Vpage_output_immediately;

// TRUE means all output intended for the screen should be passed
// through the pager.
static bool Vpage_screen_output;

static bool really_flush_to_pager = false;

static bool flushing_output_to_pager = false;

static void
clear_external_pager (void)
{
  if (external_pager)
    {
      octave_child_list::remove (external_pager->pid ());

      delete external_pager;
      external_pager = 0;
    }
}

static bool
pager_event_handler (pid_t pid, int status)
{
  bool retval = false;

  if (pid > 0)
    {
      if (WIFEXITED (status) || WIFSIGNALLED (status))
	{
	  // Avoid warning() since that will put us back in the pager,
	  // which would be bad news.

	  std::cerr << "warning: connection to external pager lost (pid = "
		    << pid << ")" << std::endl;
	  std::cerr << "warning: flushing pending output (please wait)"
		    << std::endl;

	  // Request removal of this PID from the list of child
	  // processes.

	  retval = true;
	}
    }

  return retval;
}

static void
do_sync (const char *msg, int len, bool bypass_pager)
{
  if (msg && len > 0)
    {
      if (bypass_pager)
	{
	  std::cout.write (msg, len);
	  std::cout.flush ();
	}
      else
	{
	  if (! external_pager)
	    {
	      std::string pgr = Vpager_binary;

	      if (! pgr.empty ())
		{
		  external_pager = new oprocstream (pgr.c_str ());

		  if (external_pager)
		    octave_child_list::insert (external_pager->pid (),
					       pager_event_handler);
		}
	    }

	  if (external_pager)
	    {
	      if (external_pager->good ())
		{
		  external_pager->write (msg, len);

		  external_pager->flush ();

#if defined (EPIPE)
		  if (errno == EPIPE)
		    external_pager->setstate (std::ios::failbit);
#endif
		}
	      else
		{
		  // XXX FIXME XXX -- omething is not right with the
		  // pager.  If it died then we should receive a
		  // signal for that.  If there is some other problem,
		  // then what?
		}
	    }
	  else
	    {
	      std::cout.write (msg, len);
	      std::cout.flush ();
	    }
	}
    }
}

// Assume our terminal wraps long lines.

static bool
more_than_a_screenful (const char *s, int len)
{
  if (s)
    {
      int available_rows = command_editor::terminal_rows () - 2;

      int cols = command_editor::terminal_cols ();

      int count = 0;

      int chars_this_line = 0;

      for (int i = 0; i < len; i++)
	{
	  if (*s++ == '\n')
	    {
	      count += chars_this_line / cols + 1;
	      chars_this_line = 0;
	    }
	  else
	    chars_this_line++;
	}

      if (count > available_rows)
 	return true;
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
      char *buf = eback ();

      int len = pptr () - buf;

      bool bypass_pager = (! interactive
			   || ! Vpage_screen_output
			   || (really_flush_to_pager
			       && Vpage_screen_output
			       && ! Vpage_output_immediately
			       && ! more_than_a_screenful (buf, len)));

      if (len > 0)
	{
	  do_sync (buf, len, bypass_pager);

	  flush_current_contents_to_diary ();

	  seekoff (0, std::ios::beg);
	}
    }

  return 0;
}

void
octave_pager_buf::flush_current_contents_to_diary (void)
{
  char *buf = eback () + diary_skip;

  size_t len = pptr () - buf;

  octave_diary.write (buf, len);

  diary_skip = 0;
}

void
octave_pager_buf::set_diary_skip (void)
{
  diary_skip = pptr () - eback ();
}

int
octave_diary_buf::sync (void)
{
  if (write_to_diary_file && external_diary_file)
    {
      char *buf = eback ();

      int len = pptr () - buf;

      if (len > 0)
	external_diary_file.write (buf, len);
    }

  seekoff (0, std::ios::beg);

  return 0;
}

octave_pager_stream *octave_pager_stream::instance = 0;

octave_pager_stream::octave_pager_stream (void) : std::ostream (0), pb (0)
{
  pb = new octave_pager_buf ();
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

void
octave_pager_stream::flush_current_contents_to_diary (void)
{
  if (pb)
    pb->flush_current_contents_to_diary ();
}

void
octave_pager_stream::set_diary_skip (void)
{
  if (pb)
    pb->set_diary_skip ();
}

octave_diary_stream *octave_diary_stream::instance = 0;

octave_diary_stream::octave_diary_stream (void) : std::ostream (0), db (0)
{
  db = new octave_diary_buf ();
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

      clear_external_pager ();

      unwind_protect::run_frame ("flush_octave_stdout");
    }
}

static void
close_diary_file (void)
{
  // Try to flush the current buffer to the diary now, so that things
  // like
  //
  // function foo ()
  //   diary on;
  //   ...
  //   diary off;
  // endfunction
  //
  // will do the right thing.

  octave_stdout.flush_current_contents_to_diary ();

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

  // If there is pending output in the pager buf, it should not go
  // into the diary file.
 
  octave_stdout.set_diary_skip ();

  external_diary_file.open (diary_file.c_str (), std::ios::app);

  if (! external_diary_file)
    error ("diary: can't open diary file `%s'", diary_file.c_str ());
}

DEFCMD (diary, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} diary options\n\
Create a list of all commands @emph{and} the output they produce, mixed\n\
together just as you see them on your terminal.  Valid options are:\n\
\n\
@table @code\n\
@item on\n\
Start recording your session in a file called @file{diary} in your\n\
current working directory.\n\
\n\
@item off\n\
Stop recording your session in the diary file.\n\
\n\
@item @var{file}\n\
Record your session in the file named @var{file}.\n\
@end table\n\
\n\
Without any arguments, @code{diary} toggles the current diary state.\n\
@end deffn")
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
	std::string arg = argv[1];

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
	    write_to_diary_file = true;
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

DEFCMD (more, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} more\n\
@deffnx {Command} more on\n\
@deffnx {Command} more off\n\
Turn output pagination on or off.  Without an argument, @code{more}\n\
toggles the current state.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("more");

  if (error_state)
    return retval;

  if (argc == 2)
    {
      std::string arg = argv[1];

      if (arg == "on")
	bind_builtin_variable ("page_screen_output", true);
      else if (arg == "off")
	bind_builtin_variable ("page_screen_output", false);
      else
	error ("more: unrecognized argument `%s'", arg.c_str ());
    }
  else if (argc == 1)
    {
      octave_value tmp = builtin_any_variable ("page_screen_output");

      if (! error_state)
	bind_builtin_variable ("page_screen_output", ! tmp.is_true ());
    }
  else
    print_usage ("more");

  return retval;
}

static std::string
default_pager (void)
{
  std::string pager_binary = octave_env::getenv ("PAGER");

#ifdef OCTAVE_DEFAULT_PAGER
  if (pager_binary.empty ())
    {
      pager_binary = std::string (OCTAVE_DEFAULT_PAGER);

      if (pager_binary == "less")
	{
	  pager_binary.append (" -e");

	  std::string lessflags = octave_env::getenv ("LESS");
	  if (lessflags.empty ())
	    pager_binary.append
	      (" -X -P'-- less ?pB(%pB\\%):--. (f)orward, (b)ack, (q)uit$'");
	}
    }
#endif

  return pager_binary;
}

static int
pager_binary (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("PAGER");

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
  DEFVAR (PAGER, default_pager (), pager_binary,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} PAGER\n\
The default value is normally @code{\"less\"}, @code{\"more\"}, or\n\
@code{\"pg\"}, depending on what programs are installed on your system.\n\
@xref{Installation}.\n\
\n\
When running interactively, Octave sends any output intended for your\n\
terminal that is more than one screen long to the program named by the\n\
value of the variable @code{PAGER}.\n\
@end defvr");

  DEFVAR (page_output_immediately, false, page_output_immediately,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} page_output_immediately\n\
If the value of @code{page_output_immediately} is nonzero, Octave sends\n\
output to the pager as soon as it is available.  Otherwise, Octave\n\
buffers its output and waits until just before the prompt is printed to\n\
flush it to the pager.  The default value is 0.\n\
@end defvr");

  DEFVAR (page_screen_output, true, page_screen_output,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} page_screen_output\n\
If the value of @code{page_screen_output} is nonzero, all output\n\
intended for the screen that is longer than one page is sent through a\n\
pager.  This allows you to view one screenful at a time.  Some pagers\n\
(such as @code{less}---see @ref{Installation}) are also capable of moving\n\
backward on the output.  The default value is 1.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
