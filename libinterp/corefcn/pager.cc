/*

Copyright (C) 1993-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <fstream>
#include <iostream>
#include <string>

#include "cmd-edit.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "singleton-cleanup.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "octave.h"
#include "ovl.h"
#include "pager.h"
#include "procstream.h"
#include "sighandlers.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Our actual connection to the external pager.
static oprocstream *external_pager = 0;

// TRUE means we write to the diary file.
static bool write_to_diary_file = false;

// The name of the current diary file.
static std::string diary_file ("diary");

// The diary file.
static std::ofstream external_diary_file;

static std::string
default_pager (void)
{
  std::string pager_binary = octave::sys::env::getenv ("PAGER");

#if defined (OCTAVE_DEFAULT_PAGER)
  if (pager_binary.empty ())
    pager_binary = OCTAVE_DEFAULT_PAGER;
#endif

  return pager_binary;
}

// The shell command to run as the pager.
static std::string VPAGER = default_pager ();

// The options to pass to the pager.
static std::string VPAGER_FLAGS;

// TRUE means that if output is going to the pager, it is sent as soon
// as it is available.  Otherwise, it is buffered and only sent to the
// pager when it is time to print another prompt.
static bool Vpage_output_immediately = false;

// TRUE means all output intended for the screen should be passed
// through the pager.
static bool Vpage_screen_output = true;

static bool really_flush_to_pager = false;

static bool flushing_output_to_pager = false;

static void
clear_external_pager (void)
{
  if (external_pager)
    {
      octave::child_list::remove (external_pager->pid ());

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
      if (octave::sys::wifexited (status) || octave::sys::wifsignaled (status))
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

static std::string
pager_command (void)
{
  std::string cmd = VPAGER;

  if (! (cmd.empty () || VPAGER_FLAGS.empty ()))
    cmd += " " + VPAGER_FLAGS;

  return cmd;
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
              std::string pgr = pager_command ();

              if (! pgr.empty ())
                {
                  external_pager = new oprocstream (pgr.c_str ());

                  if (external_pager)
                    octave::child_list::insert (external_pager->pid (),
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
                  // FIXME: omething is not right with the
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
      int available_rows = octave::command_editor::terminal_rows () - 2;

      int cols = octave::command_editor::terminal_cols ();

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
  if (! octave::application::interactive ()
      || octave::application::forced_interactive ()
      || really_flush_to_pager
      || (Vpage_screen_output && Vpage_output_immediately)
      || ! Vpage_screen_output)
    {
      char *buf = eback ();

      int len = pptr () - buf;

      bool bypass_pager = (! octave::application::interactive ()
                           || octave::application::forced_interactive ()
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

std::ostream&
octave_pager_stream::stream (void)
{
  return instance_ok () ? *instance : std::cout;
}

void
octave_pager_stream::flush_current_contents_to_diary (void)
{
  if (instance_ok ())
    instance->do_flush_current_contents_to_diary ();
}

void
octave_pager_stream::set_diary_skip (void)
{
  if (instance_ok ())
    instance->do_set_diary_skip ();
}

// Reinitialize the pager buffer to avoid hanging on to large internal
// buffers when they might not be needed.  This function should only be
// called when the pager is not in use.  For example, just before
// getting command-line input.

void
octave_pager_stream::reset (void)
{
  if (instance_ok ())
    instance->do_reset ();
}

void
octave_pager_stream::do_flush_current_contents_to_diary (void)
{
  if (pb)
    pb->flush_current_contents_to_diary ();
}

void
octave_pager_stream::do_set_diary_skip (void)
{
  if (pb)
    pb->set_diary_skip ();
}

void
octave_pager_stream::do_reset (void)
{
  delete pb;
  pb = new octave_pager_buf ();
  rdbuf (pb);
  setf (unitbuf);
}

bool
octave_pager_stream::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_pager_stream ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    error ("unable to create pager_stream object!");

  return retval;
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

std::ostream&
octave_diary_stream::stream (void)
{
  return instance_ok () ? *instance : std::cout;
}

// Reinitialize the diary buffer to avoid hanging on to large internal
// buffers when they might not be needed.  This function should only be
// called when the pager is not in use.  For example, just before
// getting command-line input.

void
octave_diary_stream::reset (void)
{
  if (instance_ok ())
    instance->do_reset ();
}

void
octave_diary_stream::do_reset (void)
{
  delete db;
  db = new octave_diary_buf ();
  rdbuf (db);
  setf (unitbuf);
}

bool
octave_diary_stream::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_diary_stream ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    error ("unable to create diary_stream object!");

  return retval;
}

void
flush_octave_stdout (void)
{
  if (! flushing_output_to_pager)
    {
      octave::unwind_protect frame;

      frame.protect_var (really_flush_to_pager);
      frame.protect_var (flushing_output_to_pager);

      really_flush_to_pager = true;
      flushing_output_to_pager = true;

      octave_stdout.flush ();

      clear_external_pager ();
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

  octave_pager_stream::flush_current_contents_to_diary ();

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

  octave_pager_stream::set_diary_skip ();

  external_diary_file.open (diary_file.c_str (), std::ios::app);

  if (! external_diary_file)
    error ("diary: can't open diary file '%s'", diary_file.c_str ());
}

DEFUN (diary, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} diary
@deftypefnx {} {} diary on
@deftypefnx {} {} diary off
@deftypefnx {} {} diary @var{filename}
@deftypefnx {} {[@var{status}, @var{diaryfile}] =} diary
Record a list of all commands @emph{and} the output they produce, mixed
together just as they appear on the terminal.

Valid options are:

@table @asis
@item on
Start recording a session in a file called @file{diary} in the current working
directory.

@item off
Stop recording the session in the diary file.

@item @var{filename}
Record the session in the file named @var{filename}.
@end table

With no input or output arguments, @code{diary} toggles the current diary
state.

If output arguments are requested, @code{diary} ignores inputs and returns
the current status.  The boolean @var{status} indicates whether recording is on
or off, and @var{diaryfile} is the name of the file where the session is
stored.
@seealso{history, evalc}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (diary_file.empty ())
    diary_file = "diary";

  if (nargout > 0)
    {
      // Querying diary variables
      if (nargout == 1)
        return ovl (write_to_diary_file);
      else
        return ovl (write_to_diary_file, diary_file);
    }

  if (nargin == 0)
    {
      write_to_diary_file = ! write_to_diary_file;
      open_diary_file ();
    }
  else
    {
      std::string arg = args(0).xstring_value ("diary: argument must be a string");

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

  return ovl ();
}

DEFUN (more, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} more
@deftypefnx {} {} more on
@deftypefnx {} {} more off
Turn output pagination on or off.

Without an argument, @code{more} toggles the current state.

The current state can be determined via @code{page_screen_output}.
@seealso{page_screen_output, page_output_immediately, PAGER, PAGER_FLAGS}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin > 0)
    {
      std::string arg = args(0).xstring_value ("more: argument must be string \"on\" or \"off\"");

      if (arg == "on")
        Vpage_screen_output = true;
      else if (arg == "off")
        Vpage_screen_output = false;
      else
        error ("more: argument must be \"on\" or \"off\"");
    }
  else
    Vpage_screen_output = ! Vpage_screen_output;

  return ovl ();
}

DEFUN (terminal_size, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} terminal_size ()
Return a two-element row vector containing the current size of the terminal
window in characters (rows and columns).
@seealso{list_in_columns}
@end deftypefn */)
{
  RowVector size (2, 0.0);

  size(0) = octave::command_editor::terminal_rows ();
  size(1) = octave::command_editor::terminal_cols ();

  return ovl (size);
}

DEFUN (page_output_immediately, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} page_output_immediately ()
@deftypefnx {} {@var{old_val} =} page_output_immediately (@var{new_val})
@deftypefnx {} {} page_output_immediately (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave sends
output to the pager as soon as it is available.

Otherwise, Octave buffers its output and waits until just before the prompt
is printed to flush it to the pager.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{page_screen_output, more, PAGER, PAGER_FLAGS}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (page_output_immediately);
}

DEFUN (page_screen_output, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} page_screen_output ()
@deftypefnx {} {@var{old_val} =} page_screen_output (@var{new_val})
@deftypefnx {} {} page_screen_output (@var{new_val}, "local")
Query or set the internal variable that controls whether output intended
for the terminal window that is longer than one page is sent through a
pager.

This allows you to view one screenful at a time.  Some pagers
(such as @code{less}---see @ref{Installation}) are also capable of moving
backward on the output.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{more, page_output_immediately, PAGER, PAGER_FLAGS}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (page_screen_output);
}

DEFUN (PAGER, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PAGER ()
@deftypefnx {} {@var{old_val} =} PAGER (@var{new_val})
@deftypefnx {} {} PAGER (@var{new_val}, "local")
Query or set the internal variable that specifies the program to use
to display terminal output on your system.

The default value is normally @qcode{"less"}, @qcode{"more"}, or
@qcode{"pg"}, depending on what programs are installed on your system.
@xref{Installation}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{PAGER_FLAGS, page_output_immediately, more, page_screen_output}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (PAGER);
}

DEFUN (PAGER_FLAGS, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PAGER_FLAGS ()
@deftypefnx {} {@var{old_val} =} PAGER_FLAGS (@var{new_val})
@deftypefnx {} {} PAGER_FLAGS (@var{new_val}, "local")
Query or set the internal variable that specifies the options to pass
to the pager.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{PAGER, more, page_screen_output, page_output_immediately}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (PAGER_FLAGS);
}

