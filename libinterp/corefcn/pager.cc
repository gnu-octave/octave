////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <fstream>
#include <iostream>
#include <string>

#include "child-list.h"
#include "cmd-edit.h"
#include "oct-env.h"
#include "oct-syscalls.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "octave.h"
#include "ovl.h"
#include "pager.h"
#include "procstream.h"
#include "sighandlers.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static bool
pager_event_handler (pid_t pid, int status)
{
  bool retval = false;

  if (pid > 0)
    {
      if (sys::wifexited (status) || sys::wifsignaled (status))
        {
          // Avoid warning() since that will put us back in the pager,
          // which would be bad news.

          std::cerr << "warning: connection to external pager lost (pid = "
                    << pid << ')' << std::endl;
          std::cerr << "warning: flushing pending output (please wait)"
                    << std::endl;

          // Request removal of this PID from the list of child
          // processes.

          retval = true;
        }
    }

  return retval;
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

static std::string default_pager (void)
{
  std::string pager_binary = sys::env::getenv ("PAGER");

  if (pager_binary.empty ())
    pager_binary = config::default_pager ();

  return pager_binary;
}

int
pager_buf::sync (void)
{
  output_system& output_sys = __get_output_system__ ();

  char *buf = pbase ();

  int len = pptr () - buf;

  if (output_sys.sync (buf, len))
    {
      flush_current_contents_to_diary ();

      seekoff (0, std::ios::beg);
    }

  return 0;
}

void
pager_buf::flush_current_contents_to_diary (void)
{
  char *buf = pbase () + m_diary_skip;

  std::size_t len = pptr () - buf;

  octave_diary.write (buf, len);

  m_diary_skip = 0;
}

void
pager_buf::set_diary_skip (void)
{
  m_diary_skip = pptr () - pbase ();
}

int
diary_buf::sync (void)
{
  output_system& output_sys = __get_output_system__ ();

  std::ofstream& external_diary_file = output_sys.external_diary_file ();

  if (output_sys.write_to_diary_file () && external_diary_file)
    {
      char *buf = pbase ();

      int len = pptr () - buf;

      if (len > 0)
        external_diary_file.write (buf, len);
    }

  seekoff (0, std::ios::beg);

  return 0;
}

pager_stream::pager_stream (void) : std::ostream (nullptr), m_pb (nullptr)
{
  m_pb = new pager_buf ();
  rdbuf (m_pb);
  setf (unitbuf);
}

pager_stream::~pager_stream (void)
{
  flush ();
  delete m_pb;
}

std::ostream& pager_stream::stream (void)
{
  return *this;
}

void pager_stream::flush_current_contents_to_diary (void)
{
  if (m_pb)
    m_pb->flush_current_contents_to_diary ();
}

void pager_stream::set_diary_skip (void)
{
  if (m_pb)
    m_pb->set_diary_skip ();
}

// Reinitialize the pager buffer to avoid hanging on to large internal
// buffers when they might not be needed.  This function should only be
// called when the pager is not in use.  For example, just before
// getting command-line input.

void pager_stream::reset (void)
{
  delete m_pb;
  m_pb = new pager_buf ();
  rdbuf (m_pb);
  setf (unitbuf);
}

diary_stream::diary_stream (void) : std::ostream (nullptr), m_db (nullptr)
{
  m_db = new diary_buf ();
  rdbuf (m_db);
  setf (unitbuf);
}

diary_stream::~diary_stream (void)
{
  flush ();
  delete m_db;
}

std::ostream& diary_stream::stream (void)
{
  return *this;
}

// Reinitialize the diary buffer to avoid hanging on to large internal
// buffers when they might not be needed.  This function should only be
// called when the pager is not in use.  For example, just before
// getting command-line input.

void diary_stream::reset (void)
{
  delete m_db;
  m_db = new diary_buf ();
  rdbuf (m_db);
  setf (unitbuf);
}

void flush_stdout (void)
{
  output_system& output_sys = __get_output_system__ ();

  output_sys.flush_stdout ();
}

output_system::output_system (interpreter& interp)
  : m_interpreter (interp), m_pager_stream (), m_diary_stream (),
    m_external_pager (nullptr), m_external_diary_file (),
    m_diary_file_name ("diary"), m_PAGER (default_pager ()),
    m_PAGER_FLAGS (), m_page_output_immediately (false),
    m_page_screen_output (false), m_write_to_diary_file (false),
    m_really_flush_to_pager (false), m_flushing_output_to_pager (false)
{ }

octave_value output_system::PAGER (const octave_value_list& args,
                                   int nargout)
{
  return set_internal_variable (m_PAGER, args, nargout, "PAGER", false);
}

octave_value output_system::PAGER_FLAGS (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_PAGER_FLAGS, args, nargout,
                                "PAGER_FLAGS", false);
}

octave_value
output_system::page_output_immediately (const octave_value_list& args,
                                        int nargout)
{
  return set_internal_variable (m_page_output_immediately, args, nargout,
                                "page_output_immediately");
}

octave_value
output_system::page_screen_output (const octave_value_list& args,
                                   int nargout)
{
  return set_internal_variable (m_page_screen_output, args, nargout,
                                "page_screen_output");
}

std::string output_system::pager_command (void) const
{
  std::string cmd = m_PAGER;

  if (! (cmd.empty () || m_PAGER_FLAGS.empty ()))
    cmd += ' ' + m_PAGER_FLAGS;

  return cmd;
}

void output_system::reset (void)
{
  flush_stdout ();

  m_pager_stream.reset ();
  m_diary_stream.reset ();
}

void output_system::flush_stdout (void)
{
  if (! m_flushing_output_to_pager)
    {
      unwind_protect_var<bool> restore_var1 (m_really_flush_to_pager);
      unwind_protect_var<bool> restore_var2 (m_flushing_output_to_pager);

      m_really_flush_to_pager = true;
      m_flushing_output_to_pager = true;

      std::ostream& pager_ostream = m_pager_stream.stream ();

      pager_ostream.flush ();

      clear_external_pager ();
    }
}

void output_system::close_diary (void)
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

  m_pager_stream.flush_current_contents_to_diary ();

  if (m_external_diary_file.is_open ())
    {
      octave_diary.flush ();
      m_external_diary_file.close ();
    }
}

void output_system::open_diary (void)
{
  close_diary ();

  // If there is pending output in the pager buf, it should not go
  // into the diary file.

  m_pager_stream.set_diary_skip ();

  m_external_diary_file.open (m_diary_file_name.c_str (), std::ios::app);

  if (! m_external_diary_file)
    error ("diary: can't open diary file '%s'", m_diary_file_name.c_str ());
}

bool output_system::sync (const char *buf, int len)
{
  // FIXME: The following seems to be a bit of a mess.

  if (m_interpreter.server_mode ()
      || ! m_interpreter.interactive ()
      || application::forced_interactive ()
      || m_really_flush_to_pager
      || (m_page_screen_output && m_page_output_immediately)
      || ! m_page_screen_output)
    {
      bool bypass_pager = (m_interpreter.server_mode ()
                           || ! m_interpreter.interactive ()
                           || application::forced_interactive ()
                           || ! m_page_screen_output
                           || (m_really_flush_to_pager
                               && m_page_screen_output
                               && ! m_page_output_immediately
                               && ! more_than_a_screenful (buf, len)));

      if (len > 0)
        {
          do_sync (buf, len, bypass_pager);

          return true;
        }
    }

  return false;
}

void output_system::clear_external_pager (void)
{
  if (m_external_pager)
    {
      child_list& kids = m_interpreter.get_child_list ();

      kids.remove (m_external_pager->pid ());

      delete m_external_pager;
      m_external_pager = nullptr;
    }
}

void output_system::start_external_pager (void)
{
  if (m_external_pager)
    return;

  std::string pgr = pager_command ();

  if (! pgr.empty ())
    {
      m_external_pager = new oprocstream (pgr.c_str ());

      child_list& kids = m_interpreter.get_child_list ();

      kids.insert (m_external_pager->pid (),
                   pager_event_handler);
    }
}

void output_system::do_sync (const char *msg, int len, bool bypass_pager)
{
  if (msg && len > 0)
    {
      if (bypass_pager)
        {
          if (m_interpreter.server_mode ())
            {
              event_manager& evmgr = m_interpreter.get_event_manager ();

              evmgr.interpreter_output (std::string (msg, len));
            }
          else
            {
              std::cout.write (msg, len);
              std::cout.flush ();
            }
        }
      else
        {
          start_external_pager ();

          if (m_external_pager)
            {
              if (m_external_pager->good ())
                {
                  m_external_pager->write (msg, len);

                  m_external_pager->flush ();

#if defined (EPIPE)
                  if (errno == EPIPE)
                    m_external_pager->setstate (std::ios::failbit);
#endif
                }
              else
                {
                  // FIXME: something is not right with the
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

std::ostream& __stdout__ (void)
{
  output_system& output_sys = __get_output_system__ ();

  return output_sys.__stdout__ ();
}

std::ostream& __diary__ (void)
{
  output_system& output_sys = __get_output_system__ ();

  return output_sys.__diary__ ();
}

DEFMETHOD (diary, interp, args, nargout,
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

  output_system& output_sys = interp.get_output_system ();

  if (nargout > 0)
    {
      // Querying diary variables
      if (nargout == 1)
        return ovl (output_sys.write_to_diary_file ());
      else
        return ovl (output_sys.write_to_diary_file (),
                    output_sys.diary_file_name ());
    }

  if (nargin == 0)
    {
      output_sys.write_to_diary_file (! output_sys.write_to_diary_file ());
      output_sys.open_diary ();
    }
  else
    {
      std::string arg = args(0).xstring_value ("diary: argument must be a string");

      if (arg == "on")
        {
          output_sys.write_to_diary_file (true);
          output_sys.open_diary ();
        }
      else if (arg == "off")
        {
          output_sys.close_diary ();
          output_sys.write_to_diary_file (false);
        }
      else
        {
          output_sys.diary_file_name (arg);
          output_sys.write_to_diary_file (true);
          output_sys.open_diary ();
        }
    }

  return ovl ();
}

DEFMETHOD (more, interp, args, ,
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

  output_system& output_sys = interp.get_output_system ();

  if (nargin > 0)
    {
      std::string arg = args(0).xstring_value (R"(more: argument must be string "on" or "off")");

      if (arg == "on")
        output_sys.page_screen_output (true);
      else if (arg == "off")
        output_sys.page_screen_output (false);
      else
        error (R"(more: argument must be "on" or "off")");
    }
  else
    output_sys.page_screen_output (! output_sys.page_screen_output ());

  return ovl ();
}

DEFUN (terminal_size, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{rows}, @var{cols}] =} terminal_size ()
@deftypefnx {} {} terminal_size ([@var{rows}, @var{cols}])
Query or set the size of the terminal window.  If called with no arguments,
return a two-element row vector containing the current size of the terminal
window in characters (rows and columns).  If called with a two-element vector
of integer values, set the terminal size and return the previous setting.
Setting the size manually should not be needed when using readline for
command-line editing.
@seealso{list_in_columns}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  RowVector size (2, 0.0);

  size(0) = command_editor::terminal_rows ();
  size(1) = command_editor::terminal_cols ();

  if (nargin == 1)
    {
      Matrix m = args(0).xmatrix_value ("argument must be a 2-element array");

      if (m.numel () != 2)
        error ("terminal_size: argument must be a 2-element array");

      int rows = math::x_nint (m(0));
      int cols = math::x_nint (m(1));

      if (rows <= 0 || cols <= 0)
        error ("terminal_size: rows and columns must be positive integers");

      command_editor::set_screen_size (rows, cols);
    }

  return ovl (size);
}

DEFMETHOD (page_output_immediately, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} page_output_immediately ()
@deftypefnx {} {@var{old_val} =} page_output_immediately (@var{new_val})
@deftypefnx {} {@var{old_val} =} page_output_immediately (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave sends
output to the pager as soon as it is available.

When the value is @code{false}, Octave buffers its output and waits until just
before the prompt is printed to flush it to the pager.  This is the default.

When @code{page_screen_output} is @code{false}, this variable has no effect.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{page_screen_output, more, PAGER, PAGER_FLAGS}
@end deftypefn */)
{
  output_system& output_sys = interp.get_output_system ();

  return output_sys.page_output_immediately (args, nargout);
}

DEFMETHOD (page_screen_output, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} page_screen_output ()
@deftypefnx {} {@var{old_val} =} page_screen_output (@var{new_val})
@deftypefnx {} {@var{old_val} =} page_screen_output (@var{new_val}, "local")
Query or set the internal variable that controls whether output intended
for the terminal window that is longer than one page is sent through a
pager.

This allows you to view one screenful at a time.  Some pagers
(such as @code{less}---@pxref{Installation}) are also capable of moving
backward on the output.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{more, page_output_immediately, PAGER, PAGER_FLAGS}
@end deftypefn */)
{
  output_system& output_sys = interp.get_output_system ();

  return output_sys.page_screen_output (args, nargout);
}

DEFMETHOD (PAGER, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PAGER ()
@deftypefnx {} {@var{old_val} =} PAGER (@var{new_val})
@deftypefnx {} {@var{old_val} =} PAGER (@var{new_val}, "local")
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
  output_system& output_sys = interp.get_output_system ();

  return output_sys.PAGER (args, nargout);
}

DEFMETHOD (PAGER_FLAGS, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PAGER_FLAGS ()
@deftypefnx {} {@var{old_val} =} PAGER_FLAGS (@var{new_val})
@deftypefnx {} {@var{old_val} =} PAGER_FLAGS (@var{new_val}, "local")
Query or set the internal variable that specifies the options to pass
to the pager.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{PAGER, more, page_screen_output, page_output_immediately}
@end deftypefn */)
{
  output_system& output_sys = interp.get_output_system ();

  return output_sys.PAGER_FLAGS (args, nargout);
}

OCTAVE_END_NAMESPACE(octave)
