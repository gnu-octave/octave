/*

Copyright (C) 1995-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <cassert>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <new>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <sys/select.h>
#include <sys/types.h>
#include <unistd.h>

#include "cmd-edit.h"
#include "cmd-hist.h"
#include "file-ops.h"
#include "lo-error.h"
#include "lo-mappers.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "singleton-cleanup.h"
#include "str-vec.h"

#include "build-env.h"
#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "file-io.h"
#include "graphics.h"
#include "input.h"
#include "lex.h"
#include "load-save.h"
#include "octave-link.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "ovl.h"
#include "ov.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "pt-eval.h"
#include "pt-jump.h"
#include "pt-stmt.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "syswait.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

#ifndef SHELL_PATH
#  define SHELL_PATH "/bin/sh"
#endif

void (*octave_exit) (int) = ::exit;

// TRUE means the quit() call is allowed.
bool quit_allowed = true;

// TRUE means we are exiting via the builtin exit or quit functions.
bool quitting_gracefully = false;
// This stores the exit status.
int exit_status = 0;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
bool octave_interpreter_ready = false;

// TRUE means we've processed all the init code and we are good to go.
bool octave_initialized = false;

octave_call_stack *octave_call_stack::instance = 0;

std::string
octave_call_stack::stack_frame::fcn_file_name (void) const
{
  return m_fcn ? m_fcn->fcn_file_name () : "";
}

std::string
octave_call_stack::stack_frame::fcn_name (bool print_subfn) const
{
  std::string retval;

  if (m_fcn)
    {
      std::string parent_fcn_name = m_fcn->parent_fcn_name ();

      if (print_subfn && ! parent_fcn_name.empty ())
        retval = parent_fcn_name + Vfilemarker;

      retval += m_fcn->name ();
    }
  else
    retval = "<unknown>";

  return retval;
}

bool
octave_call_stack::stack_frame::operator== (const octave_call_stack::stack_frame &rhs) const
{
  if (this->line () != rhs.line ())
    return false;
  else if (this->column () != rhs.column ())
    return false;
  else if (this->fcn_file_name () != rhs.fcn_file_name ())
    return false;
  else if (this->fcn_name () != rhs.fcn_name ())
    return false;
  else
    return true;
}

void
octave_call_stack::create_instance (void)
{
  instance = new octave_call_stack ();

  if (instance)
    {
      instance->do_push (0, symbol_table::top_scope (), 0);

      singleton_cleanup_list::add (cleanup_instance);
    }
}

int
octave_call_stack::do_current_line (void) const
{
  int retval = -1;

  if (! cs.empty ())
    {
      const stack_frame& elt = cs[curr_frame];
      retval = elt.m_line;
    }

  return retval;
}

int
octave_call_stack::do_current_column (void) const
{
  int retval = -1;

  if (! cs.empty ())
    {
      const stack_frame& elt = cs[curr_frame];
      retval = elt.m_column;
    }

  return retval;
}

size_t
octave_call_stack::do_num_user_code_frames
  (octave_idx_type& curr_user_frame) const
{
  size_t retval = 0;

  curr_user_frame = 0;

  // Look for the caller of dbstack.
  size_t xframe = cs[curr_frame].m_prev;

  bool found = false;

  size_t k = cs.size ();

  for (const_reverse_iterator p = cs.rbegin (); p != cs.rend (); p++)
    {
      octave_function *f = (*p).m_fcn;

      if (--k == xframe)
        found = true;

      if (f && f->is_user_code ())
        {
          if (! found)
            curr_user_frame++;

          retval++;
        }
    }

  // We counted how many user frames were not the one, in reverse.
  // Now set curr_user_frame to be the index in the other direction.
  curr_user_frame = retval - curr_user_frame - 1;

  return retval;
}

octave_user_code *
octave_call_stack::do_caller_user_code (size_t nskip) const
{
  octave_user_code *retval = 0;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (nskip > 0)
            nskip--;
          else
            {
              retval = dynamic_cast<octave_user_code *> (f);
              break;
            }
        }
    }

  return retval;
}

int
octave_call_stack::do_caller_user_code_line (void) const
{
  int retval = -1;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (elt.m_line > 0)
            {
              retval = elt.m_line;
              break;
            }
        }
    }

  return retval;
}

int
octave_call_stack::do_caller_user_code_column (void) const
{
  int retval = -1;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (elt.m_column)
            {
              retval = elt.m_column;
              break;
            }
        }
    }

  return retval;
}

octave_user_code *
octave_call_stack::do_debug_user_code (void) const
{
  octave_user_code *retval = 0;

  // This should never happen...
  if (curr_frame == 0)
    return retval;

  // Start looking with the caller of the calling debug function.
  size_t i = cs[curr_frame].m_prev;

  while (i != 0)
    {
      const stack_frame& elt = cs[i];

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          retval = dynamic_cast<octave_user_code *> (f);
          break;
        }
    }

  return retval;
}

int
octave_call_stack::do_debug_user_code_line (void) const
{
  int retval = -1;

  // This should never happen...
  if (curr_frame == 0)
    return retval;

  // Start looking with the caller of the calling debug function.
  size_t i = cs[curr_frame].m_prev;

  while (i != 0)
    {
      const stack_frame& elt = cs[i];

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (elt.m_line)
            {
              retval = elt.m_line;
              break;
            }
        }
    }

  return retval;
}

int
octave_call_stack::do_debug_user_code_column (void) const
{
  int retval = -1;

  // This should never happen...
  if (curr_frame == 0)
    return retval;

  // Start looking with the caller of the calling debug function.
  size_t i = cs[curr_frame].m_prev;

  while (i != 0)
    {
      const stack_frame& elt = cs[i];

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (elt.m_column)
            {
              retval = elt.m_column;
              break;
            }
        }
    }

  return retval;
}

bool
octave_call_stack::do_all_scripts (void) const
{
  bool retval = true;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && ! f->is_user_script ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

// Use static fields for the best efficiency.
// NOTE: C++0x will allow these two to be merged into one.
static const char *bt_fieldnames[] = { "file", "name", "line",
                                       "column", "scope", "context", 0
                                     };
static const octave_fields bt_fields (bt_fieldnames);

octave_map
octave_call_stack::empty_backtrace (void)
{
  return octave_map (dim_vector (0, 1), bt_fields);
}

std::list<octave_call_stack::stack_frame>
octave_call_stack::do_backtrace_frames (size_t nskip,
                                        octave_idx_type& curr_user_frame) const
{
  std::list<octave_call_stack::stack_frame> retval;

  size_t user_code_frames = do_num_user_code_frames (curr_user_frame);

  size_t nframes = nskip <= user_code_frames ? user_code_frames - nskip : 0;

  // Our list is reversed.
  curr_user_frame = nframes - curr_user_frame - 1;

  if (nframes > 0)
    {
      for (const_reverse_iterator p = cs.rbegin (); p != cs.rend (); p++)
        {
          const stack_frame& elt = *p;

          octave_function *f = elt.m_fcn;

          if (f && f->is_user_code ())
            {
              if (nskip > 0)
                nskip--;
              else
                retval.push_back (elt);
            }
        }
    }

  return retval;
}

octave_map
octave_call_stack::do_backtrace (size_t nskip,
                                 octave_idx_type& curr_user_frame,
                                 bool print_subfn) const
{
  std::list<octave_call_stack::stack_frame> frames
    = do_backtrace_frames (nskip, curr_user_frame);

  size_t nframes = frames.size ();

  octave_map retval (dim_vector (nframes, 1), bt_fields);

  Cell& file = retval.contents (0);
  Cell& name = retval.contents (1);
  Cell& line = retval.contents (2);
  Cell& column = retval.contents (3);
  Cell& scope = retval.contents (4);
  Cell& context = retval.contents (5);

  octave_idx_type k = 0;

  for (std::list<octave_call_stack::stack_frame>::const_iterator p = frames.begin ();
       p != frames.end (); p++)
    {
      const stack_frame& elt = *p;

      scope(k) = elt.m_scope;
      context(k) = elt.m_context;
      file(k) = elt.fcn_file_name ();
      name(k) = elt.fcn_name (print_subfn);
      line(k) = elt.m_line;
      column(k) = elt.m_column;

      k++;
    }

  return retval;
}

bool
octave_call_stack::do_goto_frame (size_t n, bool verbose)
{
  bool retval = false;

  if (n < cs.size ())
    {
      retval = true;

      curr_frame = n;

      const stack_frame& elt = cs[n];

      symbol_table::set_scope_and_context (elt.m_scope, elt.m_context);

      if (verbose)
        octave_stdout << "stopped in " << elt.fcn_name ()
                      << " at line " << elt.m_line
                      << " column " << elt.m_column
                      << " [" << elt.fcn_file_name () << "] "
                      << " (scope = " << elt.m_scope
                      << "[context = " << elt.m_context << "])"
                      << std::endl;
    }

  return retval;
}

bool
octave_call_stack::do_goto_frame_relative (int nskip, bool verbose)
{
  bool retval = false;

  int incr = 0;

  if (nskip < 0)
    incr = -1;
  else if (nskip > 0)
    incr = 1;

  // Start looking with the caller of dbup/dbdown/keyboard.
  size_t xframe = cs[curr_frame].m_prev;

  while (true)
    {
      if ((incr < 0 && xframe == 0) || (incr > 0 && xframe == cs.size () - 1))
        break;

      xframe += incr;

      const stack_frame& elt = cs[xframe];

      octave_function *f = elt.m_fcn;

      if (xframe == 0 || (f && f->is_user_code ()))
        {
          if (nskip > 0)
            nskip--;
          else if (nskip < 0)
            nskip++;

          if (nskip == 0)
            {
              curr_frame = xframe;
              cs[cs.size () - 1].m_prev = curr_frame;

              symbol_table::set_scope_and_context (elt.m_scope, elt.m_context);

              if (verbose)
                {
                  std::ostringstream buf;

                  if (f)
                    buf << "stopped in " << elt.fcn_name ()
                        << " at line " << elt.m_line
                        << " [" << elt.fcn_file_name () << "] "
                        << std::endl;
                  else
                    buf << "at top level" << std::endl;

                  octave_stdout << buf.str ();
                }

              retval = true;
              break;
            }
        }
      else if (incr == 0)  // Break out of infinite loop by choosing an incr.
        incr = -1;

      // There is no need to set scope and context here.  That will
      // happen when the dbup/dbdown/keyboard frame is popped and we
      // jump to the new "prev" frame set above.
    }

  return retval;
}

void
octave_call_stack::do_goto_caller_frame (void)
{
  size_t xframe = curr_frame;

  bool skipped = false;

  while (xframe != 0)
    {
      xframe = cs[xframe].m_prev;

      const stack_frame& elt = cs[xframe];

      octave_function *f = elt.m_fcn;

      if (elt.m_scope == cs[0].m_scope || (f && f->is_user_code ()))
        {
          if (! skipped)
            // We found the current user code frame, so skip it.
            skipped = true;
          else
            {
              // We found the caller user code frame.
              stack_frame tmp (elt);
              tmp.m_prev = curr_frame;

              curr_frame = cs.size ();

              cs.push_back (tmp);

              symbol_table::set_scope_and_context (tmp.m_scope, tmp.m_context);

              break;
            }
        }
    }
}

void
octave_call_stack::do_goto_base_frame (void)
{
  stack_frame tmp (cs[0]);
  tmp.m_prev = curr_frame;

  curr_frame = cs.size ();

  cs.push_back (tmp);

  symbol_table::set_scope_and_context (tmp.m_scope, tmp.m_context);
}

void
recover_from_exception (void)
{
  can_interrupt = true;
  octave_interrupt_immediately = 0;
  octave_interrupt_state = 0;
  octave_signal_caught = 0;
  octave_exception_state = octave_no_exception;
  octave_restore_signal_mask ();
  octave_catch_interrupts ();
}

int
main_loop (void)
{
  octave_save_signal_mask ();

  can_interrupt = true;

  octave_signal_hook = octave_signal_handler;
  octave_interrupt_hook = 0;
  octave_bad_alloc_hook = 0;

  octave_catch_interrupts ();

  octave_initialized = true;

  // The big loop.

  octave_lexer *lxr = (interactive
                       ? new octave_lexer ()
                       : new octave_lexer (stdin));

  octave_parser parser (*lxr);

  int retval = 0;
  do
    {
      try
        {
          reset_error_handler ();

          parser.reset ();

          if (symbol_table::at_top_level ())
            tree_evaluator::reset_debug_state ();

          retval = parser.run ();

          if (retval == 0)
            {
              if (parser.stmt_list)
                {
                  parser.stmt_list->accept (*current_evaluator);

                  octave_quit ();

                  if (! interactive)
                    {
                      bool quit = (tree_return_command::returning
                                   || tree_break_command::breaking);

                      if (tree_return_command::returning)
                        tree_return_command::returning = 0;

                      if (tree_break_command::breaking)
                        tree_break_command::breaking--;

                      if (quit)
                        break;
                    }

                  if (octave_completion_matches_called)
                    octave_completion_matches_called = false;
                  else
                    command_editor::increment_current_command_number ();
                }
              else if (parser.lexer.end_of_input)
                break;
            }
        }
      catch (const octave_interrupt_exception&)
        {
          recover_from_exception ();

          if (quitting_gracefully)
            return exit_status;

          // Required newline when the user does Ctrl+C at the prompt.
          if (interactive)
            octave_stdout << "\n";
        }
      catch (const index_exception& e)
        {
          recover_from_exception ();

          std::cerr << "error: unhandled index exception: "
                    << e.message () << " -- trying to return to prompt"
                    << std::endl;
        }
      catch (const octave_execution_exception& e)
        {
          std::string stack_trace = e.info ();

          if (! stack_trace.empty ())
            std::cerr << stack_trace;

          if (interactive)
            recover_from_exception ();
          else
            {
              // We should exit with a nonzero status.
              retval = 1;
              break;
            }
        }
      catch (const std::bad_alloc&)
        {
          recover_from_exception ();

          std::cerr << "error: out of memory -- trying to return to prompt"
                    << std::endl;
        }

#ifdef DBSTOP_NANINF
      if (Vdebug_on_naninf)
        {
          if (setjump (naninf_jump) != 0)
            debug_or_throw_exception (true);  // true = stack trace
        }
#endif
    }
  while (retval == 0);

  if (interactive)
    octave_stdout << "\n";

  if (retval == EOF)
    retval = 0;

  return retval;
}

// Fix up things before exiting.

static std::list<std::string> octave_atexit_functions;

static void
do_octave_atexit (void)
{
  static bool deja_vu = false;

  OCTAVE_SAFE_CALL (remove_input_event_hook_functions, ());

  while (! octave_atexit_functions.empty ())
    {
      std::string fcn = octave_atexit_functions.front ();

      octave_atexit_functions.pop_front ();

      OCTAVE_SAFE_CALL (reset_error_handler, ());

      OCTAVE_SAFE_CALL (feval, (fcn, octave_value_list (), 0));

      OCTAVE_SAFE_CALL (flush_octave_stdout, ());
    }

  if (! deja_vu)
    {
      deja_vu = true;

      // Process pending events and disasble octave_link event
      // processing with this call.

      octave_link::process_events (true);

      // Do this explicitly so that destructors for mex file objects
      // are called, so that functions registered with mexAtExit are
      // called.
      OCTAVE_SAFE_CALL (clear_mex_functions, ());

      OCTAVE_SAFE_CALL (command_editor::restore_terminal_state, ());

      // FIXME: is this needed?  Can it cause any trouble?
      OCTAVE_SAFE_CALL (raw_mode, (0));

      OCTAVE_SAFE_CALL (octave_history_write_timestamp, ());

      if (! command_history::ignoring_entries ())
        OCTAVE_SAFE_CALL (command_history::clean_up_and_save, ());

      OCTAVE_SAFE_CALL (gh_manager::close_all_figures, ());

      OCTAVE_SAFE_CALL (gtk_manager::unload_all_toolkits, ());

      OCTAVE_SAFE_CALL (close_files, ());

      OCTAVE_SAFE_CALL (cleanup_tmp_files, ());

      OCTAVE_SAFE_CALL (symbol_table::cleanup, ());

      OCTAVE_SAFE_CALL (sysdep_cleanup, ());

      OCTAVE_SAFE_CALL (octave_finalize_hdf5, ());

      OCTAVE_SAFE_CALL (flush_octave_stdout, ());

      if (! quitting_gracefully && interactive)
        {
          octave_stdout << "\n";

          // Yes, we want this to be separate from the call to
          // flush_octave_stdout above.

          OCTAVE_SAFE_CALL (flush_octave_stdout, ());
        }

      // Don't call singleton_cleanup_list::cleanup until we have the
      // problems with registering/unregistering types worked out.  For
      // example, uncomment the following line, then use the make_int
      // function from the examples directory to create an integer
      // object and then exit Octave.  Octave should crash with a
      // segfault when cleaning up the typinfo singleton.  We need some
      // way to force new octave_value_X types that are created in
      // .oct files to be unregistered when the .oct file shared library
      // is unloaded.
      //
      // OCTAVE_SAFE_CALL (singleton_cleanup_list::cleanup, ());

      OCTAVE_SAFE_CALL (octave_chunk_buffer::clear, ());
    }
}

void
clean_up_and_exit (int status, bool safe_to_return)
{
  do_octave_atexit ();

  if (octave_link::exit (status))
    {
      if (safe_to_return)
        return;
      else
        {
          // What should we do here?  We might be called from some
          // location other than the end of octave_execute_interpreter,
          // so it might not be safe to return.

          // We have nothing else to do at this point, and the
          // octave_link::exit function is supposed to take care of
          // exiting for us.  Assume that job won't take more than a
          // day...

          octave_sleep (86400); // FIXME: really needed?
        }
    }
  else
    {
      if (octave_exit)
        (*octave_exit) (status);
    }
}

DEFUN (quit, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {} exit\n\
@deftypefnx {} {} exit (@var{status})\n\
@deftypefnx {} {} quit\n\
@deftypefnx {} {} quit (@var{status})\n\
Exit the current Octave session.\n\
\n\
If the optional integer value @var{status} is supplied, pass that value to\n\
the operating system as Octave's exit status.  The default value is zero.\n\
\n\
When exiting, Octave will attempt to run the m-file @file{finish.m} if it\n\
exists.  User commands to save the workspace or clean up temporary files\n\
may be placed in that file.  Alternatively, another m-file may be scheduled\n\
to run using @code{atexit}.\n\
@seealso{atexit}\n\
@end deftypefn")
{
  // Confirm OK to shutdown.  Note: A dynamic function installation similar
  // to overriding polymorphism for which the GUI can install its own "quit"
  // yet call this base "quit" could be nice.  No link would be needed here.
  if (! octave_link::confirm_shutdown ())
    return ovl ();

  if (! quit_allowed)
    error ("quit: not supported in embedded mode");

  if (args.length () > 0)
    {
      int tmp = args(0).nint_value ();

      exit_status = tmp;
    }

  // Instead of simply calling exit, we simulate an interrupt
  // with a request to exit cleanly so that no matter where the
  // call to quit occurs, we will run the unwind_protect stack,
  // clear the OCTAVE_LOCAL_BUFFER allocations, etc. before
  // exiting.

  quitting_gracefully = true;

  octave_interrupt_state = -1;

  octave_throw_interrupt_exception ();

  return ovl ();
}

DEFALIAS (exit, quit);

DEFUN (warranty, , ,
       "-*- texinfo -*-\n\
@deftypefn {} {} warranty ()\n\
Describe the conditions for copying and distributing Octave.\n\
@end deftypefn")
{
  octave_stdout << "\n" << octave_name_version_and_copyright () << "\n\
\n\
GNU Octave is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 3 of the License, or\n\
(at your option) any later version.\n\
\n\
GNU Octave is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
\n";

  return ovl ();
}

// Execute a shell command.

static int
wait_for_input (int fid)
{
  int retval = -1;

#if defined (HAVE_SELECT)
  if (fid >= 0)
    {
      fd_set set;

      FD_ZERO (&set);
      FD_SET (fid, &set);

      retval = gnulib::select (FD_SETSIZE, &set, 0, 0, 0);
    }
#else
  retval = 1;
#endif

  return retval;
}

static octave_value_list
run_command_and_return_output (const std::string& cmd_str)
{
  octave_value_list retval;
  unwind_protect frame;

  iprocstream *cmd = new iprocstream (cmd_str.c_str ());

  frame.add_delete (cmd);
  frame.add_fcn (octave_child_list::remove, cmd->pid ());

  if (! *cmd)
    error ("system: unable to start subprocess for '%s'", cmd_str.c_str ());

  int fid = cmd->file_number ();

  std::ostringstream output_buf;

  char ch;

  for (;;)
    {
      if (cmd->get (ch))
        output_buf.put (ch);
      else
        {
          if (! cmd->eof () && errno == EAGAIN)
            {
              cmd->clear ();

              if (wait_for_input (fid) != 1)
                break;
            }
          else
            break;
        }
    }

  int cmd_status = cmd->close ();

  if (octave_wait::ifexited (cmd_status))
    cmd_status = octave_wait::exitstatus (cmd_status);
  else
    cmd_status = 127;

  retval = ovl (cmd_status, output_buf.str ());

  return retval;
}

enum system_exec_type { et_sync, et_async };

DEFUN (system, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {} {} system (\"@var{string}\")\n\
@deftypefnx {} {} system (\"@var{string}\", @var{return_output})\n\
@deftypefnx {} {} system (\"@var{string}\", @var{return_output}, @var{type})\n\
@deftypefnx {} {[@var{status}, @var{output}] =} system (@dots{})\n\
Execute a shell command specified by @var{string}.\n\
\n\
If the optional argument @var{type} is @qcode{\"async\"}, the process is\n\
started in the background and the process ID of the child process is\n\
returned immediately.  Otherwise, the child process is started and Octave\n\
waits until it exits.  If the @var{type} argument is omitted, it defaults to\n\
the value @qcode{\"sync\"}.\n\
\n\
If @var{system} is called with one or more output arguments, or if the\n\
optional argument @var{return_output} is true and the subprocess is started\n\
synchronously, then the output from the command is returned as a variable.\n\
Otherwise, if the subprocess is executed synchronously, its output is sent\n\
to the standard output.  To send the output of a command executed with\n\
@code{system} through the pager, use a command like\n\
\n\
@example\n\
@group\n\
[output, text] = system (\"cmd\");\n\
disp (text);\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
or\n\
\n\
@example\n\
printf (\"%s\\n\", nthargout (2, \"system\", \"cmd\"));\n\
@end example\n\
\n\
The @code{system} function can return two values.  The first is the\n\
exit status of the command and the second is any output from the\n\
command that was written to the standard output stream.  For example,\n\
\n\
@example\n\
[status, output] = system (\"echo foo; exit 2\");\n\
@end example\n\
\n\
@noindent\n\
will set the variable @code{output} to the string @samp{foo}, and the\n\
variable @code{status} to the integer @samp{2}.\n\
\n\
For commands run asynchronously, @var{status} is the process id of the\n\
command shell that is started to run the command.\n\
@seealso{unix, dos}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin == 0 || nargin > 3)
    print_usage ();

  system_exec_type type = et_sync;
  if (nargin == 3)
    {
      std::string type_str = args(2).xstring_value ("system: TYPE must be a string");

      if (type_str == "sync")
        type = et_sync;
      else if (type_str == "async")
        type = et_async;
      else
        error ("system: TYPE must be \"sync\" or \"async\"");
    }

  octave_value_list retval;

  // FIXME: Is this unwind_protect frame needed anymore (12/16/15)?
  unwind_protect frame;

  bool return_output = (nargin == 1 && nargout > 1);

  if (nargin > 1)
    {
      try
        {
          return_output = args(1).is_true ();
        }
      catch (octave_execution_exception& e)
        {
          error (e, "system: RETURN_OUTPUT must be boolean value true or false");
        }
    }

  if (return_output && type == et_async)
    error ("system: can't return output from commands run asynchronously");

  std::string cmd_str = args(0).xstring_value ("system: first argument must be a string");

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  // Work around weird double-quote handling on Windows systems.
  if (type == et_sync)
    cmd_str = "\"" + cmd_str + "\"";
#endif

  if (type == et_async)
    {
      // FIXME: maybe this should go in sysdep.cc?
#ifdef HAVE_FORK
      pid_t pid = fork ();

      if (pid < 0)
        error ("system: fork failed -- can't create child process");
      else if (pid == 0)
        {
          // FIXME: should probably replace this call with something portable.
          execl (SHELL_PATH, "sh", "-c", cmd_str.c_str (),
                 static_cast<void *> (0));

          panic_impossible ();
        }
      else
        retval(0) = pid;
#elif defined (__WIN32__)
      STARTUPINFO si;
      PROCESS_INFORMATION pi;
      ZeroMemory (&si, sizeof (si));
      ZeroMemory (&pi, sizeof (pi));
      OCTAVE_LOCAL_BUFFER (char, xcmd_str, cmd_str.length ()+1);
      strcpy (xcmd_str, cmd_str.c_str ());

      if (! CreateProcess (0, xcmd_str, 0, 0, FALSE, 0, 0, 0, &si, &pi))
        error ("system: CreateProcess failed -- can't create child process");

      retval(0) = pi.dwProcessId;
      CloseHandle (pi.hProcess);
      CloseHandle (pi.hThread);
#else
      err_disabled_feature ("system", "asynchronous system calls");
#endif
    }
  else if (return_output)
    retval = run_command_and_return_output (cmd_str);
  else
    {
      int status = system (cmd_str.c_str ());

      // The value in status is as returned by waitpid.  If
      // the process exited normally, extract the actual exit
      // status of the command.  Otherwise, return 127 as a
      // failure code.

      if (octave_wait::ifexited (status))
        status = octave_wait::exitstatus (status);

      retval(0) = status;
    }

  return retval;
}

/*
%!test
%! cmd = ls_command ();
%! [status, output] = system (cmd);
%! assert (status, 0);
%! assert (ischar (output));
%! assert (! isempty (output));

%!error system ()
%!error system (1, 2, 3)
*/

void
octave_add_atexit_function (const std::string& fname)
{
  octave_atexit_functions.push_front (fname);
}

bool
octave_remove_atexit_function (const std::string& fname)
{
  bool found = false;

  for (std::list<std::string>::iterator p = octave_atexit_functions.begin ();
       p != octave_atexit_functions.end (); p++)
    {
      if (*p == fname)
        {
          octave_atexit_functions.erase (p);
          found = true;
          break;
        }
    }

  return found;
}


DEFUN (atexit, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {} {} atexit (@var{fcn})\n\
@deftypefnx {} {} atexit (@var{fcn}, @var{flag})\n\
Register a function to be called when Octave exits.\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
function last_words ()\n\
  disp (\"Bye bye\");\n\
endfunction\n\
atexit (\"last_words\");\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
will print the message @qcode{\"Bye bye\"} when Octave exits.\n\
\n\
The additional argument @var{flag} will register or unregister @var{fcn}\n\
from the list of functions to be called when Octave exits.  If @var{flag} is\n\
true, the function is registered, and if @var{flag} is false, it is\n\
unregistered.  For example, after registering the function @code{last_words}\n\
above,\n\
\n\
@example\n\
atexit (\"last_words\", false);\n\
@end example\n\
\n\
@noindent\n\
will remove the function from the list and Octave will not call\n\
@code{last_words} when it exits.\n\
\n\
Note that @code{atexit} only removes the first occurrence of a function\n\
from the list, so if a function was placed in the list multiple times with\n\
@code{atexit}, it must also be removed from the list multiple times.\n\
@seealso{quit}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string arg = args(0).xstring_value ("atexit: FCN argument must be a string");

  bool add_mode = (nargin == 2)
    ? args(1).xbool_value ("atexit: FLAG argument must be a logical value")
    : true;

  octave_value_list retval;

  if (add_mode)
    octave_add_atexit_function (arg);
  else
    {
      bool found = octave_remove_atexit_function (arg);

      if (nargout > 0)
        retval = ovl (found);
    }

  return retval;
}

static octave_value
find_config_info (const octave_scalar_map& m, const std::string& key)
{
  if (m.isfield (key))
    {
      Cell c = m.contents (key);

      if (! c.is_empty ())
        return c(0);
    }

  return octave_value ();
}

DEFUN (octave_config_info, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {} octave_config_info ()\n\
@deftypefnx {} {} octave_config_info (@var{option})\n\
Return a structure containing configuration and installation information for\n\
Octave.\n\
\n\
If @var{option} is a string, return the configuration information for the\n\
specified option.\n\
\n\
@seealso{computer}\n\
@end deftypefn")
{
#if defined (ENABLE_DYNAMIC_LINKING)
  bool octave_supports_dynamic_linking = true;
#else
  bool octave_supports_dynamic_linking = false;
#endif

  static bool initialized = false;

  struct conf_info_struct
  {
    const char *key;
    octave_value val;
  };

  static const conf_info_struct conf_info[] =
    {
      { "DEFAULT_PAGER", OCTAVE_DEFAULT_PAGER },

#if defined (OCTAVE_ENABLE_64)
      { "ENABLE_64", true },
#else
      { "ENABLE_64", false },
#endif

#if defined (OCTAVE_ENABLE_ATOMIC_REFCOUNT)
      { "ENABLE_ATOMIC_REFCOUNT", true },
#else
      { "ENABLE_ATOMIC_REFCOUNT", false },
#endif

#if defined (OCTAVE_ENABLE_BOUNDS_CHECK)
      { "ENABLE_BOUNDS_CHECK", true },
#else
      { "ENABLE_BOUNDS_CHECK", false },
#endif

#if defined (ENABLE_DOCS)
      { "ENABLE_DOCS", true },
#else
      { "ENABLE_DOCS", false },
#endif

#if defined (ENABLE_DYNAMIC_LINKING)
      { "ENABLE_DYNAMIC_LINKING", true },
#else
      { "ENABLE_DYNAMIC_LINKING", false },
#endif

#if defined (OCTAVE_ENABLE_FLOAT_TRUNCATE)
      { "ENABLE_FLOAT_TRUNCATE", true },
#else
      { "ENABLE_FLOAT_TRUNCATE", false },
#endif

#if defined (ENABLE_JIT)
      { "ENABLE_JIT", true },
#else
      { "ENABLE_JIT", false },
#endif

#if defined (OCTAVE_ENABLE_OPENMP)
      { "ENABLE_OPENMP", true },
#else
      { "ENABLE_OPENMP", false },
#endif

      { "api_version", OCTAVE_API_VERSION },
      { "archlibdir", subst_octave_home (OCTAVE_ARCHLIBDIR) },
      { "bindir", subst_octave_home (OCTAVE_BINDIR) },
      { "canonical_host_type", OCTAVE_CANONICAL_HOST_TYPE },
      { "datadir", subst_octave_home (OCTAVE_DATADIR) },
      { "datarootdir", subst_octave_home (OCTAVE_DATAROOTDIR) },
      { "exec_prefix", subst_octave_home (OCTAVE_EXEC_PREFIX) },
      { "fcnfiledir", subst_octave_home (OCTAVE_FCNFILEDIR) },
      { "imagedir", subst_octave_home (OCTAVE_IMAGEDIR) },
      { "includedir", subst_octave_home (OCTAVE_INCLUDEDIR) },
      { "infodir", subst_octave_home (OCTAVE_INFODIR) },
      { "infofile", subst_octave_home (OCTAVE_INFOFILE) },
      { "libdir", subst_octave_home (OCTAVE_LIBDIR) },
      { "libexecdir", subst_octave_home (OCTAVE_LIBEXECDIR) },
      { "localapiarchlibdir", subst_octave_home (OCTAVE_LOCALAPIARCHLIBDIR) },
      { "localapifcnfiledir", subst_octave_home (OCTAVE_LOCALAPIFCNFILEDIR) },
      { "localapioctfiledir", subst_octave_home (OCTAVE_LOCALAPIOCTFILEDIR) },
      { "localarchlibdir", subst_octave_home (OCTAVE_LOCALARCHLIBDIR) },
      { "localfcnfiledir", subst_octave_home (OCTAVE_LOCALFCNFILEDIR) },
      { "localoctfiledir", subst_octave_home (OCTAVE_LOCALOCTFILEDIR) },
      { "localstartupfiledir", subst_octave_home (OCTAVE_LOCALSTARTUPFILEDIR) },
      { "localverarchlibdir", subst_octave_home (OCTAVE_LOCALVERARCHLIBDIR) },
      { "localverfcnfiledir", subst_octave_home (OCTAVE_LOCALVERFCNFILEDIR) },
      { "localveroctfiledir", subst_octave_home (OCTAVE_LOCALVEROCTFILEDIR) },
      { "man1dir", subst_octave_home (OCTAVE_MAN1DIR) },
      { "man1ext", OCTAVE_MAN1EXT },
      { "mandir", subst_octave_home (OCTAVE_MANDIR) },
      { "octdatadir", subst_octave_home (OCTAVE_OCTDATADIR) },
      { "octfiledir", subst_octave_home (OCTAVE_OCTFILEDIR) },
      { "octetcdir", subst_octave_home (OCTAVE_OCTETCDIR) },
      { "octincludedir", subst_octave_home (OCTAVE_OCTINCLUDEDIR) },
      { "octlibdir", subst_octave_home (OCTAVE_OCTLIBDIR) },
      { "octtestsdir", subst_octave_home (OCTAVE_OCTTESTSDIR) },
      { "prefix", subst_octave_home (OCTAVE_PREFIX) },
      { "startupfiledir", subst_octave_home (OCTAVE_STARTUPFILEDIR) },
      { "version", OCTAVE_VERSION },
      { 0, octave_value () }
    };

  struct build_info_struct
  {
    const char *key;
    const char *val;
  };

  static const build_info_struct build_info[] =
    {
      { "AMD_CPPFLAGS", octave::build_env::AMD_CPPFLAGS },
      { "AMD_LDFLAGS", octave::build_env::AMD_LDFLAGS },
      { "AMD_LIBS", octave::build_env::AMD_LIBS },
      { "AR", octave::build_env::AR },
      { "ARFLAGS", octave::build_env::ARFLAGS },
      { "ARPACK_CPPFLAGS", octave::build_env::ARPACK_CPPFLAGS },
      { "ARPACK_LDFLAGS", octave::build_env::ARPACK_LDFLAGS },
      { "ARPACK_LIBS", octave::build_env::ARPACK_LIBS },
      { "BLAS_LIBS", octave::build_env::BLAS_LIBS },
      { "CAMD_CPPFLAGS", octave::build_env::CAMD_CPPFLAGS },
      { "CAMD_LDFLAGS", octave::build_env::CAMD_LDFLAGS },
      { "CAMD_LIBS", octave::build_env::CAMD_LIBS },
      { "CARBON_LIBS", octave::build_env::CARBON_LIBS },
      { "CC", octave::build_env::CC },
      { "CCOLAMD_CPPFLAGS", octave::build_env::CCOLAMD_CPPFLAGS },
      { "CCOLAMD_LDFLAGS", octave::build_env::CCOLAMD_LDFLAGS },
      { "CCOLAMD_LIBS", octave::build_env::CCOLAMD_LIBS },
      { "CFLAGS", octave::build_env::CFLAGS },
      { "CHOLMOD_CPPFLAGS", octave::build_env::CHOLMOD_CPPFLAGS },
      { "CHOLMOD_LDFLAGS", octave::build_env::CHOLMOD_LDFLAGS },
      { "CHOLMOD_LIBS", octave::build_env::CHOLMOD_LIBS },
      { "COLAMD_CPPFLAGS", octave::build_env::COLAMD_CPPFLAGS },
      { "COLAMD_LDFLAGS", octave::build_env::COLAMD_LDFLAGS },
      { "COLAMD_LIBS", octave::build_env::COLAMD_LIBS },
      { "CPICFLAG", octave::build_env::CPICFLAG },
      { "CPPFLAGS", octave::build_env::CPPFLAGS },
      { "CURL_CPPFLAGS", octave::build_env::CURL_CPPFLAGS },
      { "CURL_LDFLAGS", octave::build_env::CURL_LDFLAGS },
      { "CURL_LIBS", octave::build_env::CURL_LIBS },
      { "CXSPARSE_CPPFLAGS", octave::build_env::CXSPARSE_CPPFLAGS },
      { "CXSPARSE_LDFLAGS", octave::build_env::CXSPARSE_LDFLAGS },
      { "CXSPARSE_LIBS", octave::build_env::CXSPARSE_LIBS },
      { "CXX", octave::build_env::CXX },
      { "CXXCPP", octave::build_env::CXXCPP },
      { "CXXFLAGS", octave::build_env::CXXFLAGS },
      { "CXXPICFLAG", octave::build_env::CXXPICFLAG },
      { "DEFS", octave::build_env::DEFS },
      { "DL_LD", octave::build_env::DL_LD },
      { "DL_LDFLAGS", octave::build_env::DL_LDFLAGS },
      { "DL_LIBS", octave::build_env::DL_LIBS },
      { "GCC_VERSION", octave::build_env::GCC_VERSION },
      { "GXX_VERSION", octave::build_env::GXX_VERSION },
      { "EXEEXT", octave::build_env::EXEEXT },
      { "F77", octave::build_env::F77 },
      { "F77_FLOAT_STORE_FLAG", octave::build_env::F77_FLOAT_STORE_FLAG },
      { "F77_INTEGER_8_FLAG", octave::build_env::F77_INTEGER_8_FLAG },
      { "FFLAGS", octave::build_env::FFLAGS },
      { "FFTW3_CPPFLAGS", octave::build_env::FFTW3_CPPFLAGS },
      { "FFTW3_LDFLAGS", octave::build_env::FFTW3_LDFLAGS },
      { "FFTW3_LIBS", octave::build_env::FFTW3_LIBS },
      { "FFTW3F_CPPFLAGS", octave::build_env::FFTW3F_CPPFLAGS },
      { "FFTW3F_LDFLAGS", octave::build_env::FFTW3F_LDFLAGS },
      { "FFTW3F_LIBS", octave::build_env::FFTW3F_LIBS },
      { "FLIBS", octave::build_env::FLIBS },
      { "FLTK_CPPFLAGS", octave::build_env::FLTK_CPPFLAGS },
      { "FLTK_LDFLAGS", octave::build_env::FLTK_LDFLAGS },
      { "FLTK_LIBS", octave::build_env::FLTK_LIBS },
      { "FONTCONFIG_CPPFLAGS", octave::build_env::FONTCONFIG_CPPFLAGS },
      { "FONTCONFIG_LIBS", octave::build_env::FONTCONFIG_LIBS },
      { "FPICFLAG", octave::build_env::FPICFLAG },
      { "FT2_CPPFLAGS", octave::build_env::FT2_CPPFLAGS },
      { "FT2_LIBS", octave::build_env::FT2_LIBS },
      { "GLPK_CPPFLAGS", octave::build_env::GLPK_CPPFLAGS },
      { "GLPK_LDFLAGS", octave::build_env::GLPK_LDFLAGS },
      { "GLPK_LIBS", octave::build_env::GLPK_LIBS },
      { "GNUPLOT", octave::build_env::GNUPLOT },
      { "HDF5_CPPFLAGS", octave::build_env::HDF5_CPPFLAGS },
      { "HDF5_LDFLAGS", octave::build_env::HDF5_LDFLAGS },
      { "HDF5_LIBS", octave::build_env::HDF5_LIBS },
      { "LAPACK_LIBS", octave::build_env::LAPACK_LIBS },
      { "LDFLAGS", octave::build_env::LDFLAGS },
      { "LD_CXX", octave::build_env::LD_CXX },
      { "LD_STATIC_FLAG", octave::build_env::LD_STATIC_FLAG },
      { "LEX", octave::build_env::LEX },
      { "LEXLIB", octave::build_env::LEXLIB },
      { "LFLAGS", octave::build_env::LFLAGS },
      { "LIBEXT", octave::build_env::LIBEXT },
      { "LIBOCTAVE", octave::build_env::LIBOCTAVE },
      { "LIBOCTINTERP", octave::build_env::LIBOCTINTERP },
      { "LIBS", octave::build_env::LIBS },
      { "LLVM_CPPFLAGS", octave::build_env::LLVM_CPPFLAGS },
      { "LLVM_LDFLAGS", octave::build_env::LLVM_LDFLAGS },
      { "LLVM_LIBS", octave::build_env::LLVM_LIBS },
      { "LN_S", octave::build_env::LN_S },
      { "MAGICK_CPPFLAGS", octave::build_env::MAGICK_CPPFLAGS },
      { "MAGICK_LDFLAGS", octave::build_env::MAGICK_LDFLAGS },
      { "MAGICK_LIBS", octave::build_env::MAGICK_LIBS },
      { "MKOCTFILE_DL_LDFLAGS", octave::build_env::MKOCTFILE_DL_LDFLAGS },
      { "OCTAVE_LINK_DEPS", octave::build_env::OCTAVE_LINK_DEPS },
      { "OCTAVE_LINK_OPTS", octave::build_env::OCTAVE_LINK_OPTS },
      { "OCT_LINK_DEPS", octave::build_env::OCT_LINK_DEPS },
      { "OCT_LINK_OPTS", octave::build_env::OCT_LINK_OPTS },
      { "OPENGL_LIBS", octave::build_env::OPENGL_LIBS },
      { "OSMESA_CPPFLAGS", octave::build_env::OSMESA_CPPFLAGS },
      { "OSMESA_LDFLAGS", octave::build_env::OSMESA_LDFLAGS },
      { "OSMESA_LIBS", octave::build_env::OSMESA_LIBS },
      { "PCRE_CPPFLAGS", octave::build_env::PCRE_CPPFLAGS },
      { "PCRE_LIBS", octave::build_env::PCRE_LIBS },
      { "PTHREAD_CFLAGS", octave::build_env::PTHREAD_CFLAGS },
      { "PTHREAD_LIBS", octave::build_env::PTHREAD_LIBS },
      { "QHULL_CPPFLAGS", octave::build_env::QHULL_CPPFLAGS },
      { "QHULL_LDFLAGS", octave::build_env::QHULL_LDFLAGS },
      { "QHULL_LIBS", octave::build_env::QHULL_LIBS },
      { "QRUPDATE_CPPFLAGS", octave::build_env::QRUPDATE_CPPFLAGS },
      { "QRUPDATE_LDFLAGS", octave::build_env::QRUPDATE_LDFLAGS },
      { "QRUPDATE_LIBS", octave::build_env::QRUPDATE_LIBS },
      { "QT_CPPFLAGS", octave::build_env::QT_CPPFLAGS },
      { "QT_LDFLAGS", octave::build_env::QT_LDFLAGS },
      { "QT_LIBS", octave::build_env::QT_LIBS },
      { "RANLIB", octave::build_env::RANLIB },
      { "RDYNAMIC_FLAG", octave::build_env::RDYNAMIC_FLAG },
      { "READLINE_LIBS", octave::build_env::READLINE_LIBS },
      { "SED", octave::build_env::SED },
      { "SHARED_LIBS", octave::build_env::SHARED_LIBS },
      { "SHLEXT", octave::build_env::SHLEXT },
      { "SHLEXT_VER", octave::build_env::SHLEXT_VER },
      { "SH_LD", octave::build_env::SH_LD },
      { "SH_LDFLAGS", octave::build_env::SH_LDFLAGS },
      { "STATIC_LIBS", octave::build_env::STATIC_LIBS },
      { "TERM_LIBS", octave::build_env::TERM_LIBS },
      { "UMFPACK_CPPFLAGS", octave::build_env::UMFPACK_CPPFLAGS },
      { "UMFPACK_LDFLAGS", octave::build_env::UMFPACK_LDFLAGS },
      { "UMFPACK_LIBS", octave::build_env::UMFPACK_LIBS },
      { "WARN_CFLAGS", octave::build_env::WARN_CFLAGS },
      { "WARN_CXXFLAGS", octave::build_env::WARN_CXXFLAGS },
      { "X11_INCFLAGS", octave::build_env::X11_INCFLAGS },
      { "X11_LIBS", octave::build_env::X11_LIBS },
      { "XTRA_CFLAGS", octave::build_env::XTRA_CFLAGS },
      { "XTRA_CXXFLAGS", octave::build_env::XTRA_CXXFLAGS },
      { "YACC", octave::build_env::YACC },
      { "YFLAGS", octave::build_env::YFLAGS },
      { "Z_CPPFLAGS", octave::build_env::Z_CPPFLAGS },
      { "Z_LDFLAGS", octave::build_env::Z_LDFLAGS },
      { "Z_LIBS", octave::build_env::Z_LIBS },
      { "config_opts", octave::build_env::config_opts },
      { 0, 0 },
    };


  static octave_scalar_map config;
  static octave_scalar_map build_env;
  static octave_scalar_map build_features = octave::build_env::features ();

  if (! initialized)
    {
      int i;

      i = 0;
      while (true)
        {
          const build_info_struct& elt = build_info[i++];

          const char *key = elt.key;

          if (key)
            build_env.assign (key, elt.val);
          else
            break;
        }

      i = 0;
      while (true)
        {
          const conf_info_struct& elt = conf_info[i++];

          const char *key = elt.key;

          if (key)
            config.assign (key, elt.val);
          else
            break;
        }

      bool unix_system = true;
      bool mac_system = false;
      bool windows_system = false;

#if defined (WIN32)
      windows_system = true;
#if ! defined (__CYGWIN__)
      unix_system = false;
#endif
#endif

#if defined (OCTAVE_USE_OS_X_API)
      mac_system = true;
#endif

      config.assign ("unix", octave_value (unix_system));
      config.assign ("mac", octave_value (mac_system));
      config.assign ("windows", octave_value (windows_system));

      config.assign ("dld", octave_value (octave_supports_dynamic_linking));

      oct_mach_info::float_format ff = oct_mach_info::native_float_format ();
      config.assign ("float_format",
                     octave_value (oct_mach_info::float_format_as_string (ff)));

      config.assign ("words_big_endian",
                     octave_value (oct_mach_info::words_big_endian ()));

      config.assign ("words_little_endian",
                     octave_value (oct_mach_info::words_little_endian ()));

      config.assign ("build_environment", octave_value (build_env));

      config.assign ("build_features", octave_value (build_features));

      initialized = true;
    }

  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value_list retval;

  if (nargin == 1)
    {
      std::string arg = args(0).xstring_value ("octave_config_info: OPTION argument must be a string");

      octave_value info = find_config_info (config, arg);

      if (info.is_undefined ())
        info = find_config_info (build_env, arg);

      if (info.is_undefined ())
        info = find_config_info (build_features, arg);

      if (info.is_undefined ())
        error ("octave_config_info: no info for '%s'", arg.c_str ());

      return info;
    }
  else
    retval = ovl (config);

  return retval;
}

/*
%!assert (ischar (octave_config_info ("version")))
%!test
%! x = octave_config_info ();
%! assert (isstruct (x));
%! assert (! isempty (x));

%!error octave_config_info (1, 2)
*/

#if defined (__GNUG__) && defined (DEBUG_NEW_DELETE)

int debug_new_delete = 0;

typedef void (*vfp)(void);
extern vfp __new_handler;

void *
__builtin_new (size_t sz)
{
  void *p;

  // malloc (0) is unpredictable; avoid it.
  if (sz == 0)
    sz = 1;
  p = gnulib::malloc (sz);
  while (p == 0)
    {
      (*__new_handler) ();
      p = gnulib::malloc (sz);
    }

  if (debug_new_delete)
    std::cerr << "__builtin_new: " << p << std::endl;

  return p;
}

void
__builtin_delete (void *ptr)
{
  if (debug_new_delete)
    std::cerr << "__builtin_delete: " << ptr << std::endl;

  if (ptr)
    free (ptr);
}

#endif
