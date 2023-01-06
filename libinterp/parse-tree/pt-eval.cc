////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#include <cctype>

#include <condition_variable>
#include <iostream>
#include <list>
#include <mutex>
#include <string>
#include <thread>

#include "cmd-edit.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-array-errwarn.h"
#include "lo-ieee.h"
#include "oct-env.h"

#include "bp-table.h"
#include "call-stack.h"
#include "cdef-manager.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "event-manager.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "mex-private.h"
#include "octave.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-mex-fcn.h"
#include "ov-usr-fcn.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "parse.h"
#include "profiler.h"
#include "pt-all.h"
#include "pt-anon-scopes.h"
#include "pt-eval.h"
#include "pt-tm-const.h"
#include "stack-frame.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Normal evaluator.

class quit_debug_exception
{
public:

  quit_debug_exception (bool all = false) : m_all (all) { }

  quit_debug_exception (const quit_debug_exception&) = default;

  quit_debug_exception& operator = (const quit_debug_exception&) = default;

  ~quit_debug_exception (void) = default;

  bool all (void) const { return m_all; }

private:

  bool m_all;
};

class debugger
{
public:

  enum execution_mode
  {
    EX_NORMAL = 0,
    EX_CONTINUE = 1,
    EX_QUIT = 2,
    EX_QUIT_ALL = 3
  };

  debugger (interpreter& interp, std::size_t level)
    : m_interpreter (interp), m_level (level),
      m_execution_mode (EX_NORMAL), m_in_debug_repl (false)
  { }

  int server_loop (void);

  void repl (const std::string& prompt = "debug> ");

  bool in_debug_repl (void) const { return m_in_debug_repl; }

  void dbcont (void) { m_execution_mode = EX_CONTINUE; }

  void dbquit (bool all = false)
  {
    if (all)
      m_execution_mode = EX_QUIT_ALL;
    else
      m_execution_mode = EX_QUIT;
  }

  bool quitting_debugger (void) const;

private:

  interpreter& m_interpreter;

  std::size_t m_level;
  execution_mode m_execution_mode;
  bool m_in_debug_repl;
};

// FIXME: Could the debugger server_loop and repl functions be merged
// with the corresponding tree_evaluator functions or do they need to
// remain separate?  They perform nearly the same functions.

int debugger::server_loop (void)
{
  // Process events from the event queue.

  tree_evaluator& tw = m_interpreter.get_evaluator ();

  void (tree_evaluator::*server_mode_fptr) (bool)
    = &tree_evaluator::server_mode;
  unwind_action act (server_mode_fptr, &tw, true);

  int exit_status = 0;

  do
    {
      if (m_execution_mode == EX_CONTINUE || tw.dbstep_flag ())
        break;

      if (quitting_debugger ())
        break;

      try
        {
          // FIXME: Should we call octave_quit in the octave::sleep
          // and/or command_editor::run_event_hooks functions?

          octave_quit ();

          // FIXME: Running the event queue should be decoupled from
          // the command_editor.

          // FIXME: Is it OK to use command_editor::run_event_hooks
          // here?  It may run more than one queued function per call,
          // and it seems that the checks at the top of the loop
          // probably need to be done after each individual event
          // function is executed.  For now, maybe the simplest thing
          // would be to pass a predicate function (lambda expression)
          // to the command_editor::run_event_hooks and have it check
          // that and break out of the eval loop(s) if the condition
          // is met?

          // FIXME: We should also use a condition variable to manage
          // the execution of entries in the queue and eliminate the
          // need for the busy-wait loop.

          command_editor::run_event_hooks ();

          release_unreferenced_dynamic_libraries ();

          sleep (0.1);
        }
      catch (const interrupt_exception&)
        {
          octave_interrupt_state = 1;
          m_interpreter.recover_from_exception ();

          // Required newline when the user does Ctrl+C at the prompt.
          if (m_interpreter.interactive ())
            octave_stdout << "\n";
        }
      catch (const index_exception& e)
        {
          m_interpreter.recover_from_exception ();

          std::cerr << "error: unhandled index exception: "
                    << e.message () << " -- trying to return to prompt"
                    << std::endl;
        }
      catch (const execution_exception& ee)
        {
          error_system& es = m_interpreter.get_error_system ();

          es.save_exception (ee);
          es.display_exception (ee);

          if (m_interpreter.interactive ())
            {
              m_interpreter.recover_from_exception ();
            }
          else
            {
              // We should exit with a nonzero status.
              exit_status = 1;
              break;
            }
        }
      catch (const quit_debug_exception& qde)
        {
          if (qde.all ())
            throw;

          // Continue in this debug level.
        }
      catch (const std::bad_alloc&)
        {
          m_interpreter.recover_from_exception ();

          std::cerr << "error: out of memory -- trying to return to prompt"
                    << std::endl;
        }
    }
  while (exit_status == 0);

  if (exit_status == EOF)
    {
      if (m_interpreter.interactive ())
        octave_stdout << "\n";

      exit_status = 0;
    }

  return exit_status;
}

void debugger::repl (const std::string& prompt_arg)
{
  unwind_protect frame;

  frame.protect_var (m_in_debug_repl);
  frame.protect_var (m_execution_mode);

  m_in_debug_repl = true;

  tree_evaluator& tw = m_interpreter.get_evaluator ();

  bool silent = tw.quiet_breakpoint_flag (false);

  frame.add (&tree_evaluator::restore_frame, &tw,
             tw.current_call_stack_frame_number ());

  tw.goto_frame (tw.debug_frame ());

  octave_user_code *caller = tw.current_user_code ();
  std::string fcn_file_nm, fcn_nm;

  if (caller)
    {
      fcn_file_nm = caller->fcn_file_name ();
      fcn_nm = fcn_file_nm.empty () ? caller->name () : fcn_file_nm;
    }

  int curr_debug_line = tw.current_line ();

  std::ostringstream buf;

  input_system& input_sys = m_interpreter.get_input_system ();

  event_manager& evmgr = m_interpreter.get_event_manager ();

  if (! fcn_nm.empty ())
    {
      if (input_sys.gud_mode ())
        {
          static char ctrl_z = 'Z' & 0x1f;

          buf << ctrl_z << ctrl_z << fcn_nm << ':' << curr_debug_line;
        }
      else
        {
          // FIXME: we should come up with a clean way to detect
          // that we are stopped on the no-op command that marks the
          // end of a function or script.

          if (! silent)
            {
              std::shared_ptr<stack_frame> frm = tw.current_user_frame ();

              frm->display_stopped_in_message (buf);
            }

          evmgr.enter_debugger_event (fcn_nm, fcn_file_nm, curr_debug_line);

          evmgr.set_workspace ();

          frame.add (&event_manager::execute_in_debugger_event, &evmgr,
                     fcn_nm, curr_debug_line);

          if (! silent)
            {
              std::string line_buf;

              if (caller)
                line_buf = caller->get_code_line (curr_debug_line);

              if (! line_buf.empty ())
                buf << curr_debug_line << ": " << line_buf;
            }
        }
    }

  if (silent)
    command_editor::erase_empty_line (true);

  std::string stopped_in_msg = buf.str ();

  if (m_interpreter.server_mode ())
    {
      if (! stopped_in_msg.empty ())
        octave_stdout << stopped_in_msg << std::endl;

      evmgr.push_event_queue ();

      frame.add (&event_manager::pop_event_queue, &evmgr);

      frame.add (&tree_evaluator::set_parser, &tw, tw.get_parser ());

      std::shared_ptr<push_parser>
      debug_parser (new push_parser (m_interpreter));

      tw.set_parser (debug_parser);

      server_loop ();
    }
  else
    {
      if (! stopped_in_msg.empty ())
        std::cerr << stopped_in_msg << std::endl;

      std::string tmp_prompt = prompt_arg;
      if (m_level > 0)
        tmp_prompt = "[" + std::to_string (m_level) + "]" + prompt_arg;

      frame.add (&input_system::set_PS1, &input_sys, input_sys.PS1 ());
      input_sys.PS1 (tmp_prompt);

      if (! m_interpreter.interactive ())
        {
          void (interpreter::*interactive_fptr) (bool)
            = &interpreter::interactive;
          frame.add (interactive_fptr, &m_interpreter,
                     m_interpreter.interactive ());

          m_interpreter.interactive (true);

          // FIXME: should debugging be possible in an embedded
          // interpreter?

          application *app = application::app ();

          if (app)
            {
              void (application::*forced_interactive_fptr) (bool)
                = &application::forced_interactive;
              frame.add (forced_interactive_fptr, app,
                         app->forced_interactive ());

              app->forced_interactive (true);
            }
        }

#if defined (OCTAVE_ENABLE_COMMAND_LINE_PUSH_PARSER)

      input_reader reader (m_interpreter);

      push_parser debug_parser (m_interpreter);

#else

      parser debug_parser (m_interpreter);

#endif

      error_system& es = m_interpreter.get_error_system ();

      while (m_in_debug_repl)
        {
          if (m_execution_mode == EX_CONTINUE || tw.dbstep_flag ())
            break;

          if (quitting_debugger ())
            break;

          try
            {
              debug_parser.reset ();

#if defined (OCTAVE_ENABLE_COMMAND_LINE_PUSH_PARSER)

              int retval = 0;

              std::string prompt
                = command_editor::decode_prompt_string (tmp_prompt);

              do
                {
                  bool eof = false;
                  std::string input_line = reader.get_input (prompt, eof);

                  if (eof)
                    {
                      retval = EOF;
                      break;
                    }

                  retval = debug_parser.run (input_line, false);

                  prompt = command_editor::decode_prompt_string (input_sys.PS2 ());
                }
              while (retval < 0);

#else

              int retval = debug_parser.run ();

#endif
              if (command_editor::interrupt (false))
                {
                  // Break regardless of m_execution_mode value.

                  quitting_debugger ();

                  break;
                }
              else
                {
                  if (retval == 0)
                    {
                      std::shared_ptr<tree_statement_list> stmt_list
                        = debug_parser.statement_list ();

                      if (stmt_list)
                        stmt_list->accept (tw);

                      if (octave_completion_matches_called)
                        octave_completion_matches_called = false;

                      // FIXME: the following statement is here because
                      // the last command may have been a dbup, dbdown, or
                      // dbstep command that changed the current debug
                      // frame.  If so, we need to reset the current frame
                      // for the call stack.  But is this right way to do
                      // this job?  What if the statement list was
                      // something like "dbup; dbstack"?  Will the call to
                      // dbstack use the right frame?  If not, how can we
                      // fix this problem?
                      tw.goto_frame (tw.debug_frame ());
                    }

                  octave_quit ();
                }
            }
          catch (const execution_exception& ee)
            {
              es.save_exception (ee);
              es.display_exception (ee);

              // Ignore errors when in debugging mode;
              m_interpreter.recover_from_exception ();
            }
          catch (const quit_debug_exception& qde)
            {
              if (qde.all ())
                throw;

              // Continue in this debug level.
            }
        }
    }
}

bool debugger::quitting_debugger (void) const
{
  if (m_execution_mode == EX_QUIT)
    {
      // If there is no enclosing debug level or the top-level
      // repl is not active, handle dbquit the same as dbcont.

      if (m_level > 0 || m_interpreter.server_mode ()
          || m_interpreter.in_top_level_repl ())
        throw quit_debug_exception ();
      else
        return true;
    }

  if (m_execution_mode == EX_QUIT_ALL)
    {
      // If the top-level repl is not active, handle "dbquit all"
      // the same as dbcont.

      if (m_interpreter.server_mode () || m_interpreter.in_top_level_repl ())
        throw quit_debug_exception (true);
      else
        return true;
    }

  return false;
}

bool tree_evaluator::at_top_level (void) const
{
  return m_call_stack.at_top_level ();
}

std::string
tree_evaluator::mfilename (const std::string& opt) const
{
  std::string fname;

  octave_user_code *fcn = m_call_stack.current_user_code ();

  if (fcn)
    {
      fname = fcn->fcn_file_name ();

      if (fname.empty ())
        fname = fcn->name ();
    }

  if (opt == "fullpathext")
    return fname;

  std::size_t dpos = fname.rfind (sys::file_ops::dir_sep_char ());
  std::size_t epos = fname.rfind ('.');

  if (epos <= dpos+1)
    epos = std::string::npos;

  if (epos != std::string::npos)
    fname = fname.substr (0, epos);

  if (opt == "fullpath")
    return fname;

  if (dpos != std::string::npos)
    fname = fname.substr (dpos+1);

  return fname;
}

void tree_evaluator::parse_and_execute (const std::string& input,
                                        bool& incomplete_parse)
{
  incomplete_parse = false;

  unwind_protect_var<bool> upv (m_in_top_level_repl, true);

  if (at_top_level ())
    {
      dbstep_flag (0);
      reset_debug_state ();
    }

  // FIXME: OK to do this job here, or should it be in the functions
  // that do the actual prompting?

  // Update the time stamp for the "prompt" so that automatically
  // finding modified files based on file modification times will
  // work.  In the future, we may do something completely different to
  // check for changes to files but for now, we rely on the prompt
  // time stamp to limit the checks for file modification times.

  Vlast_prompt_time.stamp ();

  bool eof = false;

  event_manager& evmgr = m_interpreter.get_event_manager ();

  if (command_history::add (input))
    evmgr.append_history (input);

  m_exit_status = m_parser->run (input, eof);

  if (m_exit_status == 0)
    {
      std::shared_ptr<tree_statement_list>
      stmt_list = m_parser->statement_list ();

      if (stmt_list)
        {
          command_editor::increment_current_command_number ();

          eval (stmt_list, m_interpreter.interactive ());

          evmgr.set_workspace ();
        }
      else if (m_parser->at_end_of_input ())
        m_exit_status = EOF;
    }
  else
    incomplete_parse = true;

  // FIXME: Should we be checking m_exit_status or incomplete_parse or
  // both here?  Could EOF have a value other than -1, and is there
  // possible confusion between that state and the parser returning -1?

  if (m_exit_status == -1)
    m_exit_status = 0;
  else
    m_parser->reset ();

  evmgr.pre_input_event ();
}

void tree_evaluator::get_line_and_eval (void)
{
  std::mutex mtx;
  std::unique_lock<std::mutex> lock (mtx);
  std::condition_variable cv;
  bool incomplete_parse = false;
  bool evaluation_pending = false;
  bool exiting = false;

  input_system& input_sys = m_interpreter.get_input_system ();
  event_manager& evmgr = m_interpreter.get_event_manager ();

  while (true)
    {
      // FIXME: Detect EOF?  Use readline?  If
      // so, then we need to disable idle event loop hook function
      // execution.

      std::string ps = incomplete_parse ? input_sys.PS2 () : input_sys.PS1 ();

      std::cout << command_editor::decode_prompt_string (ps);

      std::string input;
      std::getline (std::cin, input);

      if (input.empty ())
        continue;

      incomplete_parse = false;
      evaluation_pending = true;
      exiting = false;

      evmgr.post_event
      ([&] (interpreter& interp)
      {
        // INTERPRETER THREAD

        std::lock_guard<std::mutex> local_lock (mtx);

        try
          {
            interp.parse_and_execute (input, incomplete_parse);
          }
        catch (const exit_exception&)
          {
            evaluation_pending = false;
            exiting = true;
            cv.notify_all ();
            throw;
          }
        catch (const execution_exception& ee)
          {
            error_system& es = m_interpreter.get_error_system ();

            es.save_exception (ee);
            es.display_exception (ee);

            if (m_interpreter.interactive ())
              {
                m_interpreter.recover_from_exception ();
                m_parser->reset ();
                evaluation_pending = false;
                cv.notify_all ();
              }
            else
              {
                evaluation_pending = false;
                cv.notify_all ();
                throw exit_exception (1);
              }
          }
        catch (...)
          {
            evaluation_pending = false;
            cv.notify_all ();
            throw;
          }

        evaluation_pending = false;
        cv.notify_all ();
      });

      // Wait until evaluation is finished before prompting for input
      // again.

      cv.wait (lock, [&] { return ! evaluation_pending; });

      if (exiting)
        break;
    }
}

int tree_evaluator::repl (void)
{
  // The big loop.  Read, Eval, Print, Loop.  Normally user
  // interaction at the command line in a terminal session, but we may
  // also end up here when reading from a pipe or when stdin is
  // connected to a file by the magic of input redirection.

  int exit_status = 0;

  // FIXME: should this choice be a command-line option?  Note that we
  // intend that the push parser interface only be used for
  // interactive sessions.

#if defined (OCTAVE_ENABLE_COMMAND_LINE_PUSH_PARSER)
  static bool use_command_line_push_parser = true;
#else
  static bool use_command_line_push_parser = false;
#endif

  // The following logic is written as it is to allow easy transition
  // to setting USE_COMMAND_LINE_PUSH_PARSER at run time and to
  // simplify the logic of the main loop below by using the same
  // base_parser::run interface for both push and pull parsers.

  std::shared_ptr<base_parser> repl_parser;

  if (m_interpreter.interactive ())
    {
      if (use_command_line_push_parser)
        {
          push_parser *pp
            = new push_parser (m_interpreter,
                               new input_reader (m_interpreter));

          repl_parser = std::shared_ptr<base_parser> (pp);
        }
      else
        {
          parser *pp = new parser (new lexer (m_interpreter));
          repl_parser = std::shared_ptr<base_parser> (pp);
        }
    }
  else
    {
      parser *pp = new parser (new lexer (stdin, m_interpreter));
      repl_parser = std::shared_ptr<base_parser> (pp);
    }

  do
    {
      try
        {
          unwind_protect_var<bool> upv (m_in_top_level_repl, true);

          repl_parser->reset ();

          if (at_top_level ())
            {
              dbstep_flag (0);
              reset_debug_state ();
            }

          exit_status = repl_parser->run ();

          if (exit_status == 0)
            {
              std::shared_ptr<tree_statement_list>
              stmt_list = repl_parser->statement_list ();

              if (stmt_list)
                {
                  command_editor::increment_current_command_number ();

                  eval (stmt_list, m_interpreter.interactive ());
                }
              else if (repl_parser->at_end_of_input ())
                {
                  exit_status = EOF;
                  break;
                }
            }
        }
      catch (const interrupt_exception&)
        {
          m_interpreter.recover_from_exception ();

          // Required newline when the user does Ctrl+C at the prompt.
          if (m_interpreter.interactive ())
            octave_stdout << "\n";
        }
      catch (const index_exception& ie)
        {
          m_interpreter.recover_from_exception ();

          std::cerr << "error: unhandled index exception: "
                    << ie.message () << " -- trying to return to prompt"
                    << std::endl;
        }
      catch (const execution_exception& ee)
        {
          error_system& es = m_interpreter.get_error_system ();

          es.save_exception (ee);
          es.display_exception (ee);

          if (m_interpreter.interactive ())
            m_interpreter.recover_from_exception ();
          else
            {
              // We should exit with a nonzero status.
              exit_status = 1;
              break;
            }
        }
      catch (const quit_debug_exception&)
        {
          m_interpreter.recover_from_exception ();

          // FIXME: Does anything else need to happen here?
        }
      catch (const std::bad_alloc&)
        {
          m_interpreter.recover_from_exception ();

          std::cerr << "error: out of memory -- trying to return to prompt"
                    << std::endl;
        }
    }
  while (exit_status == 0);

  if (exit_status == EOF)
    {
      if (m_interpreter.interactive ())
        octave_stdout << "\n";

      exit_status = 0;
    }

  return exit_status;
}

int tree_evaluator::server_loop (void)
{
  // Process events from the event queue.

  unwind_protect_var<bool> upv1 (m_server_mode, true);

  m_exit_status = 0;

  std::shared_ptr<push_parser> parser (new push_parser (m_interpreter));
  unwind_protect_var<std::shared_ptr<push_parser>> upv2 (m_parser, parser);

  // FIXME: We are currently resetting the parser after every call to
  // recover_from_exception.  This action should probably be handled
  // in a more consistent way, but resetting the parser in every call
  // to interpreter::recover_from_exception appears to cause
  // segfaults in the test suite.

  do
    {
      try
        {
          // FIXME: Should we call octave_quit in the octave::sleep
          // and/or command_editor::run_event_hooks functions?

          octave_quit ();

          // FIXME: Running the event queue should be decoupled from
          // the command_editor.  We should also use a condition
          // variable to manage the execution of entries in the queue
          // and eliminate the need for the busy-wait loop.

          command_editor::run_event_hooks ();

          release_unreferenced_dynamic_libraries ();

          sleep (0.1);
        }
      catch (const interrupt_exception&)
        {
          octave_interrupt_state = 1;
          m_interpreter.recover_from_exception ();
          m_parser->reset ();

          // Required newline when the user does Ctrl+C at the prompt.
          if (m_interpreter.interactive ())
            octave_stdout << "\n";
        }
      catch (const index_exception& e)
        {
          m_interpreter.recover_from_exception ();
          m_parser->reset ();

          std::cerr << "error: unhandled index exception: "
                    << e.message () << " -- trying to return to prompt"
                    << std::endl;
        }
      catch (const execution_exception& ee)
        {
          error_system& es = m_interpreter.get_error_system ();

          es.save_exception (ee);
          es.display_exception (ee);

          if (m_interpreter.interactive ())
            {
              m_interpreter.recover_from_exception ();
              m_parser->reset ();
            }
          else
            {
              // We should exit with a nonzero status.
              m_exit_status = 1;
              break;
            }
        }
      catch (const quit_debug_exception&)
        {
          octave_interrupt_state = 1;
          m_interpreter.recover_from_exception ();
          m_parser->reset ();
        }
      catch (const exit_exception& xe)
        {
          m_exit_status = xe.exit_status ();
          break;
        }
      catch (const std::bad_alloc&)
        {
          m_interpreter.recover_from_exception ();
          m_parser->reset ();

          std::cerr << "error: out of memory -- trying to return to prompt"
                    << std::endl;
        }
    }
  while (m_exit_status == 0);

  if (m_exit_status == EOF)
    {
      if (m_interpreter.interactive ())
        octave_stdout << "\n";

      m_exit_status = 0;
    }

  return m_exit_status;
}

void tree_evaluator::eval (std::shared_ptr<tree_statement_list>& stmt_list,
                           bool interactive)
{
  try
    {
      stmt_list->accept (*this);

      octave_quit ();

      if (! interactive)
        {
          bool quit = (m_returning || m_breaking);

          if (m_returning)
            m_returning = 0;

          if (m_breaking)
            m_breaking--;

          if (quit)
            return;
        }

      if (octave_completion_matches_called)
        octave_completion_matches_called = false;
    }
  catch (const quit_debug_exception&)
    {
      m_interpreter.recover_from_exception ();
    }
}

octave_value_list
tree_evaluator::eval_string (const std::string& eval_str, bool silent,
                             int& parse_status, int nargout)
{
  octave_value_list retval;

  parser eval_parser (eval_str, m_interpreter);

  do
    {
      eval_parser.reset ();

      // If we are looking at
      //
      //   val = eval ("code");
      //
      // then don't allow code to be parsed as a command.

      if (nargout > 0)
        eval_parser.disallow_command_syntax ();

      parse_status = eval_parser.run ();

      if (parse_status == 0)
        {
          std::shared_ptr<tree_statement_list> stmt_list
            = eval_parser.statement_list ();

          if (stmt_list)
            {
              tree_statement *stmt = nullptr;

              if (stmt_list->length () == 1
                  && (stmt = stmt_list->front ())
                  && stmt->is_expression ())
                {
                  tree_expression *expr = stmt->expression ();

                  if (silent)
                    expr->set_print_flag (false);

                  retval = expr->evaluate_n (*this, nargout);

                  bool do_bind_ans = false;

                  if (expr->is_identifier ())
                    do_bind_ans = ! is_variable (expr);
                  else
                    do_bind_ans = ! expr->is_assignment_expression ();

                  if (do_bind_ans && ! retval.empty ())
                    bind_ans (retval(0), expr->print_result ());

                  if (nargout == 0)
                    retval = octave_value_list ();
                }
              else if (nargout == 0)
                stmt_list->accept (*this);
              else
                error ("eval: invalid use of statement list");

              if (returning () || breaking () || continuing ())
                break;
            }
          else if (eval_parser.at_end_of_input ())
            break;
        }
    }
  while (parse_status == 0);

  return retval;
}

octave_value tree_evaluator::eval_string (const std::string& eval_str,
    bool silent, int& parse_status)
{
  octave_value retval;

  octave_value_list tmp = eval_string (eval_str, silent, parse_status, 1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_value_list tree_evaluator::eval_string (const octave_value& arg,
    bool silent, int& parse_status,
    int nargout)
{
  std::string s = arg.xstring_value ("eval: expecting string argument");

  return eval_string (s, silent, parse_status, nargout);
}

octave_value_list tree_evaluator::eval (const std::string& try_code,
                                        int nargout)
{
  int parse_status = 0;

  return eval_string (try_code, nargout > 0, parse_status, nargout);
}

octave_value_list tree_evaluator::eval (const std::string& try_code,
                                        const std::string& catch_code,
                                        int nargout)
{
  octave_value_list retval;

  error_system& es = m_interpreter.get_error_system ();

  int parse_status = 0;

  bool execution_error = false;

  octave_value_list tmp;

  try
    {
      tmp = eval_string (try_code, nargout > 0, parse_status, nargout);
    }
  catch (const execution_exception& ee)
    {
      es.save_exception (ee);
      m_interpreter.recover_from_exception ();

      execution_error = true;
    }

  if (parse_status != 0 || execution_error)
    {
      tmp = eval_string (catch_code, nargout > 0, parse_status, nargout);

      retval = (nargout > 0) ? tmp : octave_value_list ();
    }
  else
    {
      if (nargout > 0)
        retval = tmp;

      // FIXME: we should really be rethrowing whatever
      // exception occurred, not just throwing an
      // execution exception.
      if (execution_error)
        throw execution_exception ();
    }

  return retval;
}

octave_value_list tree_evaluator::evalin (const std::string& context,
    const std::string& try_code,
    int nargout)
{
  unwind_action act ([=] (std::size_t frm)
  {
    m_call_stack.restore_frame (frm);
  }, m_call_stack.current_frame ());

  if (context == "caller")
    m_call_stack.goto_caller_frame ();
  else if (context == "base")
    m_call_stack.goto_base_frame ();
  else
    error (R"(evalin: CONTEXT must be "caller" or "base")");

  int parse_status = 0;

  return eval_string (try_code, nargout > 0, parse_status, nargout);
}

octave_value_list tree_evaluator::evalin (const std::string& context,
    const std::string& try_code,
    const std::string& catch_code,
    int nargout)
{
  octave_value_list retval;

  unwind_action act1 ([=] (std::size_t frm)
  {
    m_call_stack.restore_frame (frm);
  }, m_call_stack.current_frame ());

  if (context == "caller")
    m_call_stack.goto_caller_frame ();
  else if (context == "base")
    m_call_stack.goto_base_frame ();
  else
    error (R"(evalin: CONTEXT must be "caller" or "base")");

  error_system& es = m_interpreter.get_error_system ();

  int parse_status = 0;

  bool execution_error = false;

  octave_value_list tmp;

  try
    {
      tmp = eval_string (try_code, nargout > 0, parse_status, nargout);
    }
  catch (const execution_exception& ee)
    {
      es.save_exception (ee);
      m_interpreter.recover_from_exception ();

      execution_error = true;
    }

  if (parse_status != 0 || execution_error)
    {
      tmp = eval_string (catch_code, nargout > 0, parse_status, nargout);

      retval = (nargout > 0) ? tmp : octave_value_list ();
    }
  else
    {
      if (nargout > 0)
        retval = tmp;

      // FIXME: we should really be rethrowing whatever
      // exception occurred, not just throwing an
      // execution exception.
      if (execution_error)
        throw execution_exception ();
    }

  return retval;
}

void
tree_evaluator::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_argument_list (tree_argument_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_arguments_block (tree_arguments_block&)
{
  warning ("function arguments validation blocks are not supported; INCORRECT RESULTS ARE POSSIBLE");
}

void
tree_evaluator::visit_args_block_attribute_list (tree_args_block_attribute_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_args_block_validation_list (tree_args_block_validation_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_arg_validation (tree_arg_validation&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_arg_size_spec (tree_arg_size_spec&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_arg_validation_fcns (tree_arg_validation_fcns&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_binary_expression (tree_binary_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_boolean_expression (tree_boolean_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_compound_binary_expression (tree_compound_binary_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_break_command (tree_break_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  if (m_debug_mode)
    do_breakpoint (cmd.is_active_breakpoint (*this));

  if (m_in_loop_command)
    m_breaking = 1;
  else
    error ("break must appear in a loop in the same file as loop command");
}

void
tree_evaluator::visit_colon_expression (tree_colon_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_continue_command (tree_continue_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  if (m_debug_mode)
    do_breakpoint (cmd.is_active_breakpoint (*this));

  if (m_in_loop_command)
    m_continuing = 1;
}

bool
tree_evaluator::statement_printing_enabled (void)
{
  return ! (m_silent_functions && (m_statement_context == SC_FUNCTION
                                   || m_statement_context == SC_SCRIPT));
}

void
tree_evaluator::reset_debug_state (void)
{
  m_debug_mode = (m_bp_table.have_breakpoints ()
                  || m_dbstep_flag != 0
                  || m_break_on_next_stmt
                  || in_debug_repl ());
}

void
tree_evaluator::reset_debug_state (bool mode)
{
  m_debug_mode = mode;
}

void
tree_evaluator::enter_debugger (const std::string& prompt)
{
  unwind_protect frame;

  frame.add (command_history::ignore_entries,
             command_history::ignoring_entries ());

  command_history::ignore_entries (false);

  frame.add (&call_stack::restore_frame, &m_call_stack,
             m_call_stack.current_frame ());

  // Don't allow errors or warnings at the debug prompt to push us
  // into deeper levels of debugging.

  error_system& es = m_interpreter.get_error_system ();

  frame.add (&error_system::set_debug_on_error, &es, es.debug_on_error ());

  frame.add (&error_system::set_debug_on_warning, &es,
             es.debug_on_warning ());

  es.debug_on_error (false);
  es.debug_on_warning (false);

  // Go up to the nearest user code frame.

  m_debug_frame = m_call_stack.dbupdown (0);

  // FIXME: probably we just want to print one line, not the
  // entire statement, which might span many lines...
  //
  // tree_print_code tpc (octave_stdout);
  // stmt.accept (tpc);

  debugger *dbgr = new debugger (m_interpreter, m_debugger_stack.size ());

  m_debugger_stack.push (dbgr);

  frame.add ([=] (void)
  {
    delete m_debugger_stack.top ();
    m_debugger_stack.pop ();
    reset_debug_state ();
  });

  dbgr->repl (prompt);
}

void
tree_evaluator::keyboard (const std::string& prompt)
{
  enter_debugger (prompt);
}

void
tree_evaluator::dbupdown (int n, bool verbose)
{
  m_debug_frame = m_call_stack.dbupdown (n, verbose);
}

Matrix
tree_evaluator::ignored_fcn_outputs (void) const
{
  Matrix retval;

  const std::list<octave_lvalue> *lvalues = m_lvalue_list;

  if (! lvalues)
    return retval;

  octave_idx_type nbh = 0;

  for (const auto& lval : *lvalues)
    nbh += lval.is_black_hole ();

  if (nbh > 0)
    {
      retval.resize (1, nbh);

      octave_idx_type k = 0;
      octave_idx_type l = 0;

      for (const auto& lval : *lvalues)
        {
          if (lval.is_black_hole ())
            retval(l++) = k+1;

          k += lval.numel ();
        }
    }

  return retval;
}

// If NAME is an operator (like "+", "-", ...), convert it to the
// corresponding function name ("plus", "minus", ...).

static std::string
get_operator_function_name (const std::string& name)
{
  // Bow to the god of compatibility.

  // FIXME: it seems ugly to put this here, but there is no single
  // function in the parser that converts from the operator name to
  // the corresponding function name.  At least try to do it without N
  // string compares.

  // FIXME: .+, .-, **, and .** are deprecated but still need to be
  // handled here until they are removed.

  std::size_t len = name.length ();

  if (len == 3 && name == ".**")
    // deprecated
    return "power";
  else if (len == 2)
    {
      if (name[0] == '.')
        {
          switch (name[1])
            {
            case '\'':
              return "transpose";

            case '+':
              // deprecated
              return "plus";

            case '-':
              // deprecated
              return "minus";

            case '*':
              return "times";

            case '/':
              return "rdivide";

            case '^':
              return "power";

            case '\\':
              return "ldivide";

            default:
              break;
            }
        }
      else if (name[1] == '=')
        {
          switch (name[0])
            {
            case '<':
              return "le";

            case '=':
              return "eq";

            case '>':
              return "ge";

            case '~':
            case '!':
              return "ne";

            default:
              break;
            }
        }
      else if (name == "**")
        // deprecated
        return "mpower";
    }
  else if (len == 1)
    {
      switch (name[0])
        {
        case '~':
        case '!':
          return "not";

        case '\'':
          return "ctranspose";

        case '+':
          return "plus";

        case '-':
          return "minus";

        case '*':
          return "mtimes";

        case '/':
          return "mrdivide";

        case '^':
          return "mpower";

        case '\\':
          return "mldivide";

        case '<':
          return "lt";

        case '>':
          return "gt";

        case '&':
          return "and";

        case '|':
          return "or";

        default:
          break;
        }
    }

  return name;
}

// Creates a function handle that takes into account the context,
// finding local, nested, private, or sub functions.

octave_value
tree_evaluator::make_fcn_handle (const std::string& name)
{
  octave_value retval;

  // The str2func function can create a function handle with the name
  // of an operator (for example, "+").  If so, it is converted to the
  // name of the corresponding function ("+" -> "plus") and we create
  // a simple function handle using that name.

  std::string fcn_name = get_operator_function_name (name);

  // If FCN_NAME is different from NAME, then NAME is an operator.  As
  // of version 2020a, Matlab apparently uses the function name
  // corresponding to the operator to search for private and local
  // functions in the current scope but not(!) nested functions.

  bool name_is_operator = fcn_name != name;

  std::size_t pos = fcn_name.find ('.');

  if (pos != std::string::npos)
    {
      // Recognize (some of?  which ones?) the following cases
      // and create something other than a simple function handle?
      // Should we just be checking for the last two when the first
      // element of the dot-separated list is an object?  If so, then
      // should this syntax be limited to a dot-separated list with
      // exactly two elements?
      //
      //   object . method
      //   object . static-method
      //
      // Code to do that duplicates some of simple_fcn_handle::call.

      // Only accept expressions that contain one '.' separator.

      // FIXME: The logic here is a bit complicated.  Is there a good
      // way to simplify it?

      std::string meth_nm = fcn_name.substr (pos+1);

      if (meth_nm.find ('.') == std::string::npos)
        {
          std::string obj_nm = fcn_name.substr (0, pos);

          // If obj_nm is an object in the current scope with a
          // method named meth_nm, create a classsimple handle.

          octave_value object = varval (obj_nm);

          if (object.is_defined () && object.is_classdef_object ())
            {
              octave_classdef *cdef = object.classdef_object_value ();

              if (cdef)
                {
                  std::string class_nm = cdef->class_name ();

                  cdef_object cdef_obj = cdef->get_object ();

                  cdef_class cls = cdef_obj.get_class ();

                  cdef_method meth = cls.find_method (meth_nm);

                  if (meth.ok ())
                    {
                      // If the method we found is static, create a
                      // new function name from the class name and
                      // method name and create a simple function
                      // handle below.  Otherwise, create a class
                      // simple function handle.

                      if (meth.is_static ())
                        fcn_name = class_nm + '.' + meth_nm;
                      else
                        {
                          octave_value meth_fcn = meth.get_function ();

                          octave_fcn_handle *fh
                            = new octave_fcn_handle (object, meth_fcn,
                                                     class_nm, meth_nm);

                          return octave_value (fh);
                        }
                    }
                }
            }
        }

      // We didn't match anything above, so create handle to SIMPLE
      // package function or static class method.  Function resolution
      // is performed when the handle is used.

      return octave_value (new octave_fcn_handle (fcn_name));
    }

  // If the function name refers to a sub/local/private function or a
  // class method/constructor, create scoped function handle that is
  // bound to that function.  Use the same precedence list as
  // fcn_info::find but limit search to the following types of
  // functions:
  //
  //   nested functions (and subfunctions)
  //   local functions in the current file
  //   private function
  //   class method
  //
  // For anything else we create a simple function handle that will be
  // resolved dynamically in the scope where it is evaluated.

  symbol_scope curr_scope = get_current_scope ();

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  if (curr_scope)
    {
      octave_value ov_fcn
        = symtab.find_scoped_function (fcn_name, curr_scope);

      // If name is operator, we are in Fstr2func, so skip the stack
      // frame for that function.

      bool skip_first = name_is_operator;
      octave_function *curr_fcn = current_function (skip_first);

      if (ov_fcn.is_defined ())
        {
          octave_function *fcn = ov_fcn.function_value ();

          if (fcn->is_nested_function ())
            {
              if (! name_is_operator)
                {
                  // Get current stack frame and return handle to nested
                  // function.

                  std::shared_ptr<stack_frame> frame
                    = m_call_stack.get_current_stack_frame ();

                  // If we are creating a handle to the current
                  // function or a handle to a sibling function (i.e.,
                  // not a child of the current function), then use
                  // the calling stack frame as the context instead of
                  // the current stack frame.

                  // FIXME:  Do we need both checks here or is it
                  // sufficient to check that the parent of curr_fcn
                  // is the same as the parent of fcn?  Is there any
                  // case where curr_fcn could be nullptr, or does
                  // that indicate an internal error of some kind?

                  if (curr_fcn
                      && (fcn_name == curr_fcn->name ()
                          || fcn->parent_fcn_name () == curr_fcn->parent_fcn_name ()))
                    frame = frame->access_link ();

                  octave_fcn_handle *fh
                    = new octave_fcn_handle (ov_fcn, fcn_name, frame);

                  return octave_value (fh);
                }
            }
          else if (fcn->is_subfunction ()
                   /* || fcn->is_localfunction () */
                   || fcn->is_private_function ())
            {
              // Create handle to SCOPED function (sub/local function
              // or private function).

              std::list<std::string> parentage = fcn->parent_fcn_names ();

              octave_fcn_handle *fh
                = new octave_fcn_handle (ov_fcn, fcn_name, parentage);

              return octave_value (fh);
            }
        }

      if (curr_fcn && (curr_fcn->is_class_method ()
                       || curr_fcn->is_class_constructor ()))
        {
          std::string dispatch_class = curr_fcn->dispatch_class ();

          octave_value ov_meth
            = symtab.find_method (fcn_name, dispatch_class);

          if (ov_meth.is_defined ())
            {
              octave_function *fcn = ov_meth.function_value ();

              // FIXME: do we need to check that it is a method of
              // dispatch_class, or is it sufficient to just check
              // that it is a method?

              if (fcn->is_class_method ())
                {
                  // Create CLASSSIMPLE handle to method but don't
                  // bind to the method.  Lookup will be done later.

                  octave_fcn_handle *fh
                    = new octave_fcn_handle (dispatch_class, fcn_name);

                  return octave_value (fh);
                }
            }
        }
    }

  octave_value ov_fcn = symtab.find_user_function (fcn_name);

  // Create handle to SIMPLE function.  If the function is not found
  // now, then we will look for it again when the handle is used.

  return octave_value (new octave_fcn_handle (ov_fcn, fcn_name));
}

/*
%!test
%! x = {".**", "power";
%!      ".'", "transpose";
%!      ".+", "plus";
%!      ".-", "minus";
%!      ".*", "times";
%!      "./", "rdivide";
%!      ".^", "power";
%!      ".\\", "ldivide";
%!      "<=", "le";
%!      "==", "eq";
%!      ">=", "ge";
%!      "!=", "ne";
%!      "~=", "ne";
%!      "**", "mpower";
%!      "~", "not";
%!      "!", "not";
%!      "\'", "ctranspose";
%!      "+", "plus";
%!      "-", "minus";
%!      "*", "mtimes";
%!      "/", "mrdivide";
%!      "^", "mpower";
%!      "\\", "mldivide";
%!      "<", "lt";
%!      ">", "gt";
%!      "&", "and";
%!      "|", "or"};
%! for i = 1:rows (x)
%!   assert (functions (str2func (x{i,1})).function, x{i,2});
%! endfor
*/

octave_value
tree_evaluator::evaluate (tree_decl_elt *elt)
{
  // Do not allow functions to return null values.

  tree_identifier *id = elt->ident ();

  return id ? id->evaluate (*this).storable_value () : octave_value ();
}

bool
tree_evaluator::is_variable (const std::string& name) const
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->is_variable (name);
}

bool
tree_evaluator::is_local_variable (const std::string& name) const
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->is_local_variable (name);
}

bool
tree_evaluator::is_variable (const tree_expression *expr) const
{
  if (expr->is_identifier ())
    {
      const tree_identifier *id
        = dynamic_cast<const tree_identifier *> (expr);

      if (id->is_black_hole ())
        return false;

      return is_variable (id->symbol ());
    }

  return false;
}

bool
tree_evaluator::is_defined (const tree_expression *expr) const
{
  if (expr->is_identifier ())
    {
      const tree_identifier *id
        = dynamic_cast<const tree_identifier *> (expr);

      return is_defined (id->symbol ());
    }

  return false;
}

bool
tree_evaluator::is_variable (const symbol_record& sym) const
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->is_variable (sym);
}

bool
tree_evaluator::is_defined (const symbol_record& sym) const
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->is_defined (sym);
}

bool tree_evaluator::is_global (const std::string& name) const
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->is_global (name);
}

octave_value
tree_evaluator::varval (const symbol_record& sym) const
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->varval (sym);
}

octave_value
tree_evaluator::varval (const std::string& name) const
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->varval (name);
}

void tree_evaluator::install_variable (const std::string& name,
                                       const octave_value& value,
                                       bool global)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  return frame->install_variable (name, value, global);
}

octave_value
tree_evaluator::global_varval (const std::string& name) const
{
  return m_call_stack.global_varval (name);
}

octave_value&
tree_evaluator::global_varref (const std::string& name)
{
  return m_call_stack.global_varref (name);
}

void
tree_evaluator::global_assign (const std::string& name,
                               const octave_value& val)
{
  m_call_stack.global_varref (name) = val;
}

octave_value
tree_evaluator::top_level_varval (const std::string& name) const
{
  return m_call_stack.get_top_level_value (name);
}

void
tree_evaluator::top_level_assign (const std::string& name,
                                  const octave_value& val)
{
  m_call_stack.set_top_level_value (name, val);
}

void
tree_evaluator::assign (const std::string& name, const octave_value& val)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  frame->assign (name, val);
}

void
tree_evaluator::assignin (const std::string& context,
                          const std::string& name, const octave_value& val)
{
  // FIXME: Can this be done without an unwind-protect frame, simply
  // by getting a reference to the caller or base stack frame and
  // calling assign on that?

  unwind_action act ([=] (std::size_t frm)
  {
    m_call_stack.restore_frame (frm);
  }, m_call_stack.current_frame ());

  if (context == "caller")
    m_call_stack.goto_caller_frame ();
  else if (context == "base")
    m_call_stack.goto_base_frame ();
  else
    error (R"(assignin: CONTEXT must be "caller" or "base")");

  if (valid_identifier (name))
    {
      // Put the check here so that we don't slow down assignments
      // generally.  Any that go through Octave's parser should have
      // already been checked.

      if (iskeyword (name))
        error ("assignin: invalid assignment to keyword '%s'",
               name.c_str ());

      assign (name, val);
    }
  else
    error ("assignin: invalid variable name '%s'", name.c_str ());
}

void
tree_evaluator::source_file (const std::string& file_name,
                             const std::string& context,
                             bool verbose, bool require_file)
{
  // Map from absolute name of script file to recursion level.  We
  // use a map instead of simply placing a limit on recursion in the
  // source_file function so that two mutually recursive scripts
  // written as
  //
  //   foo1.m:
  //   ------
  //   foo2
  //
  //   foo2.m:
  //   ------
  //   foo1
  //
  // and called with
  //
  //   foo1
  //
  // (for example) will behave the same if they are written as
  //
  //   foo1.m:
  //   ------
  //   source ("foo2.m")
  //
  //   foo2.m:
  //   ------
  //   source ("foo1.m")
  //
  // and called with
  //
  //   source ("foo1.m")
  //
  // (for example).

  static std::map<std::string, int> source_call_depth;

  std::string file_full_name
    = sys::file_ops::tilde_expand (file_name);

  std::size_t pos
    = file_full_name.find_last_of (sys::file_ops::dir_sep_str ());

  std::string dir_name = file_full_name.substr (0, pos);

  file_full_name = sys::env::make_absolute (file_full_name);

  unwind_protect frame;

  if (source_call_depth.find (file_full_name) == source_call_depth.end ())
    source_call_depth[file_full_name] = -1;

  frame.protect_var (source_call_depth[file_full_name]);

  source_call_depth[file_full_name]++;

  if (source_call_depth[file_full_name] >= max_recursion_depth ())
    error ("max_recursion_depth exceeded");

  if (! context.empty ())
    {
      frame.add (&call_stack::restore_frame, &m_call_stack,
                 m_call_stack.current_frame ());

      if (context == "caller")
        m_call_stack.goto_caller_frame ();
      else if (context == "base")
        m_call_stack.goto_base_frame ();
      else
        error (R"(source: CONTEXT must be "caller" or "base")");
    }

  // Find symbol name that would be in symbol_table, if it were loaded.
  std::size_t dir_end
    = file_name.find_last_of (sys::file_ops::dir_sep_chars ());
  dir_end = (dir_end == std::string::npos) ? 0 : dir_end + 1;

  std::size_t extension = file_name.find_last_of ('.');
  if (extension == std::string::npos)
    extension = file_name.length ();

  std::string symbol = file_name.substr (dir_end, extension - dir_end);
  std::string full_name = sys::canonicalize_file_name (file_name);

  // Check if this file is already loaded (or in the path)
  symbol_table& symtab = m_interpreter.get_symbol_table ();
  octave_value ov_code = symtab.fcn_table_find (symbol);

  // For compatibility with Matlab, accept both scripts and
  // functions.

  if (ov_code.is_user_code ())
    {
      octave_user_code *code = ov_code.user_code_value ();

      if (! code
          || (sys::canonicalize_file_name (code->fcn_file_name ())
              != full_name))
        {
          // Wrong file, so load it below.
          ov_code = octave_value ();
        }
    }
  else
    {
      // Not a script, so load it below.
      ov_code = octave_value ();
    }

  // If no symbol of this name, or the symbol is for a different
  // file, load.

  if (ov_code.is_undefined ())
    {
      try
        {
          ov_code = parse_fcn_file (m_interpreter, file_full_name,
                                    file_name, dir_name, "", "",
                                    require_file, true, false, false);
        }
      catch (execution_exception& ee)
        {
          error (ee, "source: error sourcing file '%s'",
                 file_full_name.c_str ());
        }
    }

  // Return or error if we don't have a valid script or function.

  if (ov_code.is_undefined ())
    return;

  if (! ov_code.is_user_code ())
    error ("source: %s is not a script", full_name.c_str ());

  if (verbose)
    {
      octave_stdout << "executing commands from " << full_name << " ... ";
      octave_stdout.flush ();
    }

  octave_user_code *code = ov_code.user_code_value ();

  code->call (*this, 0, octave_value_list ());

  if (verbose)
    octave_stdout << "done." << std::endl;
}

void
tree_evaluator::set_auto_fcn_var (stack_frame::auto_var_type avt,
                                  const octave_value& val)
{
  m_call_stack.set_auto_fcn_var (avt, val);
}

octave_value
tree_evaluator::get_auto_fcn_var (stack_frame::auto_var_type avt) const
{
  return m_call_stack.get_auto_fcn_var (avt);
}

void
tree_evaluator::define_parameter_list_from_arg_vector
(tree_parameter_list *param_list, const octave_value_list& args)
{
  if (! param_list || param_list->varargs_only ())
    return;

  int i = -1;

  for (tree_decl_elt *elt : *param_list)
    {
      i++;

      octave_lvalue ref = elt->lvalue (*this);

      if (i < args.length ())
        {
          if (args(i).is_defined () && args(i).is_magic_colon ())
            {
              if (! eval_decl_elt (elt))
                error ("no default value for argument %d", i+1);
            }
          else
            ref.define (args(i));
        }
      else
        eval_decl_elt (elt);
    }
}

void
tree_evaluator::undefine_parameter_list (tree_parameter_list *param_list)
{
  for (tree_decl_elt *elt : *param_list)
    {
      octave_lvalue ref = elt->lvalue (*this);

      ref.assign (octave_value::op_asn_eq, octave_value ());
    }
}

// END is documented in op-kw-docs.
DEFMETHOD (end, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} end
Last element of an array or the end of any @code{for}, @code{parfor},
@code{if}, @code{do}, @code{while}, @code{function}, @code{switch},
@code{try}, or @code{unwind_protect} block.

As an index of an array, the magic index @qcode{"end"} refers to the
last valid entry in an indexing operation.

Example:

@example
@group
@var{x} = [ 1 2 3; 4 5 6 ];
@var{x}(1,end)
   @result{} 3
@var{x}(end,1)
   @result{} 4
@var{x}(end,end)
   @result{} 6
@end group
@end example
@seealso{for, parfor, if, do, while, function, switch, try, unwind_protect}
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.evaluate_end_expression (args);
}

/*
%!test <*58830>
%! fail ("__undef_sym__ (end)",
%!       "invalid use of 'end': may only be used to index existing value");

%!test <*58953>
%! x = 1:10;
%! assert (x(end), 10);
%! assert (x(minus (end, 1)), 9);
%! assert (x(minus (minus (end, 1), 1)), 8);
*/

octave_value_list
tree_evaluator::convert_to_const_vector (tree_argument_list *args)
{
  std::list<octave_value> arg_vals;

  for (auto elt : *args)
    {
      // FIXME: is it possible for elt to be invalid?

      if (! elt)
        break;

      octave_value tmp = elt->evaluate (*this);

      if (tmp.is_cs_list ())
        {
          octave_value_list tmp_ovl = tmp.list_value ();

          for (octave_idx_type i = 0; i < tmp_ovl.length (); i++)
            arg_vals.push_back (tmp_ovl(i));
        }
      else if (tmp.is_defined ())
        arg_vals.push_back (tmp);
    }

  return octave_value_list (arg_vals);
}

octave_value_list
tree_evaluator::convert_return_list_to_const_vector
(tree_parameter_list *ret_list, int nargout, const Matrix& ignored_outputs,
 const Cell& varargout)
{
  octave_idx_type vlen = varargout.numel ();
  int len = ret_list->length ();

  // Special case.  Will do a shallow copy.
  if (len == 0)
    return varargout;
  else
    {
      int i = 0;
      int k = 0;
      int num_ignored = ignored_outputs.numel ();
      int ignored = num_ignored > 0 ? ignored_outputs(k) - 1 : -1;

      if (nargout <= len)
        {
          int nout = nargout > 0 ? nargout : 1;
          octave_value_list retval (nout);

          for (tree_decl_elt *elt : *ret_list)
            {
              if (nargout == 0 && ! is_defined (elt->ident ()))
                break;

              if (ignored >= 0 && i == ignored)
                {
                  i++;
                  k++;
                  ignored = k < num_ignored ? ignored_outputs(k) - 1 : -1;
                }
              else
                retval(i++) = evaluate (elt);

              if (i == nout)
                break;
            }

          return retval;
        }
      else
        {
          octave_value_list retval (len + vlen);

          for (tree_decl_elt *elt : *ret_list)
            {
              if (ignored >= 0 && i == ignored)
                {
                  i++;
                  k++;
                  ignored = k < num_ignored ? ignored_outputs(k) - 1 : -1;
                }
              else
                retval(i++) = evaluate (elt);
            }

          for (octave_idx_type j = 0; j < vlen; j++)
            retval(i++) = varargout(j);

          return retval;
        }
    }
}

bool
tree_evaluator::eval_decl_elt (tree_decl_elt *elt)
{
  bool retval = false;

  tree_identifier *id = elt->ident ();
  tree_expression *expr = elt->expression ();

  if (id && expr)
    {
      octave_lvalue ult = id->lvalue (*this);

      octave_value init_val = expr->evaluate (*this);

      ult.assign (octave_value::op_asn_eq, init_val);

      retval = true;
    }

  return retval;
}

bool
tree_evaluator::switch_case_label_matches (tree_switch_case *expr,
    const octave_value& val)
{
  tree_expression *label = expr->case_label ();

  octave_value label_value = label->evaluate (*this);

  if (label_value.is_defined ())
    {
      if (label_value.iscell ())
        {
          Cell cell (label_value.cell_value ());

          for (octave_idx_type i = 0; i < cell.rows (); i++)
            {
              for (octave_idx_type j = 0; j < cell.columns (); j++)
                {
                  bool match = val.is_equal (cell(i, j));

                  if (match)
                    return true;
                }
            }
        }
      else
        return val.is_equal (label_value);
    }

  return false;
}

void tree_evaluator::push_stack_frame (const symbol_scope& scope)
{
  m_call_stack.push (scope);
}

void tree_evaluator::push_stack_frame (octave_user_function *fcn,
                                       const std::shared_ptr<stack_frame>& closure_frames)
{
  m_call_stack.push (fcn, closure_frames);
}

void tree_evaluator::push_stack_frame (octave_user_function *fcn,
                                       const stack_frame::local_vars_map& local_vars,
                                       const std::shared_ptr<stack_frame>& closure_frames)
{
  m_call_stack.push (fcn, local_vars, closure_frames);
}

void tree_evaluator::push_stack_frame (octave_user_script *script)
{
  m_call_stack.push (script);
}

void tree_evaluator::push_stack_frame (octave_function *fcn)
{
  m_call_stack.push (fcn);
}

void tree_evaluator::pop_stack_frame (void)
{
  m_call_stack.pop ();
}

int tree_evaluator::current_line (void) const
{
  return m_call_stack.current_line ();
}

int tree_evaluator::current_column (void) const
{
  return m_call_stack.current_column ();
}

int tree_evaluator::debug_user_code_line (void) const
{
  return m_call_stack.debug_user_code_line ();
}

int tree_evaluator::debug_user_code_column (void) const
{
  return m_call_stack.debug_user_code_column ();
}

void tree_evaluator::debug_where (std::ostream& os) const
{
  std::shared_ptr<stack_frame> frm = m_call_stack.current_user_frame ();

  frm->display_stopped_in_message (os);
}

octave_user_code *tree_evaluator::current_user_code (void) const
{
  return m_call_stack.current_user_code ();
}

unwind_protect *tree_evaluator::curr_fcn_unwind_protect_frame (void)
{
  return m_call_stack.curr_fcn_unwind_protect_frame ();
}

octave_user_code *tree_evaluator::debug_user_code (void) const
{
  return m_call_stack.debug_user_code ();
}

octave_function *tree_evaluator::current_function (bool skip_first) const
{
  return m_call_stack.current_function (skip_first);
}

octave_function *tree_evaluator::caller_function (void) const
{
  return m_call_stack.current_function (true);
}

bool tree_evaluator::goto_frame (std::size_t n, bool verbose)
{
  return m_call_stack.goto_frame (n, verbose);
}

void tree_evaluator::goto_caller_frame (void)
{
  m_call_stack.goto_caller_frame ();
}

void tree_evaluator::goto_base_frame (void)
{
  m_call_stack.goto_base_frame ();
}

void tree_evaluator::restore_frame (std::size_t n)
{
  return m_call_stack.restore_frame (n);
}

std::string tree_evaluator::get_dispatch_class (void) const
{
  return m_call_stack.get_dispatch_class ();
}

void tree_evaluator::set_dispatch_class (const std::string& class_name)
{
  m_call_stack.set_dispatch_class (class_name);
}

bool
tree_evaluator::is_class_method_executing (std::string& dclass) const
{
  return m_call_stack.is_class_method_executing (dclass);
}

bool
tree_evaluator::is_class_constructor_executing (std::string& dclass) const
{
  return m_call_stack.is_class_constructor_executing (dclass);
}

std::list<std::shared_ptr<stack_frame>>
                                     tree_evaluator::backtrace_frames (octave_idx_type& curr_user_frame) const
{
  return m_call_stack.backtrace_frames (curr_user_frame);
}

std::list<std::shared_ptr<stack_frame>>
                                     tree_evaluator::backtrace_frames (void) const
{
  return m_call_stack.backtrace_frames ();
}

std::list<frame_info>
tree_evaluator::backtrace_info (octave_idx_type& curr_user_frame,
                                bool print_subfn) const
{
  return m_call_stack.backtrace_info (curr_user_frame, print_subfn);
}

std::list<frame_info> tree_evaluator::backtrace_info (void) const
{
  return m_call_stack.backtrace_info ();
}

octave_map
tree_evaluator::backtrace (octave_idx_type& curr_user_frame,
                           bool print_subfn) const
{
  return m_call_stack.backtrace (curr_user_frame, print_subfn);
}

octave_map tree_evaluator::backtrace (void) const
{
  return m_call_stack.backtrace ();
}

octave_map tree_evaluator::empty_backtrace (void) const
{
  return m_call_stack.empty_backtrace ();
}

std::string tree_evaluator::backtrace_message (void) const
{
  std::list<frame_info> frames = backtrace_info ();

  std::ostringstream buf;

  for (const auto& frm : frames)
    {
      buf << "    " << frm.fcn_name ();

      int line = frm.line ();

      if (line > 0)
        {
          buf << " at line " << line;

          int column = frm.column ();

          if (column > 0)
            buf << " column " << column;

          buf << "\n";
        }
    }

  return buf.str ();
}

void tree_evaluator::push_dummy_scope (const std::string& name)
{
  symbol_scope dummy_scope (name + "$dummy");

  m_call_stack.push (dummy_scope);
}

void tree_evaluator::pop_scope (void)
{
  m_call_stack.pop ();
}

symbol_scope tree_evaluator::get_top_scope (void) const
{
  return m_call_stack.top_scope ();
}

symbol_scope tree_evaluator::get_current_scope (void) const
{
  return m_call_stack.current_scope ();
}

void tree_evaluator::mlock (bool skip_first) const
{
  octave_function *fcn = m_call_stack.current_function (skip_first);

  if (! fcn)
    error ("mlock: invalid use outside a function");

  if (fcn->is_builtin_function ())
    {
      warning ("mlock: locking built-in function has no effect");
      return;
    }

  fcn->lock ();
}

void tree_evaluator::munlock (bool skip_first) const
{
  octave_function *fcn = m_call_stack.current_function (skip_first);

  if (! fcn)
    error ("munlock: invalid use outside a function");

  if (fcn->is_builtin_function ())
    {
      warning ("munlock: unlocking built-in function has no effect");
      return;
    }

  fcn->unlock ();
}

bool tree_evaluator::mislocked (bool skip_first) const
{
  octave_function *fcn = m_call_stack.current_function (skip_first);

  if (! fcn)
    error ("mislocked: invalid use outside a function");

  return fcn->islocked ();
}

octave_value
tree_evaluator::max_stack_depth (const octave_value_list& args, int nargout)
{
  return m_call_stack.max_stack_depth (args, nargout);
}

void tree_evaluator::display_call_stack (void) const
{
  m_call_stack.display ();
}

octave_value tree_evaluator::find (const std::string& name)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  octave_value val = frame->varval (name);

  if (val.is_defined ())
    return val;

  // Subfunction.  I think it only makes sense to check for
  // subfunctions if we are currently executing a function defined
  // from a .m file.

  octave_value fcn = frame->find_subfunction (name);

  if (fcn.is_defined ())
    return fcn;

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  return symtab.fcn_table_find (name, ovl ());
}

void tree_evaluator::clear_objects (void)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  frame->clear_objects ();
}

void tree_evaluator::clear_variable (const std::string& name)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  frame->clear_variable (name);
}

void tree_evaluator::clear_variable_pattern (const std::string& pattern)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  frame->clear_variable_pattern (pattern);
}

void tree_evaluator::clear_variable_regexp (const std::string& pattern)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  frame->clear_variable_regexp (pattern);
}

void tree_evaluator::clear_variables (void)
{
  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  frame->clear_variables ();
}

void tree_evaluator::clear_global_variable (const std::string& name)
{
  m_call_stack.clear_global_variable (name);
}

void
tree_evaluator::clear_global_variable_pattern (const std::string& pattern)
{
  m_call_stack.clear_global_variable_pattern (pattern);
}

void tree_evaluator::clear_global_variable_regexp(const std::string& pattern)
{
  m_call_stack.clear_global_variable_regexp (pattern);
}

void tree_evaluator::clear_global_variables (void)
{
  m_call_stack.clear_global_variables ();
}

void tree_evaluator::clear_all (bool force)
{
  // FIXME: should this also clear objects?

  clear_variables ();
  clear_global_variables ();

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  symtab.clear_functions (force);
}

void tree_evaluator::clear_symbol (const std::string& name)
{
  // FIXME: are we supposed to do both here?

  clear_variable (name);

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  symtab.clear_function (name);
}

void tree_evaluator::clear_symbol_pattern (const std::string& pattern)
{
  // FIXME: are we supposed to do both here?

  clear_variable_pattern (pattern);

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  symtab.clear_function_pattern (pattern);
}

void tree_evaluator::clear_symbol_regexp (const std::string& pattern)
{
  // FIXME: are we supposed to do both here?

  clear_variable_regexp (pattern);

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  symtab.clear_function_regexp (pattern);
}

std::list<std::string> tree_evaluator::global_variable_names (void) const
{
  return m_call_stack.global_variable_names ();
}

std::list<std::string> tree_evaluator::top_level_variable_names (void) const
{
  return m_call_stack.top_level_variable_names ();
}

std::list<std::string> tree_evaluator::variable_names (void) const
{
  return m_call_stack.variable_names ();
}

// Return a pointer to the user-defined function FNAME.  If FNAME is empty,
// search backward for the first user-defined function in the
// current call stack.

octave_user_code *
tree_evaluator::get_user_code (const std::string& fname,
                               const std::string& class_name)
{
  octave_user_code *user_code = nullptr;

  if (fname.empty ())
    user_code = m_call_stack.debug_user_code ();
  else
    {
      std::string name = fname;

      if (sys::file_ops::dir_sep_char () != '/' && name[0] == '@')
        {
          auto beg = name.begin () + 2;  // never have @/method
          auto end = name.end () - 1;    // never have trailing '/'
          std::replace (beg, end, '/', sys::file_ops::dir_sep_char ());
        }

      std::size_t name_len = name.length ();

      if (name_len > 2 && name.substr (name_len-2) == ".m")
        name = name.substr (0, name_len-2);

      if (name.empty ())
        return nullptr;

      symbol_table& symtab = m_interpreter.get_symbol_table ();

      octave_value fcn;
      std::size_t p2 = std::string::npos;

      if (name[0] == '@')
        {
          std::size_t p1 = name.find (sys::file_ops::dir_sep_char (), 1);

          if (p1 == std::string::npos)
            return nullptr;

          std::string dispatch_type = name.substr (1, p1-1);

          p2 = name.find ('>', p1);

          std::string method = name.substr (p1+1, p2-1);

          fcn = symtab.find_method (method, dispatch_type);
        }
      else if (! class_name.empty ())
        {
          cdef_manager& cdm = m_interpreter.get_cdef_manager ();

          fcn = cdm.find_method (class_name, name);

          // If there is no classdef method, then try legacy classes.
          if (fcn.is_undefined ())
            fcn = symtab.find_method (name, class_name);
        }
      else
        {
          p2 = name.find ('>');

          std::string main_fcn = name.substr (0, p2);

          fcn = symtab.find_function (main_fcn);
        }

      // List of function names sub1>sub2>...
      std::string subfuns;

      if (p2 != std::string::npos)
        subfuns = name.substr (p2+1);

      if (fcn.is_defined () && fcn.is_user_code ())
        user_code = fcn.user_code_value ();

      if (! user_code || subfuns.empty ())
        return user_code;

      fcn = user_code->find_subfunction (subfuns);

      if (fcn.is_undefined ())
        return nullptr;

      user_code = fcn.user_code_value ();
    }

  return user_code;
}

std::string
tree_evaluator::current_function_name (bool skip_first) const
{
  octave_function *curfcn = m_call_stack.current_function (skip_first);

  if (curfcn)
    return curfcn->name ();

  return "";
}

bool
tree_evaluator::in_user_code (void) const
{
  return m_call_stack.current_user_code () != nullptr;
}

void
tree_evaluator::visit_decl_command (tree_decl_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  if (m_debug_mode)
    do_breakpoint (cmd.is_active_breakpoint (*this));

  // FIXME: tree_decl_init_list is not derived from tree, so should it
  // really have an accept method?

  tree_decl_init_list *init_list = cmd.initializer_list ();

  if (init_list)
    init_list->accept (*this);
}

void
tree_evaluator::visit_decl_elt (tree_decl_elt& elt)
{
  tree_identifier *id = elt.ident ();

  if (id)
    {
      if (elt.is_global ())
        m_call_stack.make_global (id->symbol ());
      else if (elt.is_persistent ())
        m_call_stack.make_persistent (id->symbol ());
      else
        error ("declaration list element not global or persistent");

      octave_lvalue ult = id->lvalue (*this);

      if (ult.is_undefined ())
        {
          tree_expression *expr = elt.expression ();

          octave_value init_val;

          if (expr)
            init_val = expr->evaluate (*this);
          else
            init_val = Matrix ();

          ult.assign (octave_value::op_asn_eq, init_val);
        }
    }
}

template <typename T>
void
tree_evaluator::execute_range_loop (const range<T>& rng, int line,
                                    octave_lvalue& ult,
                                    tree_statement_list *loop_body)
{
  octave_idx_type steps = rng.numel ();

  if (math::isinf (rng.limit ()))
    warning_with_id ("Octave:infinite-loop",
                     "FOR loop limit is infinite, will stop after %"
                     OCTAVE_IDX_TYPE_FORMAT " steps", steps);

  for (octave_idx_type i = 0; i < steps; i++)
    {
      if (m_echo_state)
        m_echo_file_pos = line;

      octave_value val (rng.elem (i));

      ult.assign (octave_value::op_asn_eq, val);

      if (loop_body)
        loop_body->accept (*this);

      if (quit_loop_now ())
        break;
    }
}

void
tree_evaluator::visit_simple_for_command (tree_simple_for_command& cmd)
{
  int line = cmd.line ();
  if (line < 0)
    line = 1;

  if (m_echo_state)
    {
      echo_code (line);
      line++;
    }

  if (m_debug_mode)
    do_breakpoint (cmd.is_active_breakpoint (*this));

  // FIXME: need to handle PARFOR loops here using cmd.in_parallel ()
  // and cmd.maxproc_expr ();

  unwind_protect_var<bool> upv (m_in_loop_command, true);

  tree_expression *expr = cmd.control_expr ();

  octave_value rhs = expr->evaluate (*this);

  if (rhs.is_undefined ())
    return;

  tree_expression *lhs = cmd.left_hand_side ();

  octave_lvalue ult = lhs->lvalue (*this);

  tree_statement_list *loop_body = cmd.body ();

  if (rhs.is_range ())
    {
      // FIXME: is there a better way to dispatch here?

      if (rhs.is_double_type ())
        {
          execute_range_loop (rhs.range_value (), line, ult, loop_body);
          return;
        }

      // For now, disable all but range<double>.

#if 0
      if (rhs.is_int64_type ())
        {
          execute_range_loop (rhs.int64_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_uint64_type ())
        {
          execute_range_loop (rhs.uint64_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_int32_type ())
        {
          execute_range_loop (rhs.int32_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_uint32_type ())
        {
          execute_range_loop (rhs.uint32_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_int16_type ())
        {
          execute_range_loop (rhs.int16_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_uint16_type ())
        {
          execute_range_loop (rhs.uint16_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_int8_type ())
        {
          execute_range_loop (rhs.int8_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_uint8_type ())
        {
          execute_range_loop (rhs.uint8_range_value (), line, ult, loop_body);
          return;
        }

      if (rhs.is_single_type ())
        {
          execute_range_loop (rhs.float_range_value (), line, ult, loop_body);
          return;
        }
#endif
    }

  if (rhs.is_scalar_type ())
    {
      if (m_echo_state)
        m_echo_file_pos = line;

      ult.assign (octave_value::op_asn_eq, rhs);

      if (loop_body)
        loop_body->accept (*this);

      // Maybe decrement break and continue states.
      quit_loop_now ();

      return;
    }

  // Also handle any range types not explicitly handled above, though
  // not as efficiently as the specialized code above.

  if (rhs.is_range () || rhs.is_matrix_type () || rhs.iscell ()
      || rhs.is_string () || rhs.isstruct ())
    {
      // A matrix or cell is reshaped to 2 dimensions and iterated by
      // columns.

      dim_vector dv = rhs.dims ().redim (2);

      octave_idx_type nrows = dv(0);
      octave_idx_type steps = dv(1);

      octave_value arg = rhs;
      if (rhs.ndims () > 2)
        arg = arg.reshape (dv);

      if (nrows > 0 && steps > 0)
        {
          octave_value_list idx;
          octave_idx_type iidx;

          // for row vectors, use single index to speed things up.
          if (nrows == 1)
            {
              idx.resize (1);
              iidx = 0;
            }
          else
            {
              idx.resize (2);
              idx(0) = octave_value::magic_colon_t;
              iidx = 1;
            }

          for (octave_idx_type i = 1; i <= steps; i++)
            {
              if (m_echo_state)
                m_echo_file_pos = line;

              // index_op expects one-based indices.
              idx(iidx) = i;
              octave_value val = arg.index_op (idx);

              ult.assign (octave_value::op_asn_eq, val);

              if (loop_body)
                loop_body->accept (*this);

              if (quit_loop_now ())
                break;
            }
        }
      else
        {
          // Handle empty cases, while still assigning to loop var.
          ult.assign (octave_value::op_asn_eq, arg);
        }

      return;
    }

  error ("invalid type in for loop expression near line %d, column %d",
         cmd.line (), cmd.column ());
}

void
tree_evaluator::visit_complex_for_command (tree_complex_for_command& cmd)
{
  int line = cmd.line ();
  if (line < 0)
    line = 1;

  if (m_echo_state)
    {
      echo_code (line);
      line++;
    }

  if (m_debug_mode)
    do_breakpoint (cmd.is_active_breakpoint (*this));

  unwind_protect_var<bool> upv (m_in_loop_command, true);

  tree_expression *expr = cmd.control_expr ();

  octave_value rhs = expr->evaluate (*this);

  if (rhs.is_undefined ())
    return;

  if (! rhs.isstruct ())
    error ("in statement 'for [X, Y] = VAL', VAL must be a structure");

  // Cycle through structure elements.  First element of id_list
  // is set to value and the second is set to the name of the
  // structure element.

  tree_argument_list *lhs = cmd.left_hand_side ();

  auto p = lhs->begin ();

  tree_expression *elt = *p++;

  octave_lvalue val_ref = elt->lvalue (*this);

  elt = *p;

  octave_lvalue key_ref = elt->lvalue (*this);

  const octave_map tmp_val = rhs.map_value ();

  tree_statement_list *loop_body = cmd.body ();

  string_vector keys = tmp_val.keys ();

  octave_idx_type nel = keys.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      if (m_echo_state)
        m_echo_file_pos = line;

      std::string key = keys[i];

      const Cell val_lst = tmp_val.contents (key);

      octave_idx_type n = val_lst.numel ();

      octave_value val = (n == 1) ? val_lst(0) : octave_value (val_lst);

      val_ref.assign (octave_value::op_asn_eq, val);
      key_ref.assign (octave_value::op_asn_eq, key);

      if (loop_body)
        loop_body->accept (*this);

      if (quit_loop_now ())
        break;
    }
}

void tree_evaluator::visit_spmd_command (tree_spmd_command& cmd)
{
  // For now, we just execute the commands serially.

  tree_statement_list *body = cmd.body ();

  if (body)
    body->accept (*this);
}

octave_value
tree_evaluator::evaluate_anon_fcn_handle (tree_anon_fcn_handle& afh)
{
  // FIXME: should CMD_LIST be limited to a single expression?
  // I think that is what Matlab does.

  symbol_scope new_scope;
  symbol_scope scope = afh.scope ();
  if (scope)
    new_scope = scope.dup ();

  tree_parameter_list *param_list = afh.parameter_list ();
  tree_parameter_list *param_list_dup
    = param_list ? param_list->dup (new_scope) : nullptr;

  tree_parameter_list *ret_list = nullptr;

  tree_statement_list *stmt_list = nullptr;

  symbol_scope parent_scope = get_current_scope ();

  new_scope.set_parent (parent_scope);
  new_scope.set_primary_parent (parent_scope);

  tree_expression *expr = afh.expression ();
  if (expr)
    {
      tree_expression *expr_dup = expr->dup (new_scope);
      tree_statement *stmt = new tree_statement (expr_dup, nullptr);
      stmt_list = new tree_statement_list (stmt);
    }

  tree_anon_scopes anon_fcn_ctx (afh);

  std::set<std::string> free_vars = anon_fcn_ctx.free_variables ();

  stack_frame::local_vars_map local_vars;

  std::shared_ptr<stack_frame> frame
    = m_call_stack.get_current_stack_frame ();

  for (auto& name : free_vars)
    {
      octave_value val = frame->varval (name);

      if (val.is_defined ())
        local_vars[name] = val;
    }

  octave_user_function *af
    = new octave_user_function (new_scope, param_list_dup, ret_list,
                                stmt_list);

  octave_function *curr_fcn = m_call_stack.current_function ();

  bool is_nested = false;

  if (curr_fcn)
    {
      // FIXME: maybe it would be better to just stash curr_fcn
      // instead of individual bits of info about it?

      // An anonymous function defined inside another nested function
      // or parent of a nested function also behaves like a nested
      // function.

      if (curr_fcn->is_parent_function () || curr_fcn->is_nested_function ())
        {
          is_nested = true;
          af->mark_as_nested_function ();
          new_scope.set_nesting_depth (parent_scope.nesting_depth () + 1);
        }

      af->stash_dir_name (curr_fcn->dir_name ());

      new_scope.cache_fcn_file_name (curr_fcn->fcn_file_name ());
      new_scope.cache_dir_name (curr_fcn->dir_name ());

      // The following is needed so that class method dispatch works
      // properly for anonymous functions that wrap class methods.

      if (curr_fcn->is_class_method () || curr_fcn->is_class_constructor ())
        af->stash_dispatch_class (curr_fcn->dispatch_class ());

      af->stash_fcn_file_name (curr_fcn->fcn_file_name ());
    }

  af->mark_as_anonymous_function ();

  octave_value ov_fcn (af);

  return (is_nested
          ? octave_value (new octave_fcn_handle (ov_fcn, local_vars, frame))
          : octave_value (new octave_fcn_handle (ov_fcn, local_vars)));
}

octave_value_list
tree_evaluator::execute_builtin_function (octave_builtin& builtin_function,
    int nargout,
    const octave_value_list& args)
{
  octave_value_list retval;

  if (args.has_magic_colon ())
    error ("invalid use of colon in function argument list");

  profiler::enter<octave_builtin> block (m_profiler, builtin_function);

  octave_builtin::fcn fcn = builtin_function.function ();

  if (fcn)
    retval = (*fcn) (args, nargout);
  else
    {
      octave_builtin::meth meth = builtin_function.method ();

      retval = (*meth) (m_interpreter, args, nargout);
    }

  // Do not allow null values to be returned from functions.
  // FIXME: perhaps true builtins should be allowed?

  retval.make_storable_values ();

  // Fix the case of a single undefined value.
  // This happens when a compiled function uses
  //
  //   octave_value retval;
  //
  // instead of
  //
  //   octave_value_list retval;
  //
  // the idiom is very common, so we solve that here.

  if (retval.length () == 1 && retval.xelem (0).is_undefined ())
    retval.clear ();

  return retval;
}

octave_value_list
tree_evaluator::execute_mex_function (octave_mex_function& mex_function,
                                      int nargout,
                                      const octave_value_list& args)
{
  octave_value_list retval;

  if (args.has_magic_colon ())
    error ("invalid use of colon in function argument list");

  profiler::enter<octave_mex_function> block (m_profiler, mex_function);

  retval = call_mex (mex_function, args, nargout);

  return retval;
}

octave_value_list
tree_evaluator::execute_user_script (octave_user_script& user_script,
                                     int nargout,
                                     const octave_value_list& args)
{
  octave_value_list retval;

  std::string file_name = user_script.fcn_file_name ();

  if (args.length () != 0 || nargout != 0)
    error ("invalid call to script %s", file_name.c_str ());

  tree_statement_list *cmd_list = user_script.body ();

  if (! cmd_list)
    return retval;

  // FIXME: Maybe this check belongs in the places where we push a new
  // stack frame?  Or in the call_stack push method itself?

  if (m_call_stack.size () >= static_cast<std::size_t> (m_max_recursion_depth))
    error ("max_recursion_depth exceeded");

  unwind_protect_var<stmt_list_type> upv (m_statement_context, SC_SCRIPT);

  profiler::enter<octave_user_script> block (m_profiler, user_script);

  if (echo ())
    push_echo_state (tree_evaluator::ECHO_SCRIPTS, file_name);

  // FIXME: Should we be using tree_evaluator::eval here?

  cmd_list->accept (*this);

  if (m_returning)
    m_returning = 0;

  if (m_breaking)
    m_breaking--;

  return retval;
}

void
tree_evaluator::visit_octave_user_script (octave_user_script&)
{
  // ??
  panic_impossible ();
}

octave_value_list
tree_evaluator::execute_user_function (octave_user_function& user_function,
                                       int nargout,
                                       const octave_value_list& xargs)
{
  octave_value_list retval;

  // If this function is a classdef constructor, extract the first input
  // argument, which must be the partially constructed object instance.

  octave_value_list args (xargs);
  octave_value_list ret_args;

  int nargin = args.length ();

  if (user_function.is_classdef_constructor ())
    {
      if (nargin > 0)
        {
          ret_args = args.slice (0, 1, true);
          --nargin;
          args = args.slice (1, nargin, true);
        }
      else
        panic_impossible ();
    }

  // FIXME: this probably shouldn't be a double-precision matrix.
  Matrix ignored_outputs = ignored_fcn_outputs ();

  tree_parameter_list *param_list = user_function.parameter_list ();

  bool takes_varargs = false;
  int max_inputs = 0;

  if (param_list)
    {
      takes_varargs = param_list->takes_varargs ();
      max_inputs = param_list->length ();
    }

  if (! takes_varargs && nargin > max_inputs)
    {
      std::string name = user_function.name ();

      if (name.empty ())
        name = "@<anonymous>";

      error_with_id ("Octave:invalid-fun-call",
                     "%s: function called with too many inputs",
                     name.c_str ());
    }

  define_parameter_list_from_arg_vector (param_list, args);

  tree_parameter_list *ret_list = user_function.return_list ();

  if (ret_list && ! ret_list->takes_varargs ())
    {
      int max_outputs = ret_list->length ();

      if (nargout > max_outputs)
        {
          std::string name = user_function.name ();

          error_with_id ("Octave:invalid-fun-call",
                         "%s: function called with too many outputs",
                         name.c_str ());
        }
    }

  bind_auto_fcn_vars (xargs.name_tags (), ignored_outputs, nargin,
                      nargout, user_function.takes_varargs (),
                      user_function.all_va_args (args));

  // For classdef constructor, pre-populate the output arguments
  // with the pre-initialized object instance, extracted above.

  if (user_function.is_classdef_constructor ())
    {
      if (! ret_list)
        error ("%s: invalid classdef constructor, no output argument defined",
               user_function.dispatch_class ().c_str ());

      define_parameter_list_from_arg_vector (ret_list, ret_args);
    }

  // FIXME: Maybe this check belongs in the places where we push a
  // new stack frame?  Or in the call_stack push method itself?

  if (m_call_stack.size () >= static_cast<std::size_t> (m_max_recursion_depth))
    error ("max_recursion_depth exceeded");

  unwind_action act2 ([&user_function] ()
  {
    user_function.restore_warning_states ();
  });

  // Evaluate the commands that make up the function.

  unwind_protect_var<stmt_list_type> upv (m_statement_context, SC_FUNCTION);

  tree_statement_list *cmd_list = user_function.body ();

  if (cmd_list)
    {
      profiler::enter<octave_user_function>
      block (m_profiler, user_function);

      if (echo ())
        push_echo_state (tree_evaluator::ECHO_FUNCTIONS,
                         user_function.fcn_file_name ());

      if (user_function.is_special_expr ())
        {
          panic_if (cmd_list->length () != 1);

          tree_statement *stmt = cmd_list->front ();

          tree_expression *expr = stmt->expression ();

          if (expr)
            {
              m_call_stack.set_location (stmt->line (), stmt->column ());

              retval = expr->evaluate_n (*this, nargout);
            }
        }
      else
        cmd_list->accept (*this);

      if (m_returning)
        m_returning = 0;

      if (m_breaking)
        m_breaking--;
    }

  // Copy return values out.

  if (ret_list && ! user_function.is_special_expr ())
    {
      Cell varargout;

      if (ret_list->takes_varargs ())
        {
          octave_value varargout_varval = varval ("varargout");

          if (varargout_varval.is_defined ())
            varargout = varargout_varval.xcell_value ("varargout must be a cell array object");
        }

      retval = convert_return_list_to_const_vector (ret_list, nargout,
               ignored_outputs,
               varargout);
    }

  return retval;
}

void
tree_evaluator::visit_octave_user_function (octave_user_function&)
{
  // ??
  panic_impossible ();
}

void
tree_evaluator::visit_octave_user_function_header (octave_user_function&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_octave_user_function_trailer (octave_user_function&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_function_def (tree_function_def& cmd)
{
  octave_value fcn = cmd.function ();

  octave_function *f = fcn.function_value ();

  if (f)
    {
      std::string nm = f->name ();

      symbol_table& symtab = m_interpreter.get_symbol_table ();

      symtab.install_cmdline_function (nm, fcn);

      // Make sure that any variable with the same name as the new
      // function is cleared.

      assign (nm);
    }
}

void
tree_evaluator::visit_identifier (tree_identifier&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_if_clause (tree_if_clause&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_if_command (tree_if_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  // FIXME: tree_if_command_list is not derived from tree, so should it
  // really have an accept method?

  tree_if_command_list *lst = cmd.cmd_list ();

  if (lst)
    lst->accept (*this);
}

void
tree_evaluator::visit_if_command_list (tree_if_command_list& lst)
{
  for (tree_if_clause *tic : lst)
    {
      tree_expression *expr = tic->condition ();

      if (! (in_debug_repl ()
             && m_call_stack.current_frame () == m_debug_frame))
        m_call_stack.set_location (tic->line (), tic->column ());

      if (m_debug_mode && ! tic->is_else_clause ())
        do_breakpoint (tic->is_active_breakpoint (*this));

      if (tic->is_else_clause () || is_logically_true (expr, "if"))
        {
          tree_statement_list *stmt_lst = tic->commands ();

          if (stmt_lst)
            stmt_lst->accept (*this);

          break;
        }
    }
}

void
tree_evaluator::visit_index_expression (tree_index_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_matrix (tree_matrix&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_cell (tree_cell&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_multi_assignment (tree_multi_assignment&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_no_op_command (tree_no_op_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  if (m_debug_mode && cmd.is_end_of_fcn_or_script ())
    do_breakpoint (cmd.is_active_breakpoint (*this), true);
}

void
tree_evaluator::visit_constant (tree_constant&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_fcn_handle (tree_fcn_handle&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_parameter_list (tree_parameter_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_postfix_expression (tree_postfix_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_prefix_expression (tree_prefix_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_return_command (tree_return_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  if (m_debug_mode)
    do_breakpoint (cmd.is_active_breakpoint (*this));

  // Act like dbcont.

  if (in_debug_repl () && m_call_stack.current_frame () == m_debug_frame)
    dbcont ();
  else if (m_statement_context == SC_FUNCTION
           || m_statement_context == SC_SCRIPT
           || m_in_loop_command)
    m_returning = 1;
}

void
tree_evaluator::visit_simple_assignment (tree_simple_assignment&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();
  tree_expression *expr = stmt.expression ();

  if (cmd || expr)
    {
      if (! (in_debug_repl ()
             && m_call_stack.current_frame () == m_debug_frame))
        m_call_stack.set_location (stmt.line (), stmt.column ());

      try
        {
          if (cmd)
            {
              unwind_protect_var<const std::list<octave_lvalue> *>
              upv (m_lvalue_list, nullptr);

              cmd->accept (*this);
            }
          else
            {
              if (m_echo_state)
                {
                  int line = stmt.line ();
                  if (line < 0)
                    line = 1;
                  echo_code (line);
                  m_echo_file_pos = line + 1;
                }

              if (m_debug_mode)
                do_breakpoint (expr->is_active_breakpoint (*this));

              // FIXME: maybe all of this should be packaged in
              // one virtual function that returns a flag saying whether
              // or not the expression will take care of binding ans and
              // printing the result.

              // FIXME: it seems that we should just have to
              // evaluate the expression and that should take care of
              // everything, binding ans as necessary?

              octave_value tmp_result = expr->evaluate (*this, 0);

              if (tmp_result.is_defined ())
                {
                  bool do_bind_ans = false;

                  if (expr->is_identifier ())
                    do_bind_ans = ! is_variable (expr);
                  else
                    do_bind_ans = ! expr->is_assignment_expression ();

                  if (do_bind_ans)
                    bind_ans (tmp_result, expr->print_result ()
                              && statement_printing_enabled ());
                }
            }
        }
      catch (const std::bad_alloc&)
        {
          // FIXME: We want to use error_with_id here so that give users
          // control over this error message but error_with_id will
          // require some memory allocations.  Is there anything we can
          // do to make those more likely to succeed?

          error_with_id ("Octave:bad-alloc",
                         "out of memory or dimension too large for Octave's index type");
        }
      catch (const interrupt_exception&)
        {
          // If we are debugging, then continue with next statement.
          // Otherwise, jump out of here.

          if (m_debug_mode)
            m_interpreter.recover_from_exception ();
          else
            throw;
        }
      catch (const execution_exception& ee)
        {
          error_system& es = m_interpreter.get_error_system ();

          if ((m_interpreter.interactive ()
               || application::forced_interactive ())
              && ((es.debug_on_error ()
                   && m_bp_table.debug_on_err (es.last_error_id ()))
                  || (es.debug_on_caught ()
                      && m_bp_table.debug_on_caught (es.last_error_id ())))
              && in_user_code ())
            {
              es.save_exception (ee);
              es.display_exception (ee);

              enter_debugger ();

              // It doesn't make sense to continue execution after an
              // error occurs so force the debugger to quit all debug
              // levels and return the the top prompt.

              throw quit_debug_exception (true);
            }
          else
            throw;
        }
    }
}

void
tree_evaluator::visit_statement_list (tree_statement_list& lst)
{
  // FIXME: commented out along with else clause below.
  // static octave_value_list empty_list;

  auto p = lst.begin ();

  if (p != lst.end ())
    {
      while (true)
        {
          tree_statement *elt = *p++;

          if (! elt)
            error ("invalid statement found in statement list!");

          octave_quit ();

          elt->accept (*this);

          if (m_breaking || m_continuing)
            break;

          if (m_returning)
            break;

          if (p == lst.end ())
            break;
          else
            {
              // Clear previous values before next statement is
              // evaluated so that we aren't holding an extra
              // reference to a value that may be used next.  For
              // example, in code like this:
              //
              //   X = rand (N);  # refcount for X should be 1
              //                  # after this statement
              //
              //   X(idx) = val;  # no extra copy of X should be
              //                  # needed, but we will be faked
              //                  # out if retval is not cleared
              //                  # between statements here

              //              result_values = empty_list;
            }
        }
    }
}

void
tree_evaluator::visit_switch_case (tree_switch_case&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_switch_case_list (tree_switch_case_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_switch_command (tree_switch_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  if (m_debug_mode)
    do_breakpoint (cmd.is_active_breakpoint (*this));

  tree_expression *expr = cmd.switch_value ();

  if (! expr)
    error ("missing value in switch command near line %d, column %d",
           cmd.line (), cmd.column ());

  octave_value val = expr->evaluate (*this);

  tree_switch_case_list *lst = cmd.case_list ();

  if (lst)
    {
      for (tree_switch_case *t : *lst)
        {
          if (t->is_default_case () || switch_case_label_matches (t, val))
            {
              tree_statement_list *stmt_lst = t->commands ();

              if (stmt_lst)
                stmt_lst->accept (*this);

              break;
            }
        }
    }
}

void
tree_evaluator::visit_try_catch_command (tree_try_catch_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  bool execution_error = false;
  octave_scalar_map err_map;

  tree_statement_list *try_code = cmd.body ();

  if (try_code)
    {
      // unwind frame before catch block

      unwind_protect frame;

      interpreter_try (frame);

      // The catch code is *not* added to unwind_protect stack; it
      // doesn't need to be run on interrupts.

      try
        {
          try_code->accept (*this);
        }
      catch (const execution_exception& ee)
        {
          execution_error = true;

          error_system& es = m_interpreter.get_error_system ();

          es.save_exception (ee);

          err_map.assign ("message", es.last_error_message ());
          err_map.assign ("identifier", es.last_error_id ());
          err_map.assign ("stack", es.last_error_stack ());

          m_interpreter.recover_from_exception ();
        }

      // Actions attached to unwind_protect frame will run here, prior
      // to executing the catch block.
    }

  if (execution_error)
    {
      tree_statement_list *catch_code = cmd.cleanup ();

      if (catch_code)
        {
          tree_identifier *expr_id = cmd.identifier ();

          if (expr_id)
            {
              octave_lvalue ult = expr_id->lvalue (*this);

              ult.assign (octave_value::op_asn_eq, err_map);
            }

          // perform actual "catch" block
          catch_code->accept (*this);
        }
    }
}

void
tree_evaluator::do_unwind_protect_cleanup_code (tree_statement_list *list)
{
  unwind_protect frame;

  frame.protect_var (octave_interrupt_state);
  octave_interrupt_state = 0;

  // We want to preserve the last location info for possible
  // backtracking.

  frame.add (&call_stack::set_line, &m_call_stack,
             m_call_stack.current_line ());

  frame.add (&call_stack::set_column, &m_call_stack,
             m_call_stack.current_column ());

  // Similarly, if we have seen a return or break statement, allow all
  // the cleanup code to run before returning or handling the break.
  // We don't have to worry about continue statements because they can
  // only occur in loops.

  frame.protect_var (m_returning);
  m_returning = 0;

  frame.protect_var (m_breaking);
  m_breaking = 0;

  try
    {
      if (list)
        list->accept (*this);
    }
  catch (const execution_exception& ee)
    {
      error_system& es = m_interpreter.get_error_system ();

      es.save_exception (ee);
      m_interpreter.recover_from_exception ();

      if (m_breaking || m_returning)
        frame.discard (2);
      else
        frame.run (2);

      frame.discard (2);

      throw;
    }

  // The unwind_protects are popped off the stack in the reverse of
  // the order they are pushed on.

  // FIXME: these statements say that if we see a break or
  // return statement in the cleanup block, that we want to use the
  // new value of the breaking or returning flag instead of restoring
  // the previous value.  Is that the right thing to do?  I think so.
  // Consider the case of
  //
  //   function foo ()
  //     unwind_protect
  //       fprintf (stderr, "1: this should always be executed\n");
  //       break;
  //       fprintf (stderr, "1: this should never be executed\n");
  //     unwind_protect_cleanup
  //       fprintf (stderr, "2: this should always be executed\n");
  //       return;
  //       fprintf (stderr, "2: this should never be executed\n");
  //     end_unwind_protect
  //   endfunction
  //
  // If we reset the value of the breaking flag, both the returning
  // flag and the breaking flag will be set, and we shouldn't have
  // both.  So, use the most recent one.  If there is no return or
  // break in the cleanup block, the values should be reset to
  // whatever they were when the cleanup block was entered.

  if (m_breaking || m_returning)
    frame.discard (2);
  else
    frame.run (2);
}

void
tree_evaluator::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  if (m_echo_state)
    {
      int line = cmd.line ();
      if (line < 0)
        line = 1;
      echo_code (line);
      m_echo_file_pos = line + 1;
    }

  tree_statement_list *cleanup_code = cmd.cleanup ();

  tree_statement_list *unwind_protect_code = cmd.body ();

  if (unwind_protect_code)
    {
      try
        {
          unwind_protect_code->accept (*this);
        }
      catch (const execution_exception& ee)
        {
          error_system& es = m_interpreter.get_error_system ();

          // FIXME: Maybe we should be able to temporarily set the
          // interpreter's exception handling state to something "safe"
          // while the cleanup block runs instead of just resetting it
          // here?
          es.save_exception (ee);
          m_interpreter.recover_from_exception ();

          // Run the cleanup code on exceptions, so that it is run even
          // in case of interrupt or out-of-memory.
          do_unwind_protect_cleanup_code (cleanup_code);

          // If an error occurs inside the cleanup code, a new
          // exception will be thrown instead of the original.
          throw;
        }
      catch (const interrupt_exception&)
        {
          // The comments above apply here as well.
          m_interpreter.recover_from_exception ();
          do_unwind_protect_cleanup_code (cleanup_code);
          throw;
        }

      // Also execute the unwind_protect_cleanump code if the
      // unwind_protect block runs without error.
      do_unwind_protect_cleanup_code (cleanup_code);
    }
}

void
tree_evaluator::visit_while_command (tree_while_command& cmd)
{
  int line = cmd.line ();
  if (line < 0)
    line = 1;

  if (m_echo_state)
    {
      echo_code (line);
      line++;
    }

  unwind_protect_var<bool> upv (m_in_loop_command, true);

  tree_expression *expr = cmd.condition ();

  if (! expr)
    panic_impossible ();

  for (;;)
    {
      if (m_echo_state)
        m_echo_file_pos = line;

      if (m_debug_mode)
        do_breakpoint (cmd.is_active_breakpoint (*this));

      if (is_logically_true (expr, "while"))
        {
          tree_statement_list *loop_body = cmd.body ();

          if (loop_body)
            loop_body->accept (*this);

          if (quit_loop_now ())
            break;
        }
      else
        break;
    }
}

void
tree_evaluator::visit_do_until_command (tree_do_until_command& cmd)
{
  int line = cmd.line ();
  if (line < 0)
    line = 1;

  if (m_echo_state)
    {
      echo_code (line);
      line++;
    }

  unwind_protect_var<bool> upv (m_in_loop_command, true);

  tree_expression *expr = cmd.condition ();

  if (! expr)
    panic_impossible ();

  for (;;)
    {
      if (m_echo_state)
        m_echo_file_pos = line;

      tree_statement_list *loop_body = cmd.body ();

      if (loop_body)
        loop_body->accept (*this);

      if (quit_loop_now ())
        break;

      if (m_debug_mode)
        do_breakpoint (cmd.is_active_breakpoint (*this));

      if (is_logically_true (expr, "do-until"))
        break;
    }
}

void
tree_evaluator::visit_superclass_ref (tree_superclass_ref&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_metaclass_query (tree_metaclass_query&)
{
  panic_impossible ();
}

void tree_evaluator::bind_ans (const octave_value& val, bool print)
{
  static std::string ans = "ans";

  if (val.is_defined ())
    {
      if (val.is_cs_list ())
        {
          octave_value_list lst = val.list_value ();

          for (octave_idx_type i = 0; i < lst.length (); i++)
            bind_ans (lst(i), print);
        }
      else
        {
          // FIXME: Maybe assign could also return the assigned value,
          // just for convenience?

          assign (ans, val);

          if (print)
            {
              // Use varval instead of displaying VAL directly so that
              // we get the right type and value for things like
              // magic_int values that may mutate when stored.

              octave_value_list args = ovl (varval (ans));
              args.stash_name_tags (string_vector (ans));
              feval ("display", args);
            }
        }
    }
}

void
tree_evaluator::do_breakpoint (tree_statement& stmt)
{
  do_breakpoint (stmt.is_active_breakpoint (*this),
                 stmt.is_end_of_fcn_or_script ());
}

void
tree_evaluator::do_breakpoint (bool is_breakpoint,
                               bool is_end_of_fcn_or_script)
{
  bool break_on_this_statement = false;

  if (is_breakpoint)
    break_on_this_statement = true;
  else if (m_dbstep_flag > 0)
    {
      if (m_call_stack.current_frame () == m_debug_frame)
        {
          if (m_dbstep_flag == 1 || is_end_of_fcn_or_script)
            {
              // We get here if we are doing a "dbstep" or a "dbstep N" and
              // the count has reached 1 so that we must stop and return to
              // debug prompt.  Alternatively, "dbstep N" has been used but
              // the end of the frame has been reached so we stop at the last
              // line and return to prompt.

              break_on_this_statement = true;
            }
          else
            {
              // Executing "dbstep N".  Decrease N by one and continue.

              m_dbstep_flag--;
            }

        }
      else if (m_dbstep_flag == 1
               && m_call_stack.current_frame () < m_debug_frame)
        {
          // We stepped out from the end of a function.

          m_debug_frame = m_call_stack.current_frame ();

          break_on_this_statement = true;
        }
    }
  else if (m_dbstep_flag == -1)
    {
      // We get here if we are doing a "dbstep in".

      break_on_this_statement = true;

      m_debug_frame = m_call_stack.current_frame ();
    }
  else if (m_dbstep_flag == -2)
    {
      // We get here if we are doing a "dbstep out".  Check for end of
      // function and whether the current frame is the same as the
      // cached value because we want to step out from the frame where
      // "dbstep out" was evaluated, not from any functions called from
      // that frame.

      if (is_end_of_fcn_or_script
          && m_call_stack.current_frame () == m_debug_frame)
        m_dbstep_flag = -1;
    }

  if (! break_on_this_statement)
    break_on_this_statement = m_break_on_next_stmt;

  m_break_on_next_stmt = false;

  if (break_on_this_statement)
    {
      m_dbstep_flag = 0;

      enter_debugger ();
    }
}

bool
tree_evaluator::is_logically_true (tree_expression *expr,
                                   const char *warn_for)
{
  bool expr_value = false;

  m_call_stack.set_location (expr->line (), expr->column ());

  octave_value t1 = expr->evaluate (*this);

  if (t1.is_defined ())
    return t1.is_true ();
  else
    error ("%s: undefined value used in conditional expression", warn_for);

  return expr_value;
}

octave_value
tree_evaluator::max_recursion_depth (const octave_value_list& args,
                                     int nargout)
{
  return set_internal_variable (m_max_recursion_depth, args, nargout,
                                "max_recursion_depth", 0);
}

symbol_info_list
tree_evaluator::glob_symbol_info (const std::string& pattern) const
{
  return m_call_stack.glob_symbol_info (pattern);
}

symbol_info_list
tree_evaluator::regexp_symbol_info (const std::string& pattern) const
{
  return m_call_stack.regexp_symbol_info (pattern);
}

symbol_info_list
tree_evaluator::get_symbol_info (void)
{
  return m_call_stack.get_symbol_info ();
}

symbol_info_list
tree_evaluator::top_scope_symbol_info (void) const
{
  return m_call_stack.top_scope_symbol_info ();
}

octave_map tree_evaluator::get_autoload_map (void) const
{
  Cell fcn_names (dim_vector (m_autoload_map.size (), 1));
  Cell file_names (dim_vector (m_autoload_map.size (), 1));

  octave_idx_type i = 0;
  for (const auto& fcn_fname : m_autoload_map)
    {
      fcn_names(i) = fcn_fname.first;
      file_names(i) = fcn_fname.second;

      i++;
    }

  octave_map m;

  m.assign ("function", fcn_names);
  m.assign ("file", file_names);

  return m;
}

std::string tree_evaluator::lookup_autoload (const std::string& nm) const
{
  std::string retval;

  auto p = m_autoload_map.find (nm);

  if (p != m_autoload_map.end ())
    {
      load_path& lp = m_interpreter.get_load_path ();

      retval = lp.find_file (p->second);
    }

  return retval;
}

std::list<std::string> tree_evaluator::autoloaded_functions (void) const
{
  std::list<std::string> names;

  for (const auto& fcn_fname : m_autoload_map)
    names.push_back (fcn_fname.first);

  return names;
}

std::list<std::string>
tree_evaluator::reverse_lookup_autoload (const std::string& nm) const
{
  std::list<std::string> names;

  for (const auto& fcn_fname : m_autoload_map)
    if (nm == fcn_fname.second)
      names.push_back (fcn_fname.first);

  return names;
}

void tree_evaluator::add_autoload (const std::string& fcn,
                                   const std::string& nm)
{
  std::string file_name = check_autoload_file (nm);

  m_autoload_map[fcn] = file_name;
}

void tree_evaluator::remove_autoload (const std::string& fcn,
                                      const std::string& nm)
{
  check_autoload_file (nm);

  // Remove function from symbol table and autoload map.
  symbol_table& symtab = m_interpreter.get_symbol_table ();

  symtab.clear_dld_function (fcn);

  m_autoload_map.erase (fcn);
}

octave_value
tree_evaluator::whos_line_format (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_whos_line_format, args, nargout,
                                "whos_line_format");
}

octave_value
tree_evaluator::silent_functions (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_silent_functions, args, nargout,
                                "silent_functions");
}

octave_value
tree_evaluator::string_fill_char (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_string_fill_char, args, nargout,
                                "string_fill_char");
}

// Final step of processing an indexing error.  Add the name of the
// variable being indexed, if any, then issue an error.  (Will this also
// be needed by pt-lvalue, which calls subsref?)

void tree_evaluator::final_index_error (index_exception& ie,
                                        const tree_expression *expr)
{
  std::string extra_message;

  if (is_variable (expr))
    {
      std::string var = expr->name ();

      ie.set_var (var);

      symbol_table& symtab = m_interpreter.get_symbol_table ();

      octave_value fcn = symtab.find_function (var);

      if (fcn.is_function ())
        {
          octave_function *fp = fcn.function_value ();

          if (fp && fp->name () == var)
            extra_message
              = " (note: variable '" + var + "' shadows function)";
        }
    }

  std::string msg = ie.message () + extra_message;

  error_with_id (ie.err_id (), "%s", msg.c_str ());
}

octave_value
tree_evaluator::do_who (int argc, const string_vector& argv,
                        bool return_list, bool verbose)
{
  return m_call_stack.do_who (argc, argv, return_list, verbose);
}

octave_value_list
tree_evaluator::make_value_list (tree_argument_list *args,
                                 const string_vector& arg_nm)
{
  octave_value_list retval;

  if (args)
    {
      unwind_protect_var<const std::list<octave_lvalue> *>
      upv (m_lvalue_list, nullptr);

      int len = args->length ();

      unwind_protect_var<int> upv2 (m_index_position);
      unwind_protect_var<int> upv3 (m_num_indices);

      m_num_indices = len;

      std::list<octave_value> arg_vals;

      int k = 0;

      for (auto elt : *args)
        {
          // FIXME: is it possible for elt to be invalid?

          if (! elt)
            break;

          m_index_position = k++;

          octave_value tmp = elt->evaluate (*this);

          if (tmp.is_cs_list ())
            {
              octave_value_list tmp_ovl = tmp.list_value ();

              for (octave_idx_type i = 0; i < tmp_ovl.length (); i++)
                arg_vals.push_back (tmp_ovl(i));
            }
          else if (tmp.is_defined ())
            arg_vals.push_back (tmp);
        }

      retval = octave_value_list (arg_vals);
    }

  octave_idx_type n = retval.length ();

  if (n > 0)
    retval.stash_name_tags (arg_nm);

  return retval;
}

std::list<octave_lvalue>
tree_evaluator::make_lvalue_list (tree_argument_list *lhs)
{
  std::list<octave_lvalue> retval;

  for (tree_expression *elt : *lhs)
    retval.push_back (elt->lvalue (*this));

  return retval;
}

void
tree_evaluator::push_echo_state (int type, const std::string& file_name,
                                 int pos)
{
  unwind_protect *frame = m_call_stack.curr_fcn_unwind_protect_frame ();

  if (frame)
    {
      push_echo_state_cleanup (*frame);

      set_echo_state (type, file_name, pos);
    }
}

void
tree_evaluator::set_echo_state (int type, const std::string& file_name,
                                int pos)
{
  m_echo_state = echo_this_file (file_name, type);
  m_echo_file_name = file_name;
  m_echo_file_pos = pos;
}

void
tree_evaluator::uwp_set_echo_state (bool state, const std::string& file_name,
                                    int pos)
{
  m_echo_state = state;
  m_echo_file_name = file_name;
  m_echo_file_pos = pos;
}

void
tree_evaluator::maybe_set_echo_state (void)
{
  octave_function *caller = caller_function ();

  if (caller && caller->is_user_code ())
    {
      octave_user_code *fcn = dynamic_cast<octave_user_code *> (caller);

      int type = fcn->is_user_function () ? ECHO_FUNCTIONS : ECHO_SCRIPTS;

      std::string file_name = fcn->fcn_file_name ();

      // We want the line where "echo" was called, not the line number
      // stored in the stack frame that was created for the echo
      // function (that will always be -1).

      int pos = m_call_stack.current_user_code_line ();

      if (pos < 0)
        pos = 1;

      set_echo_state (type, file_name, pos);
    }
}

void
tree_evaluator::push_echo_state_cleanup (unwind_protect& frame)
{
  frame.add (&tree_evaluator::uwp_set_echo_state, this,
             m_echo_state, m_echo_file_name, m_echo_file_pos);
}

bool tree_evaluator::maybe_push_echo_state_cleanup (void)
{
  // This function is expected to be called from ECHO, which would be
  // the top of the call stack.  If the caller of ECHO is a
  // user-defined function or script, then set up unwind-protect
  // elements to restore echo state.

  unwind_protect *frame = m_call_stack.curr_fcn_unwind_protect_frame ();

  if (frame)
    {
      push_echo_state_cleanup (*frame);
      return true;
    }

  return false;
}


octave_value
tree_evaluator::echo (const octave_value_list& args, int)
{
  bool cleanup_pushed = maybe_push_echo_state_cleanup ();

  string_vector argv = args.make_argv ();

  switch (args.length ())
    {
    case 0:
      if ((m_echo & ECHO_SCRIPTS) || (m_echo & ECHO_FUNCTIONS))
        {
          m_echo = ECHO_OFF;
          m_echo_files.clear ();
        }
      else
        m_echo = ECHO_SCRIPTS;
      break;

    case 1:
      {
        std::string arg0 = argv[0];

        if (arg0 == "on")
          m_echo = ECHO_SCRIPTS;
        else if (arg0 == "off")
          m_echo = ECHO_OFF;
        else
          {
            std::string file = fcn_file_in_path (arg0);
            file = sys::env::make_absolute (file);

            if (file.empty ())
              error ("echo: no such file %s", arg0.c_str ());

            if (m_echo & ECHO_ALL)
              {
                // Echo is enabled for all functions, so turn it off
                // for this one.

                m_echo_files[file] = false;
              }
            else
              {
                // Echo may be enabled for specific functions.

                auto p = m_echo_files.find (file);

                if (p == m_echo_files.end ())
                  {
                    // Not this one, so enable it.

                    m_echo |= ECHO_FUNCTIONS;
                    m_echo_files[file] = true;
                  }
                else
                  {
                    // This one is already in the list.  Flip the
                    // status for it.

                    p->second = ! p->second;
                  }
              }
          }
      }
      break;

    case 2:
      {
        std::string arg0 = argv[0];
        std::string arg1 = argv[1];

        if (arg1 == "on" || arg1 == "off")
          std::swap (arg0, arg1);

        if (arg0 == "on")
          {
            if (arg1 == "all")
              {
                m_echo = (ECHO_SCRIPTS | ECHO_FUNCTIONS | ECHO_ALL);
                m_echo_files.clear ();
              }
            else
              {
                std::string file = fcn_file_in_path (arg1);
                file = sys::env::make_absolute (file);

                if (file.empty ())
                  error ("echo: no such file %s", arg1.c_str ());

                m_echo |= ECHO_FUNCTIONS;
                m_echo_files[file] = true;
              }
          }
        else if (arg0 == "off")
          {
            if (arg1 == "all")
              {
                m_echo = ECHO_OFF;
                m_echo_files.clear ();
              }
            else
              {
                std::string file = fcn_file_in_path (arg1);
                file = sys::env::make_absolute (file);

                if (file.empty ())
                  error ("echo: no such file %s", arg1.c_str ());

                m_echo_files[file] = false;
              }
          }
        else
          print_usage ();
      }
      break;

    default:
      print_usage ();
      break;
    }

  if (cleanup_pushed)
    maybe_set_echo_state ();

  return octave_value ();
}

bool tree_evaluator::in_debug_repl (void) const
{
  return (m_debugger_stack.empty ()
          ? false : m_debugger_stack.top()->in_debug_repl ());
}

void tree_evaluator::dbcont (void)
{
  if (! m_debugger_stack.empty ())
    m_debugger_stack.top()->dbcont ();
}

void tree_evaluator::dbquit (bool all)
{
  if (! m_debugger_stack.empty ())
    m_debugger_stack.top()->dbquit (all);
}

static octave_value end_value (const octave_value& value,
                               octave_idx_type index_position,
                               octave_idx_type num_indices)
{
  dim_vector dv = value.dims ();
  int ndims = dv.ndims ();

  if (num_indices < ndims)
    {
      for (int i = num_indices; i < ndims; i++)
        dv(num_indices-1) *= dv(i);

      if (num_indices == 1)
        {
          ndims = 2;
          dv.resize (ndims);
          dv(1) = 1;
        }
      else
        {
          ndims = num_indices;
          dv.resize (ndims);
        }
    }

  return (index_position < ndims
          ? octave_value (dv(index_position)) : octave_value (1.0));
}

octave_value_list
tree_evaluator::evaluate_end_expression (const octave_value_list& args)
{
  int nargin = args.length ();

  if (nargin != 0 && nargin != 3)
    print_usage ();

  if (nargin == 3)
    {
      octave_idx_type index_position
        = args(1).xidx_type_value ("end: K must be integer value");

      if (index_position < 1)
        error ("end: K must be greater than zero");

      octave_idx_type num_indices
        = args(2).xidx_type_value ("end: N must be integer value");

      if (num_indices < 1)
        error ("end: N must be greater than zero");

      return end_value (args(0), index_position-1, num_indices);
    }

  // If m_indexed_object is undefined, then this use of 'end' is
  // either appearing in a function call argument list or in an
  // attempt to index an undefined symbol.  There seems to be no
  // reasonable way to provide a better error message.  So just fail
  // with an invalid use message.  See bug #58830.

  if (m_indexed_object.is_undefined ())
    error ("invalid use of 'end': may only be used to index existing value");

  octave_value expr_result;

  if (m_index_list.empty ())
    expr_result = m_indexed_object;
  else
    {
      try
        {
          // When evaluating "end" with no arguments, we should have
          // been called from the built-in Fend function that appears
          // in the context of an argument list.  Fend will be
          // evaluated in its own stack frame.  But we need to
          // evaluate the partial expression that the special "end"
          // token applies to in the calling stack frame.

          unwind_action act ([=] (std::size_t frm)
          {
            m_call_stack.restore_frame (frm);
          }, m_call_stack.current_frame ());

          std::size_t n = m_call_stack.find_current_user_frame ();
          m_call_stack.goto_frame (n);

          // End is only valid inside argument lists used for
          // indexing.  The dispatch class is set by the function that
          // evaluates the argument list.

          // Silently ignore extra output values.

          octave_value_list tmp
            = m_indexed_object.subsref (m_index_type, m_index_list, 1);

          expr_result = tmp.length () ? tmp(0) : octave_value ();

          if (expr_result.is_cs_list ())
            err_indexed_cs_list ();
        }
      catch (const index_exception&)
        {
          error ("error evaluating partial expression for END");
        }
    }

  if (expr_result.isobject ())
    {
      // FIXME: is there a better way to lookup and execute a method
      // that handles all the details like setting the dispatch class
      // appropriately?

      std::string dispatch_class = expr_result.class_name ();

      symbol_table& symtab = m_interpreter.get_symbol_table ();

      octave_value meth = symtab.find_method ("end", dispatch_class);

      if (meth.is_defined ())
        return m_interpreter.feval
               (meth, ovl (expr_result, m_index_position+1, m_num_indices), 1);
    }

  return end_value (expr_result, m_index_position, m_num_indices);
}

octave_value
tree_evaluator::PS4 (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_PS4, args, nargout, "PS4");
}

bool tree_evaluator::echo_this_file (const std::string& file, int type) const
{
  if ((type & m_echo) == ECHO_SCRIPTS)
    {
      // Asking about scripts and echo is enabled for them.
      return true;
    }

  if ((type & m_echo) == ECHO_FUNCTIONS)
    {
      // Asking about functions and echo is enabled for functions.
      // Now, which ones?

      auto p = m_echo_files.find (file);

      if (m_echo & ECHO_ALL)
        {
          // Return true ulness echo was turned off for a specific
          // file.

          return (p == m_echo_files.end () || p->second);
        }
      else
        {
          // Return true if echo is specifically enabled for this file.

          return p != m_echo_files.end () && p->second;
        }
    }

  return false;
}

void tree_evaluator::echo_code (int line)
{
  std::string prefix = command_editor::decode_prompt_string (m_PS4);

  octave_function *curr_fcn = m_call_stack.current_function ();

  if (curr_fcn && curr_fcn->is_user_code ())
    {
      octave_user_code *code = dynamic_cast<octave_user_code *> (curr_fcn);

      int num_lines = line - m_echo_file_pos + 1;

      std::deque<std::string> lines
        = code->get_code_lines (m_echo_file_pos, num_lines);

      for (auto& elt : lines)
        octave_stdout << prefix << elt << std::endl;
    }
}

// Decide if it's time to quit a for or while loop.
bool tree_evaluator::quit_loop_now (void)
{
  octave_quit ();

  // Maybe handle 'continue N' someday...

  if (m_continuing)
    m_continuing--;

  bool quit = (m_returning || m_breaking || m_continuing);

  if (m_breaking)
    m_breaking--;

  return quit;
}

void tree_evaluator::bind_auto_fcn_vars (const string_vector& arg_names,
    const Matrix& ignored_outputs,
    int nargin, int nargout,
    bool takes_varargs,
    const octave_value_list& va_args)
{
  set_auto_fcn_var (stack_frame::ARG_NAMES, Cell (arg_names));
  set_auto_fcn_var (stack_frame::IGNORED, ignored_outputs);
  set_auto_fcn_var (stack_frame::NARGIN, nargin);
  set_auto_fcn_var (stack_frame::NARGOUT, nargout);
  set_auto_fcn_var (stack_frame::SAVED_WARNING_STATES, octave_value ());

  if (takes_varargs)
    assign ("varargin", va_args.cell_value ());
}

std::string
tree_evaluator::check_autoload_file (const std::string& nm) const
{
  if (sys::env::absolute_pathname (nm))
    return nm;

  std::string full_name = nm;

  octave_user_code *fcn = m_call_stack.current_user_code ();

  bool found = false;

  if (fcn)
    {
      std::string fname = fcn->fcn_file_name ();

      if (! fname.empty ())
        {
          fname = sys::env::make_absolute (fname);
          fname = fname.substr (0, fname.find_last_of (sys::file_ops::dir_sep_str ()) + 1);

          sys::file_stat fs (fname + nm);

          if (fs.exists ())
            {
              full_name = fname + nm;
              found = true;
            }
        }
    }

  if (! found)
    warning_with_id ("Octave:autoload-relative-file-name",
                     "autoload: '%s' is not an absolute filename",
                     nm.c_str ());

  return full_name;
}

DEFMETHOD (max_recursion_depth, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} max_recursion_depth ()
@deftypefnx {} {@var{old_val} =} max_recursion_depth (@var{new_val})
@deftypefnx {} {@var{old_val} =} max_recursion_depth (@var{new_val}, "local")
Query or set the internal limit on the number of times a function may
be called recursively.

If the limit is exceeded, an error message is printed and control returns to
the top level.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{max_stack_depth}
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.max_recursion_depth (args, nargout);
}

/*
%!test
%! orig_val = max_recursion_depth ();
%! old_val = max_recursion_depth (2*orig_val);
%! assert (orig_val, old_val);
%! assert (max_recursion_depth (), 2*orig_val);
%! max_recursion_depth (orig_val);
%! assert (max_recursion_depth (), orig_val);

%!error max_recursion_depth (1, 2)
*/

DEFMETHOD (whos_line_format, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} whos_line_format ()
@deftypefnx {} {@var{old_val} =} whos_line_format (@var{new_val})
@deftypefnx {} {@var{old_val} =} whos_line_format (@var{new_val}, "local")
Query or set the format string used by the command @code{whos}.

A full format string is:
@c Set example in small font to prevent overfull line

@smallexample
%[modifier]<command>[:width[:left-min[:balance]]];
@end smallexample

The following command sequences are available:

@table @code
@item %a
Prints attributes of variables (c=complex, s=sparse, f=formal parameter,
g=global, p=persistent).

@item %b
Prints number of bytes occupied by variables.

@item %c
Prints class names of variables.

@item %e
Prints elements held by variables.

@item %n
Prints variable names.

@item %s
Prints dimensions of variables.

@item %t
Prints type names of variables.
@end table

Every command may also have an alignment modifier:

@table @code
@item l
Left alignment.

@item r
Right alignment (default).

@item c
Column-aligned (only applicable to command %s).
@end table

The @code{width} parameter is a positive integer specifying the minimum
number of columns used for printing.  No maximum is needed as the field will
auto-expand as required.

The parameters @code{left-min} and @code{balance} are only available when
the column-aligned modifier is used with the command @samp{%s}.
@code{balance} specifies the column number within the field width which
will be aligned between entries.  Numbering starts from 0 which indicates
the leftmost column.  @code{left-min} specifies the minimum field width to
the left of the specified balance column.

The default format is:

@example
"  %la:5; %ln:6; %cs:16:6:1;  %rb:12;  %lc:-1;@backslashchar{}n"
@end example

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{whos}
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.whos_line_format (args, nargout);
}

DEFMETHOD (silent_functions, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} silent_functions ()
@deftypefnx {} {@var{old_val} =} silent_functions (@var{new_val})
@deftypefnx {} {@var{old_val} =} silent_functions (@var{new_val}, "local")
Query or set the internal variable that controls whether internal
output from a function is suppressed.

If this option is disabled, Octave will display the results produced by
evaluating expressions within a function body that are not terminated with
a semicolon.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.silent_functions (args, nargout);
}

/*
%!test
%! orig_val = silent_functions ();
%! old_val = silent_functions (! orig_val);
%! assert (orig_val, old_val);
%! assert (silent_functions (), ! orig_val);
%! silent_functions (orig_val);
%! assert (silent_functions (), orig_val);

%!error silent_functions (1, 2)
*/

DEFMETHOD (string_fill_char, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} string_fill_char ()
@deftypefnx {} {@var{old_val} =} string_fill_char (@var{new_val})
@deftypefnx {} {@var{old_val} =} string_fill_char (@var{new_val}, "local")
Query or set the internal variable used to pad all rows of a character
matrix to the same length.

The value must be a single character and the default is @qcode{" "} (a
single space).  For example:

@example
@group
string_fill_char ("X");
[ "these"; "are"; "strings" ]
      @result{}  "theseXX"
          "areXXXX"
          "strings"
@end group
@end example

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.string_fill_char (args, nargout);
}

/*
## string_fill_char() function call must be outside of %!test block
## due to the way a %!test block is wrapped inside a function
%!shared orig_val, old_val
%! orig_val = string_fill_char ();
%! old_val  = string_fill_char ("X");
%!test
%! assert (orig_val, old_val);
%! assert (string_fill_char (), "X");
%! assert (["these"; "are"; "strings"], ["theseXX"; "areXXXX"; "strings"]);
%! string_fill_char (orig_val);
%! assert (string_fill_char (), orig_val);

%!assert ( [ [], {1} ], {1} )

%!error string_fill_char (1, 2)
*/

DEFMETHOD (PS4, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} PS4 ()
@deftypefnx {} {@var{old_val} =} PS4 (@var{new_val})
@deftypefnx {} {@var{old_val} =} PS4 (@var{new_val}, "local")
Query or set the character string used to prefix output produced
when echoing commands is enabled.

The default value is @qcode{"+ "}.
@xref{Diary and Echo Commands}, for a description of echoing commands.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{echo, PS1, PS2}
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.PS4 (args, nargout);
}

DEFMETHOD (echo, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} echo
@deftypefnx {} {} echo on
@deftypefnx {} {} echo off
@deftypefnx {} {} echo on all
@deftypefnx {} {} echo off all
@deftypefnx {} {} echo @var{function} on
@deftypefnx {} {} echo @var{function} off
Control whether commands are displayed as they are executed.

Valid options are:

@table @code
@item on
Enable echoing of commands as they are executed in script files.

@item off
Disable echoing of commands as they are executed in script files.

@item on all
Enable echoing of commands as they are executed in script files and
functions.

@item off all
Disable echoing of commands as they are executed in script files and
functions.

@item @var{function} on
Enable echoing of commands as they are executed in the named function.

@item @var{function} off
Disable echoing of commands as they are executed in the named function.
@end table

@noindent
With no arguments, @code{echo} toggles the current echo state.

@seealso{PS4}
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.echo (args, nargout);
}

/*
%!error echo ([])
%!error echo (0)
%!error echo ("")
%!error echo ("Octave")
%!error echo ("off", "invalid")
%!error echo ("on", "invalid")
%!error echo ("on", "all", "all")
*/

OCTAVE_END_NAMESPACE(octave)
