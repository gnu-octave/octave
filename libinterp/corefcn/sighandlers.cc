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

#include <csignal>
#include <cstdlib>

#include <iostream>
#include <new>

#if defined (OCTAVE_USE_WINDOWS_API)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#endif

#include "cmd-edit.h"
#include "oct-syscalls.h"
#include "quit.h"
#include "singleton-cleanup.h"
#include "signal-wrappers.h"

#include "debug.h"
#include "defun.h"
#include "error.h"
#include "input.h"
#include "interpreter.h"
#include "load-save.h"
#include "octave.h"
#include "oct-map.h"
#include "pager.h"
#include "pt-bp.h"
#include "pt-eval.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

namespace octave
{
  // Nonzero means we have already printed a message for this series of
  // SIGPIPES.  We assume that the writer will eventually give up.
  int pipe_handler_error_count = 0;

  // TRUE means we can be interrupted.
  bool can_interrupt = false;

  // TRUE means we should try to enter the debugger on SIGINT.
  bool Vdebug_on_interrupt = false;

  // Allow users to avoid writing octave-workspace for SIGHUP (sent by
  // closing gnome-terminal, for example).  Note that this variable has
  // no effect if Vcrash_dumps_octave_core is FALSE.
  static bool Vsighup_dumps_octave_core = true;

  // Similar to Vsighup_dumps_octave_core, but for SIGTERM signal.
  static bool Vsigterm_dumps_octave_core = true;

  // List of signals we have caught since last call to octave::signal_handler.
  static bool *signals_caught = 0;

  // Forward declarations.
  static void user_terminate (int sig_number);
  static void user_abort (int sig_number);

  class
  base_interrupt_manager
  {
  public:

    base_interrupt_manager (void) { }

    // No copying!

    base_interrupt_manager (const base_interrupt_manager&) = delete;

    base_interrupt_manager& operator = (const base_interrupt_manager&) = delete;

    virtual ~base_interrupt_manager (void) = default;

    virtual void do_jump_to_enclosing_context (void) = 0;

    virtual void do_user_terminate (int sig_number) = 0;

    virtual void do_user_abort (int sig_number) = 0;

    virtual void do_raise_sigint (void) = 0;
  };

#if defined (OCTAVE_USE_WINDOWS_API)

  class
  w32_interrupt_manager : public base_interrupt_manager
  {
  public:

    w32_interrupt_manager (void)
      : thread (0), thread_id (0)
    {
      thread_id = GetCurrentThreadId ();

      DuplicateHandle (GetCurrentProcess (), GetCurrentThread (),
                       GetCurrentProcess (), &thread, 0, FALSE,
                       DUPLICATE_SAME_ACCESS);
    }

    // No copying!

    w32_interrupt_manager (const w32_interrupt_manager&) = delete;

    w32_interrupt_manager& operator = (const w32_interrupt_manager&) = delete;

    ~w32_interrupt_manager (void) = default;

    static void jump_to_enclosing_context_sync (void)
    {
#if defined (_MSC_VER)
      _fpreset ();
#endif
      ::octave_jump_to_enclosing_context ();
    }

    void do_jump_to_enclosing_context (void)
    {
      bool is_interrupt_thread = (GetCurrentThreadId () == thread_id);

      if (is_interrupt_thread)
        jump_to_enclosing_context_sync ();
      else
        {

          CONTEXT threadContext;

          SuspendThread (thread);
          threadContext.ContextFlags = CONTEXT_CONTROL;
          GetThreadContext (thread, &threadContext);

#if (defined (__MINGW64__) || defined (_WIN64))
          threadContext.Rip = (DWORD64) jump_to_enclosing_context_sync;
#else
          threadContext.Eip = (DWORD) jump_to_enclosing_context_sync;
#endif

          SetThreadContext (thread, &threadContext);

          ResumeThread (thread);
        }
    }

    void do_user_terminate (int sig_number)
    {
      bool is_interrupt_thread = (GetCurrentThreadId () == thread_id);

      if (is_interrupt_thread)
        octave::user_terminate (sig_number);
      else
        {
          SuspendThread (thread);
          octave::user_terminate (sig_number);
          ResumeThread (thread);
        }
    }

    void do_user_abort (int sig_number)
    {
      bool is_interrupt_thread = (GetCurrentThreadId () == thread_id);

      if (is_interrupt_thread)
        octave::user_abort (sig_number);
      else
        {
          SuspendThread (thread);
          octave::user_abort (sig_number);
          ResumeThread (thread);
        }
    }

    void do_raise_sigint (void)
    {
      bool is_interrupt_thread = (GetCurrentThreadId () == thread_id);

      if (is_interrupt_thread)
        octave_raise_wrapper (SIGINT);
      else
        {
          SuspendThread (thread);
          octave_raise_wrapper (SIGINT);
          ResumeThread (thread);
        }
    }

  private:

    // A handle to the thread that is running the octave interpreter.
    HANDLE thread;

    // The ID of the thread that is running the octave interpreter.
    DWORD thread_id;
  };

#endif

  class
  posix_interrupt_manager : public base_interrupt_manager
  {
  public:

    posix_interrupt_manager (void)
      : base_interrupt_manager ()
    { }

    // No copying!

    posix_interrupt_manager (const posix_interrupt_manager&) = delete;

    posix_interrupt_manager&
    operator = (const posix_interrupt_manager&) = delete;

    ~posix_interrupt_manager (void) = default;

    void do_jump_to_enclosing_context (void)
    {
      ::octave_jump_to_enclosing_context ();
    }

    void do_user_terminate (int sig_number)
    {
      octave::user_terminate (sig_number);
    }

    void do_user_abort (int sig_number)
    {
      octave::user_abort (sig_number);
    }

    void do_raise_sigint (void)
    {
      octave_raise_wrapper (SIGINT);
    }
  };

  class
  interrupt_manager
  {
  public:

    ~interrupt_manager (void) = default;

    static bool init (void) { return instance_ok (); }

    static void jump_to_enclosing_context (void)
    {
      if (instance_ok ())
        instance->do_jump_to_enclosing_context ();
    }

    static void user_terminate (int sig_number)
    {
      if (instance_ok ())
        instance->do_user_terminate (sig_number);
    }

    static void user_abort (int sig_number)
    {
      if (instance_ok ())
        instance->do_user_abort (sig_number);
    }

    static void raise_sigint (void)
    {
      if (instance_ok ())
        instance->do_raise_sigint ();
    }

  private:

    interrupt_manager (void) { }

    // No copying!

    interrupt_manager (const interrupt_manager&) = delete;

    interrupt_manager& operator = (const interrupt_manager&) = delete;

    static bool instance_ok (void)
    {
      bool retval = true;

      if (! instance)
        {
          instance = create_instance ();

          if (instance)
            singleton_cleanup_list::add (cleanup_instance);
        }

      if (! instance)
        error ("unable to create interrupt_manager");

      return retval;
    }

    static base_interrupt_manager *create_instance (void)
    {
#if defined (OCTAVE_USE_WINDOWS_API)
      return new w32_interrupt_manager ();
#else
      return new posix_interrupt_manager ();
#endif
    }

    static void cleanup_instance (void) { delete instance; instance = 0; }

    static base_interrupt_manager* instance;
  };

  base_interrupt_manager *interrupt_manager::instance = 0;

  static void
  my_friendly_exit (int sig, bool save_vars = true)
  {
    std::cerr << "fatal: caught signal "
              << octave_strsignal_wrapper (sig)
              << " -- stopping myself..." << std::endl;

    if (save_vars)
      dump_octave_core ();

    sysdep_cleanup ();

    throw octave::exit_exception (1);
  }

  // Called from octave_quit () to actually do something about the signals
  // we have caught.

  void
  signal_handler (void)
  {
    // The list of signals is relatively short, so we will just go
    // linearly through the list.

    static int sigchld;
    static int sigfpe;
    static int sighup;
    static int sigterm;
    static int sigpipe;

    static const bool have_sigchld = octave_get_sig_number ("SIGCHLD", &sigchld);
    static const bool have_sigfpe = octave_get_sig_number ("SIGFPE", &sigfpe);
    static const bool have_sighup = octave_get_sig_number ("SIGHUP", &sighup);
    static const bool have_sigterm = octave_get_sig_number ("SIGTERM", &sigterm);
    static const bool have_sigpipe = octave_get_sig_number ("SIGPIPE", &sigpipe);

    for (int i = 0; i < octave_num_signals (); i++)
      {
        if (signals_caught[i])
          {
            signals_caught[i] = false;

            if (have_sigchld && i == sigchld)
              {
                volatile interrupt_handler saved_interrupt_handler
                  = ignore_interrupts ();

                void *context = octave_block_child ();

                child_list::wait ();

                set_interrupt_handler (saved_interrupt_handler);

                octave_unblock_child (context);

                child_list::reap ();
              }
            else if (have_sigfpe && i == sigfpe)
              std::cerr << "warning: floating point exception" << std::endl;
            else if (have_sighup && i == sighup)
              my_friendly_exit (sighup, Vsighup_dumps_octave_core);
            else if (have_sigterm && i == sigterm)
              my_friendly_exit (sigterm, Vsigterm_dumps_octave_core);
            else if (have_sigpipe && i == sigpipe)
              std::cerr << "warning: broken pipe" << std::endl;
          }
      }
  }

  sig_handler *
  set_signal_handler (int sig, sig_handler *handler, bool restart_syscalls)
  {
    return octave_set_signal_handler_internal (sig, handler, restart_syscalls);
  }

  sig_handler *
  set_signal_handler (const char *signame, sig_handler *handler,
                      bool restart_syscalls)
  {
    return octave_set_signal_handler_by_name (signame, handler,
                                              restart_syscalls);
  }

  static void
  generic_sig_handler (int sig)
  {
    octave_signal_caught = 1;

    signals_caught[sig] = true;
  }

#if defined (__alpha__)
  static void
  sigfpe_handler (int sig)
  {
    if (can_interrupt && octave_interrupt_state >= 0)
      {
        octave_signal_caught = 1;

        signals_caught[sig] = true;

        octave_interrupt_state++;
      }
  }
#endif

  // Handle SIGHUP and SIGTERM.

  static void
  user_terminate (int sig_number)
  {
    if (! octave_initialized)
      exit (1);

    octave_signal_caught = 1;

    signals_caught[sig_number] = true;

    if (octave_interrupt_immediately)
      {
        // Try to get to a place where it is safe to throw an
        // exception.

        interrupt_manager::jump_to_enclosing_context ();
      }
  }

  // Handle SIGINT.
  //
  // This also has to work for SIGBREAK (on systems that have it), so we
  // use the value of sig, instead of just assuming that it is called
  // for SIGINT only.

  static void
  user_abort (int sig_number)
  {
    if (! octave_initialized)
      exit (1);

    if (can_interrupt)
      {
        if (Vdebug_on_interrupt)
          {
            if (! octave_debug_on_interrupt_state)
              {
                octave::tree_evaluator::debug_mode = true;
                octave_debug_on_interrupt_state = true;

                return;
              }
            else
              {
                // Clear the flag and do normal interrupt stuff.

                octave::tree_evaluator::debug_mode
                  = bp_table::have_breakpoints () || Vdebugging;
                octave_debug_on_interrupt_state = false;
              }
          }

        if (octave_interrupt_immediately)
          {
            if (octave_interrupt_state == 0)
              octave_interrupt_state = 1;

            // Try to get to a place where it is safe to throw an
            // exception.

            interrupt_manager::jump_to_enclosing_context ();
          }
        else
          {
            // If we are already cleaning up from a previous interrupt,
            // take note of the fact that another interrupt signal has
            // arrived.

            if (octave_interrupt_state < 0)
              octave_interrupt_state = 0;

            octave_signal_caught = 1;
            octave_interrupt_state++;

            if (octave::application::interactive ()
                && ! octave::application::forced_interactive ()
                && octave_interrupt_state == 2)
              std::cerr << "Press Control-C again to abort." << std::endl;

            if (octave_interrupt_state >= 3)
              my_friendly_exit (sig_number, true);
          }
      }
  }

  static void
  sigterm_handler (int sig)
  {
    interrupt_manager::user_terminate (sig);
  }

  static void
  sigint_handler (int sig)
  {
    interrupt_manager::user_abort (sig);
  }

  static void
  sigpipe_handler (int sig)
  {
    octave_signal_caught = 1;

    signals_caught[sig] = true;

    // Don't loop forever on account of this.

    if (pipe_handler_error_count++ > 100 && octave_interrupt_state >= 0)
      octave_interrupt_state++;
  }

  interrupt_handler
  catch_interrupts (void)
  {
    interrupt_handler retval;

    interrupt_manager::init ();

    retval.int_handler = set_signal_handler ("SIGINT", sigint_handler);
    retval.brk_handler = set_signal_handler ("SIGBREAK", sigint_handler);

    return retval;
  }

  interrupt_handler
  ignore_interrupts (void)
  {
    interrupt_handler retval;

    interrupt_manager::init ();

    retval.int_handler = set_signal_handler ("SIGINT", SIG_IGN);
    retval.brk_handler = set_signal_handler ("SIGBREAK", SIG_IGN);

    return retval;
  }

  interrupt_handler
  set_interrupt_handler (const volatile interrupt_handler& h,
                         bool restart_syscalls)
  {
    interrupt_handler retval;

    interrupt_manager::init ();

    retval.int_handler = set_signal_handler ("SIGINT", h.int_handler,
                                             restart_syscalls);

    retval.brk_handler = set_signal_handler ("SIGBREAK", h.brk_handler,
                                             restart_syscalls);

    return retval;
  }

  // Install all the handlers for the signals we might care about.

  void
  install_signal_handlers (void)
  {
    if (! signals_caught)
      signals_caught = new bool [octave_num_signals ()];

    for (int i = 0; i < octave_num_signals (); i++)
      signals_caught[i] = false;

    catch_interrupts ();

    set_signal_handler ("SIGABRT", generic_sig_handler);
    set_signal_handler ("SIGALRM", generic_sig_handler);
    set_signal_handler ("SIGBUS", generic_sig_handler);
    set_signal_handler ("SIGCHLD", generic_sig_handler);

    // SIGCLD
    // SIGCONT

    set_signal_handler ("SIGEMT", generic_sig_handler);

#if defined (__alpha__)
    set_signal_handler ("SIGFPE", sigfpe_handler);
#else
    set_signal_handler ("SIGFPE", generic_sig_handler);
#endif

    set_signal_handler ("SIGHUP", sigterm_handler);
    set_signal_handler ("SIGILL", generic_sig_handler);

    // SIGINFO
    // SIGINT

    set_signal_handler ("SIGIOT", generic_sig_handler);
    set_signal_handler ("SIGLOST", generic_sig_handler);
    set_signal_handler ("SIGPIPE", sigpipe_handler);
    set_signal_handler ("SIGPOLL", SIG_IGN);

    // SIGPROF
    // SIGPWR

    set_signal_handler ("SIGQUIT", generic_sig_handler);
    set_signal_handler ("SIGSEGV", generic_sig_handler);

    // SIGSTOP

    set_signal_handler ("SIGSYS", generic_sig_handler);
    set_signal_handler ("SIGTERM", sigterm_handler);
    set_signal_handler ("SIGTRAP", generic_sig_handler);

    // SIGTSTP
    // SIGTTIN
    // SIGTTOU
    // SIGURG

    set_signal_handler ("SIGUSR1", generic_sig_handler);
    set_signal_handler ("SIGUSR2", generic_sig_handler);
    set_signal_handler ("SIGVTALRM", generic_sig_handler);
    set_signal_handler ("SIGIO", SIG_IGN);

#if 0
    set_signal_handler ("SIGWINCH", sigwinch_handler);
#endif

    set_signal_handler ("SIGXCPU", generic_sig_handler);
    set_signal_handler ("SIGXFSZ", generic_sig_handler);
  }

  static void
  set_sig_struct_field (octave_scalar_map& m, const char *signame)
  {
    int signum;

    // The names in the struct do not include the leading "SIG" prefix.

    if (octave_get_sig_number (signame, &signum))
      m.assign (&signame[3], signum);
  }

  static octave_scalar_map
  make_sig_struct (void)
  {
    octave_scalar_map m;

    set_sig_struct_field (m, "SIGABRT");
    set_sig_struct_field (m, "SIGALRM");
    set_sig_struct_field (m, "SIGBUS");
    set_sig_struct_field (m, "SIGCHLD");
    set_sig_struct_field (m, "SIGCLD");
    set_sig_struct_field (m, "SIGCONT");
    set_sig_struct_field (m, "SIGEMT");
    set_sig_struct_field (m, "SIGFPE");
    set_sig_struct_field (m, "SIGHUP");
    set_sig_struct_field (m, "SIGILL");
    set_sig_struct_field (m, "SIGINFO");
    set_sig_struct_field (m, "SIGINT");
    set_sig_struct_field (m, "SIGIO");
    set_sig_struct_field (m, "SIGIOT");
    set_sig_struct_field (m, "SIGKILL");
    set_sig_struct_field (m, "SIGLOST");
    set_sig_struct_field (m, "SIGPIPE");
    set_sig_struct_field (m, "SIGPOLL");
    set_sig_struct_field (m, "SIGPROF");
    set_sig_struct_field (m, "SIGPWR");
    set_sig_struct_field (m, "SIGQUIT");
    set_sig_struct_field (m, "SIGSEGV");
    set_sig_struct_field (m, "SIGSTKFLT");
    set_sig_struct_field (m, "SIGSTOP");
    set_sig_struct_field (m, "SIGSYS");
    set_sig_struct_field (m, "SIGTERM");
    set_sig_struct_field (m, "SIGTRAP");
    set_sig_struct_field (m, "SIGTSTP");
    set_sig_struct_field (m, "SIGTTIN");
    set_sig_struct_field (m, "SIGTTOU");
    set_sig_struct_field (m, "SIGUNUSED");
    set_sig_struct_field (m, "SIGURG");
    set_sig_struct_field (m, "SIGUSR1");
    set_sig_struct_field (m, "SIGUSR2");
    set_sig_struct_field (m, "SIGVTALRM");
    set_sig_struct_field (m, "SIGWINCH");
    set_sig_struct_field (m, "SIGXCPU");
    set_sig_struct_field (m, "SIGXFSZ");

    return m;
  }
}

DEFUN (SIG, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} SIG ()
Return a structure containing Unix signal names and their defined values.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  static octave_scalar_map m = octave::make_sig_struct ();

  return ovl (m);
}

/*
%!assert (isstruct (SIG ()))
%!assert (! isempty (SIG ()))

%!error SIG (1)
*/

DEFUN (debug_on_interrupt, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} debug_on_interrupt ()
@deftypefnx {} {@var{old_val} =} debug_on_interrupt (@var{new_val})
@deftypefnx {} {} debug_on_interrupt (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave will try
to enter debugging mode when it receives an interrupt signal (typically
generated with @kbd{C-c}).

If a second interrupt signal is received before reaching the debugging mode,
a normal interrupt will occur.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{debug_on_error, debug_on_warning}
@end deftypefn */)
{
  return set_internal_variable (octave::Vdebug_on_interrupt, args, nargout,
                                "debug_on_interrupt");
}

/*
%!test
%! orig_val = debug_on_interrupt ();
%! old_val = debug_on_interrupt (! orig_val);
%! assert (orig_val, old_val);
%! assert (debug_on_interrupt (), ! orig_val);
%! debug_on_interrupt (orig_val);
%! assert (debug_on_interrupt (), orig_val);

%!error (debug_on_interrupt (1, 2))
*/

DEFUN (sighup_dumps_octave_core, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} sighup_dumps_octave_core ()
@deftypefnx {} {@var{old_val} =} sighup_dumps_octave_core (@var{new_val})
@deftypefnx {} {} sighup_dumps_octave_core (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave tries
to save all current variables to the file @file{octave-workspace} if it
receives a hangup signal.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return set_internal_variable (octave::Vsighup_dumps_octave_core,
                                args, nargout,
                                "sighup_dumps_octave_core");
}

/*
%!test
%! orig_val = sighup_dumps_octave_core ();
%! old_val = sighup_dumps_octave_core (! orig_val);
%! assert (orig_val, old_val);
%! assert (sighup_dumps_octave_core (), ! orig_val);
%! sighup_dumps_octave_core (orig_val);
%! assert (sighup_dumps_octave_core (), orig_val);

%!error (sighup_dumps_octave_core (1, 2))
*/

DEFUN (sigterm_dumps_octave_core, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} sigterm_dumps_octave_core ()
@deftypefnx {} {@var{old_val} =} sigterm_dumps_octave_core (@var{new_val})
@deftypefnx {} {} sigterm_dumps_octave_core (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave tries
to save all current variables to the file @file{octave-workspace} if it
receives a terminate signal.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return set_internal_variable (octave::Vsigterm_dumps_octave_core,
                                args, nargout,
                                "sigterm_dumps_octave_core");
}

/*
%!test
%! orig_val = sigterm_dumps_octave_core ();
%! old_val = sigterm_dumps_octave_core (! orig_val);
%! assert (orig_val, old_val);
%! assert (sigterm_dumps_octave_core (), ! orig_val);
%! sigterm_dumps_octave_core (orig_val);
%! assert (sigterm_dumps_octave_core (), orig_val);

%!error (sigterm_dumps_octave_core (1, 2))
*/
