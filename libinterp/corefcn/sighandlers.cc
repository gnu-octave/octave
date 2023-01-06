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

#include <csignal>
#include <cstdlib>

#include <iostream>
#include <new>

#if defined (OCTAVE_USE_WINDOWS_API)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#endif

#include "child-list.h"
#include "cmd-edit.h"
#include "oct-syscalls.h"
#include "quit.h"
#include "signal-wrappers.h"

#include "defun.h"
#include "error.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-save.h"
#include "octave.h"
#include "oct-map.h"
#include "pager.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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

// Similar to Vsighup_dumps_octave_core, but for SIGQUIT signal.
static bool Vsigquit_dumps_octave_core = true;

// Similar to Vsighup_dumps_octave_core, but for SIGTERM signal.
static bool Vsigterm_dumps_octave_core = true;

// List of signals we have caught since last call to signal_handler.
static bool *signals_caught = nullptr;

static void
my_friendly_exit (int sig, bool save_vars = true)
{
  std::cerr << "fatal: caught signal "
            << octave_strsignal_wrapper (sig)
            << " -- stopping myself..." << std::endl;

  if (save_vars)
    {
      load_save_system& load_save_sys = __get_load_save_system__ ();

      load_save_sys.dump_octave_core ();
    }

  sysdep_cleanup ();

  throw exit_exception (1);
}

// Called from octave_quit () to actually do something about the signals
// we have caught.

void
respond_to_pending_signals (void)
{
  // The list of signals is relatively short, so we will just go
  // linearly through the list.

  // Interrupt signals are currently handled separately.

  static int sigint;
  static const bool have_sigint
    = octave_get_sig_number ("SIGINT", &sigint);

  static int sigbreak;
  static const bool have_sigbreak
    = octave_get_sig_number ("SIGBREAK", &sigbreak);

  // Termination signals.

  static int sighup;
  static const bool have_sighup
    = octave_get_sig_number ("SIGHUP", &sighup);

  static int sigquit;
  static const bool have_sigquit
    = octave_get_sig_number ("SIGQUIT", &sigquit);

  static int sigterm;
  static const bool have_sigterm
    = octave_get_sig_number ("SIGTERM", &sigterm);

  // Alarm signals.

  static int sigalrm;
  static const bool have_sigalrm
    = octave_get_sig_number ("SIGALRM", &sigalrm);

  static int sigvtalrm;
  static const bool have_sigvtalrm
    = octave_get_sig_number ("SIGVTALRM", &sigvtalrm);

  // I/O signals.

  static int sigio;
  static const bool have_sigio
    = octave_get_sig_number ("SIGIO", &sigio);

  static int siglost;
  static const bool have_siglost
    = octave_get_sig_number ("SIGLOST", &siglost);

  static int sigpipe;
  static const bool have_sigpipe
    = octave_get_sig_number ("SIGPIPE", &sigpipe);

  // Job control signals.

  static int sigchld;
  static const bool have_sigchld
    = octave_get_sig_number ("SIGCHLD", &sigchld);

  static int sigcld;
  static const bool have_sigcld
    = octave_get_sig_number ("SIGCLD", &sigcld);

  // Resource limit signals.

  static int sigxcpu;
  static const bool have_sigxcpu
    = octave_get_sig_number ("SIGXCPU", &sigxcpu);

  static int sigxfsz;
  static const bool have_sigxfsz
    = octave_get_sig_number ("SIGXFSZ", &sigxfsz);

  // User signals.

  static int sigusr1;
  static const bool have_sigusr1
    = octave_get_sig_number ("SIGUSR1", &sigusr1);

  static int sigusr2;
  static const bool have_sigusr2
    = octave_get_sig_number ("SIGUSR2", &sigusr2);

  child_list& kids = __get_child_list__ ();

  for (int sig = 0; sig < octave_num_signals (); sig++)
    {
      if (signals_caught[sig])
        {
          signals_caught[sig] = false;

          if ((have_sigchld && sig == sigchld)
              || (have_sigcld && sig == sigcld))
            {
              // FIXME: should we block or ignore?
              volatile interrupt_handler saved_interrupt_handler
                = ignore_interrupts ();

              void *context = octave_block_child ();

              kids.wait ();

              set_interrupt_handler (saved_interrupt_handler);

              octave_unblock_child (context);

              kids.reap ();
            }
          else if (have_sigpipe && sig == sigpipe)
            {
              std::cerr << "warning: broken pipe" << std::endl;

              // Don't loop forever on account of this.
              // FIXME: is this really needed?  Does it do anything
              // useful now?

              if (pipe_handler_error_count++ > 100
                  && octave_interrupt_state >= 0)
                octave_interrupt_state++;
            }
          else if (have_sighup && sig == sighup)
            my_friendly_exit (sighup, Vsighup_dumps_octave_core);
          else if (have_sigquit && sig == sigquit)
            my_friendly_exit (sigquit, Vsigquit_dumps_octave_core);
          else if (have_sigterm && sig == sigterm)
            my_friendly_exit (sigterm, Vsigterm_dumps_octave_core);
          else if ((have_sigalrm && sig == sigalrm)
                   || (have_sigvtalrm && sig == sigvtalrm)
                   || (have_sigio && sig == sigio)
                   || (have_siglost && sig == siglost)
                   || (have_sigxcpu && sig == sigxcpu)
                   || (have_sigxfsz && sig == sigxfsz)
                   || (have_sigusr1 && sig == sigusr1)
                   || (have_sigusr2 && sig == sigusr2))
            std::cerr << "warning: ignoring signal: "
                      << octave_strsignal_wrapper (sig)
                      << std::endl;
          else if ((have_sigint && sig == sigint)
                   || (have_sigbreak && sig == sigbreak))
            ; // Handled separately; do nothing.
          else
            std::cerr << "warning: ignoring unexpected signal: "
                      << octave_strsignal_wrapper (sig)
                      << std::endl;
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
  // FIXME: this function may execute in a separate signal handler or
  // signal watcher thread so it should probably be more careful about
  // how it accesses global objects.

  octave_signal_caught = 1;

  signals_caught[sig] = true;

  static int sigint;
  static const bool have_sigint
    = octave_get_sig_number ("SIGINT", &sigint);

  static int sigbreak;
  static const bool have_sigbreak
    = octave_get_sig_number ("SIGBREAK", &sigbreak);

  if ((have_sigint && sig == sigint)
      || (have_sigbreak && sig == sigbreak))
    {
      if (! octave_initialized)
        exit (1);

      if (can_interrupt)
        {
          octave_signal_caught = 1;
          octave_interrupt_state++;
        }
    }
}

static void
deadly_sig_handler (int sig)
{
  std::cerr << "fatal: caught signal "
            << octave_strsignal_wrapper (sig)
            << " -- stopping myself..." << std::endl;

  octave_set_default_signal_handler (sig);

  octave_raise_wrapper (sig);
}

static void
fpe_sig_handler (int)
{
  // FIXME: is there something better we can do?

  std::cerr << "warning: floating point exception" << std::endl;
}

interrupt_handler
catch_interrupts (void)
{
  interrupt_handler retval;

  retval.int_handler = set_signal_handler ("SIGINT", generic_sig_handler);
  retval.brk_handler = set_signal_handler ("SIGBREAK", generic_sig_handler);

  return retval;
}

interrupt_handler
ignore_interrupts (void)
{
  interrupt_handler retval;

  retval.int_handler = set_signal_handler ("SIGINT", SIG_IGN);
  retval.brk_handler = set_signal_handler ("SIGBREAK", SIG_IGN);

  return retval;
}

interrupt_handler
set_interrupt_handler (const volatile interrupt_handler& h,
                       bool restart_syscalls)
{
  interrupt_handler retval;

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

  // Interrupt signals.

  catch_interrupts ();

  // Program Error signals.  These are most likely unrecoverable for
  // us.

  set_signal_handler ("SIGABRT", deadly_sig_handler);
  set_signal_handler ("SIGBUS", deadly_sig_handler);
  set_signal_handler ("SIGEMT", deadly_sig_handler);
  set_signal_handler ("SIGILL", deadly_sig_handler);
  // SIGIOT is normally another name for SIGABRT.
  set_signal_handler ("SIGIOT", deadly_sig_handler);
  set_signal_handler ("SIGSEGV", deadly_sig_handler);
  set_signal_handler ("SIGSYS", deadly_sig_handler);
  set_signal_handler ("SIGTRAP", deadly_sig_handler);

  // Handle SIGFPE separately.

  set_signal_handler ("SIGFPE", fpe_sig_handler);

  // Handle other signals for which the default action is to terminate
  // the program.

  // Termination signals.

  set_signal_handler ("SIGHUP", generic_sig_handler);
  set_signal_handler ("SIGQUIT", generic_sig_handler);
  set_signal_handler ("SIGTERM", generic_sig_handler);

  // Alarm signals.

  set_signal_handler ("SIGALRM", generic_sig_handler);
  set_signal_handler ("SIGVTALRM", generic_sig_handler);

  // I/O signals.

  set_signal_handler ("SIGLOST", generic_sig_handler);
  set_signal_handler ("SIGPIPE", generic_sig_handler);

  // Job control signals.  We only recognize signals about child
  // processes.

  set_signal_handler ("SIGCHLD", generic_sig_handler);
  set_signal_handler ("SIGCLD", generic_sig_handler);

  // Resource limit signals.

  // FIXME: does it really make sense to try to handle the CPU limit
  // signal?

  set_signal_handler ("SIGXCPU", generic_sig_handler);
  set_signal_handler ("SIGXFSZ", generic_sig_handler);

  // User signals.

  set_signal_handler ("SIGUSR1", generic_sig_handler);
  set_signal_handler ("SIGUSR2", generic_sig_handler);

  // This does nothing on Windows systems.
  octave_create_interrupt_watcher_thread (generic_sig_handler);
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

DEFUN (SIG, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{S} =} SIG ()
Return a structure containing Unix signal names and their defined values.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  static octave_scalar_map m = make_sig_struct ();

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
@deftypefnx {} {@var{old_val} =} debug_on_interrupt (@var{new_val}, "local")
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
  return set_internal_variable (Vdebug_on_interrupt, args, nargout,
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

%!error debug_on_interrupt (1, 2)
*/

DEFUN (sighup_dumps_octave_core, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} sighup_dumps_octave_core ()
@deftypefnx {} {@var{old_val} =} sighup_dumps_octave_core (@var{new_val})
@deftypefnx {} {@var{old_val} =} sighup_dumps_octave_core (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave tries
to save all current variables to the file @file{octave-workspace} if it
receives a hangup signal.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return set_internal_variable (Vsighup_dumps_octave_core,
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

%!error sighup_dumps_octave_core (1, 2)
*/

DEFUN (sigquit_dumps_octave_core, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} sigquit_dumps_octave_core ()
@deftypefnx {} {@var{old_val} =} sigquit_dumps_octave_core (@var{new_val})
@deftypefnx {} {@var{old_val} =} sigquit_dumps_octave_core (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave tries
to save all current variables to the file @file{octave-workspace} if it
receives a quit signal.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return set_internal_variable (Vsigquit_dumps_octave_core,
                                args, nargout,
                                "sigquit_dumps_octave_core");
}

/*
%!test
%! orig_val = sigquit_dumps_octave_core ();
%! old_val = sigquit_dumps_octave_core (! orig_val);
%! assert (orig_val, old_val);
%! assert (sigquit_dumps_octave_core (), ! orig_val);
%! sigquit_dumps_octave_core (orig_val);
%! assert (sigquit_dumps_octave_core (), orig_val);

%!error sigquit_dumps_octave_core (1, 2)
*/

DEFUN (sigterm_dumps_octave_core, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} sigterm_dumps_octave_core ()
@deftypefnx {} {@var{old_val} =} sigterm_dumps_octave_core (@var{new_val})
@deftypefnx {} {@var{old_val} =} sigterm_dumps_octave_core (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave tries
to save all current variables to the file @file{octave-workspace} if it
receives a terminate signal.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return set_internal_variable (Vsigterm_dumps_octave_core,
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

%!error sigterm_dumps_octave_core (1, 2)
*/

OCTAVE_END_NAMESPACE(octave)
