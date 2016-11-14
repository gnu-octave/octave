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

/*

The signal blocking macros defined below were adapted from similar
functions from GNU Bash, the Bourne Again SHell, copyright (C) 1994
Free Software Foundation, Inc.

*/

// This file should always be included after config.h!

#if ! defined (octave_sighandlers_h)
#define octave_sighandlers_h 1

#include "octave-config.h"

#include "child-list.h"

namespace octave
{
  // This type must match the typedef in signal-wrappers.h.
  typedef void sig_handler (int);

  struct
  interrupt_handler
  {
    sig_handler *int_handler;
    sig_handler *brk_handler;
  };

  // Nonzero means we have already printed a message for this series of
  // SIGPIPES.  We assume that the writer will eventually give up.
  extern int pipe_handler_error_count;

  // TRUE means we can be interrupted.
  extern OCTINTERP_API bool can_interrupt;

  extern OCTINTERP_API sig_handler *
  set_signal_handler (int sig, sig_handler *h,
                      bool restart_syscalls = true);

  extern OCTINTERP_API sig_handler *
  set_signal_handler (const char *signame, sig_handler *h,
                      bool restart_syscalls = true);

  extern OCTINTERP_API void install_signal_handlers (void);

  extern OCTINTERP_API void signal_handler (void);

  extern OCTINTERP_API interrupt_handler catch_interrupts (void);

  extern OCTINTERP_API interrupt_handler ignore_interrupts (void);

  extern OCTINTERP_API interrupt_handler
  set_interrupt_handler (const volatile interrupt_handler& h,
                         bool restart_syscalls = true);

  // TRUE means we should try to enter the debugger on SIGINT.
  extern OCTINTERP_API bool Vdebug_on_interrupt;
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::interrupt_handler' instead")
typedef octave::interrupt_handler octave_interrupt_handler;

OCTAVE_DEPRECATED ("use 'octave::sig_handler' instead")
typedef octave::sig_handler octave_sig_handler;

OCTAVE_DEPRECATED ("use 'octave::pipe_handler_error_count' instead")
static auto& pipe_handler_error_count = octave::pipe_handler_error_count;

OCTAVE_DEPRECATED ("use 'octave::can_interrupt' instead")
static auto& can_interrupt = octave::can_interrupt;

OCTAVE_DEPRECATED ("use 'octave::set_signal_handler' instead")
inline octave::sig_handler *
octave_set_signal_handler (int sig, octave::sig_handler *handler,
                           bool restart_syscalls = true)
{
  return octave::set_signal_handler (sig, handler, restart_syscalls);
}

OCTAVE_DEPRECATED ("use 'octave::set_signal_handler' instead")
inline octave::sig_handler *
octave_set_signal_handler (const char *signame, octave::sig_handler *handler,
                           bool restart_syscalls = true)
{
  return octave::set_signal_handler (signame, handler, restart_syscalls);
}

OCTAVE_DEPRECATED ("use 'octave::set_signal_handler' instead")
const auto install_signal_handlers = octave::install_signal_handlers;

OCTAVE_DEPRECATED ("use 'octave::signal_handler' instead")
const auto octave_signal_handler = octave::signal_handler;

OCTAVE_DEPRECATED ("use 'octave::interrupt_handler' instead")
const auto octave_catch_interrupts = octave::catch_interrupts;

OCTAVE_DEPRECATED ("use 'octave::ignore_interrupts' instead")
const auto octave_ignore_interrupts = octave::ignore_interrupts;

OCTAVE_DEPRECATED ("use 'octave::set_interrupt_handler' instead")
const auto octave_set_interrupt_handler = octave::set_interrupt_handler;

OCTAVE_DEPRECATED ("use 'octave::Vdebug_on_interrupt' instead")
static auto& Vdebug_on_interrupt = octave::Vdebug_on_interrupt;

#endif

#endif

