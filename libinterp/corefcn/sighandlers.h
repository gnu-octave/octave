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

OCTAVE_BEGIN_NAMESPACE(octave)

// This type must match the typedef in signal-wrappers.h.
typedef void sig_handler (int);

struct interrupt_handler
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

extern OCTINTERP_API void respond_to_pending_signals (void);

extern OCTINTERP_API interrupt_handler catch_interrupts (void);

extern OCTINTERP_API interrupt_handler ignore_interrupts (void);

extern OCTINTERP_API interrupt_handler
set_interrupt_handler (const volatile interrupt_handler& h,
                       bool restart_syscalls = true);

// TRUE means we should try to enter the debugger on SIGINT.
extern OCTINTERP_API bool Vdebug_on_interrupt;

OCTAVE_END_NAMESPACE(octave)

#endif
