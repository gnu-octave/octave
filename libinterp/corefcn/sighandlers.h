/*

Copyright (C) 1993-2015 John W. Eaton

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

// FIXME: the data should probably be private...

typedef void octave_sig_handler (int);

struct
octave_interrupt_handler
{
  octave_sig_handler *int_handler;
  octave_sig_handler *brk_handler;
};

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
extern int pipe_handler_error_count;

// TRUE means we can be interrupted.
extern OCTINTERP_API bool can_interrupt;

extern OCTINTERP_API octave_sig_handler *
octave_set_signal_handler (int sig, octave_sig_handler *,
                           bool restart_syscalls = true);

extern OCTINTERP_API octave_sig_handler *
octave_set_signal_handler (const char *signame, octave_sig_handler *,
                           bool restart_syscalls = true);

extern OCTINTERP_API void install_signal_handlers (void);

extern OCTINTERP_API void octave_signal_handler (void);

extern OCTINTERP_API octave_interrupt_handler octave_catch_interrupts (void);

extern OCTINTERP_API octave_interrupt_handler octave_ignore_interrupts (void);

extern OCTINTERP_API octave_interrupt_handler
octave_set_interrupt_handler (const volatile octave_interrupt_handler&,
                              bool restart_syscalls = true);

// extern void ignore_sigchld (void);

// TRUE means we should try to enter the debugger on SIGINT.
extern OCTINTERP_API bool Vdebug_on_interrupt;

#endif
