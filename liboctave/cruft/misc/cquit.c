/*

Copyright (C) 2003-2016 John W. Eaton

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

#include <signal.h>
#include <string.h>

#include "quit.h"

octave_jmp_buf current_context;

void
octave_save_current_context (void *save_buf)
{
  memcpy (save_buf, current_context, sizeof (octave_jmp_buf));
}

void
octave_restore_current_context (void *save_buf)
{
  memcpy (current_context, save_buf, sizeof (octave_jmp_buf));
}

void
octave_jump_to_enclosing_context (void)
{
#if defined (OCTAVE_HAVE_SIG_JUMP)
  siglongjmp (current_context, 1);
#else
  longjmp (current_context, 1);
#endif
}

sig_atomic_t octave_interrupt_immediately = 0;

sig_atomic_t octave_interrupt_state = 0;

sig_atomic_t octave_exception_state = 0;

sig_atomic_t octave_exit_exception_status = 0;

sig_atomic_t octave_exit_exception_safe_to_return = 0;

volatile sig_atomic_t octave_signal_caught = 0;

