/*

Copyright (C) 2002-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstring>

#include <iostream>
#include <new>

#include "quit.h"

void (*octave_signal_hook) (void) = nullptr;
void (*octave_interrupt_hook) (void) = nullptr;
void (*octave_bad_alloc_hook) (void) = nullptr;

void
octave_handle_signal (void)
{
  if (octave_signal_hook)
    octave_signal_hook ();

  if (octave_interrupt_state > 0)
    {
      octave_interrupt_state = -1;
      octave_throw_interrupt_exception ();
    }
}

void
clean_up_and_exit (int exit_status, bool /* safe_to_return */)
{
  exit (exit_status);
}

void
octave_throw_interrupt_exception (void)
{
  if (octave_interrupt_hook)
    octave_interrupt_hook ();

  throw octave::interrupt_exception ();
}

void
octave_throw_execution_exception (void)
{
  // FIXME: would a hook function be useful here?

  octave_exception_state = octave_exec_exception;

  throw octave::execution_exception ();
}

void
octave_throw_bad_alloc (void)
{
  if (octave_bad_alloc_hook)
    octave_bad_alloc_hook ();

  octave_exception_state = octave_alloc_exception;

  throw std::bad_alloc ();
}

void
octave_throw_exit_exception (int exit_status, int safe_to_return)
{
  octave_exception_state = octave_quit_exception;

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

  throw octave::exit_exception (exit_status, safe_to_return);

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif
}

void
octave_rethrow_exception (void)
{
  if (octave_interrupt_state)
    {
      octave_interrupt_state = -1;
      octave_throw_interrupt_exception ();
    }
  else
    {
      switch (octave_exception_state)
        {
        case octave_exec_exception:
          octave_throw_execution_exception ();
          break;

        case octave_alloc_exception:
          octave_throw_bad_alloc ();
          break;

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

        case octave_quit_exception:
          octave_throw_exit_exception (octave_exit_exception_status,
                                       octave_exit_exception_safe_to_return);
          break;

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif

        default:
          break;
        }
    }
}
