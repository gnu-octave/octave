/*

Copyright (C) 2003-2018 John W. Eaton

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

#include <signal.h>
#include <string.h>

#include "quit.h"

sig_atomic_t octave_interrupt_state = 0;

sig_atomic_t octave_exception_state = 0;

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

sig_atomic_t octave_exit_exception_status = 0;

sig_atomic_t octave_exit_exception_safe_to_return = 0;

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif

volatile sig_atomic_t octave_signal_caught = 0;
