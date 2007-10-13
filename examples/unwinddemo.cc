/*

Copyright (C) 2007 John W. Eaton

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

#include <octave/oct.h>
#include <octave/unwind-prot.h>

void
err_hand (const char *fmt, ...)
{
  // Do nothing!!
}

DEFUN_DLD (unwinddemo, args, nargout, "Unwind Demo")
{
  int nargin = args.length();
  octave_value retval;
  if (nargin < 2)
    print_usage ();
  else
    {
      NDArray a = args(0).array_value ();
      NDArray b = args(1).array_value ();

      if (! error_state)
        {
          unwind_protect::begin_frame ("Funwinddemo");
          unwind_protect_ptr (current_liboctave_warning_handler);
          set_liboctave_warning_handler(err_hand);
          retval = octave_value (quotient (a, b));
          unwind_protect::run_frame ("Funwinddemo");
        }
    }
  return retval;
}
