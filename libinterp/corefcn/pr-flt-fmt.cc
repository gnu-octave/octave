/*

Copyright (C) 1993-2018 John W. Eaton

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

#include "defun.h"
#include "error.h"
#include "pr-flt-fmt.h"
#include "variables.h"

// The precision of the numbers printed by the default output
// routines.
static int Voutput_precision = 5;

int
output_precision (void)
{
  return Voutput_precision;
}

void
set_output_prec (int prec)
{
  Voutput_precision = prec;
}

DEFUN (output_max_field_width, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} output_max_field_width
This function is obsolete and will be removed from a future version
of Octave.
@end deftypefn */)
{
  return octave_value (20);
}

DEFUN (output_precision, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} output_precision ()
@deftypefnx {} {@var{old_val} =} output_precision (@var{new_val})
@deftypefnx {} {} output_precision (@var{new_val}, "local")
Query or set the internal variable that specifies the minimum number of
significant figures to display for numeric output.

Note that regardless of the value set for @code{output_precision}, the
number of digits of precision displayed is limited to 16 for double
precision values and 7 for single precision values.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{format, fixed_point_format}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (output_precision, 0, 16);
}
