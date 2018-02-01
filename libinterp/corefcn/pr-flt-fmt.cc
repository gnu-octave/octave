/*

Copyright (C) 1993-2017 John W. Eaton

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

// The maximum field width for a number printed by the default output
// routines.
static int Voutput_max_field_width = 10;

// The precision of the numbers printed by the default output
// routines.
static int Voutput_precision = 5;

int
output_max_field_width (void)
{
  return Voutput_max_field_width;
}

int
output_precision (void)
{
  return Voutput_precision;
}

void
set_output_prec_and_fw (int prec, int fw)
{
  Voutput_precision = prec;
  Voutput_max_field_width = fw;
}

DEFUN (output_max_field_width, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} output_max_field_width ()
@deftypefnx {} {@var{old_val} =} output_max_field_width (@var{new_val})
@deftypefnx {} {} output_max_field_width (@var{new_val}, "local")
Query or set the internal variable that specifies the maximum width
of a numeric output field.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{format, fixed_point_format, output_precision}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (output_max_field_width, 0,
                                            std::numeric_limits<int>::max ());
}

DEFUN (output_precision, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} output_precision ()
@deftypefnx {} {@var{old_val} =} output_precision (@var{new_val})
@deftypefnx {} {} output_precision (@var{new_val}, "local")
Query or set the internal variable that specifies the minimum number of
significant figures to display for numeric output.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{format, fixed_point_format, output_max_field_width}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (output_precision, -1,
                                            std::numeric_limits<int>::max ());
}
