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

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (output_precision, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} output_precision ()
@deftypefnx {} {@var{old_val} =} output_precision (@var{new_val})
@deftypefnx {} {@var{old_val} =} output_precision (@var{new_val}, "local")
Query or set the internal variable that specifies the minimum number of
significant figures to display for numeric output.

Note that regardless of the value set for @code{output_precision}, the
number of digits of precision displayed is limited to 16 for double
precision values and 7 for single precision values.  Also, calls to the
@code{format} function that change numeric display can also change the set
value for @code{output_precision}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{format, fixed_point_format}
@end deftypefn */)
{
  return set_internal_variable (Voutput_precision, args, nargout,
                                "output_precision", 0, 16);
}

OCTAVE_END_NAMESPACE(octave)
