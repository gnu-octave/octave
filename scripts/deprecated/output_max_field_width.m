## Copyright (C) 2018-2019 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{val} =} output_max_field_width ()
## @deftypefnx {} {@var{old_val} =} output_max_field_width (@var{new_val})
## @deftypefnx {} {} output_max_field_width (@var{new_val}, "local")
##
## @code{output_max_field_width} is deprecated and will be removed in Octave
## version 7.  Use @code{output_precision} instead.
##
## Query or set the internal variable that specifies the maximum width
## of a numeric output field.
##
## When called from inside a function with the @qcode{"local"} option, the
## variable is changed locally for the function and any subroutines it calls.
## The original variable value is restored when exiting the function.
## @seealso{format, fixed_point_format, output_precision}
## @end deftypefn

## FIXME: DEPRECATED: Remove in version 7.

function retval = output_max_field_width (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "output_max_field_width is obsolete and will be removed from a future version of Octave, please use output_precision instead\n");
  endif

  retval = 20;

endfunction
