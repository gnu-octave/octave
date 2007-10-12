## Copyright (C) 2005, 2006, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} single (@var{val})
## Convert the numeric value @var{val} to single precision.
##
## @strong{Note}: this function currently returns its argument converted
## to double precision because Octave does not yet have a single-precision
## numeric data type.
## @end deftypefn

function retval = single (val)

  if (nargin == 1 && isnumeric (val))
    retval = double(val);
  else
    print_usage ();
  endif

endfunction
