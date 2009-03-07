## Copyright (C) 1996, 1997, 1998, 2000, 2005, 2006, 2007, 2008 John W. Eaton
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
## @deftypefn {Function File} {} logical (@var{arg})
## Convert @var{arg} to a logical value.  For example,
##
## @example
## logical ([-1, 0, 1])
## @end example
##
## @noindent
## is equivalent to
##
## @example
## [-1, 0, 1] != 0
## @end example
## @end deftypefn

## Author: jwe

function y = logical (x)

  if (nargin == 1)
    if (islogical (x))
      y = x;
    elseif (isempty (x))
      y = zeros (size (x), "logical");
    elseif (isnumeric (x))
      if (any (isnan (x(:))))
	error ("invalid conversion from NaN to logical");
      else
	y = x != 0;
      endif
    else
      error ("logical not defined for type `%s'", typeinfo (x));
    endif
  else
    print_usage ();
  endif

endfunction

%!assert (logical ([]), zeros ([0, 0], "logical"));
%!assert (logical (zeros (2, 0)), zeros ([2, 0], "logical"));
%!assert (logical (0), false);
%!assert (logical (13), true);
%!assert (logical (-13), true);
%!assert (logical (int8 (13)), true);
%!assert (logical (int8 (-13)), true);
%!assert (logical ([-1, 0, 1, Inf]), [-1, 0, 1, Inf] != 0);
%!error (logical ([-1, 0, 1, NaN, Inf]))
%!error (logical (NaN))
