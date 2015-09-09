## Copyright (C) 2004-2015 John W. Eaton
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
## @deftypefn {Function File} {@var{r} =} bitmax (@var{precision})
##
## @code{bitmax} is deprecated and will be removed in Octave version 4.4.
## Use @code{flintmax (precision) - 1} for the equivalent functionality.
##
## Return the largest integer @var{r} that can be represented within a
## floating point value.
##
## The default class is @qcode{"double"}, but @qcode{"single"} is a valid
## option.  On IEEE 754 compatible systems, @code{bitmax} is
## @w{@math{2^{53} - 1}} for @qcode{"double"} and @w{@math{2^{24} - 1}} for
## @qcode{"single"}.
##
## @seealso{flintmax, intmax, realmax, realmin}
## @end deftypefn

## Deprecated in version 4.2

function r = bitmax (precision)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "bitmax is obsolete and will be removed from a future version of Octave, please use flintmax instead");
  endif

  if (nargin == 0)
    precision = "double";
  endif
  r = flintmax (precision) - 1;

endfunction
