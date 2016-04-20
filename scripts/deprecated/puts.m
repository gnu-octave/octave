## Copyright (C) 1993-2016 John W. Eaton
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
## @deftypefn  {} {} puts (@var{string})
## @deftypefnx {} {@var{numbytes} =} puts (@var{string})
##
## @code{puts} is deprecated and will be removed in Octave version 4.6.
## Use @code{fputs} for the equivalent functionality.
##
## Write a string to the standard output with no formatting.
##
## The string is written verbatim to the standard output.  Use @code{disp} to
## automatically append a newline character appropriate for the local machine.
##
## The optional output returns the number of bytes written to stdout.
##
## @seealso{fputs, disp}
## @end deftypefn

## Deprecated in version 4.2

function numbytes = puts (str)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "puts is obsolete and will be removed from a future version of Octave, please use fputs instead");
  endif

  numbytes = fputs (stdout (), str);

endfunction
