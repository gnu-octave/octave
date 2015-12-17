## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn {} {} usleep (@var{microseconds})\n\
##
## @code{usleep} is deprecated and will be removed in Octave version 4.6.
## Use @code{pause} instead.
##
## Suspend the execution of the program for the given number of
## microseconds (1e-6 seconds).
##
## @seealso{pause}
## @end deftypefn

function usleep (microseconds)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "usleep is obsolete and will be removed from a future version of Octave, please use pause instead");
  endif

  if (nargin == 1)
    pause (microseconds / 1e6);
  else
    print_usage ();
  endif

endfunction

%!test
%! usleep (1000);

%!error (usleep ())
%!error (usleep (1, 2))
