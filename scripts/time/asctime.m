########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{str} =} asctime (@var{tm_struct})
## Convert a time structure to a string using the following
## format: @qcode{"ddd mmm mm HH:MM:SS yyyy@backslashchar{}n"}.
##
## For example:
##
## @example
## @group
## asctime (localtime (time ()))
##      @result{} "Mon Feb 17 01:15:06 1997@backslashchar{}n"
## @end group
## @end example
##
## This is equivalent to @code{ctime (time ())}.
## @seealso{ctime, localtime, time}
## @end deftypefn

function str = asctime (tm_struct)

  if (nargin < 1)
    print_usage ();
  endif

  str = strftime ("%a %b %d %H:%M:%S %Y\n", tm_struct);

endfunction


%!test
%! t = time ();
%! assert (strcmp (asctime (localtime (t)), ctime (t)));

%!assert (asctime (localtime (time ()))(end), "\n")

%!error <Invalid call> asctime ()
