## Copyright (C) 2018 Rik Wehbring
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
## @deftypefn {} {} toascii (@var{str})
##
## @code{toascii} is deprecated and will be removed in Octave version 6.
## Use @code{double} instead.
##
## Return ASCII representation of @var{str} in a matrix.
##
## For example:
##
## @example
## @group
## toascii ("ASCII"
##      @result{} [ 65, 83, 67, 73, 73 ]
## @end group
##
## @end example
## @seealso{double, char}
## @end deftypefn

## FIXME: DEPRECATED: Remove in version 6.

function retval = toascii (str)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "toascii is obsolete and will be removed from a future version of Octave, please use double instead");
  endif

  if (nargin != 1)
    print_usage ();
  endif

  if (iscell (str))
    retval = cellfun (@(x) bitand (double (x), 0x7F), str, "uniformoutput", 0);
  else
    retval = bitand (double (str), 0x7F);  # Restrict to 7-bit ASCII
  endif

endfunction


## First test is necessary to provoke 1-time legacy warning
%!test
%! warning ("off", "Octave:deprecated-function", "local");
%! toascii ("");

%!assert (toascii (char (0:127)), 0:127)
%!assert (toascii (" ":"@"), 32:64)
%!assert (toascii ("A":"Z"), 65:90)
%!assert (toascii ("[":"`"), 91:96)
%!assert (toascii ("a":"z"), 97:122)
%!assert (toascii ("{":"~"), 123:126)

%!error toascii ()
%!error toascii (1, 2)
