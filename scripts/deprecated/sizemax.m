## Copyright (C) 2019 Rik Wehbring
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
## @deftypefn {} {} sizemax ()
##
## @code{sizemax} is deprecated and will be removed in Octave version 8.
##
## Return the largest value allowed for the size of an array.
##
## If Octave is compiled with 64-bit indexing, the result is of class int64,
## otherwise it is of class int32.  The maximum array size is slightly smaller
## than the maximum value allowable for the relevant class as reported by
## @code{intmax}.
## @seealso{intmax}
## @end deftypefn

## FIXME: DEPRECATED: Remove in version 8.

function retval = sizemax (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "sizemax is obsolete and will be removed from a future version of Octave\n");
  endif

  if (nargin != 0)
    print_usage ();
  endif

  if (__have_feature__ ("ENABLE_64"))
    retval = intmax ("int64") - 1;
  else
    retval = intmax ("int32") - 1;
  endif

endfunction


%!assert (sizemax () >= (intmax ("int32") - 1))
%!error sizemax (0)
