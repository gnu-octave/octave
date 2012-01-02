## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn {Function File} {} swapbytes (@var{x})
## Swap the byte order on values, converting from little endian to big
## endian and vice versa.  For example:
##
## @example
## @group
## swapbytes (uint16 (1:4))
## @result{} [   256   512   768  1024]
## @end group
## @end example
##
## @seealso{typecast, cast}
## @end deftypefn

function y = swapbytes (x)

  if (nargin != 1)
    print_usage ();
  endif

  clx = class (x);
  if (strcmp (clx, "int8") || strcmp (clx, "uint8") || isempty (x))
    y = x;
  else
    if (strcmp (clx, "int16") || strcmp (clx, "uint16"))
      nb = 2;
    elseif (strcmp (clx, "int32") || strcmp (clx, "uint32"))
      nb = 4;
    elseif (strcmp (clx, "int64") || strcmp (clx, "uint64")
            || strcmp (clx, "double"))
      nb = 8;
    else
      error ("swapbytes: invalid class of object");
    endif
    y = reshape (typecast (reshape (typecast (x(:), "uint8"), nb, numel (x))
                           ([nb : -1 : 1], :) (:), clx), size(x));
  endif

endfunction


%!assert (double (swapbytes (uint16 (1:4))), [256 512 768 1024])
%!error (swapbytes ())
%!error (swapbytes (1, 2))

