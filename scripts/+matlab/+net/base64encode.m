########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn {} {@var{b64_str} =} matlab.net.base64encode (@var{in})
##
## Convert @var{in} to a base64 encoded string @var{b64_str}.
##
## The input @var{in} can be a string or numeric vector.
## The output @var{b64_str} will be encoded according to RFC 4648.
##
## @seealso{matlab.net.base64decode, base64_decode, base64_encode,
## unicode2native}
## @end deftypefn

function b64_str = base64encode (in)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isvector (in) || ! (isnumeric (in) || ischar (in)))
    error ("base64encode: IN must be a numeric or character vector");
  endif

  if (any (in != round (in)))
    error ("base64encode: IN must consist of integers");
  endif

  b64_str = base64_encode (uint8 (in));

endfunction


## Test char vector input
%!assert (matlab.net.base64encode (1), "AQ==")
%!assert (matlab.net.base64encode (255), "/w==")
%!assert (matlab.net.base64encode (1:3), "AQID")
%!assert (matlab.net.base64encode ("a"), "YQ==")
%!assert (matlab.net.base64encode ("abcdefg"), "YWJjZGVmZw==")

## Test input validation
%!error <Invalid call> matlab.net.base64encode ()
%!error <numeric or character vector> matlab.net.base64encode ({1,2})
%!error <numeric or character vector> matlab.net.base64encode ([1,2;3,4])
%!error <consist of integers> matlab.net.base64encode (pi)
