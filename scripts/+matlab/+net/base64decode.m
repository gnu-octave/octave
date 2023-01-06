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
## @deftypefn {} {@var{out_vec} =} matlab.net.base64decode (@var{b64_str})
##
## Convert base64 encoded @var{b64_str} to uint8 vector @var{out_vec}.
##
## The input @var{b64_str} must be a string vector.
## The output @var{out_vec} will be a uint8 vector that is decoded
## according to RFC 4648.
##
## @seealso{matlab.net.base64encode, base64_decode, base64_encode,
## native2unicode}
## @end deftypefn

function out_vec = base64decode (b64_str)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isvector (b64_str) || ! ischar (b64_str))
    error ("base64decode: B64_STR must be a base64 encoded character vector");
  endif

  out_vec = uint8 (__base64_decode_bytes__ (b64_str));

endfunction


## Test char vector input
%!assert (matlab.net.base64decode ("AQ=="), uint8 (1))
%!assert (matlab.net.base64decode ("/w=="), uint8 (255))
%!assert (matlab.net.base64decode ("AQID"), uint8 (1:3))
%!assert (matlab.net.base64decode ("YQ=="), uint8 ("a"))
%!assert (matlab.net.base64decode ("YWJjZGVmZw=="), uint8 ("abcdefg"))

## Test input validation
%!error <Invalid call> matlab.net.base64decode ()
%!error <character vector> matlab.net.base64decode (pi)
%!error <character vector> matlab.net.base64decode ({1,2})
%!error <character vector> matlab.net.base64decode ([1,2;3,4])
