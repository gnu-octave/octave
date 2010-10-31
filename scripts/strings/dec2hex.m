## Copyright (C) 1996, 1999, 2000, 2001, 2003, 2005, 2006, 2007, 2008, 2009
##               Daniel Calvelo
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
## @deftypefn {Function File} {} dec2hex (@var{d}, @var{len})
## Return the hexadecimal string corresponding to the non-negative 
## integer @var{d}.  For example:
##
## @example
## @group
## dec2hex (2748)
##      @result{} "ABC"
## @end group
## @end example
##
## If @var{d} is a vector, return a string matrix, one row per value,
## padded with leading zeros to the width of the largest value.
##
## The optional second argument, @var{len}, specifies the minimum
## number of digits in the result.
## @seealso{hex2dec, dec2base, dec2bin}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function h = dec2hex (d, len)

  if (nargin == 1)
    h = dec2base (d, 16);
  elseif (nargin == 2)
    h = dec2base (d, 16, len);
  else
    print_usage ();
  endif

endfunction

%!assert(strcmpi (dec2hex (2748), "abc"));
%!assert(strcmpi (dec2hex (2748, 5), "00abc"));

%% Test input validation
%!error dec2hex ();
%!error dec2hex (1, 2, 3);

