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
## @deftypefn {Function File} {} dec2bin (@var{n}, @var{len})
## Return a binary number corresponding to the non-negative decimal number
## @var{n}, as a string of ones and zeros.  For example:
##
## @example
## @group
## dec2bin (14)
##      @result{} "1110"
## @end group
## @end example
##
## If @var{n} is a vector, returns a string matrix, one row per value,
## padded with leading zeros to the width of the largest value.
##
## The optional second argument, @var{len}, specifies the minimum
## number of digits in the result.
## @seealso{bin2dec, dec2base, base2dec, hex2dec, dec2hex}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function retval = dec2bin (n, len)

  if (nargin == 1)
    retval = dec2base (n, 2);
  elseif (nargin == 2)
    retval = dec2base (n, 2, len);
  else
    print_usage ();
  endif

endfunction

%!assert(strcmp (dec2bin (14), "1110"));

%!error dec2bin ();

%!assert(strcmp (dec2bin (14, 6), "001110"));

%!error dec2bin (1, 2, 3);

