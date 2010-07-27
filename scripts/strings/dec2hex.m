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
## @deftypefn {Function File} {} dec2hex (@var{n}, @var{len})
## Return the hexadecimal string corresponding to the non-negative 
## integer @var{n}.  For example:
##
## @example
## @group
## dec2hex (2748)
##      @result{} "ABC"
## @end group
## @end example
##
## If @var{n} is a vector, returns a string matrix, one row per value,
## padded with leading zeros to the width of the largest value.
##
## The optional second argument, @var{len}, specifies the minimum
## number of digits in the result.
## @seealso{hex2dec, dec2base, base2dec, bin2dec, dec2bin}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function retval = dec2hex (n, len)

  if (nargin == 1)
    retval = dec2base (n, 16);
  elseif (nargin == 2)
    retval = dec2base (n, 16, len);
  else
    print_usage ();
  endif

endfunction

%!assert(strcmp (tolower (dec2hex (2748)), "abc"));

%!error dec2hex ();

%!assert(strcmp (tolower (dec2hex (2748, 5)), "00abc"));

%!error dec2hex (1, 2, 3);

